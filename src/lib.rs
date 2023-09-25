use std::ops;
use std::str::CharIndices;

type Predicate = fn(char) -> bool;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenBuilder<'a> {
    Char(char),
    Predicate(Predicate),
    String(&'a str),
    Repeat(Box<TokenBuilder<'a>>, usize, Option<usize>),
    And(Box<TokenBuilder<'a>>, Box<TokenBuilder<'a>>),
    Or(Box<TokenBuilder<'a>>, Box<TokenBuilder<'a>>),
    Eof
    // TODO: Skip
    // TODO: Case Insensitive
    // TODO: Convert strings like \x0A to '\n' or \u0041 to 'A'
}

pub trait Tok<'a> {
    fn tok(self) -> TokenBuilder<'a>;
}

impl<'a> Tok<'a> for char {
    fn tok(self) -> TokenBuilder<'a> {
        TokenBuilder::Char(self)
    }
}

impl<'a> Tok<'a> for &'a str {
    fn tok(self) -> TokenBuilder<'a> {
        TokenBuilder::String(self)
    }
}

// TODO: Try to implement Tok for Predicate. Now I get the compiler error when I'm using tok().
// impl<'a> Tok<'a> for Predicate {
//     fn tok(self) -> TokenBuilder<'a> {
//         TokenBuilder::Predicate(self)
//     }
// }

pub fn p(predicate: Predicate) -> TokenBuilder<'static> {
    TokenBuilder::Predicate(predicate)
}

impl<'a> TokenBuilder<'a> {
    pub fn rep(self, min: usize, max: Option<usize>) -> Self {
        if let Some(max) = max {
            if min > max {
                panic!("regex_repeat: min parameters can't be greater than max")
            }
        }
        
        TokenBuilder::Repeat(Box::new(self), min, max)
    }

    pub fn rep0(self) -> Self {
        self.rep(0, None)
    }

    pub fn rep1(self) -> Self {
        self.rep(1, None)
    }

    pub fn opt(self) -> Self {
        self.rep(0, Some(1))
    }
}

#[cfg(test)]
mod regex_should {
    use super::{TokenBuilder, Tok};

    #[test]
    fn make_repeat_3_4_when_rep_min_is_3_and_max_is_4() {
        let actual = "abc".tok().rep(3, Some(4));
        let expected = TokenBuilder::Repeat(Box::new(TokenBuilder::String("abc")), 3, Some(4));

        assert_eq!(expected, actual);
    }

    
    #[test]
    #[should_panic]
    fn panic_when_rep_min_is_4_and_max_is_3() {
        let _ = "abc".tok().rep(4, Some(3));
    }

    #[test]
    fn make_repeat_0_1_when_opt() {
        let actual = 'a'.tok().opt();
        let expected = TokenBuilder::Repeat(Box::new(TokenBuilder::Char('a')), 0, Some(1));

        assert_eq!(expected, actual);
    }
}

impl<'a> ops::BitAnd<TokenBuilder<'a>> for TokenBuilder<'a> {
    type Output = TokenBuilder<'a>;

    fn bitand(self, rhs: TokenBuilder<'a>) -> TokenBuilder<'a> {
        TokenBuilder::And(Box::new(self), Box::new(rhs))
    }
}

impl<'a> ops::BitOr<TokenBuilder<'a>> for TokenBuilder<'a> {
    type Output = TokenBuilder<'a>;

    fn bitor(self, rhs: TokenBuilder<'a>) -> TokenBuilder<'a> {
        TokenBuilder::And(Box::new(self), Box::new(rhs))
    }
}

#[cfg(test)]
mod regex_and_should {
    use super::{Tok, TokenBuilder};

    #[test]
    fn make_and_char_a_char_b () {
        let actual ='a'.tok() & 'b'.tok();
        let expected = TokenBuilder::And(Box::new(TokenBuilder::Char('a')), Box::new(TokenBuilder::Char('b')));

        assert_eq!(expected, actual);
    }
}

#[allow(non_upper_case_globals)]
const eof: TokenBuilder = TokenBuilder::Eof;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Position {
    line: usize,
    column: usize
}

impl Position {
    fn new() -> Self {
        Position { line: 1, column: 1 }
    }

    fn inc_line(&mut self) {
        self.line += 1;
        self.column = 1;
    }

    fn inc_column(&mut self) {
        self.column += 1;
    }
}

#[cfg(test)]
mod position_should {
    use super::Position;

    #[test]
    fn make_position_1_1_when_new() {
        let actual = Position::new();
        let expected = Position { line: 1, column: 1 };

        assert_eq!(expected, actual);
    }

    #[test]
    fn make_position_3_1_from_2_100_when_inc_line() {
        let mut actual = Position { line: 2, column: 100 };
        actual.inc_line();
        
        let expected = Position { line: 3, column: 1 };

        assert_eq!(expected, actual);
    }

    #[test]
    fn make_position_2_101_from_2_100_when_inc_column() {
        let mut actual = Position { line: 2, column: 100 };
        actual.inc_column();
        
        let expected = Position { line: 2, column: 101 };

        assert_eq!(expected, actual);
    }
}

pub trait CharStream {
    fn peek(&self) -> Option<char>;

    fn next(&mut self);

    fn position(&self) -> Position;

    fn store_state(&mut self);

    fn restore_state(&mut self);

    fn discard_state(&mut self);
}

pub struct StringCharStream<'a> {
    items: CharIndices<'a>,
    next: Option<char>,
    position: Position,
    states: Vec<(Option<char>, CharIndices<'a>)>
}

impl<'a> StringCharStream<'a> {
    pub fn new(s: &'a str) -> Self {
        let mut items = s.char_indices();
        let next = items.next().map(|(_, c)| c);
        StringCharStream {
            items,
            next,
            position: Position::new(),
            states: Vec::new()
        }
    }
}

impl<'a> CharStream for StringCharStream<'a> {
    fn peek(&self) -> Option<char> {
        self.next 
    }

    fn next(&mut self) {
        self.next = self.items.next().map(|(_, c)| c);

        if let Some(next) = self.next {
            if next == '\n' || next == '\r' {
                self.position.inc_line();
            } else {
                self.position.inc_column();
            }
        }
    }

    fn position(&self) -> Position {
        self.position
    }

    fn store_state(&mut self) {
        self.states.push((self.next, self.items.clone()));
    }

    fn restore_state(&mut self) {
        if let Some((next, items)) = self.states.pop() {
            self.next = next;
            self.items = items;
        } else {
            panic!("restore_state: stack is empty")
        }
    }

    fn discard_state(&mut self) {
        if self.states.pop().is_none() {
            panic!("discard_state: stack is empty")
        }
    }
}

#[cfg(test)]
mod string_char_stream_should {
    use super::{StringCharStream, CharStream};

    #[test]
    fn return_ab_from_string_ab() {
        let mut stream = StringCharStream::new("ab");

        assert_eq!(Some('a'), stream.peek());
        stream.next();

        assert_eq!(Some('b'), stream.peek());
        stream.next();

        assert_eq!(None, stream.peek());
    }

    #[test]
    fn restore_state_after_store() {
        let mut stream = StringCharStream::new("ab");
        assert_eq!(0, stream.states.len());
        assert_eq!(Some('a'), stream.peek());

        stream.store_state();
        stream.next();
        assert_eq!(1, stream.states.len());
        assert_eq!(Some('b'), stream.peek());

        stream.restore_state();
        assert_eq!(0, stream.states.len());
        assert_eq!(Some('a'), stream.peek());
    }

    #[test]
    fn discard_state_after_store() {
        let mut stream = StringCharStream::new("ab");
        assert_eq!(0, stream.states.len());
        assert_eq!(Some('a'), stream.peek());

        stream.store_state();
        stream.next();
        assert_eq!(1, stream.states.len());
        assert_eq!(Some('b'), stream.peek());

        stream.discard_state();
        assert_eq!(0, stream.states.len());
        assert_eq!(Some('b'), stream.peek());
    }

    #[test]
    #[should_panic]
    fn panic_when_discard_without_store() {
        let mut stream = StringCharStream::new("ab");
        
        stream.discard_state();
    }

    #[test]
    #[should_panic]
    fn panic_when_restore_without_store() {
        let mut stream = StringCharStream::new("ab");
        
        stream.restore_state();
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Error {
    UnexpectedEof,
    ExpectEof,
    ExpectChar,
    UnrecognizedChar
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct ParseError {
    error: Error,
    position: Position
}

pub type Mapper<Token> = fn(String) -> Token;

#[derive(Debug, PartialEq)]
pub struct Tokenizer<'a, Token> {
    builder: TokenBuilder<'a>,
    mapper: Mapper<Token>
}

#[derive(Debug, PartialEq)]
pub enum Rule<'a, Token> {
    Single(Tokenizer<'a, Token>),
    Multiple(Vec<Tokenizer<'a, Token>>)
}

impl<'a, Token> ops::Shr<Mapper<Token>> for TokenBuilder<'a> {
    type Output = Rule<'a, Token>;
    
    fn shr(self, rhs: Mapper<Token>) -> Self::Output {
        Rule::Single(Tokenizer {
            builder: self,
            mapper: rhs
        })
    }
}

impl<'a, Token> ops::BitOr<Rule<'a, Token>> for Rule<'a, Token> {
    type Output = Rule<'a, Token>;

    fn bitor(self, rhs: Rule<'a, Token>) -> Rule<'a, Token> {
        match (self, rhs) {
            (Rule::Single(tokenizer1), Rule::Single(tokenizer2)) =>
                Rule::Multiple(vec![tokenizer1, tokenizer2]),
            (Rule::Single(tokenizer1), Rule::Multiple(tokenizers2)) => {
                let mut result = vec![tokenizer1];
                result.extend(tokenizers2);
                
                Rule::Multiple(result)
            },
            (Rule::Multiple(mut tokenizers1), Rule::Single(tokenizer2)) => {
                tokenizers1.push(tokenizer2);

                Rule::Multiple(tokenizers1)
            },
            (Rule::Multiple(mut tokenizers1), Rule::Multiple(tokenizers2)) => {
                tokenizers1.extend(tokenizers2);

                Rule::Multiple(tokenizers1)
            }
        }
    }
}

#[cfg(test)]
mod tokenizer_should {
    use super::{p, Rule, Tokenizer};

    #[derive(Debug, PartialEq)]
    enum Token {
        Identifier(String),
        Integer(u32)
    }

    fn make_identifier(name: String) -> Token {
        Token::Identifier(name)
    }

    fn make_integer(value: String) -> Token {
        Token::Integer(u32::from_str_radix(&value, 10).unwrap())
    }

    #[test]
    fn make_multiple_rules() {
        let identifier = p(|c| c.is_alphabetic()) & p(|c| c.is_alphanumeric()).rep0();
        let integer = p(|c| c.is_digit(10)).rep1();

        let actual
            = identifier.clone() >> make_identifier
            | integer.clone() >> make_integer;
        let expected = Rule::Multiple(vec![
            Tokenizer {
                builder: identifier,
                mapper: make_identifier
            },
            Tokenizer {
                builder: integer,
                mapper: make_integer
            }
        ]);

        assert_eq!(expected, actual);
    }
}

pub mod parser;