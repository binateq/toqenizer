use std::ops::{BitOr, Shr, Range};

type Predicate = fn(char) -> bool;

#[derive(Debug, PartialEq, Clone)]
pub enum Element<'a> {
    Char(char),
    Predicate(Predicate),
    String(&'a str),
    Repeat(Box<Element<'a>>, Range<u32>),
    Sequence(Vec<Box<Element<'a>>>),
    Or(Box<Element<'a>>, Box<Element<'a>>),
    Eof
    // TODO: Skip
    // TODO: Case Insensitive
    // TODO: Convert strings like \x0A to '\n' or \u0041 to 'A'
}

pub trait Elem<'a> {
    fn elem(self) -> Element<'a>;
}

impl<'a> Elem<'a> for char {
    fn elem(self) -> Element<'a> {
        Element::Char(self)
    }
}

impl<'a> Elem<'a> for &'a str {
    fn elem(self) -> Element<'a> {
        Element::String(self)
    }
}

impl<'a> Elem<'a> for Vec<Element<'a>> {
    fn elem(self) -> Element<'a> {
        let mut vector = Vec::new();

        for element in self.into_iter() {
            vector.push(Box::new(element))
        }

        Element::Sequence(vector)
    }
}

#[cfg(test)]
mod elem_should {
    use super::{Element, Elem};

    #[test]
    fn make_char_element() {
        assert_eq!(Element::Char('a'), 'a'.elem());
    }

    #[test]
    fn make_string_element() {
        assert_eq!(Element::String("foo"), "foo".elem());
    }

    #[test]
    fn make_sequence_element() {
        let expected = Element::Sequence(vec![
            Box::new(Element::Char('a')),
            Box::new(Element::String("foo"))]);
        assert_eq!(expected, vec!['a'.elem(), "foo".elem()].elem());
    }
}

// TODO: Try to implement Tok for Predicate. Now I get the compiler error when I'm using tok().
// impl<'a> Tok<'a> for Predicate {
//     fn tok(self) -> TokenBuilder<'a> {
//         TokenBuilder::Predicate(self)
//     }
// }

pub fn p(predicate: Predicate) -> Element<'static> {
    Element::Predicate(predicate)
}

impl<'a> Element<'a> {
    pub fn rep(self, range: Range<u32>) -> Self {
        Element::Repeat(Box::new(self), range)
    }

    pub fn rep0(self) -> Self {
        self.rep(0..u32::MAX)
    }

    pub fn rep1(self) -> Self {
        self.rep(1..u32::MAX)
    }

    pub fn opt(self) -> Self {
        self.rep(0..1)
    }
}

#[cfg(test)]
mod regex_should {
    use super::{Element, Elem};

    #[test]
    fn make_repeat_3_4_when_rep_min_is_3_and_max_is_4() {
        let actual = "abc".elem().rep(3..4);
        let expected = Element::Repeat(Box::new(Element::String("abc")), 3..4);

        assert_eq!(expected, actual);
    }

    
    #[test]
    fn make_repeat_4_3_when_rep_min_is_4_and_max_is_3() {
        let actual = "abc".elem().rep(4..3);
        let expected = Element::Repeat(Box::new(Element::String("abc")), 4..3);

        assert_eq!(expected, actual);
    }

    #[test]
    fn make_repeat_0_1_when_opt() {
        let actual = 'a'.elem().opt();
        let expected = Element::Repeat(Box::new(Element::Char('a')), 0..1);

        assert_eq!(expected, actual);
    }
}

impl<'a> BitOr<Element<'a>> for Element<'a> {
    type Output = Element<'a>;

    fn bitor(self, rhs: Element<'a>) -> Element<'a> {
        Element::Or(Box::new(self), Box::new(rhs))
    }
}

#[cfg(test)]
mod regex_or_should {
    use super::{Elem, Element};

    #[test]
    fn make_or_char_a_char_b () {
        let actual ='a'.elem() | 'b'.elem();
        let expected = Element::Or(Box::new(Element::Char('a')), Box::new(Element::Char('b')));

        assert_eq!(expected, actual);
    }
}

#[allow(non_upper_case_globals)]
pub const eof: Element = Element::Eof;

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
    builder: Element<'a>,
    mapper: Mapper<Token>
}

#[derive(Debug, PartialEq)]
pub enum Rule<'a, Token> {
    Single(Tokenizer<'a, Token>),
    Multiple(Vec<Tokenizer<'a, Token>>)
}

impl<'a, Token> Shr<Mapper<Token>> for Element<'a> {
    type Output = Rule<'a, Token>;
    
    fn shr(self, rhs: Mapper<Token>) -> Self::Output {
        Rule::Single(Tokenizer {
            builder: self,
            mapper: rhs
        })
    }
}

impl<'a, Token> BitOr<Rule<'a, Token>> for Rule<'a, Token> {
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
    use super::{p, Rule, Tokenizer, Elem};

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
        let identifier = vec![p(|c| c.is_alphabetic()), p(|c| c.is_alphanumeric()).rep0()].elem();
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

pub mod stream;
pub mod parser;