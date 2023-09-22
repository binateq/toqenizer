use std::str::CharIndices;

#[derive(Debug, PartialEq)]
pub enum Regex<'a> {
    Char(char),
    String(&'a str),
    Repeat(Box<Regex<'a>>, usize, Option<usize>),
    And(Box<Regex<'a>>, Box<Regex<'a>>),
    Or(Box<Regex<'a>>, Box<Regex<'a>>),
    Eof
}

pub fn char(c: char) -> Regex<'static> {
    Regex::Char(c)
}

pub fn string(s: &str) -> Regex {
    Regex::String(s)
}

pub fn repeat(parser: Regex, min: usize, max: Option<usize>) -> Regex {
    if let Some(max) = max {
        if min > max {
            panic!("repeat: min parameters can't be greaater than max")
        }
    }
    
    Regex::Repeat(Box::new(parser), min, max)
}

#[cfg(test)]
mod repeat_should {
    use crate::{repeat, string, Regex};

    #[test]
    fn make_repeat_3_4_when_min_is_3_and_max_is_4() {
        let actual = repeat(string("abc"), 3, Some(4));
        let expected = Regex::Repeat(Box::new(Regex::String("abc")), 3, Some(4));

        assert_eq!(expected, actual);
    }

    
    #[test]
    #[should_panic]
    fn panic_when_min_is_4_and_max_is_3() {
        let _ = repeat(string("abc"), 4, Some(3));
    }
}

pub fn optional(parser: Regex) -> Regex {
    repeat(parser, 0, Some(1))
}

#[cfg(test)]
mod optional_should {
    use super::{char, optional, Regex};

    #[test]
    fn make_repeat_0_1() {
        let actual = optional(char('a'));
        let expected = Regex::Repeat(Box::new(Regex::Char('a')), 0, Some(1));

        assert_eq!(expected, actual);
    }
}

pub fn repeat0(parser: Regex) -> Regex {
    repeat(parser, 0, None)
}

pub fn repeat1(parser: Regex) -> Regex {
    repeat(parser, 1, None)
}

pub fn and<'a>(parser1: Regex<'a>, parser2: Regex<'a>) -> Regex<'a> {
    Regex::And(Box::new(parser1), Box::new(parser2))
}

#[cfg(test)]
mod and_should {
    use super::{and, char, Regex};

    #[test]
    fn make_and_char_a_char_b () {
        let actual = and(char('a'), char('b'));
        let expected = Regex::And(Box::new(Regex::Char('a')), Box::new(Regex::Char('b')));

        assert_eq!(expected, actual);
    }
}

pub fn or<'a>(parser1: Regex<'a>, parser2: Regex<'a>) -> Regex<'a> {
    Regex::And(Box::new(parser1), Box::new(parser2))
}

pub fn eof() -> Regex<'static> {
    Regex::Eof
}

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
        let next = items.next().map(|(_, char)| char);
        StringCharStream {
            items: items,
            next: next,
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
        self.next = self.items.next().map(|(_, char)| char);

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

pub enum Error {
    UnexpectedEof,
    ExpectEof,
    ExpectChar
}

pub struct ParseError {
    error: Error,
    position: Position
}

pub mod parser;