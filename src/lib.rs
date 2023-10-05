#![recursion_limit = "16"]
//#![feature(trace_macros)]

use std::collections::HashMap;
use std::ops::{BitAnd, BitOr, Range};

#[derive(Debug, PartialEq, Clone)]
pub enum Regex<'a> {
    Char(char),
    Predicate(fn(char) -> bool),
    String(&'a str),
    Skip(Box<Regex<'a>>),
    CaseInsensitive(Box<Regex<'a>>),
    Replace(Box<Regex<'a>>, &'a str),
    Map(Box<Regex<'a>>, fn(String) -> String),
    Repeat(Box<Regex<'a>>, Range<u32>),
    And(Box<Regex<'a>>, Box<Regex<'a>>),
    Or(Box<Regex<'a>>, Box<Regex<'a>>),
    Reference(&'a str),
    Eof
}

pub trait ToRegex<'a> {
    fn to_regex(self) -> Regex<'a>;
}

impl<'a> ToRegex<'a> for char {
    fn to_regex(self) -> Regex<'a> {
        Regex::Char(self)
    }
}

impl<'a> ToRegex<'a> for &'a str {
    fn to_regex(self) -> Regex<'a> {
        Regex::String(self)
    }
}

impl<'a> ToRegex<'_> for fn(char) -> bool {
    fn to_regex(self) -> Regex<'static> {
        Regex::Predicate(self)
    }
}

#[cfg(test)]
mod to_regex_should {
    use super::{Regex, ToRegex};

    #[test]
    fn make_char_regex() {
        assert_eq!(Regex::Char('a'), 'a'.to_regex());
    }

    #[test]
    fn make_string_regex() {
        assert_eq!(Regex::String("foo"), "foo".to_regex());
    }

    fn is_digit(c: char) -> bool { c.is_ascii_digit() }

    #[test]
    fn make_predicate_regex() {
        let actual = (is_digit as fn (char) -> bool).to_regex();
        assert_eq!(Regex::Predicate(is_digit), actual);
    }
}

impl<'a> Regex<'a> {
    pub fn rep(self, range: Range<u32>) -> Self {
        Regex::Repeat(Box::new(self), range)
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

    pub fn skip(self) -> Self {
        Regex::Skip(Box::new(self))
    }

    pub fn ci(self) -> Self {
        Regex::CaseInsensitive(Box::new(self))
    }

    pub fn replace(self, literal: &'a str) -> Self {
        Regex::Replace(Box::new(self), literal)
    }

    pub fn map(self, mapper: fn(String) -> String) -> Self {
        Regex::Map(Box::new(self), mapper)
    }
}

#[cfg(test)]
mod regex_should {
    use super::{Regex, ToRegex};

    #[test]
    fn make_repeat_3_4_when_rep_min_is_3_and_max_is_4() {
        let actual = "abc".to_regex().rep(3..4);
        let expected =
        Regex::Repeat(
            Box::new(Regex::String("abc")),
            3..4);

        assert_eq!(expected, actual);
    }

    
    #[test]
    fn make_repeat_4_3_when_rep_min_is_4_and_max_is_3() {
        let actual = "abc".to_regex().rep(4..3);
        let expected =
        Regex::Repeat(
            Box::new(Regex::String("abc")),
            4..3);

        assert_eq!(expected, actual);
    }

    #[test]
    fn make_repeat_0_1_when_opt() {
        let actual = 'a'.to_regex().opt();
        let expected =
        Regex::Repeat(
            Box::new(Regex::Char('a')),
            0..1);

        assert_eq!(expected, actual);
    }
}

impl<'a> BitAnd<Regex<'a>> for Regex<'a> {
    type Output = Regex<'a>;

    fn bitand(self, rhs: Regex<'a>) -> Regex<'a> {
        Regex::And(Box::new(self), Box::new(rhs))
    }
}

#[cfg(test)]
mod regex_and_should {
    use super::{ToRegex, Regex};

    #[test]
    fn make_and_char_a_char_b_char_c () {
        let actual ='a'.to_regex() & 'b'.to_regex() & 'c'.to_regex();
        let expected =
        Regex::And(
            Box::new(Regex::And(
                Box::new(Regex::Char('a')), 
                Box::new(Regex::Char('b')))), 
            Box::new(Regex::Char('c')));

        assert_eq!(expected, actual);
    }
}

impl<'a> BitOr<Regex<'a>> for Regex<'a> {
    type Output = Regex<'a>;

    fn bitor(self, rhs: Regex<'a>) -> Regex<'a> {
        Regex::Or(Box::new(self), Box::new(rhs))
    }
}

#[cfg(test)]
mod regex_or_should {
    use super::{ToRegex, Regex};

    #[test]
    fn make_or_char_a_char_b () {
        let actual ='a'.to_regex() | 'b'.to_regex();
        let expected =
        Regex::Or(
            Box::new(Regex::Char('a')),
            Box::new(Regex::Char('b')));

        assert_eq!(expected, actual);
    }
}

#[allow(non_upper_case_globals)]
pub const eof: Regex = Regex::Eof;

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
    UnrecognizedToken,
    UnknownName
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct ParseError {
    error: Error,
    position: Position
}


#[derive(Debug, PartialEq)]
pub struct Rule<'a, Token> {
    builder: Regex<'a>,
    mapper: fn(String) -> Token
}

pub struct Parser<'a, Token> {
    dictionary: HashMap<&'a str, Regex<'a>>,
    rules: Vec<Rule<'a, Token>>
}

pub mod stream;
pub mod nfa;
pub mod macros;