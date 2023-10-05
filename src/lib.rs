#![recursion_limit = "16"]
//#![feature(trace_macros)]

use std::collections::HashMap;
use std::ops::{BitAnd, BitOr, Range};

type Predicate = fn(char) -> bool;
type StringMapper = fn(String) -> String;

#[derive(Debug, PartialEq, Clone)]
pub enum Element<'a> {
    Char(char),
    Predicate(Predicate),
    String(&'a str),
    Skip(Box<Element<'a>>),
    CaseInsensitive(Box<Element<'a>>),
    Replace(Box<Element<'a>>, &'a str),
    Map(Box<Element<'a>>, StringMapper),
    Repeat(Box<Element<'a>>, Range<u32>),
    And(Box<Element<'a>>, Box<Element<'a>>),
    Or(Box<Element<'a>>, Box<Element<'a>>),
    Reference(&'a str),
    Eof
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
}

pub fn p(predicate: Predicate) -> Element<'static> {
    Element::Predicate(predicate)
}

pub fn r<'a>(identifier: &'a str) -> Element<'a> {
    Element::Reference(identifier)
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

    pub fn skip(self) -> Self {
        Element::Skip(Box::new(self))
    }

    pub fn ci(self) -> Self {
        Element::CaseInsensitive(Box::new(self))
    }

    pub fn replace(self, literal: &'a str) -> Self {
        Element::Replace(Box::new(self), literal)
    }

    pub fn map(self, mapper: StringMapper) -> Self {
        Element::Map(Box::new(self), mapper)
    }
}

#[cfg(test)]
mod regex_should {
    use super::{Element, Elem};

    #[test]
    fn make_repeat_3_4_when_rep_min_is_3_and_max_is_4() {
        let actual = "abc".elem().rep(3..4);
        let expected =
        Element::Repeat(
            Box::new(Element::String("abc")),
            3..4);

        assert_eq!(expected, actual);
    }

    
    #[test]
    fn make_repeat_4_3_when_rep_min_is_4_and_max_is_3() {
        let actual = "abc".elem().rep(4..3);
        let expected =
        Element::Repeat(
            Box::new(Element::String("abc")),
            4..3);

        assert_eq!(expected, actual);
    }

    #[test]
    fn make_repeat_0_1_when_opt() {
        let actual = 'a'.elem().opt();
        let expected =
        Element::Repeat(
            Box::new(Element::Char('a')),
            0..1);

        assert_eq!(expected, actual);
    }
}

impl<'a> BitAnd<Element<'a>> for Element<'a> {
    type Output = Element<'a>;

    fn bitand(self, rhs: Element<'a>) -> Element<'a> {
        Element::And(Box::new(self), Box::new(rhs))
    }
}

#[cfg(test)]
mod regex_and_should {
    use super::{Elem, Element};

    #[test]
    fn make_and_char_a_char_b_char_c () {
        let actual ='a'.elem() & 'b'.elem() & 'c'.elem();
        let expected =
        Element::And(
            Box::new(Element::And(
                Box::new(Element::Char('a')), 
                Box::new(Element::Char('b')))), 
            Box::new(Element::Char('c')));

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
        let expected =
        Element::Or(
            Box::new(Element::Char('a')),
            Box::new(Element::Char('b')));

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
    UnrecognizedToken,
    UnknownName
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct ParseError {
    error: Error,
    position: Position
}

pub type Mapper<Token> = fn(String) -> Token;

#[derive(Debug, PartialEq)]
pub struct Rule<'a, Token> {
    builder: &'a Element<'a>,
    mapper: Mapper<Token>
}

pub struct Parser<'a, Token> {
    dictionary: HashMap<&'a str, Element<'a>>,
    rules: Vec<Rule<'a, Token>>
}

pub mod stream;
pub mod parser;
pub mod language;