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
    fn contain_1_1_after_new() {
        let expected = Position { line: 1, column: 1};
        let actual = Position::new();

        assert_eq!(expected, actual);
    }

    #[test]
    fn increment_column_after_inc_column() {
        let mut position = Position::new();
        position.inc_column();

        assert_eq!(2, position.column);
    }

    #[test]
    fn increment_line_and_set_column_to_1_after_inc_line() {
        let mut position = Position::new();
        position.inc_column(); // here column equals to 2
        position.inc_line(); // here column should be equals to 1

        assert_eq!(2, position.line);
        assert_eq!(1, position.column);
    }
}


pub trait CharStream {
    fn peek(&self) -> Option<char>;

    fn next(&mut self) -> ();

    fn position(&self) -> Position;
}


use core::str::Chars;

pub struct StringCharStream<'a> {
    position: Position,
    next: Option<char>,
    chars: Chars<'a>
}

impl<'a> StringCharStream<'a> {
    fn new(str: &'a str) -> Self {
        let mut chars = str.chars();
        let next = chars.next();

        StringCharStream {
            position: Position::new(),
            next,
            chars
        }
    }
}

impl<'a> CharStream for StringCharStream<'a> {
    fn peek(&self) -> Option<char> {
        self.next
    }

    fn next(&mut self) -> () {
        self.next = self.chars.next()
    }

    fn position(&self) -> Position {
        self.position
    }
}


pub enum Error {
    ExpectedEof,
    ExpectedCharacter,
    UnexpectedEof
}

pub struct ParseError {
    position: Position,
    error: Error
}

impl ParseError {
    fn new(stream: &dyn CharStream, error: Error) -> Self {
        ParseError {
            position: stream.position(),
            error
        }
    }
}


mod parsers;
