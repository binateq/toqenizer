use std::io::{Read, Seek, SeekFrom};
use std::str::CharIndices;
use read_char::{read_next_char, Error};
use super::Position;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum CharStreamError {
    Eof,
    IOError(std::io::ErrorKind),
    NotUtf8,
    StateStackIsEmpty
}

pub trait CharStream {
    fn peek(&self) -> Result<char, CharStreamError>;

    fn next(&mut self);

    fn position(&self) -> Position;

    fn store_state(&mut self) -> Result<(), CharStreamError>;

    fn restore_state(&mut self) -> Result<(), CharStreamError>;

    fn discard_state(&mut self) -> Result<(), CharStreamError>;
}

pub struct StringCharStream<'a> {
    items: CharIndices<'a>,
    next: Result<char, CharStreamError>,
    position: Position,
    states: Vec<(Result<char, CharStreamError>, CharIndices<'a>)>
}

impl<'a> StringCharStream<'a> {
    pub fn new(s: &'a str) -> Self {
        let mut items = s.char_indices();
        let next = items.next().map(|(_, c)| c).ok_or(CharStreamError::Eof);
        StringCharStream {
            items,
            next,
            position: Position::new(),
            states: Vec::new()
        }
    }
}

impl<'a> CharStream for StringCharStream<'a> {
    fn peek(&self) -> Result<char, CharStreamError> {
        self.next 
    }

    fn next(&mut self) {
        self.next = self.items.next().map(|(_, c)| c).ok_or(CharStreamError::Eof);

        if let Ok(next) = self.next {
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

    fn store_state(&mut self) -> Result<(), CharStreamError> {
        self.states.push((self.next, self.items.clone()));

        Ok(())
    }

    fn restore_state(&mut self) -> Result<(), CharStreamError> {
        if let Some((next, items)) = self.states.pop() {
            self.next = next;
            self.items = items;

            Ok(())
        } else {
            Err(CharStreamError::StateStackIsEmpty)
        }
    }

    fn discard_state(&mut self) -> Result<(), CharStreamError> {
        if self.states.pop().is_none() {
            Err(CharStreamError::StateStackIsEmpty)
        } else {
            Ok(())
        }
    }
}

#[cfg(test)]
mod string_char_stream_should {
    use super::{StringCharStream, CharStream, CharStreamError};

    #[test]
    fn return_ab_from_string_ab() {
        let mut stream = StringCharStream::new("ab");

        assert_eq!(Ok('a'), stream.peek());
        stream.next();

        assert_eq!(Ok('b'), stream.peek());
        stream.next();

        assert_eq!(Err(CharStreamError::Eof), stream.peek());
    }

    #[test]
    fn restore_state_after_store() {
        let mut stream = StringCharStream::new("ab");
        assert_eq!(0, stream.states.len());
        assert_eq!(Ok('a'), stream.peek());

        assert_eq!(Ok(()), stream.store_state());
        stream.next();
        assert_eq!(1, stream.states.len());
        assert_eq!(Ok('b'), stream.peek());

        assert_eq!(Ok(()), stream.restore_state());
        assert_eq!(0, stream.states.len());
        assert_eq!(Ok('a'), stream.peek());
    }

    #[test]
    fn discard_state_after_store() {
        let mut stream = StringCharStream::new("ab");
        assert_eq!(0, stream.states.len());
        assert_eq!(Ok('a'), stream.peek());

        assert_eq!(Ok(()), stream.store_state());
        stream.next();
        assert_eq!(1, stream.states.len());
        assert_eq!(Ok('b'), stream.peek());

        assert_eq!(Ok(()), stream.discard_state());
        assert_eq!(0, stream.states.len());
        assert_eq!(Ok('b'), stream.peek());
    }

    #[test]
    fn error_when_discard_without_store() {
        let mut stream = StringCharStream::new("ab");
        
        assert_eq!(Err(CharStreamError::StateStackIsEmpty), stream.restore_state());
    }

    #[test]
    fn error_when_restore_without_store() {
        let mut stream = StringCharStream::new("ab");
        
        assert_eq!(Err(CharStreamError::StateStackIsEmpty), stream.restore_state());
    }
}

pub struct SeekReadCharStream<SeekRead> where SeekRead: Seek + Read {
    seek_read: SeekRead,
    next: Result<char, CharStreamError>,
    position: Position,
    states: Vec<(Result<char, CharStreamError>, u64)>
}

impl<SeekRead> SeekReadCharStream<SeekRead> where SeekRead: Seek + Read {
    pub fn new(seek_read: SeekRead) -> Self {
        let mut char_stream = SeekReadCharStream {
            seek_read,
            next: Ok('\0'),
            position: Position::new(),
            states: Vec::new()
        };

        char_stream.next();

        char_stream
    }
}

impl<SeekRead> CharStream for SeekReadCharStream<SeekRead> where SeekRead: Seek + Read {
    fn peek(&self) -> Result<char, CharStreamError> {
        self.next
    }

    fn next(&mut self) {
        self.next = match read_next_char(&mut self.seek_read) {
            Ok(next) => {
                if next == '\n' || next == '\r' {
                    self.position.inc_line();
                } else {
                    self.position.inc_column();
                }

                Ok(next)
            },
            Err(Error::EOF) => Err(CharStreamError::Eof),
            Err(Error::NotAnUtf8(_)) => Err(CharStreamError::NotUtf8),
            Err(Error::Io(error)) => Err(CharStreamError::IOError(error.kind())),
        }
    }

    fn position(&self) -> Position {
        self.position
    }

    fn store_state(&mut self) -> Result<(), CharStreamError> {
        match self.seek_read.stream_position() {
            Ok(position) => {
                self.states.push((self.next, position));

                Ok(())
            },
            Err(error) => Err(CharStreamError::IOError(error.kind()))
        }
    }

    fn restore_state(&mut self) -> Result<(), CharStreamError> {
        if let Some((next, offset)) = self.states.pop() {
            if let Err(error) = self.seek_read.seek(SeekFrom::Start(offset)) {
                Err(CharStreamError::IOError(error.kind()))
            } else {
                self.next = next;

                Ok(())
            }
        } else {
            Err(CharStreamError::StateStackIsEmpty)
        }
    }

    fn discard_state(&mut self) -> Result<(), CharStreamError> {
        if self.states.pop().is_none() {
            Err(CharStreamError::StateStackIsEmpty)
        } else {
            Ok(())
        }
    }
}
