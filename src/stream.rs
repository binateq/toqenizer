use std::str::CharIndices;
use super::Position;

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