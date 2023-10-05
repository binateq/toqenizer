use std::collections::HashMap;
use std::ops::Range;
use super::{Predicate, Element, ParseError, Error, Parser, StringMapper};
use super::stream::CharStream;

pub trait NfaParser<'a, Token> {
    fn parse(&self, stream: &mut dyn CharStream) -> Result<Token, ParseError>;
}

impl<'a, Token> NfaParser<'a, Token> for Parser<'a, Token> {
    fn parse(&self, stream: &mut dyn CharStream) -> Result<Token, ParseError> {
        for rule in &self.rules {
            if let Ok(string) = string_parse(rule.builder, stream, &self.dictionary) {
                return Ok((rule.mapper)(string))
            }
        }

        Err(ParseError { error: Error::UnrecognizedToken, position: stream.position() })
    }
}

struct ParserState<'a> {
    buffer: String,
    stream: &'a mut dyn CharStream,
    skip_flag: bool,
    case_insensitive_flag: bool,
}

impl<'a> ParserState<'a> {
    fn error(&self, error: Error) -> Result<(), ParseError> {
        Err(ParseError {
            error,
            position: self.stream.position()
        })
    }

    fn eq_char(&self, c1: char, c2: char) -> bool {
        if self.case_insensitive_flag {
            c1.eq_ignore_ascii_case(&c2)
        } else {
            c1.eq(&c2)
        }
    }

    fn store(&mut self, c: char) {
        if !self.skip_flag {
            self.buffer.push(c)
        }
    }

    fn parse_char(&mut self, c: char) -> Result<(), ParseError> {
        if let Some(next_char) = self.stream.peek() {
            if self.eq_char(c, next_char) {
                self.store(next_char);
                self.stream.next();
    
                Ok(())
            } else {
                self.error(Error::ExpectChar)
            }
        } else {
            self.error(Error::UnexpectedEof)
        }
    }

    fn parse_predicate(&mut self, predicate: &Predicate) -> Result<(), ParseError> {
        if let Some(next_char) = self.stream.peek() {
            if predicate(next_char) {
                self.store(next_char);
                self.stream.next();
    
                Ok(())
            } else {
                self.error(Error::ExpectChar)
            }
        } else {
            self.error(Error::UnexpectedEof)
        }
    }

    fn parse_string(&mut self, string: &str) -> Result<(), ParseError> {
        let buffer_length = self.buffer.len();
        self.stream.store_state();
    
        for c in string.chars() {
            if let Some(next_char) = self.stream.peek() {
                if self.eq_char(c, next_char) {
                    self.store(next_char);
                    self.stream.next();
                } else {
                    self.buffer.truncate(buffer_length);
                    self.stream.restore_state();
                    
                    return self.error(Error::ExpectChar);
                }
            } else {
                self.buffer.truncate(buffer_length);
                self.stream.restore_state();
                
                return self.error(Error::UnexpectedEof);
            }
        }
    
        self.stream.discard_state();
        Ok(())
    }

    fn parse_repeat(&mut self, element: &Element, range: &Range<u32>, dictionary: &HashMap<&str, Element>) -> Result<(), ParseError> {
        if range.is_empty() {
            return Ok(());
        }
        
        let buffer_length = self.buffer.len();
        self.stream.store_state();
    
        for _ in 0..range.start {
            if let Err(parse_error) = self.parse(&element, dictionary) {
                self.buffer.truncate(buffer_length);
                self.stream.restore_state();
    
                return Err(parse_error);
            }
        }
    
        for _ in range.start..range.end {
            if let Err(_) = self.parse(&element, dictionary) {
                break;
            }
        }
    
        self.stream.discard_state();
        Ok(())
    }

    fn parse_and(&mut self, element1: &Element, element2: &Element, dictionary: &HashMap<&str, Element>) -> Result<(), ParseError> {
        let buffer_length = self.buffer.len();
        self.stream.store_state();
    
        if let Err(error) = self.parse(element1, dictionary) {
            return Err(error);
        }
    
        if let Err(error) = self.parse(element2, dictionary) {
            self.buffer.truncate(buffer_length);
            self.stream.restore_state();
    
            return Err(error);
        }
    
        self.stream.discard_state();
        Ok(())
    }

    fn parse_or(&mut self, element1: &Element, element2: &Element, dictionary: &HashMap<&str, Element>) -> Result<(), ParseError> {
        let buffer_length = self.buffer.len();
        self.stream.store_state();
    
        if let Err(_) = self.parse(element1, dictionary) {
            if let Err(parse_error) = self.parse(element2, dictionary) {
                self.buffer.truncate(buffer_length);
                self.stream.restore_state();
        
                return Err(parse_error);
            }
        }
    
        self.stream.discard_state();
        Ok(())
    }

    fn parse_eof(&mut self) -> Result<(), ParseError> {
        if self.stream.peek().is_none() {
            Ok(())
        } else {
            self.error(Error::ExpectEof)
        }
    }

    fn parse_by_name(&mut self, name: &str, dictionary: &HashMap<&str, Element>) -> Result<(), ParseError> {
        if let Some(element) = dictionary.get(name) {
            self.parse(element, dictionary)
        } else {
            self.error(Error::UnknownName)
        }
    }

    fn parse_skip(&mut self, element: &Element, dictionary: &HashMap<&str, Element>) -> Result<(), ParseError> {
        let tmp = self.skip_flag;
        self.skip_flag = true;

        let result = self.parse(element, dictionary);
        self.skip_flag = tmp;

        result
    }

    fn parse_case_insensitive(&mut self, element: &Element, dictionary: &HashMap<&str, Element>) -> Result<(), ParseError> {
        let tmp = self.case_insensitive_flag;
        self.case_insensitive_flag = true;

        let result = self.parse(element, dictionary);
        self.case_insensitive_flag = tmp;

        result
    }

    fn parse_replace(&mut self, element: &Element, string: &str, dictionary: &HashMap<&str, Element>) -> Result<(), ParseError> {
        let tmp = self.buffer.clone();
        self.buffer = String::new();

        let result = self.parse(element, dictionary);
        self.buffer = tmp;

        if result.is_ok() {
            self.buffer.push_str(string);
        }

        result
    }

    fn parse_map(&mut self, element: &Element, mapper: &StringMapper, dictionary: &HashMap<&str, Element>) -> Result<(), ParseError> {
        let tmp = self.buffer.clone();
        self.buffer = String::new();

        let result = self.parse(element, dictionary);
        let new_buffer = self.buffer.clone();
        self.buffer = tmp;

        if result.is_ok() {
            self.buffer.push_str(&mapper(new_buffer));
        }

        result
    }

    fn parse(&mut self, element: &Element, dictionary: &HashMap<&str, Element>) -> Result<(), ParseError> {
        match element {
            Element::Char(char) => self.parse_char(*char),
            Element::Predicate(predicate) => self.parse_predicate(predicate),
            Element::String(string) => self.parse_string(*string),
            Element::Repeat(element, range) => self.parse_repeat(element, range, dictionary),
            Element::And(element1, element2) => self.parse_and(element1, element2, dictionary),
            Element::Or(element1, element2) => self.parse_or(element1, element2, dictionary),
            Element::Eof => self.parse_eof(),
            Element::Reference(name) => self.parse_by_name(*name, dictionary),
            Element::Skip(element) => self.parse_skip(element, dictionary),
            Element::CaseInsensitive(element) => self.parse_case_insensitive(element, dictionary),
            Element::Replace(element, string) => self.parse_replace(element, string, dictionary),
            Element::Map(element, mapper) => self.parse_map(element, mapper, dictionary),
        }
    }
}

fn string_parse(element: &Element, stream: &mut dyn CharStream, dictionary: &HashMap<&str, Element>) -> Result<String, ParseError> {
    let mut state = ParserState {
        buffer: String::new(),
        stream,
        skip_flag: false,
        case_insensitive_flag: false,
    };

    state.parse(element, dictionary).map(|_| state.buffer)
}

#[cfg(test)]
mod parse_string_should {
    use std::collections::HashMap;
    use super::string_parse;
    use super::super::{Elem, Element, p, eof, toq};
    use super::super::stream::{CharStream, StringCharStream};

    #[test]
    fn parse_abc_when_regex_is_abc_eof() {
        let mut stream = StringCharStream::new("abc");
        let regex = 'a'.elem() & 'b'.elem() & 'c'.elem() & eof;

        let actual = string_parse(&regex, &mut stream, &HashMap::new());

        assert_eq!(Ok("abc".to_string()), actual);
    }

    #[test]
    fn parse_digits() {
        let mut stream = StringCharStream::new("1234567.89");
        let regex = p(|c: char| c.is_digit(10)).rep1();

        let actual = string_parse(&regex, &mut stream, &HashMap::new());

        assert_eq!(Ok("1234567".to_string()), actual);
        assert_eq!(Some('.'), stream.peek());
    }

    #[test]
    fn parse_macro_with_is_ascii_digit() {
        let mut stream = StringCharStream::new("1234abcd");
        let regex = toq!(@is_ascii_digit+);

        let actual = string_parse(&regex, &mut stream, &HashMap::new());

        assert_eq!(Ok("1234".to_string()), actual);
        assert_eq!(Some('a'), stream.peek());
    }

    #[test]
    fn parse_predicate() {
        let mut stream = StringCharStream::new("1234.abcd");
        let regex = Element::Predicate(|c| c != '.').rep1();

        let actual = string_parse(&regex, &mut stream, &HashMap::new());

        assert_eq!(Ok("1234".to_string()), actual);
        assert_eq!(Some('.'), stream.peek());
    }

    #[test]
    fn parse_macro_at_with_expression() {
        let mut stream = StringCharStream::new("1234.abcd");
        let regex = toq!(@{|c| c != '.'}+);

        let actual = string_parse(&regex, &mut stream, &HashMap::new());

        assert_eq!(Ok("1234".to_string()), actual);
        assert_eq!(Some('.'), stream.peek());
    }

    #[test]
    fn parse_macro_arrow_with_expression() {
        let mut stream = StringCharStream::new("1234.abcd");
        let regex = toq!(@is_ascii_digit+ => {|s| s.chars().rev().collect()});

        let actual = string_parse(&regex, &mut stream, &HashMap::new());

        assert_eq!(Ok("4321".to_string()), actual);
        assert_eq!(Some('.'), stream.peek());
    }
}