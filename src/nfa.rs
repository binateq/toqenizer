use std::collections::HashMap;
use std::ops::Range;
use super::{Regex, ParseError, Error, Parser};
use super::stream::CharStream;

pub trait NfaParser<'a, Token> {
    fn parse(&self, stream: &mut dyn CharStream) -> Result<Token, ParseError>;
}

impl<'a, Token> NfaParser<'a, Token> for Parser<'a, Token> {
    fn parse(&self, stream: &mut dyn CharStream) -> Result<Token, ParseError> {
        for rule in &self.rules {
            if let Ok(string) = string_parse(&rule.builder, stream, &self.dictionary) {
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

    fn parse_predicate(&mut self, predicate: &fn(char) -> bool) -> Result<(), ParseError> {
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

    fn parse_repeat(&mut self, regex: &Regex, range: &Range<u32>, dictionary: &HashMap<&str, Regex>) -> Result<(), ParseError> {
        if range.is_empty() {
            return Ok(());
        }
        
        let buffer_length = self.buffer.len();
        self.stream.store_state();
    
        for _ in 0..range.start {
            if let Err(parse_error) = self.parse(&regex, dictionary) {
                self.buffer.truncate(buffer_length);
                self.stream.restore_state();
    
                return Err(parse_error);
            }
        }
    
        for _ in range.start..range.end {
            if let Err(_) = self.parse(&regex, dictionary) {
                break;
            }
        }
    
        self.stream.discard_state();
        Ok(())
    }

    fn parse_and(&mut self, regex1: &Regex, regex2: &Regex, dictionary: &HashMap<&str, Regex>) -> Result<(), ParseError> {
        let buffer_length = self.buffer.len();
        self.stream.store_state();
    
        if let Err(error) = self.parse(regex1, dictionary) {
            return Err(error);
        }
    
        if let Err(error) = self.parse(regex2, dictionary) {
            self.buffer.truncate(buffer_length);
            self.stream.restore_state();
    
            return Err(error);
        }
    
        self.stream.discard_state();
        Ok(())
    }

    fn parse_or(&mut self, regex1: &Regex, regex2: &Regex, dictionary: &HashMap<&str, Regex>) -> Result<(), ParseError> {
        let buffer_length = self.buffer.len();
        self.stream.store_state();
    
        if let Err(_) = self.parse(regex1, dictionary) {
            if let Err(parse_error) = self.parse(regex2, dictionary) {
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

    fn parse_by_name(&mut self, name: &str, dictionary: &HashMap<&str, Regex>) -> Result<(), ParseError> {
        if let Some(element) = dictionary.get(name) {
            self.parse(element, dictionary)
        } else {
            self.error(Error::UnknownName)
        }
    }

    fn parse_skip(&mut self, regex: &Regex, dictionary: &HashMap<&str, Regex>) -> Result<(), ParseError> {
        let tmp = self.skip_flag;
        self.skip_flag = true;

        let result = self.parse(regex, dictionary);
        self.skip_flag = tmp;

        result
    }

    fn parse_case_insensitive(&mut self, regex: &Regex, dictionary: &HashMap<&str, Regex>) -> Result<(), ParseError> {
        let tmp = self.case_insensitive_flag;
        self.case_insensitive_flag = true;

        let result = self.parse(regex, dictionary);
        self.case_insensitive_flag = tmp;

        result
    }

    fn parse_replace(&mut self, regex: &Regex, string: &str, dictionary: &HashMap<&str, Regex>) -> Result<(), ParseError> {
        let tmp = self.buffer.clone();
        self.buffer = String::new();

        let result = self.parse(regex, dictionary);
        self.buffer = tmp;

        if result.is_ok() {
            self.buffer.push_str(string);
        }

        result
    }

    fn parse_map(&mut self, regex: &Regex, mapper: &fn(String) -> String, dictionary: &HashMap<&str, Regex>) -> Result<(), ParseError> {
        let tmp = self.buffer.clone();
        self.buffer = String::new();

        let result = self.parse(regex, dictionary);
        let new_buffer = self.buffer.clone();
        self.buffer = tmp;

        if result.is_ok() {
            self.buffer.push_str(&mapper(new_buffer));
        }

        result
    }

    fn parse(&mut self, regex: &Regex, dictionary: &HashMap<&str, Regex>) -> Result<(), ParseError> {
        match regex {
            Regex::Char(char) => self.parse_char(*char),
            Regex::Predicate(predicate) => self.parse_predicate(predicate),
            Regex::String(string) => self.parse_string(*string),
            Regex::Repeat(element, range) => self.parse_repeat(element, range, dictionary),
            Regex::And(element1, element2) => self.parse_and(element1, element2, dictionary),
            Regex::Or(element1, element2) => self.parse_or(element1, element2, dictionary),
            Regex::Eof => self.parse_eof(),
            Regex::Reference(name) => self.parse_by_name(*name, dictionary),
            Regex::Skip(element) => self.parse_skip(element, dictionary),
            Regex::CaseInsensitive(element) => self.parse_case_insensitive(element, dictionary),
            Regex::Replace(element, string) => self.parse_replace(element, string, dictionary),
            Regex::Map(element, mapper) => self.parse_map(element, mapper, dictionary),
        }
    }
}

fn string_parse(regex: &Regex, stream: &mut dyn CharStream, dictionary: &HashMap<&str, Regex>) -> Result<String, ParseError> {
    let mut state = ParserState {
        buffer: String::new(),
        stream,
        skip_flag: false,
        case_insensitive_flag: false,
    };

    state.parse(regex, dictionary).map(|_| state.buffer)
}

#[cfg(test)]
mod string_parse_should {
    use std::collections::HashMap;
    use super::string_parse;
    use super::super::{ToRegex, Regex, eof, regex};
    use super::super::stream::{CharStream, StringCharStream};

    #[test]
    fn parse_sequence_and_eof() {
        let mut stream = StringCharStream::new("abc");
        let regex = 'a'.to_regex() & 'b'.to_regex() & 'c'.to_regex() & eof;

        let actual = string_parse(&regex, &mut stream, &HashMap::new());

        assert_eq!(Ok("abc".to_string()), actual);
    }

    #[test]
    fn parse_predicate() {
        let mut stream = StringCharStream::new("1234567.89");
        let regex = Regex::Predicate(|c: char| c.is_digit(10)).rep1();

        let actual = string_parse(&regex, &mut stream, &HashMap::new());

        assert_eq!(Ok("1234567".to_string()), actual);
        assert_eq!(Some('.'), stream.peek());
    }

    #[test]
    fn parse_macro_with_method_predicate() {
        let mut stream = StringCharStream::new("1234abcd");
        let regex = regex!(@is_ascii_digit+);

        let actual = string_parse(&regex, &mut stream, &HashMap::new());

        assert_eq!(Ok("1234".to_string()), actual);
        assert_eq!(Some('a'), stream.peek());
    }

    #[test]
    fn parse_macro_with_expression_predicate() {
        let mut stream = StringCharStream::new("1234.abcd");
        let regex = regex!(@{|c| c != '.'}+);

        let actual = string_parse(&regex, &mut stream, &HashMap::new());

        assert_eq!(Ok("1234".to_string()), actual);
        assert_eq!(Some('.'), stream.peek());
    }

    #[test]
    fn parse_macro_arrow_with_expression() {
        let mut stream = StringCharStream::new("1234.abcd");
        let regex = regex!(@is_ascii_digit+ => {|s| s.chars().rev().collect()});

        let actual = string_parse(&regex, &mut stream, &HashMap::new());

        assert_eq!(Ok("4321".to_string()), actual);
        assert_eq!(Some('.'), stream.peek());
    }
}