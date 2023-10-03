use std::ops::Range;
use super::{Predicate, Element, ParseError, Error, Tokenizer, Rule};
use super::stream::CharStream;

fn parse_char(char: char, buffer: &mut String, stream: &mut dyn CharStream) -> Result<(), ParseError> {
    if let Some(next_char) = stream.peek() {
        if char == next_char {
            buffer.push(next_char);
            stream.next();

            Ok(())
        } else {
            Err(ParseError{ error: Error::ExpectChar, position: stream.position() })
        }
    } else {
        Err(ParseError{ error: Error::UnexpectedEof, position: stream.position() })
    }
}

fn parse_predicate(predicate: &Predicate, buffer: &mut String, stream: &mut dyn CharStream) -> Result<(), ParseError> {
    if let Some(next_char) = stream.peek() {
        if predicate(next_char) {
            buffer.push(next_char);
            stream.next();

            Ok(())
        } else {
            Err(ParseError{ error: Error::ExpectChar, position: stream.position() })
        }
    } else {
        Err(ParseError{ error: Error::UnexpectedEof, position: stream.position() })
    }
}

fn parse_string(string: &str, buffer: &mut String, stream: &mut dyn CharStream) -> Result<(), ParseError> {
    let buffer_length = buffer.len();
    stream.store_state();

    for char in string.chars() {
        if let Some(next_char) = stream.peek() {
            if char == next_char {
                buffer.push(next_char);
                stream.next();
            } else {
                buffer.truncate(buffer_length);
                stream.restore_state();
                
                return Err(ParseError{ error: Error::ExpectChar, position: stream.position() });
            }
        } else {
            buffer.truncate(buffer_length);
            stream.restore_state();
            
            return Err(ParseError{ error: Error::UnexpectedEof, position: stream.position() });
        }
    }

    stream.discard_state();
    Ok(())
}

fn parse_repeat(element: &Element, range: &Range<u32>, buffer: &mut String, stream: &mut dyn CharStream) -> Result<(), ParseError> {
    if range.is_empty() {
        return Ok(());
    }
    
    let buffer_length = buffer.len();
    stream.store_state();

    for _ in 0..range.start {
        if let Err(parse_error) = parse_into_buffer(buffer, &element, stream) {
            buffer.truncate(buffer_length);
            stream.restore_state();

            return Err(parse_error);
        }
    }

    for _ in range.start..range.end {
        if let Err(_) = parse_into_buffer(buffer, &element, stream) {
            break;
        }
    }

    stream.discard_state();
    Ok(())
}

fn parse_and(element1: &Element, element2: &Element, buffer: &mut String, stream: &mut dyn CharStream) -> Result<(), ParseError> {
    let buffer_length = buffer.len();
    stream.store_state();

    if let Err(error) = parse_into_buffer(buffer, element1, stream) {
        return Err(error);
    }

    if let Err(error) = parse_into_buffer(buffer, element2, stream) {
        buffer.truncate(buffer_length);
        stream.restore_state();

        return Err(error);
    }

    stream.discard_state();
    Ok(())
}

fn parse_or(element1: &Element, element2: &Element, buffer: &mut String, stream: &mut dyn CharStream) -> Result<(), ParseError> {
    let buffer_length = buffer.len();
    stream.store_state();

    if let Err(_) = parse_into_buffer(buffer, element1, stream) {
        if let Err(parse_error) = parse_into_buffer(buffer, element2, stream) {
            buffer.truncate(buffer_length);
            stream.restore_state();
    
            return Err(parse_error);
        }
    }

    stream.discard_state();
    Ok(())
}

fn parse_eof(stream: &mut dyn CharStream) -> Result<(), ParseError> {
    if stream.peek().is_none() {
        Ok(())
    } else {
        Err(ParseError{ error: Error::ExpectEof, position: stream.position() })
    }
}

fn parse_into_buffer(buffer: &mut String, element: &Element, stream: &mut dyn CharStream) -> Result<(), ParseError> {
    match element {
        Element::Char(char) => parse_char(*char, buffer, stream),
        Element::Predicate(predicate) => parse_predicate(predicate, buffer, stream),
        Element::String(string) => parse_string(*string, buffer, stream),
        Element::Repeat(element, range) => parse_repeat(element.as_ref(), range, buffer, stream),
        Element::And(element1, element2) => parse_and(element1, element2, buffer, stream),
        Element::Or(element1, element2) => parse_or(element1.as_ref(), element2.as_ref(), buffer, stream),
        Element::Eof => parse_eof(stream),
        Element::Reference(_identifier) => Ok(()),
    }
}

pub fn parse_element(element: &Element, stream: &mut dyn CharStream) -> Result<String, ParseError> {
    let mut buffer = String::new();
    
    if let Err(parse_error) = parse_into_buffer(&mut buffer, element, stream) {
        Err(parse_error)
    } else {
        Ok(buffer)
    }
}

#[cfg(test)]
mod parse_element_should {
    use super::super::stream::CharStream;
    use super::super::toq;

    use super::parse_element;
    use super::super::{Elem, p, eof};
    use super::super::stream::StringCharStream;

    #[test]
    fn parse_abc_when_regex_is_abc_eof() {
        let mut stream = StringCharStream::new("abc");
        let regex = 'a'.elem() & 'b'.elem() & 'c'.elem() & eof;

        assert_eq!(Ok("abc".to_string()), parse_element(&regex, &mut stream));
    }

    #[test]
    fn parse_digits() {
        let mut stream = StringCharStream::new("1234567.89");
        let digits1 = p(|c: char| c.is_digit(10)).rep1();

        assert_eq!(Ok("1234567".to_string()), parse_element(&digits1, &mut stream));
        assert_eq!(Some('.'), stream.peek());
    }

    #[test]
    fn parse_whith_is_ascii_digit() {
        let mut stream = StringCharStream::new("1234abcd");
        let digits1 = toq!(@is_ascii_digit+);

        assert_eq!(Ok("1234".to_string()), parse_element(&digits1, &mut stream));
        assert_eq!(Some('a'), stream.peek());
    }
}

fn parse_tokenizer<'a, Token>(tokenizer: &Tokenizer<'a, Token>, stream: &mut dyn CharStream) -> Result<Token, ParseError> {
    parse_element(&tokenizer.builder, stream).map(tokenizer.mapper)
}

fn parse_tokenizers<'a, Token>(tokenizers: &Vec<Tokenizer<'a, Token>>, stream: &mut dyn CharStream) -> Result<Token, ParseError> {
    for tokenizer in tokenizers {
        let result = parse_tokenizer(tokenizer, stream);

        if result.is_ok() {
            return result;
        }
    }

    Err(ParseError{ error: Error::UnrecognizedChar, position: stream.position() })
}

pub fn parse_rule<'a, Token>(rule: &Rule<'a, Token>, stream: &mut dyn CharStream) -> Result<Token, ParseError> {
    match rule {
        Rule::Single(tokenizer) => parse_tokenizer(&tokenizer, stream),
        Rule::Multiple(tokenizers) => parse_tokenizers(&tokenizers, stream)
    }
}

#[cfg(test)]
mod parse_rule_should {
    use super::parse_rule;
    use super::super::{p, ParseError, Error, Position};
    use super::super::stream::StringCharStream;

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
    fn recognize_identifer() {
        let mut stream = StringCharStream::new("abc");
        let identifier = p(|c| c.is_alphabetic()) & p(|c| c.is_alphanumeric()).rep0();
        let integer = p(|c| c.is_digit(10)).rep1();

        let rule
            = identifier.clone() >> make_identifier
            | integer.clone() >> make_integer;

        let actual = parse_rule(&rule, &mut stream);
        let expected = Ok(Token::Identifier("abc".to_string()));

        assert_eq!(expected, actual);
    }

    #[test]
    fn recognize_integer() {
        let mut stream = StringCharStream::new("123");
        let identifier = p(|c| c.is_alphabetic()) & p(|c| c.is_alphanumeric()).rep0();
        let integer = p(|c| c.is_digit(10)).rep1();

        let rule
            = identifier.clone() >> make_identifier
            | integer.clone() >> make_integer;

        let actual = parse_rule(&rule, &mut stream);
        let expected = Ok(Token::Integer(123));

        assert_eq!(expected, actual);
    }

    #[test]
    fn do_not_recognize_plus_sign() {
        let mut stream = StringCharStream::new("+123");
        let identifier = p(|c| c.is_alphabetic()) & p(|c| c.is_alphanumeric()).rep0();
        let integer = p(|c| c.is_digit(10)).rep1();

        let rule
            = identifier >> make_identifier
            | integer >> make_integer;

        let actual = parse_rule(&rule, &mut stream);

        assert_eq!(Err(ParseError { error: Error::UnrecognizedChar, position: Position { line: 1, column: 1 }}), actual);
    }
}