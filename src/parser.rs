use super::{Predicate, TokenBuilder, CharStream, ParseError, Error, Tokenizer, Rule};

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

fn parse_repeat(builder: &TokenBuilder, min: usize, max: Option<usize>, buffer: &mut String, stream: &mut dyn CharStream) -> Result<(), ParseError> {
    let buffer_length = buffer.len();
    stream.store_state();

    for _ in 0..min {
        if let Err(parse_error) = parse_into_buffer(buffer, &builder, stream) {
            buffer.truncate(buffer_length);
            stream.restore_state();

            return Err(parse_error);
        }
    }

    if let Some(max) = max {
        for _ in (max - min)..max {
            if let Err(parse_error) = parse_into_buffer(buffer, &builder, stream) {
                buffer.truncate(buffer_length);
                stream.restore_state();
                
                return Err(parse_error);
            }
        }
        } else {
        while let Ok(()) = parse_into_buffer(buffer, &builder, stream) { }
    }

    stream.discard_state();
    Ok(())
}

fn parse_and(builder1: &TokenBuilder, builder2: &TokenBuilder, buffer: &mut String, stream: &mut dyn CharStream) -> Result<(), ParseError> {
    let buffer_length = buffer.len();
    stream.store_state();

    if let Err(parse_error) = parse_into_buffer(buffer, builder1, stream) {
        return Err(parse_error);
    }

    if let Err(parse_error) = parse_into_buffer(buffer, builder2, stream) {
        buffer.truncate(buffer_length);
        stream.restore_state();

        Err(parse_error)
    } else {
        stream.discard_state();
        Ok(())
    }
}

fn parse_or(builder1: &TokenBuilder, builder2: &TokenBuilder, buffer: &mut String, stream: &mut dyn CharStream) -> Result<(), ParseError> {
    let buffer_length = buffer.len();
    stream.store_state();

    if let Err(_) = parse_into_buffer(buffer, builder1, stream) {
        if let Err(parse_error) = parse_into_buffer(buffer, builder2, stream) {
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

fn parse_into_buffer(buffer: &mut String, builder: &TokenBuilder, stream: &mut dyn CharStream) -> Result<(), ParseError> {
    match builder {
        TokenBuilder::Char(char) => parse_char(*char, buffer, stream),
        TokenBuilder::Predicate(predicate) => parse_predicate(predicate, buffer, stream),
        TokenBuilder::String(string) => parse_string(*string, buffer, stream),
        TokenBuilder::Repeat(regex, min, max) => parse_repeat(regex.as_ref(), *min, *max, buffer, stream),
        TokenBuilder::And(regex1, regex2) => parse_and(regex1.as_ref(), regex2.as_ref(), buffer, stream),
        TokenBuilder::Or(regex1, regex2) => parse_or(regex1.as_ref(), regex2.as_ref(), buffer, stream),
        TokenBuilder::Eof => parse_eof(stream)
    }
}

pub fn parse_builder(builder: &TokenBuilder, stream: &mut dyn CharStream) -> Result<String, ParseError> {
    let mut buffer = String::new();
    
    if let Err(parse_error) = parse_into_buffer(&mut buffer, builder, stream) {
        Err(parse_error)
    } else {
        Ok(buffer)
    }
}

#[cfg(test)]
mod parse_builder_should {
    use super::parse_builder;
    use super::super::{StringCharStream, Tok, p, eof};

    #[test]
    fn parse_abc_when_regex_is_abc_eof() {
        let mut stream = StringCharStream::new("abc");
        let regex = 'a'.tok() & 'b'.tok() & 'c'.tok() & eof;

        assert_eq!(Ok("abc".to_string()), parse_builder(&regex, &mut stream));
    }

    #[test]
    fn parse_digits() {
        let mut stream = StringCharStream::new("1234567.89");
        let digits1 = p(|c: char| c.is_digit(10)).rep1();

        assert_eq!(Ok("1234567".to_string()), parse_builder(&digits1, &mut stream));
        assert_eq!(Some('.'), stream.next);
    }
}

fn parse_tokenizer<'a, Token>(tokenizer: &Tokenizer<'a, Token>, stream: &mut dyn CharStream) -> Result<Token, ParseError> {
    parse_builder(&tokenizer.builder, stream).map(tokenizer.mapper)
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
    use crate::Position;

    use super::super::{p, StringCharStream, Error, ParseError};
    use super::parse_rule;

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

        assert_eq!(Ok(Token::Identifier("abc".to_string())), actual);
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

        assert_eq!(Ok(Token::Integer(123)), actual);
    }

    #[test]
    fn do_not_recognize_plus_sign() {
        let mut stream = StringCharStream::new("+123");
        let identifier = p(|c| c.is_alphabetic()) & p(|c| c.is_alphanumeric()).rep0();
        let integer = p(|c| c.is_digit(10)).rep1();

        let rule
            = identifier.clone() >> make_identifier
            | integer.clone() >> make_integer;

        let actual = parse_rule(&rule, &mut stream);

        assert_eq!(Err(ParseError { error: Error::UnrecognizedChar, position: Position { line: 1, column: 1 }}), actual);
    }
}