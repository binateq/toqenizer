use super::{Predicate, TokenBuilder, CharStream, ParseError, Error};

fn parse_char(char: char, buffer: &mut Vec<char>, stream: &mut dyn CharStream) -> Result<(), ParseError> {
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

fn parse_predicate(predicate: &Predicate, buffer: &mut Vec<char>, stream: &mut dyn CharStream) -> Result<(), ParseError> {
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

fn parse_string(string: &str, buffer: &mut Vec<char>, stream: &mut dyn CharStream) -> Result<(), ParseError> {
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

fn parse_repeat(regex: &TokenBuilder, min: usize, max: Option<usize>, buffer: &mut Vec<char>, stream: &mut dyn CharStream) -> Result<(), ParseError> {
    let buffer_length = buffer.len();
    stream.store_state();

    for _ in 0..min {
        if let Err(parse_error) = parse_into_buffer(buffer, &regex, stream) {
            buffer.truncate(buffer_length);
            stream.restore_state();

            return Err(parse_error);
        }
    }

    if let Some(max) = max {
        for _ in (max - min)..max {
            if let Err(parse_error) = parse_into_buffer(buffer, &regex, stream) {
                buffer.truncate(buffer_length);
                stream.restore_state();
                
                return Err(parse_error);
            }
        }
        } else {
        while let Ok(()) = parse_into_buffer(buffer, &regex, stream) { }
    }

    stream.discard_state();
    Ok(())
}

fn parse_and(regex1: &TokenBuilder, regex2: &TokenBuilder, buffer: &mut Vec<char>, stream: &mut dyn CharStream) -> Result<(), ParseError> {
    let buffer_length = buffer.len();
    stream.store_state();

    if let Err(parse_error) = parse_into_buffer(buffer, regex1, stream) {
        return Err(parse_error);
    }

    if let Err(parse_error) = parse_into_buffer(buffer, regex2, stream) {
        buffer.truncate(buffer_length);
        stream.restore_state();

        Err(parse_error)
    } else {
        stream.discard_state();
        Ok(())
    }
}

fn parse_or(regex1: &TokenBuilder, regex2: &TokenBuilder, buffer: &mut Vec<char>, stream: &mut dyn CharStream) -> Result<(), ParseError> {
    let buffer_length = buffer.len();
    stream.store_state();

    if let Err(_) = parse_into_buffer(buffer, regex1, stream) {
        if let Err(parse_error) = parse_into_buffer(buffer, regex2, stream) {
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

fn parse_into_buffer(buffer: &mut Vec::<char>, regex: &TokenBuilder, stream: &mut dyn CharStream) -> Result<(), ParseError> {
    match regex {
        TokenBuilder::Char(char) => parse_char(*char, buffer, stream),
        TokenBuilder::Predicate(predicate) => parse_predicate(predicate, buffer, stream),
        TokenBuilder::String(string) => parse_string(*string, buffer, stream),
        TokenBuilder::Repeat(regex, min, max) => parse_repeat(regex.as_ref(), *min, *max, buffer, stream),
        TokenBuilder::And(regex1, regex2) => parse_and(regex1.as_ref(), regex2.as_ref(), buffer, stream),
        TokenBuilder::Or(regex1, regex2) => parse_or(regex1.as_ref(), regex2.as_ref(), buffer, stream),
        TokenBuilder::Eof => parse_eof(stream)
    }
}

pub fn parse(regex: &TokenBuilder, stream: &mut dyn CharStream) -> Result<String, ParseError> {
    let mut buffer = Vec::new();
    
    if let Err(parse_error) = parse_into_buffer(&mut buffer, regex, stream) {
        Err(parse_error)
    } else {
        Ok(buffer.into_iter().collect())
    }
}

#[cfg(test)]
mod parse_should {
    use super::parse;
    use super::super::{StringCharStream, Tok, p, eof};

    #[test]
    fn parse_abc_when_regex_is_abc_eof() {
        let mut stream = StringCharStream::new("abc");
        let regex = 'a'.tok() & 'b'.tok() & 'c'.tok() & eof;

        assert_eq!(Ok("abc".to_string()), parse(&regex, &mut stream));
    }

    #[test]
    fn parse_digits() {
        let mut stream = StringCharStream::new("1234567.89");
        let digits1 = p(|c: char| c.is_digit(10)).rep1();

        assert_eq!(Ok("1234567".to_string()), parse(&digits1, &mut stream));
        assert_eq!(Some('.'), stream.next);
    }
}