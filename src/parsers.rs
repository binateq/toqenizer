use super::*;

pub fn peof(stream: &mut dyn CharStream) -> Result<(), ParseError> {
    if stream.peek().is_none() {
        Ok(())
    } else {
        Err(ParseError::new(stream, Error::ExpectedEof))
    }
}

#[cfg(test)]
mod peof_should {
    use super::StringCharStream;
    use super::peof;

    #[test]
    fn return_ok_on_empty_stream() {
        let mut stream = StringCharStream::new(&"");
        let eof = peof(&mut stream);

        assert!(eof.is_ok());
    }

    #[test]
    fn return_err_on_nonempty_stream() {
        let mut stream = StringCharStream::new(&"non empty stream");
        let eof = peof(&mut stream);

        assert!(eof.is_err());
    }
}


pub fn pchar(stream: &mut dyn CharStream, c: char) -> Result<char, ParseError> {
    let cs = "abc".chars();

    if let Some(c_next) = stream.peek() {
        if c_next == c {
            stream.next();
            
            Ok(c)
        } else {
            Err(ParseError::new(stream, Error::UnexpectedEof))
        }
    } else {
        Err(ParseError::new(stream, Error::ExpectedCharacter))
    }
}
