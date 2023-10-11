//use std::io::stdin;
use toqenizer::rules;

enum Token {
    Word(String),
    Number(f64),
    Punctuation(String)
}

fn main() {
    let rules = rules! {
        spaces = { @is_whitespace* }
        letter = { @is_alphabetic }
        word = { letter+ ('-' letter+ )* skip(spaces) }
        digit = { @is_ascii_digit }
        number = { digit+ ('.' digit+)? skip(spaces) }

        word => { |word| Token::Word(word) }
        number => { |number| Token::Number(number.parse::<f64>().unwrap())}
        { ',' spaces } => { |_| Token::Punctuation(",".to_string()) }
        { '.' spaces } => { |_| Token::Punctuation(".".to_string()) }
        { ';' spaces } => { |_| Token::Punctuation(";".to_string()) }
        { '?'+ spaces } => { |_| Token::Punctuation("?".to_string()) }
        { '!'+ spaces} => { |_| Token::Punctuation("!".to_string()) }
        { '-' spaces } => { |_| Token::Punctuation("-".to_string()) }
        { "..." spaces } => { |_| Token::Punctuation("…".to_string()) }
        { "---" spaces } => { |_| Token::Punctuation("—".to_string()) }
    };

    println!("Hello, world!");
}
