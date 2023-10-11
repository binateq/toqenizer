use std::collections::HashMap;
use std::io::stdin;
use toqenizer::rules;
use toqenizer::stream::StringCharStream;
use toqenizer::nfa::NfaParser;

#[derive(PartialEq)]
enum Token {
    Word(String),
    Number(String),
    Punctuation(String),
    Spaces,
}

fn main() {
    let rules = rules! {
        spaces = { @is_whitespace* }
        letter = { @is_alphabetic }
        word = { letter+ ('-' letter+ )* }
        digit = { @is_ascii_digit }
        number = { digit+ ('.' digit+)? }

        word => { |word| Token::Word(word) }
        number => { |number| Token::Number(number) }
        { ',' } => { |_| Token::Punctuation(",".to_string()) }
        { '.' } => { |_| Token::Punctuation(".".to_string()) }
        { ';' } => { |_| Token::Punctuation(";".to_string()) }
        { '?'+ } => { |_| Token::Punctuation("?".to_string()) }
        { '!'+ } => { |_| Token::Punctuation("!".to_string()) }
        { '-' } => { |_| Token::Punctuation("-".to_string()) }
        { "..." } => { |_| Token::Punctuation("…".to_string()) }
        { "---" } => { |_| Token::Punctuation("—".to_string()) }
        spaces => { |_| Token::Spaces }
    };

    let mut words = HashMap::new();
    let mut numbers = HashMap::new();
    let mut punctuations = HashMap::new();

    let mut line = String::new();

    while let Ok(_) = stdin().read_line(&mut line) {
        let mut stream = StringCharStream::new(&line);

        while let Ok(token) = rules.parse(&mut stream) {
            match token {
                Token::Spaces => (),
                Token::Word(word) => {
                    let _ = words.entry(word).and_modify(|v| *v += 1).or_insert(1);
                },
                Token::Number(number) => {
                    let _ = numbers.entry(number).and_modify(|v| *v += 1).or_insert(1);
                },
                Token::Punctuation(punctuation) => {
                    let _ = punctuations.entry(punctuation).and_modify(|v| *v += 1).or_insert(1);
                },
            }
        }
    }

    println!("Words:");
    for (word, count) in words {
        println!("  {}: {}", word, count);
    }
    
    println!("Numbers:");
    for (number, count) in numbers {
        println!("  {}: {}", number, count);
    }
    
    println!("Punctuations:");
    for (punctuation, count) in punctuations {
        println!("  {}: {}", punctuation, count);
    }
}