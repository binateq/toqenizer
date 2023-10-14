use std::collections::HashMap;
use std::fs::File;
use toqenizer::rules;
use toqenizer::stream::SeekReadCharStream;
use toqenizer::nfa::NfaParser;

#[derive(PartialEq, Debug)]
enum Token {
    Word(String),
    Number(String),
    Punctuation(String),
}

fn main() {
    let rules = rules! {
        spaces = { @is_whitespace* }
        letter = { @is_alphabetic }
        word = { letter+ ('\'' letter+)? }
        digit = { @is_ascii_digit }
        number = { digit+ ('.' digit+)? }

        () = { spaces }

        word => { |_, word| Token::Word(word) }
        number => { |_, number| Token::Number(number) }
        { '\'' } => { |_, _| Token::Punctuation("'".to_string()) }
        { '.' } => { |_, _| Token::Punctuation(".".to_string()) }
        { ',' } => { |_, _| Token::Punctuation(",".to_string()) }
        { ';' } => { |_, _| Token::Punctuation(";".to_string()) }
        { ':' } => { |_, _| Token::Punctuation(":".to_string()) }
        { '?'+ } => { |_, _| Token::Punctuation("?".to_string()) }
        { '!'+ } => { |_, _| Token::Punctuation("!".to_string()) }
        { '-' } => { |_, _| Token::Punctuation("-".to_string()) }
        { "..." } => { |_, _| Token::Punctuation("…".to_string()) }
        { "---" } => { |_, _| Token::Punctuation("—".to_string()) }
    };

    let mut words = HashMap::new();
    let mut numbers = HashMap::new();
    let mut punctuations = HashMap::new();

    {
        let file = File::open("hamlet.txt").unwrap();
        let mut stream = SeekReadCharStream::new(file);

        while let Ok(token) = rules.parse(&mut stream) {
            match token {
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