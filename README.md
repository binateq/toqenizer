# toqenizer

Research Rust project to play with declarative macros.
Generates run-time NFA tokenizer from lex-style rules.

## Example

```rust
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
    // These rules describe words, numbers,
    // and punctuation symbols in an usual text.
    let rules = rules! {
        spaces = { @is_whitespace* }
        letter = { @is_alphabetic }
        word = { letter+ ('\'' letter+)? skip(spaces) }
        digit = { @is_ascii_digit }
        number = { digit+ ('.' digit+)? skip(spaces) }

        word => { |word| Token::Word(word) }
        number => { |number| Token::Number(number) }
        { '\'' spaces } => { |_| Token::Punctuation("'".to_string()) }
        { '.' spaces } => { |_| Token::Punctuation(".".to_string()) }
        { ',' spaces } => { |_| Token::Punctuation(",".to_string()) }
        { ';' spaces } => { |_| Token::Punctuation(";".to_string()) }
        { ':' spaces } => { |_| Token::Punctuation(":".to_string()) }
        { '?'+ spaces } => { |_| Token::Punctuation("?".to_string()) }
        { '!'+ spaces } => { |_| Token::Punctuation("!".to_string()) }
        { '-' spaces } => { |_| Token::Punctuation("-".to_string()) }
        { "..." spaces } => { |_| Token::Punctuation("…".to_string()) }
        { "---" spaces } => { |_| Token::Punctuation("—".to_string()) }
    };

    let mut words = HashMap::new();
    let mut numbers = HashMap::new();
    let mut punctuations = HashMap::new();

    let mut stream = SeekReadCharStream::new(File::open("hamlet.txt").unwrap());

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
```

## Regular Expression Syntax

### Chars

Chars like `'a'` or `'\n'` match with the single character from the input stream.

#### Examples

```rust
let regex = regex!{ 'a' };
let mut stream = StringCharStream::new("abcd");

assert_eq!(Ok("a".to_string()), string_parse(&regex, &mut stream, HashMap::new()));
assert_eq!(Ok('b'), stream.peek());
```

### Strings

Strings like `"foo"` or `"\r bar \t"` match with the sequence of characters from the input stream.

#### Examples

```rust
let regex = regex!{ "abc" };
let mut stream = StringCharStream::new("abcd");

assert_eq!(Ok("abc".to_string()), string_parse(&regex, &mut stream, HashMap::new()));
assert_eq!(Ok('d'), stream.peek());
```

### Character predicates

Character predicates let check input characters with the help of Rust functions or closures. The **toqenizer** has short and long forms of predicates.

The short predicate `@is_ascii_digit` is expanded to the code `|c: char| c.is_ascii_digit()`. Here [`is_ascii_digit()`](https://doc.rust-lang.org/std/primitive.char.html#method.is_ascii_digit) is the standard Rust character method.

You can use any of such methods, f.e. `is_alphanumeric()`, `is_lowercase()`, `is_control()`, and so on.

The long predicate contains the Rust code `@{ |c| c != '.' }` and may be used for any reasonable checks.

#### Examples

```rust
let regex = regex!{ @is_alphabetic };
let mut stream = StringCharStream::new("abcd");

assert_eq!(Ok("a".to_string()), string_parse(&regex, &mut stream, HashMap::new()));
assert_eq!(Ok('b'), stream.peek());
```

```rust
let regex = regex!{ @{ |char_parameter| char_parameter != 'A' } };
let mut stream = StringCharStream::new("abcd");

assert_eq!(Ok("a".to_string()), string_parse(&regex, &mut stream, HashMap::new()));
assert_eq!(Ok('b'), stream.peek());
```

### End of Input Stream

The `eof` regex matches with the end of input stream.

### Repetitions

The **toqenizer** has four operators of repetition: `?`, `*`, `+`, and `{min, max}`.

* The `?` makes previous regex an optional.
* The `*` matches with 0 or more repetitions of previous regex.
* The `*` matches with 1 or more repetitions of previous regex.
* The `{min,max}` matches with `min..max` repetitions of previous regex. 

## BNF

```
analyzer_description = { statement }

statement = ( assignment | rule )

assignment = identifier "=" "{" regexp "}"

rule = identifier "=>" "{" fn(String) -> Token "}"
     | "{" regexp "}" "=>" "{" fn(String) -> Token "}"

regexp = terminal { regexp }
       | terminal "|" regexp
       | terminal "?"
       | terminal "*"
       | terminal "+"
       | terminal "{" unsigned "," unsigned "}"
       | terminal "=>" string
       | terminal "=>" "{" fn(String) -> String "}"

terminal = char
         | string
         | "@" identifier
         | "@" "{" fn(char) -> bool "}"
         | "eof"
         | "skip" "(" regexp ")"
         | "ci" "(" regexp ")"
         | "(" regexp ")"
         | identifier
```