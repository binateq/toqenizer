# toqenizer

Rust crate to make lexical analizers.

## Example

```rust
enum Token {
  Number(f64),
  Identifier(String),
  Add,
  Sub,
  Mul,
  Div,
}

let rules = rules! {
  digit = { @is_ascii_digit }
  letter = { @is_ascii_alphabetic }
  number = { digit+ ('.' digit*)? }
  identifier = { letter (digit | letter) }

  number => { |value| Token::Number(f64::from_str(&value).unwrap()) }
  identifier => { |name| Token::Identifier(name) }
  { '+' } => { |_| Token::Add }
  { '-' } => { |_| Token::Sub }
  { '*' } => { |_| Token::Mul }
  { '/' } => { |_| Token::Div }

};
```

## BNF of syntax

```
analyzer_description = { statement }

statement = ( assignment | rule ) ";"

assignment = "let" identifier "=" regexp

rule = regexp ":" "{" Rust_expression "}"

regexp = element { element }

element = char // char in apostrophes like 'a' or '\n'
        | string // string in quotes like "let" or "\"foo\""
        | identifier // reference to regexp definition
        | "@" identifier
        | "@" "{" Rust_expression "}"
        | regexp "|" regexp
        | "(" regexp ")"
        | element "?"
        | element "*"
        | element "+"
        | element "{" uint "," uint "}"
        | element "=>" string
        | element "=>" "{" Rust_expression "}"
        | "skip" "(" regexp ")"
        | "ci" "(" regexp ")"
```