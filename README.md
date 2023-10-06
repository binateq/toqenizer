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
  Open,
  Close,
}

let parser = rules! {
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
  { '(' } => { |_| Token::Open }
  { ')' } => { |_| Token::Close }
};

let mut stream = StringCharStream::new("x*(15.2-foo)/y+7");
assert_eq!(Ok(Token::Identifier("x")), parser.parse(&mut stream));
assert_eq!(Ok(Token::Mul), parser.parse(&mut stream));
assert_eq!(Ok(Token::Open), parser.parse(&mut stream));
assert_eq!(Ok(Token::Number(15.2)), parser.parse(&mut stream));
assert_eq!(Ok(Token::Sub), parser.parse(&mut stream));
assert_eq!(Ok(Token::Identifier("foo")), parser.parse(&mut stream));
assert_eq!(Ok(Token::Close), parser.parse(&mut stream));
assert_eq!(Ok(Token::Div), parser.parse(&mut stream));
assert_eq!(Ok(Token::Identifier("y")), parser.parse(&mut stream));
assert_eq!(Ok(Token::Add), parser.parse(&mut stream));
assert_eq!(Ok(Token::Number(7)), parser.parse(&mut stream));
assert!(parser.parse(&mut stream).is_err());

```

## Regular Expression Syntax

### Chars

Chars like `'a'` or `'\n'` match with single character from the input stream.

#### Examples

```rust
let regex = regex!{ 'a' };
let mut stream = StringCharStream::new("abcd");

assert_eq!(Ok("a".to_string()), string_parse(&regex, &mut stream, HashMap::new()));
assert_eq!(Ok('b'), stream.peek());
```

### Strings

Strings like `"foo"` or `"\r bar \t"` match with sequence of characters from the input stream.

#### Examples

```rust
let regex = regex!{ "abc" };
let mut stream = StringCharStream::new("abcd");

assert_eq!(Ok("abc".to_string()), string_parse(&regex, &mut stream, HashMap::new()));
assert_eq!(Ok('d'), stream.peek());
```

### Character predicates

Character predicates let match input characters with the help of Rust functions of closures. The **toqenizer** has short and long forms of predicates.

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