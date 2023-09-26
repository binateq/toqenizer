# toqenizer

Rust crate to making lexical analizers.

## Syntax

```
let digit = @is_ascii_digit;
let letter = @is_ascii_letter;
let spaces = skip @is_ascii_whitespace*; // just skips spaces from input
let spaces1 = @{c == ' ' || c == '\t'}+; // `c` is the name of the lambda parameter

let hex = case insensitive ('0' | '1' | '2' | '3' | '4' | '5' | '6'
    | '7' | '8' | '9' | 'A' | 'B' | 'C' | 'D' | 'E' | 'F');

let identifier = '_' | letter ('_' | letter | digit)*;
identifier: { Token::Identifier text };

digit+: { Token::Integer u32::from_str(&text) } // `text` is the name of the lambda parameter

skip("0x") hex+: { Token::Hexadecimal text } // hexadecimal digits only without prefix "0x" because `skip`
```

## BNF of syntax

```
analyzer_description = { statement }

statement = ( assignment | rule ) ";"

assignment = "let" identifier "=" regexps

rule = regexps ":" "{" Rust_expression "}"

regexps = regexp { regexp }

regexp = "'" char "'"
       | '"' string '"'
       | regexp "?"
       | regexp "*"
       | regexp "+"
       | regexp "{" uint "," uint "}"
       | "skip" regexp
       | "case" "insensitive" regexp
       | identifier
       | "@" identifier
       | "@" "{" Rust_expression "}"
       | regexp "|" regexp
       | "(" regexps ")"
```