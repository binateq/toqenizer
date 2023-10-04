# toqenizer

Rust crate to making lexical analizers.

## Syntax

```
let digit = @is_ascii_digit;
let letter = @is_ascii_letter;
let spaces = skip @is_ascii_whitespace*; // just skips spaces from input
let spaces1 = @{ c == ' ' || c == '\t' }+; // `c` is the name of the lambda
                                         // parameter

let hex = case insensitive ('0' | '1' | '2' | '3' | '4' | '5' | '6'
    | '7' | '8' | '9' | 'A' | 'B' | 'C' | 'D' | 'E' | 'F');

let identifier = '_' | letter ('_' | letter | digit)*;
identifier: { Token::Identifier s };

digit+: { Token::Integer u32::from_str(&s) } // `text` is the name of the
                                             // lambda parameter

skip("0x") hex+: { Token::Hexadecimal s } // hexadecimal digits only without
                                          // prefix "0x" because `skip`

string = skip '"'
         ( @{c != '"' && c != '\\' && c != '\n' && c != '\r'}
         | "\\\\": "\\"
         | "\\\"": "\""
         | "\\n": "\n"
         | skip "\" digit{1, 3}: {
             char.from_u32_unchecked(u32::from_str_radix(&s, 10).unwrap())
           }
         )*
         skip '"';
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
        | element "?"
        | element "*"
        | element "+"
        | element "{" uint "," uint "}"
        | element "=>" string
        | element "=>" "{" Rust_expression "}"
        | "skip" element
        | "case" "insensitive" element
        | element "|" element
        | "(" regexp ")"
```