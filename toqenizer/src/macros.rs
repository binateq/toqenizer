#[macro_export]
macro_rules! regex {
    // Postfix unary operators

    // ?
    ([$($operators:tt)*] [$value:expr $(, $values:expr)*] ? $($rest:tt)*) => {
        $crate::regex!([ $($operators)* ] [$value.opt() $(, $values)*] $($rest)*)
    };

    ([$($operators:tt)*] [] ? $($rest:tt)*) => {
        compile_error!(concat!(
            "No operand for unary '?'. Rest tokens: ",
            stringify!($($rest)*)
        ))
    };

    // *
    ([$($operators:tt)*] [$value:expr $(, $values:expr)*] * $($rest:tt)*) => {
        $crate::regex!([$($operators)*] [$value.rep0() $(, $values)*] $($rest)*)
    };

    ([$($operators:tt)*] [] * $($rest:tt)*) => {
        compile_error!(concat!(
            "No operand for unary '*'. Rest tokens: ",
            stringify!($($rest)*)
        ))
    };

    // +
    ([$($operators:tt)*] [$value:expr $(, $values:expr)*] + $($rest:tt)*) => {
        $crate::regex!([$($operators)*] [$value.rep1() $(, $values)*] $($rest)*)
    };

    ([$($operators:tt)*] [] + $($rest:tt)*) => {
        compile_error!(concat!(
            "No operand for unary '+'. Rest tokens: ",
            stringify!($($rest)*)
        ))
    };

    // {min, max}
    ([$($operators:tt)*] [$value:expr $(, $values:expr)*] { $min:literal , $max:literal } $($rest:tt)*) => {
        $crate::regex!([$($operators)*] [$value.rep($min..$max) $(, $values)*] $($rest)*)
    };

    ([$($operators:tt)*] [] { $min:literal , $max:literal } $($rest:tt)*) => {
        compile_error!(concat!(
            "No operand for unary '{min, max}'. Rest tokens: ",
            stringify!($($rest)*)
        ))
    };

    // => "string"
    ([$($operators:tt)*] [$value:expr $(, $values:expr)*] => $string:literal $($rest:tt)*) => {
        $crate::regex!([$($operators)*] [$value.replace($string) $(, $values)*] $($rest)*)
    };

    ([$($operators:tt)*] [] => $string:literal $($rest:tt)*) => {
        compile_error!(concat!(
            "No operand for unary '=> \"string\"'. Rest tokens: ",
            stringify!($($rest)*)
        ))
    };

    // => { }
    ([$($operators:tt)*] [$value:expr $(, $values:expr)*] => { $mapper:expr } $($rest:tt)*) => {
        $crate::regex!([$($operators)*] [$value.map($mapper) $(, $values)*] $($rest)*)
    };

    ([$($operators:tt)*] [] { $mapper:expr } $($rest:tt)*) => {
        compile_error!(concat!(
            "No operand for unary '=> { expr }'. Rest tokens: ",
            stringify!($($rest)*)
        ))
    };

    ([$($operators:tt)*] [$($values:expr)*] => $($rest:tt)*) => {
        compile_error!(concat!(
            "Unrecognized right operand for '=>'. Should be string or curly-closed expression. Rest tokens: ",
            stringify!($($rest)*)
        ))
    };

    // Reduce 'and' (&) when two primitives on value stack

    ([& $($operators:tt)*] [$right:expr, $left:expr $(, $values:expr)*] $constant:literal $($rest:tt)*) => {
        $crate::regex!([& $($operators)*] [$crate::ToRegex::to_regex($constant), $left & $right $(, $values)*] $($rest)*)
    };

    ([& $($operators:tt)*] [$right:expr, $left:expr $(, $values:expr)*] @ $identifier:ident $($rest:tt)*) => {
        $crate::regex!([& $($operators)*] [$crate::Regex::Predicate(|c| c.$identifier()), $left & $right $(, $values)*] $($rest)*)
    };

    ([& $($operators:tt)*] [$right:expr, $left:expr $(, $values:expr),*] @ { $predicate:expr } $($rest:tt)*) => {
        $crate::regex!([& $($operators)*] [$crate::Regex::Predicate($predicate), $left & $right $(, $values)*] $($rest)*)
    };

    ([& $($operators:tt)*] [$right:expr, $left:expr $(, $values:expr),*] ( $($regex:tt)+ ) $($rest:tt)*) => {
        $crate::regex!([& $($operators)*] [$crate::regex!($($regex)+), $left & $right $(, $values)*] $($rest)*)
    };

    ([& $($operators:tt)*] [$right:expr, $left:expr $(, $values:expr),*] skip ( $($regex:tt)+ ) $($rest:tt)*) => {
        $crate::regex!([& $($operators)*] [$crate::regex!($($regex)+).skip(), $left & $right $(, $values)*] $($rest)*)
    };

    ([& $($operators:tt)*] [$right:expr, $left:expr $(, $values:expr),*] ci ( $($regex:tt)+ ) $($rest:tt)*) => {
        $crate::regex!([& $($operators)*] [$crate::regex!($($regex)+).ci(), $left & $right $(, $values)*] $($rest)*)
    };

    ([& $($operators:tt)*] [$right:expr, $left:expr $(, $values:expr)*] $identifier:ident $($rest:tt)*) => {
        $crate::regex!([& $($operators)*] [$crate::Regex::Reference(stringify!($identifier)), $left & $right $(, $values)*] $($rest)*)
    };

    // Reduce 'or' (|) when 'and'/'or' on operator stack

    ([& $($operators:tt)*] [$right:expr, $left:expr $(, $values:expr)*] | $($rest:tt)*) => {
        $crate::regex!([| $($operators)*] [$left & $right $(, $values)*] $($rest)*)
    };

    ([| $($operators:tt)*] [$right:expr, $left:expr $(, $values:expr)*] | $($rest:tt)*) => {
        $crate::regex!([| $($operators)*] [$left | $right $(, $values)*] $($rest)*)
    };

    // Push 'or' (|) when no 'and'/'or' on operator stack

    ([$($operators:tt)*] [$($values:expr),+] | $($rest:tt)*) => {
        $crate::regex!([| $($operators)*] [$($values),+] $($rest)*)
    };

    // Reduce 'or' (|) and 'and' (&) when no rest

    ([& $($operators:tt)*] [$right:expr, $left:expr $(, $values:expr)*]) => {
        $crate::regex!([$($operators)*] [$left & $right $(, $values)*])
    };

    ([| $($operators:tt)*] [$right:expr, $left:expr $(, $values:expr)*]) => {
        $crate::regex!([$($operators)*] [$left | $right $(, $values)*])
    };

    // Push primitives to value stack when no other conditions

    ([$($operators:tt)*] [] $constant:literal $($rest:tt)*) => {
        $crate::regex!([$($operators)*] [$crate::ToRegex::to_regex($constant)] $($rest)*)
    };

    ([$($operators:tt)*] [] @ $identifier:ident $($rest:tt)*) => {
        $crate::regex!([$($operators),*] [$crate::Regex::Predicate(|c| c.$identifier())] $($rest)*)
    };

    ([$($operators:tt)*] [] @ { $predicate:expr } $($rest:tt)*) => {
        $crate::regex!([$($operators),*] [$crate::Regex::Predicate($predicate)] $($rest)*)
    };

    ([$($operators:tt)*] [] ( $($regex:tt)+ ) $($rest:tt)*) => {
        $crate::regex!([$($operators),*] [$crate::regex!($($regex)+)] $($rest)*)
    };

    ([$($operators:tt)*] [] skip ( $($regex:tt)+ ) $($rest:tt)*) => {
        $crate::regex!([$($operators),*] [$crate::regex!($($regex)+).skip()] $($rest)*)
    };

    ([$($operators:tt)*] [] ci ( $($regex:tt)+ ) $($rest:tt)*) => {
        $crate::regex!([$($operators),*] [$crate::regex!($($regex)+).ci()] $($rest)*)
    };

    ([$($operators:tt)*] [] $identifier:ident $($rest:tt)*) => {
        $crate::regex!([$($operators)*] [$crate::Regex::Reference(stringify!($identifier))] $($rest)*)
    };

    // Pass value to value stack when 'or' (|) on operator stack and single value on value stack

    ([| $($operators:tt)*] [$value:expr] $constant:literal $($rest:tt)*) => {
        $crate::regex!([| $($operators)*] [$crate::ToRegex::to_regex($constant), $value] $($rest)*)
    };

    ([| $($operators:tt)*] [$value:expr] @ $identifier:ident $($rest:tt)*) => {
        $crate::regex!([| $($operators),*] [$crate::Regex::Predicate(|c| c.$identifier()), $value] $($rest)*)
    };

    ([| $($operators:tt)*] [$value:expr] @ { $predicate:expr } $($rest:tt)*) => {
        $crate::regex!([| $($operators),*] [$crate::Regex::Predicate($predicate), $value] $($rest)*)
    };

    ([| $($operators:tt)*] [$value:expr] ( $($regex:tt)+ ) $($rest:tt)*) => {
        $crate::regex!([| $($operators),*] [$crate::regex!($($regex)+), $value] $($rest)*)
    };

    ([| $($operators:tt)*] [$value:expr] skip ( $($regex:tt)+ ) $($rest:tt)*) => {
        $crate::regex!([| $($operators),*] [$crate::regex!($($regex)+).skip(), $value] $($rest)*)
    };

    ([| $($operators:tt)*] [$value:expr] ci ( $($regex:tt)+ ) $($rest:tt)*) => {
        $crate::regex!([| $($operators),*] [$crate::regex!($($regex)+).ci(), $value] $($rest)*)
    };

    ([| $($operators:tt)*] [$value:expr] $identifier:ident $($rest:tt)*) => {
        $crate::regex!([| $($operators)*] [$crate::Regex::Reference(stringify!($identifier)), $value] $($rest)*)
    };

    // Insert 'and' (&) to operator stack when single value on value stack

    ([$($operators:tt)*] [$value:expr] $constant:literal $($rest:tt)*) => {
        $crate::regex!([& $($operators)*] [$crate::ToRegex::to_regex($constant), $value] $($rest)*)
    };

    ([$($operators:tt)*] [$value:expr] @ $identifier:ident $($rest:tt)*) => {
        $crate::regex!([& $($operators),*] [$crate::Regex::Predicate(|c| c.$identifier()), $value] $($rest)*)
    };

    ([$($operators:tt)*] [$value:expr] @ { $predicate:expr } $($rest:tt)*) => {
        $crate::regex!([& $($operators),*] [$crate::Regex::Predicate($predicate), $value] $($rest)*)
    };

    ([$($operators:tt)*] [$value:expr] ( $($regex:tt)+ ) $($rest:tt)*) => {
        $crate::regex!([& $($operators),*] [$crate::regex!($($regex)+), $value] $($rest)*)
    };

    ([$($operators:tt)*] [$value:expr] skip ( $($regex:tt)+ ) $($rest:tt)*) => {
        $crate::regex!([& $($operators),*] [$crate::regex!($($regex)+).skip(), $value] $($rest)*)
    };

    ([$($operators:tt)*] [$value:expr] ci ( $($regex:tt)+ ) $($rest:tt)*) => {
        $crate::regex!([& $($operators),*] [$crate::regex!($($regex)+).ci(), $value] $($rest)*)
    };

    ([$($operators:tt)*] [$value:expr] $identifier:ident $($rest:tt)*) => {
        $crate::regex!([& $($operators)*] [$crate::Regex::Reference(stringify!($identifier)), $value] $($rest)*)
    };

    // Final rules

    ([] [ $value:expr ]) => {
        $value
    };

    // Start rule

    ($($tokens:tt)*) => {
        $crate::regex!([] [] $($tokens)*)
    };
}


#[cfg(test)]
mod regex_should {
    use super::super::{Regex, regex};

    #[test]
    fn parse_char_literal() {
        assert_eq!(Regex::Char('a'), regex!('a'));
    }

    #[test]
    fn parse_string_literal() {
        assert_eq!(Regex::String("foo"), regex!("foo"));
    }

    #[test]
    fn parse_and() {
        assert_eq!(
            Regex::And(
                Box::new(Regex::Char('a')),
                Box::new(Regex::String("foo"))),
            regex!('a' "foo"));
    }

    #[test]
    fn parse_and_and() {
        assert_eq!(
            Regex::And(
                Box::new(Regex::And(
                    Box::new(Regex::Reference("a")),
                    Box::new(Regex::Reference("b")))),
                Box::new(Regex::Reference("c"))),
            regex!(a b c));
    }

    #[test]
    fn parse_or() {
        assert_eq!(
            Regex::Or(
                Box::new(Regex::Char('a')),
                Box::new(Regex::String("foo"))),
            regex!('a' | "foo"));
    }

    #[test]
    fn parse_question() {
        assert_eq!(Regex::Repeat(Box::new(Regex::String("foo")), 0..1), regex!("foo"?));
    }

    #[test]
    fn parse_asterisk() {
        assert_eq!(Regex::Repeat(Box::new(Regex::String("foo")), 0..u32::MAX), regex!("foo"*));
    }

    #[test]
    fn parse_plus() {
        assert_eq!(Regex::Repeat(Box::new(Regex::String("foo")), 1..u32::MAX), regex!("foo"+));
    }

    #[test]
    fn parse_curly_brackets() {
        assert_eq!(Regex::Repeat(Box::new(Regex::String("foo")), 3..5), regex!("foo"{3,5}));
    }

    #[test]
    fn parse_arrow() {
        assert_eq!(Regex::Replace(Box::new(Regex::Char('a')), "b"), regex!('a' => "b"));
    }

    #[test]
    fn parse_skip_in_middle() {
        assert_eq!(
            Regex::And(
                Box::new(Regex::And(
                    Box::new(Regex::Char('a')),
                    Box::new(Regex::Skip(Box::new(Regex::Char('b'))))
                )),
                Box::new(Regex::Char('c'))
            ),
            regex!('a' skip('b') 'c'));
    }

    #[test]
    fn parse_skip_in_begin() {
        assert_eq!(
            Regex::And(
                Box::new(Regex::Skip(Box::new(Regex::Char('b')))),
                Box::new(Regex::Char('c'))
            ),
            regex!(skip('b') 'c'));
    }

    #[test]
    fn parse_case_insensitive_in_middle() {
        assert_eq!(
            Regex::And(
                Box::new(Regex::And(
                    Box::new(Regex::Char('a')),
                    Box::new(Regex::CaseInsensitive(Box::new(Regex::Char('b'))))
                )),
                Box::new(Regex::Char('c'))
            ),
            regex!('a' ci('b') 'c'));
    }

    #[test]
    fn parse_case_insensitive_in_begin() {
        assert_eq!(
            Regex::And(
                Box::new(Regex::CaseInsensitive(Box::new(Regex::Char('b')))),
                Box::new(Regex::Char('c'))
            ),
            regex!(ci('b') 'c'));
    }

    #[test]
    fn parse_identifier() {
        assert_eq!(
            Regex::And(
                Box::new(Regex::Reference("letter")),
                Box::new(Regex::Repeat(Box::new(Regex::Or(
                    Box::new(Regex::Reference("letter")),
                    Box::new(Regex::Reference("digit"))
                )),
                0..u32::MAX))
            ),
            regex!(letter (letter | digit)*)
        );
    }
}

#[macro_export]
macro_rules! parser {
    // Statements

    // rule
    ([$(($identifiers:expr,$regexps:expr)),*] [$($rules:expr),*] $identifier:ident => { $mapper:expr } $($rest:tt)*) => {
        $crate::parser!([$(($identifiers,$regexps)),*] [$crate::Rule::new($crate::Regex::Reference(stringify!($identifier)), $mapper) $(, $rules)*] $($rest)*)
    };

    ([$(($identifiers:expr,$regexps:expr)),*] [$($rules:expr),*] $constant:literal => { $mapper:expr } $($rest:tt)*) => {
        $crate::parser!([$(($identifiers,$regexps)),*] [$crate::Rule::new($crate::regex!($constant), $mapper) $(, $rules)*] $($rest)*)
    };

    ([$(($identifiers:expr,$regexps:expr)),*] [$($rules:expr),*] { $($regex:tt)+ } => { $mapper:expr } $($rest:tt)*) => {
        $crate::parser!([$(($identifiers,$regexps)),*] [$crate::Rule::new($crate::regex!($($regex)+), $mapper) $(, $rules)*] $($rest)*)
    };

    ([$(($identifiers:expr,$regexps:expr)),*] [$($rules:expr),*] $identifier:ident => $value:expr ; $($rest:tt)*) => {
        $crate::parser!([$(($identifiers,$regexps)),*] [$crate::Rule::new($crate::Regex::Reference(stringify!($identifier)), { |_, _| $value }) $(, $rules)*] $($rest)*)
    };

    ([$(($identifiers:expr,$regexps:expr)),*] [$($rules:expr),*] $constant:literal => $value:expr ; $($rest:tt)*) => {
        $crate::parser!([$(($identifiers,$regexps)),*] [$crate::Rule::new($crate::regex!($constant), { |_, _| $value }) $(, $rules)*] $($rest)*)
    };

    ([$(($identifiers:expr,$regexps:expr)),*] [$($rules:expr),*] { $($regex:tt)+ } => $value:expr ; $($rest:tt)*) => {
        $crate::parser!([$(($identifiers,$regexps)),*] [$crate::Rule::new($crate::regex!($($regex)+), { |_, _| $value }) $(, $rules)*] $($rest)*)
    };

    // definition
    ([$(($identifiers:expr,$regexps:expr)),*] [$($rules:expr),*] $identifier:ident = { $($regex:tt)+ } $($rest:tt)*) => {
        $crate::parser!([(stringify!($identifier),$crate::regex!($($regex)+)) $(, ($identifiers,$regexps))*] [$($rules),*] $($rest)*)
    };

    ([$(($identifiers:expr,$regexps:expr)),*] [$($rules:expr),*] () = { $($regex:tt)+ } $($rest:tt)*) => {
        $crate::parser!([("()",$crate::regex!($($regex)+)) $(, ($identifiers,$regexps))*] [$($rules),*] $($rest)*)
    };

    // Finish rules

    ([$(($identifiers:expr,$regexps:expr)),*] []) => {
        compile_error!("No rules found.")
    };

    ([] [$($rules:expr),+]) => {
        $crate::Parser {
            definitions: std::collections::HashMap::new(),
            rules: [$($rules),+].into_iter().rev().collect()
        }
    };

    ([$(($identifiers:expr,$regexes:expr)),+] [$($rules:expr),+]) => {
        $crate::Parser {
            definitions: [$(($identifiers,$regexes)),+].into_iter().rev().collect(),
            rules: [$($rules),+].into_iter().rev().collect()
        }
    };

    // Start rule

    ($($tokens:tt)*) => {
        $crate::parser!([] [] $($tokens)*)
    };
}

#[cfg(test)]
mod rules_should {
    use std::collections::HashMap;
    use super::super::{Parser, Rule, Regex, Position};

    #[derive(Debug, PartialEq)]
    enum Token {
        Identifier(String),
        Plus
    }

    fn make_identifier(_: Position, name: String) -> Token {
        Token::Identifier(name)
    }

    fn make_plus(_: Position, _: String) -> Token {
        Token::Plus
    }

    #[test]
    fn make_single_rule() {
        let actual = parser!({'a'+ } => { make_identifier });
        let expected = Parser {
            definitions: HashMap::new(),
            rules: vec![Rule::new(Regex::Repeat(Box::new(Regex::Char('a')), 1..u32::MAX), make_identifier)]
        };

        assert_eq!(expected, actual);
    }

    #[test]
    fn make_definitions_and_rules() {
        let actual = parser! {
            digit = { '1' }
            letter = { 'a' }
            identifier = { letter (letter | digit)* }

            identifier => { make_identifier }
            { '+' } => { make_plus }
        };

        let mut definitions = HashMap::new();
        definitions.insert("digit", Regex::Char('1'));
        definitions.insert("letter", Regex::Char('a'));
        definitions.insert("identifier", 
            Regex::And(
                Box::new(Regex::Reference("letter")),
                Box::new(Regex::Repeat(Box::new(Regex::Or(
                    Box::new(Regex::Reference("letter")),
                    Box::new(Regex::Reference("digit"))
                )),
                0..u32::MAX))));
        let rules = vec![
            Rule::new(Regex::Reference("identifier"), make_identifier),
            Rule::new(Regex::Char('+'), make_plus)
        ];
        let expected = Parser {
            definitions,
            rules
        };

        assert_eq!(expected.rules, actual.rules);
    }
}