#[macro_export]
macro_rules! regex {
    // Postfix unary operators

    // ?
    ([$($operator_stack:tt)*] [$last:expr $(, $value_stack:expr)*] ? $($rest:tt)*) => {
        $crate::regex!([ $($operator_stack)* ] [$last.opt() $(, $value_stack)*] $($rest)*)
    };

    ([$($operator_stack:tt)*] [] ? $($rest:tt)*) => {
        compile_error!(concat!(
            "No operand for unary '?'. Rest tokens: ",
            stringify!($($rest)*)
        ))
    };

    // *
    ([$($operator_stack:tt)*] [$last:expr $(, $value_stack:expr)*] * $($rest:tt)*) => {
        $crate::regex!([$($operator_stack)*] [$last.rep0() $(, $value_stack)*] $($rest)*)
    };

    ([$($operator_stack:tt)*] [] * $($rest:tt)*) => {
        compile_error!(concat!(
            "No operand for unary '*'. Rest tokens: ",
            stringify!($($rest)*)
        ))
    };

    // +
    ([$($operator_stack:tt)*] [$last:expr $(, $value_stack:expr)*] + $($rest:tt)*) => {
        $crate::regex!([$($operator_stack)*] [$last.rep1() $(, $value_stack)*] $($rest)*)
    };

    ([$($operator_stack:tt)*] [] + $($rest:tt)*) => {
        compile_error!(concat!(
            "No operand for unary '+'. Rest tokens: ",
            stringify!($($rest)*)
        ))
    };

    // {min, max}
    ([$($operator_stack:tt)*] [$last:expr $(, $value_stack:expr)*] { $min:literal , $max:literal } $($rest:tt)*) => {
        $crate::regex!([$($operator_stack)*] [$last.rep($min..$max) $(, $value_stack)*] $($rest)*)
    };

    ([$($operator_stack:tt)*] [] { $min:literal , $max:literal } $($rest:tt)*) => {
        compile_error!(concat!(
            "No operand for unary '{min, max}'. Rest tokens: ",
            stringify!($($rest)*)
        ))
    };

    // => "string"
    ([$($operator_stack:tt)*] [$last:expr $(, $value_stack:expr)*] => $string:literal $($rest:tt)*) => {
        $crate::regex!([$($operator_stack)*] [$last.replace($string) $(, $value_stack)*] $($rest)*)
    };

    ([$($operator_stack:tt)*] [] => $string:literal $($rest:tt)*) => {
        compile_error!(concat!(
            "No operand for unary '=> \"string\"'. Rest tokens: ",
            stringify!($($rest)*)
        ))
    };

    // => { }
    ([$($operator_stack:tt)*] [$last:expr $(, $value_stack:expr)*] => { $mapper:expr } $($rest:tt)*) => {
        $crate::regex!([$($operator_stack)*] [$last.map($mapper) $(, $value_stack)*] $($rest)*)
    };

    ([$($operator_stack:tt)*] [] { $mapper:expr } $($rest:tt)*) => {
        compile_error!(concat!(
            "No operand for unary '=> { expr }'. Rest tokens: ",
            stringify!($($rest)*)
        ))
    };

    ([$($operator_stack:tt)*] [$($value_stack:expr)*] => $($rest:tt)*) => {
        compile_error!(concat!(
            "Unrecognied right operand for '=>'. Should be string or curly-closed expression. Rest tokens: ",
            stringify!($($rest)*)
        ))
    };

    // Reduce 'and' (&) when two primitives on value stack

    ([& $($operator_stack:tt)*] [$right:expr, $left:expr $(, $value_stack:expr)*] $constant:literal $($rest:tt)*) => {
        $crate::regex!([& $($operator_stack)*] [$crate::ToRegex::to_regex($constant), $left & $right $(, $value_stack)*] $($rest)*)
    };

    ([& $($operator_stack:tt)*] [$right:expr, $left:expr $(, $value_stack:expr)*] @ $identifier:ident $($rest:tt)*) => {
        $crate::regex!([& $($operator_stack)*] [$crate::Regex::Predicate(|c| c.$identifier()), $left & $right $(, $value_stack)*] $($rest)*)
    };

    ([& $($operator_stack:tt)*] [$right:expr, $left:expr $(, $value_stack:expr),*] @ { $predicate:expr } $($rest:tt)*) => {
        $crate::regex!([& $($operator_stack)*] [$crate::Regex::Predicate($predicate), $left & $right $(, $value_stack)*] $($rest)*)
    };

    ([& $($operator_stack:tt)*] [$right:expr, $left:expr $(, $value_stack:expr),*] ( $($regex:tt)+ ) $($rest:tt)*) => {
        $crate::regex!([& $($operator_stack)*] [$crate::regex!($($regex)+), $left & $right $(, $value_stack)*] $($rest)*)
    };

    ([& $($operator_stack:tt)*] [$right:expr, $left:expr $(, $value_stack:expr),*] skip ( $($regex:tt)+ ) $($rest:tt)*) => {
        $crate::regex!([& $($operator_stack)*] [$crate::regex!($($regex)+).skip(), $left & $right $(, $value_stack)*] $($rest)*)
    };

    ([& $($operator_stack:tt)*] [$right:expr, $left:expr $(, $value_stack:expr),*] ci ( $($regex:tt)+ ) $($rest:tt)*) => {
        $crate::regex!([& $($operator_stack)*] [$crate::regex!($($regex)+).ci(), $left & $right $(, $value_stack)*] $($rest)*)
    };

    ([& $($operator_stack:tt)*] [$right:expr, $left:expr $(, $value_stack:expr)*] $identifier:ident $($rest:tt)*) => {
        $crate::regex!([& $($operator_stack)*] [$crate::Regex::Reference(stringify!($identifier)), $left & $right $(, $value_stack)*] $($rest)*)
    };

    // Reduce 'or' (|) when 'and'/'or' on operator stack

    ([& $($operator_stack:tt)*] [$right:expr, $left:expr $(, $value_stack:expr)*] | $($rest:tt)*) => {
        $crate::regex!([| $($operator_stack)*] [$left & $right $(, $value_stack)*] $($rest)*)
    };

    ([| $($operator_stack:tt)*] [$right:expr, $left:expr $(, $value_stack:expr)*] | $($rest:tt)*) => {
        $crate::regex!([| $($operator_stack)*] [$left | $right $(, $value_stack)*] $($rest)*)
    };

    // Push 'or' (|) when no 'and'/'or' on operator stack

    ([$($operator_stack:tt)*] [$($value_stack:expr),+] | $($rest:tt)*) => {
        $crate::regex!([| $($operator_stack)*] [$($value_stack),+] $($rest)*)
    };

    // Reduce 'or' (|) and 'and' (&) when no rest

    ([& $($operator_stack:tt)*] [$right:expr, $left:expr $(, $value_stack:expr)*]) => {
        $crate::regex!([$($operator_stack)*] [$left & $right $(, $value_stack)*])
    };

    ([| $($operator_stack:tt)*] [$right:expr, $left:expr $(, $value_stack:expr)*]) => {
        $crate::regex!([$($operator_stack)*] [$left | $right $(, $value_stack)*])
    };

    // Push primitives to value stack when no other conditions

    ([$($operator_stack:tt)*] [] $constant:literal $($rest:tt)*) => {
        $crate::regex!([$($operator_stack)*] [$crate::ToRegex::to_regex($constant)] $($rest)*)
    };

    ([$($operator_stack:tt)*] [] @ $identifier:ident $($rest:tt)*) => {
        $crate::regex!([$($operator_stack),*] [$crate::Regex::Predicate(|c| c.$identifier())] $($rest)*)
    };

    ([$($operator_stack:tt)*] [] @ { $predicate:expr } $($rest:tt)*) => {
        $crate::regex!([$($operator_stack),*] [$crate::Regex::Predicate($predicate)] $($rest)*)
    };

    ([$($operator_stack:tt)*] [] ( $($regex:tt)+ ) $($rest:tt)*) => {
        $crate::regex!([$($operator_stack),*] [$crate::regex!($($regex)+)] $($rest)*)
    };

    ([$($operator_stack:tt)*] [] skip ( $($regex:tt)+ ) $($rest:tt)*) => {
        $crate::regex!([$($operator_stack),*] [$crate::regex!($($regex)+).skip()] $($rest)*)
    };

    ([$($operator_stack:tt)*] [] ci ( $($regex:tt)+ ) $($rest:tt)*) => {
        $crate::regex!([$($operator_stack),*] [$crate::regex!($($regex)+).ci()] $($rest)*)
    };

    ([$($operator_stack:tt)*] [] $identifier:ident $($rest:tt)*) => {
        $crate::regex!([$($operator_stack)*] [$crate::Regex::Reference(stringify!($identifier))] $($rest)*)
    };

    // Pass value to value stack when 'or' (|) on operator stack and single value on value stack

    ([| $($operator_stack:tt)*] [$value:expr] $constant:literal $($rest:tt)*) => {
        $crate::regex!([| $($operator_stack)*] [$crate::ToRegex::to_regex($constant), $value] $($rest)*)
    };

    ([| $($operator_stack:tt)*] [$value:expr] @ $identifier:ident $($rest:tt)*) => {
        $crate::regex!([| $($operator_stack),*] [$crate::Regex::Predicate(|c| c.$identifier()), $value] $($rest)*)
    };

    ([| $($operator_stack:tt)*] [$value:expr] @ { $predicate:expr } $($rest:tt)*) => {
        $crate::regex!([| $($operator_stack),*] [$crate::Regex::Predicate($predicate), $value] $($rest)*)
    };

    ([| $($operator_stack:tt)*] [$value:expr] ( $($regex:tt)+ ) $($rest:tt)*) => {
        $crate::regex!([| $($operator_stack),*] [$crate::regex!($($regex)+), $value] $($rest)*)
    };

    ([| $($operator_stack:tt)*] [$value:expr] skip ( $($regex:tt)+ ) $($rest:tt)*) => {
        $crate::regex!([| $($operator_stack),*] [$crate::regex!($($regex)+).skip(), $value] $($rest)*)
    };

    ([| $($operator_stack:tt)*] [$value:expr] ci ( $($regex:tt)+ ) $($rest:tt)*) => {
        $crate::regex!([| $($operator_stack),*] [$crate::regex!($($regex)+).ci(), $value] $($rest)*)
    };

    ([| $($operator_stack:tt)*] [$value:expr] $identifier:ident $($rest:tt)*) => {
        $crate::regex!([| $($operator_stack)*] [$crate::Regex::Reference(stringify!($identifier)), $value] $($rest)*)
    };

    // Insert 'and' (&) to operator stack when single value on value stack

    ([$($operator_stack:tt)*] [$value:expr] $constant:literal $($rest:tt)*) => {
        $crate::regex!([& $($operator_stack)*] [$crate::ToRegex::to_regex($constant), $value] $($rest)*)
    };

    ([$($operator_stack:tt)*] [$value:expr] @ $identifier:ident $($rest:tt)*) => {
        $crate::regex!([& $($operator_stack),*] [$crate::Regex::Predicate(|c| c.$identifier()), $value] $($rest)*)
    };

    ([$($operator_stack:tt)*] [$value:expr] @ { $predicate:expr } $($rest:tt)*) => {
        $crate::regex!([& $($operator_stack),*] [$crate::Regex::Predicate($predicate), $value] $($rest)*)
    };

    ([$($operator_stack:tt)*] [$value:expr] ( $($regex:tt)+ ) $($rest:tt)*) => {
        $crate::regex!([& $($operator_stack),*] [$crate::regex!($($regex)+), $value] $($rest)*)
    };

    ([$($operator_stack:tt)*] [$value:expr] skip ( $($regex:tt)+ ) $($rest:tt)*) => {
        $crate::regex!([& $($operator_stack),*] [$crate::regex!($($regex)+).skip(), $value] $($rest)*)
    };

    ([$($operator_stack:tt)*] [$value:expr] ci ( $($regex:tt)+ ) $($rest:tt)*) => {
        $crate::regex!([& $($operator_stack),*] [$crate::regex!($($regex)+).ci(), $value] $($rest)*)
    };

    ([$($operator_stack:tt)*] [$value:expr] $identifier:ident $($rest:tt)*) => {
        $crate::regex!([& $($operator_stack)*] [$crate::Regex::Reference(stringify!($identifier)), $value] $($rest)*)
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
macro_rules! rules {
    // Statements

    // rule
    ([$(($identifiers:expr,$regexes:expr)),*] [$($rules:expr),*] $identifier:ident => { $mapper:expr } $($rest:tt)*) => {
        $crate::rules!([$(($identifiers,$regexes)),*] [$crate::Rule::new($crate::Regex::Reference(stringify!($identifier)), $mapper) $(, $rules)*] $($rest)*)
    };

    ([$(($identifiers:expr,$regexes:expr)),*] [$($rules:expr),*] { $($regex:tt)+ } => { $mapper:expr } $($rest:tt)*) => {
        $crate::rules!([$(($identifiers,$regexes)),*] [$crate::Rule::new($crate::regex!($($regex)+), $mapper) $(, $rules)*] $($rest)*)
    };

    // definition
    ([$(($identifiers:expr,$regexes:expr)),*] [$($rules:expr),*] $identifier:ident = { $($regex:tt)+ } $($rest:tt)*) => {
        $crate::rules!([(stringify!($identifier),$crate::regex!($($regex)+)) $(, ($identifiers,$regexes))*] [$($rules),*] $($rest)*)
    };

    ([$(($identifiers:expr,$regexes:expr)),*] [$($rules:expr),*] () = { $($regex:tt)+ } $($rest:tt)*) => {
        $crate::rules!([("()",$crate::regex!($($regex)+)) $(, ($identifiers,$regexes))*] [$($rules),*] $($rest)*)
    };

    // Finish rules

    ([$(($identifiers:expr,$regexex:expr)),*] []) => {
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
        $crate::rules!([] [] $($tokens)*)
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
        let actual = rules!({'a'+ } => { make_identifier });
        let expected = Parser {
            definitions: HashMap::new(),
            rules: vec![Rule::new(Regex::Repeat(Box::new(Regex::Char('a')), 1..u32::MAX), make_identifier)]
        };

        assert_eq!(expected, actual);
    }

    #[test]
    fn make_definitions_and_rules() {
        let actual = rules! {
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