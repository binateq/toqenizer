#[macro_export]
macro_rules! regex {
    // Postfix unary operators

    ([$($operator_stack:tt)*] [$last:expr $(, $value_stack:expr)*] ? $($rest:tt)*) => {
        regex!([ $($operator_stack)* ] [$last.opt() $(, $value_stack)*] $($rest)*)
    };

    ([$($operator_stack:tt)*] [] ? $($rest:tt)*) => {
        compile_error!(concat!(
            "No operand for unary '?'. Rest tokens: ",
            stringify!($($rest)*)
        ))
    };

    ([$($operator_stack:tt)*] [$last:expr $(, $value_stack:expr)*] * $($rest:tt)*) => {
        regex!([$($operator_stack)*] [$last.rep0() $(, $value_stack)*] $($rest)*)
    };

    ([$($operator_stack:tt)*] [] ? $($rest:tt)*) => {
        compile_error!(concat!(
            "No operand for unary '*'. Rest tokens: ",
            stringify!($($rest)*)
        ))
    };

    ([$($operator_stack:tt)*] [$last:expr $(, $value_stack:expr)*] + $($rest:tt)*) => {
        regex!([$($operator_stack)*] [$last.rep1() $(, $value_stack)*] $($rest)*)
    };

    ([$($operator_stack:tt)*] [] ? $($rest:tt)*) => {
        compile_error!(concat!(
            "No operand for unary '+'. Rest tokens: ",
            stringify!($($rest)*)
        ))
    };

    ([$($operator_stack:tt)*] [$last:expr $(, $value_stack:expr)*] { $min:literal , $max:literal } $($rest:tt)*) => {
        regex!([$($operator_stack)*] [$last.rep($min..$max) $(, $value_stack)*] $($rest)*)
    };

    ([$($operator_stack:tt)*] [] ? $($rest:tt)*) => {
        compile_error!(concat!(
            "No operand for unary '{min, max}'. Rest tokens: ",
            stringify!($($rest)*)
        ))
    };

    ([$($operator_stack:tt)*] [$last:expr $(, $value_stack:expr)*] => $string:literal $($rest:tt)*) => {
        regex!([$($operator_stack)*] [$last.replace($string) $(, $value_stack)*] $($rest)*)
    };

    ([$($operator_stack:tt)*] [] ? $($rest:tt)*) => {
        compile_error!(concat!(
            "No operand for unary '=> \"string\"'. Rest tokens: ",
            stringify!($($rest)*)
        ))
    };

    ([$($operator_stack:tt)*] [$last:expr $(, $value_stack:expr)*] => { $mapper:expr } $($rest:tt)*) => {
        regex!([$($operator_stack)*] [$last.map($mapper) $(, $value_stack)*] $($rest)*)
    };

    ([$($operator_stack:tt)*] [] ? $($rest:tt)*) => {
        compile_error!(concat!(
            "No operand for unary '=> { expr }'. Rest tokens: ",
            stringify!($($rest)*)
        ))
    };

    // Keywords

    ([begin $($operator_stack:tt)*] [$($value_stack:expr)*] end $($rest:tt)*) => {
        regex!([$($operator_stack)*] [$($value_stack)*] $($rest)*)
    };

    ([$($operator_stack:tt)*] [$($value_stack:expr)*] end $($rest:tt)*) => {
        compile_error!(concat!(
            "Unpair close parenthesis ')'. Rest tokens: ",
            stringify!($($rest)*)
        ))
    };

    ([& $($operator_stack:tt)*] [$right:expr, $left:expr $(, $value_stack:expr)*] end $($rest:tt)*) => {
        regex!([$($operator_stack),*] [$left & $right $(, $value_stack)*] end $($rest)*)
    };

    ([| $($operator_stack:tt)*] [$right:expr, $left:expr $(, $value_stack:expr)*] end $($rest:tt)*) => {
        regex!([$($operator_stack),*] [$left | $right $(, $value_stack)*] end $($rest)*)
    };

    ([$($operator_stack:tt)*] [$($value_stack:expr)*] skip $($rest:tt)*) => {
        regex!([skip $($operator_stack)*] [$($value_stack)*] $($rest)*)
    };

    ([$($operator_stack:tt)*] [$($value_stack:expr)*] case insensitive $($rest:tt)*) => {
        regex!([case insensitive $($operator_stack)*] [$($value_stack)*] $($rest)*)
    };

    // Reduce AND (&) when two primitives on value stack

    ([& $($operator_stack:tt)*] [$right:expr, $left:expr $(, $value_stack:expr)*] $constant:literal $($rest:tt)*) => {
        regex!([& $($operator_stack)*] [crate::ToRegex::to_regex($constant), $left & $right $(, $value_stack)*] $($rest)*)
    };

    ([& $($operator_stack:tt)*] [$right:expr, $left:expr $(, $value_stack:expr)*] $identifier:ident $($rest:tt)*) => {
        regex!([& $($operator_stack)*] [crate::Regex::Reference(stringify!($identifier)), $left & $right $(, $value_stack)*] $($rest)*)
    };

    ([& $($operator_stack:tt)*] [$right:expr, $left:expr $(, $value_stack:expr)*] @ $identifier:ident $($rest:tt)*) => {
        regex!([& $($operator_stack)*] [crate::Regex::Predicate(|c| c.$identifier()), $left & $right $(, $value_stack)*] $($rest)*)
    };

    ([& $($operator_stack:tt)*] [$right:expr, $left:expr $(, $value_stack:expr),*] @ { $predicate:expr } $($rest:tt)*) => {
        regex!([& $($operator_stack)*] [crate::Regex::Predicate($predicate), $left & $right $(, $value_stack)*] $($rest)*)
    };

    // Reduce SKIP

    ([skip $($operator_stack:tt)*] [$last:expr $(, $value_stack:expr),*] $constant:literal $($rest:tt)*) => {
        regex!([$($operator_stack)*] [$last & crate::ToRegex::to_regex($constant).skip() $(, $value_stack)*] $($rest)*)
    }; 

    ([skip $($operator_stack:tt)*] [] $constant:literal $($rest:tt)*) => {
        regex!([$($operator_stack)*] [crate::ToRegex::to_regex($constant).skip()] $($rest)*)
    }; 

    // Reduce CASE INSENSITIVE

    ([case insensitive $($operator_stack:tt)*] [$last:expr $(, $value_stack:expr),*] $constant:literal $($rest:tt)*) => {
        regex!([$($operator_stack)*] [$last & crate::ToRegex::to_regex($constant).ci() $(, $value_stack)*] $($rest)*)
    }; 

    ([case insensitive $($operator_stack:tt)*] [] $constant:literal $($rest:tt)*) => {
        regex!([$($operator_stack)*] [crate::ToRegex::to_regex($constant).ci()] $($rest)*)
    }; 

    // Push AND (&) when begin on operator stack

    ([begin $($operator_stack:tt)*] [$($value_stack:expr),+] $constant:literal $($rest:tt)*) => {
        regex!([& begin $($operator_stack)*] [crate::ToRegex::to_regex($constant) $(, $value_stack)+] $($rest)*)
    };

    ([begin $($operator_stack:tt)*] [$($value_stack:expr),+] $identifier:ident $($rest:tt)*) => {
        regex!([& begin $($operator_stack)*] [crate::Regex::Reference(stringify!($identifier)) $(, $value_stack)+] $($rest)*)
    };

    ([begin $($operator_stack:tt)*] [$($value_stack:expr),+] @ $identifier:ident $($rest:tt)*) => {
        regex!([& begin $($operator_stack)*] [crate::Regex::Predicate(|c| c.$identifier()) $(, $value_stack)+] $($rest)*)
    };

    ([begin $($operator_stack:tt)*] [$($value_stack:expr),+] @ { $predicate:expr } $($rest:tt)*) => {
        regex!([& begin $($operator_stack)*] [crate::Regex::Predicate($predicate) $(, $value_stack)+] $($rest)*)
    };

    // Reduce OR (|) when AND/OR on operator stack

    ([& $($operator_stack:tt)*] [$right:expr, $left:expr $(, $value_stack:expr)*] | $($rest:tt)*) => {
        regex!([| $($operator_stack)*] [$left & $right $(, $value_stack)*] $($rest)*)
    };

    ([| $($operator_stack:tt)*] [$right:expr, $left:expr $(, $value_stack:expr)*] | $($rest:tt)*) => {
        regex!([| $($operator_stack)*] [$left | $right $(, $value_stack)*] $($rest)*)
    };

    // Push OR (|) when no AND/OR on operator stack

    ([$($operator_stack:tt)*] [$($value_stack:expr),*] | $($rest:tt)*) => {
        regex!([| $($operator_stack)*] [$($value_stack),*] $($rest)*)
    };

    // Push primitives to value stack when no other conditions

    ([$($operator_stack:tt)*] [$($value_stack:expr),*] $constant:literal $($rest:tt)*) => {
        regex!([$($operator_stack)*] [crate::ToRegex::to_regex($constant) $(, $value_stack)*] $($rest)*)
    };

    ([$($operator_stack:tt)*] [$($value_stack:expr),*] $identifier:ident $($rest:tt)*) => {
        regex!([$($operator_stack)*] [crate::Regex::Reference(stringify!($identifier)) $(, $value_stack)*] $($rest)*)
    };

    ([$($operator_stack:tt)*] [$($value_stack:expr),*] @ $identifier:ident $($rest:tt)*) => {
        regex!([$($operator_stack),*] [crate::Regex::Predicate(|c| c.$identifier()) $(, $value_stack)*] $($rest)*)
    };

    ([$($operator_stack:tt),*] [$($value_stack:expr),*] @ { $predicate:expr } $($rest:tt)*) => {
        regex!([$($operator_stack),*] [crate::Regex::Predicate($predicate) $(, $value_stack)*] $($rest)*)
    };

    // Parentheses

    ([$($operator_stack:tt),*] [$($value_stack:expr),*] ( $($inner:tt)+ ) $($rest:tt)*) => {
        regex!([begin $($operator_stack),*] [$($value_stack),*] $($inner)+ end $($rest)*)
    };

    // Final rules

    ([] [ $value:expr ]) => {
        $value
    };

    // Start rule

    ($($tokens:tt)*) => {
        regex!([begin] [] $($tokens)* end)
    };
}


#[cfg(test)]
mod regex_should {
    use super::super::Regex;

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
            regex!('a' skip 'b' 'c'));
    }

    #[test]
    fn parse_skip_in_begin() {
        assert_eq!(
            Regex::And(
                Box::new(Regex::Skip(Box::new(Regex::Char('b')))),
                Box::new(Regex::Char('c'))
            ),
            regex!(skip 'b' 'c'));
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
            regex!('a' case insensitive 'b' 'c'));
    }

    #[test]
    fn parse_case_insensitive_in_begin() {
        assert_eq!(
            Regex::And(
                Box::new(Regex::CaseInsensitive(Box::new(Regex::Char('b')))),
                Box::new(Regex::Char('c'))
            ),
            regex!(case insensitive 'b' 'c'));
    }
}