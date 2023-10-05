#[macro_export]
macro_rules! toq {
    // Postfix unary operators

    ([$($operator_stack:tt)*] [$last:expr $(, $value_stack:expr)*] ? $($rest:tt)*) => {
        toq!([ $($operator_stack)* ] [$last.opt() $(, $value_stack)*] $($rest)*)
    };

    ([$($operator_stack:tt)*] [] ? $($rest:tt)*) => {
        compile_error!(concat!(
            "No operand for unary '?'. Rest tokens: ",
            stringify!($($rest)*)
        ))
    };

    ([$($operator_stack:tt)*] [$last:expr $(, $value_stack:expr)*] * $($rest:tt)*) => {
        toq!([$($operator_stack)*] [$last.rep0() $(, $value_stack)*] $($rest)*)
    };

    ([$($operator_stack:tt)*] [] ? $($rest:tt)*) => {
        compile_error!(concat!(
            "No operand for unary '*'. Rest tokens: ",
            stringify!($($rest)*)
        ))
    };

    ([$($operator_stack:tt)*] [$last:expr $(, $value_stack:expr)*] + $($rest:tt)*) => {
        toq!([$($operator_stack)*] [$last.rep1() $(, $value_stack)*] $($rest)*)
    };

    ([$($operator_stack:tt)*] [] ? $($rest:tt)*) => {
        compile_error!(concat!(
            "No operand for unary '+'. Rest tokens: ",
            stringify!($($rest)*)
        ))
    };

    ([$($operator_stack:tt)*] [$last:expr $(, $value_stack:expr)*] { $min:literal , $max:literal } $($rest:tt)*) => {
        toq!([$($operator_stack)*] [$last.rep($min..$max) $(, $value_stack)*] $($rest)*)
    };

    ([$($operator_stack:tt)*] [] ? $($rest:tt)*) => {
        compile_error!(concat!(
            "No operand for unary '{min, max}'. Rest tokens: ",
            stringify!($($rest)*)
        ))
    };

    ([$($operator_stack:tt)*] [$last:expr $(, $value_stack:expr)*] => $string:literal $($rest:tt)*) => {
        toq!([$($operator_stack)*] [$last.replace($string) $(, $value_stack)*] $($rest)*)
    };

    ([$($operator_stack:tt)*] [] ? $($rest:tt)*) => {
        compile_error!(concat!(
            "No operand for unary '=> \"string\"'. Rest tokens: ",
            stringify!($($rest)*)
        ))
    };

    ([$($operator_stack:tt)*] [$last:expr $(, $value_stack:expr)*] => { $mapper:expr } $($rest:tt)*) => {
        toq!([$($operator_stack)*] [$last.map($mapper) $(, $value_stack)*] $($rest)*)
    };

    ([$($operator_stack:tt)*] [] ? $($rest:tt)*) => {
        compile_error!(concat!(
            "No operand for unary '=> { expr }'. Rest tokens: ",
            stringify!($($rest)*)
        ))
    };

    // Keywords

    ([begin $($operator_stack:tt)*] [$($value_stack:expr)*] end $($rest:tt)*) => {
        toq!([$($operator_stack)*] [$($value_stack)*] $($rest)*)
    };

    ([$($operator_stack:tt)*] [$($value_stack:expr)*] end $($rest:tt)*) => {
        compile_error!(concat!(
            "Unpair close parenthesis ')'. Rest tokens: ",
            stringify!($($rest)*)
        ))
    };

    ([& $($operator_stack:tt)*] [$right:expr, $left:expr $(, $value_stack:expr)*] end $($rest:tt)*) => {
        toq!([$($operator_stack),*] [$left & $right $(, $value_stack)*] end $($rest)*)
    };

    ([| $($operator_stack:tt)*] [$right:expr, $left:expr $(, $value_stack:expr)*] end $($rest:tt)*) => {
        toq!([$($operator_stack),*] [$left | $right $(, $value_stack)*] end $($rest)*)
    };

    ([$($operator_stack:tt)*] [$($value_stack:expr)*] skip $($rest:tt)*) => {
        toq!([skip $($operator_stack)*] [$($value_stack)*] $($rest)*)
    };

    ([$($operator_stack:tt)*] [$($value_stack:expr)*] case insensitive $($rest:tt)*) => {
        toq!([case insensitive $($operator_stack)*] [$($value_stack)*] $($rest)*)
    };

    // Reduce AND (&) when two primitives on value stack

    ([& $($operator_stack:tt)*] [$right:expr, $left:expr $(, $value_stack:expr)*] $constant:literal $($rest:tt)*) => {
        toq!([& $($operator_stack)*] [crate::Elem::elem($constant), $left & $right $(, $value_stack)*] $($rest)*)
    };

    ([& $($operator_stack:tt)*] [$right:expr, $left:expr $(, $value_stack:expr)*] $identifier:ident $($rest:tt)*) => {
        toq!([& $($operator_stack)*] [crate::r(stringify!($identifier)), $left & $right $(, $value_stack)*] $($rest)*)
    };

    ([& $($operator_stack:tt)*] [$right:expr, $left:expr $(, $value_stack:expr)*] @ $identifier:ident $($rest:tt)*) => {
        toq!([& $($operator_stack)*] [crate::Element::Predicate(|c| c.$identifier()), $left & $right $(, $value_stack)*] $($rest)*)
    };

    ([& $($operator_stack:tt)*] [$right:expr, $left:expr $(, $value_stack:expr),*] @ { $predicate:expr } $($rest:tt)*) => {
        toq!([& $($operator_stack)*] [crate::Element::Predicate($predicate), $left & $right $(, $value_stack)*] $($rest)*)
    };

    // Reduce SKIP

    ([skip $($operator_stack:tt)*] [$last:expr $(, $value_stack:expr),*] $constant:literal $($rest:tt)*) => {
        toq!([$($operator_stack)*] [$last & crate::Elem::elem($constant).skip() $(, $value_stack)*] $($rest)*)
    }; 

    ([skip $($operator_stack:tt)*] [] $constant:literal $($rest:tt)*) => {
        toq!([$($operator_stack)*] [crate::Elem::elem($constant).skip()] $($rest)*)
    }; 

    // Reduce CASE INSENSITIVE

    ([case insensitive $($operator_stack:tt)*] [$last:expr $(, $value_stack:expr),*] $constant:literal $($rest:tt)*) => {
        toq!([$($operator_stack)*] [$last & crate::Elem::elem($constant).ci() $(, $value_stack)*] $($rest)*)
    }; 

    ([case insensitive $($operator_stack:tt)*] [] $constant:literal $($rest:tt)*) => {
        toq!([$($operator_stack)*] [crate::Elem::elem($constant).ci()] $($rest)*)
    }; 

    // Push AND (&) when begin on operator stack

    ([begin $($operator_stack:tt)*] [$($value_stack:expr),+] $constant:literal $($rest:tt)*) => {
        toq!([& begin $($operator_stack)*] [crate::Elem::elem($constant) $(, $value_stack)+] $($rest)*)
    };

    ([begin $($operator_stack:tt)*] [$($value_stack:expr),+] $identifier:ident $($rest:tt)*) => {
        toq!([& begin $($operator_stack)*] [crate::r(stringify!($identifier)) $(, $value_stack)+] $($rest)*)
    };

    ([begin $($operator_stack:tt)*] [$($value_stack:expr),+] @ $identifier:ident $($rest:tt)*) => {
        toq!([& begin $($operator_stack)*] [crate::Element::Predicate(|c| c.$identifier()) $(, $value_stack)+] $($rest)*)
    };

    ([begin $($operator_stack:tt)*] [$($value_stack:expr),+] @ { $predicate:expr } $($rest:tt)*) => {
        toq!([& begin $($operator_stack)*] [crate::Element::Predicate($predicate) $(, $value_stack)+] $($rest)*)
    };

    // Reduce OR (|) when AND/OR on operator stack

    ([& $($operator_stack:tt)*] [$right:expr, $left:expr $(, $value_stack:expr)*] | $($rest:tt)*) => {
        toq!([| $($operator_stack)*] [$left & $right $(, $value_stack)*] $($rest)*)
    };

    ([| $($operator_stack:tt)*] [$right:expr, $left:expr $(, $value_stack:expr)*] | $($rest:tt)*) => {
        toq!([| $($operator_stack)*] [$left | $right $(, $value_stack)*] $($rest)*)
    };

    // Push OR (|) when no AND/OR on operator stack

    ([$($operator_stack:tt)*] [$($value_stack:expr),*] | $($rest:tt)*) => {
        toq!([| $($operator_stack)*] [$($value_stack),*] $($rest)*)
    };

    // Push primitives to value stack when no other conditions

    ([$($operator_stack:tt)*] [$($value_stack:expr),*] $constant:literal $($rest:tt)*) => {
        toq!([$($operator_stack)*] [crate::Elem::elem($constant) $(, $value_stack)*] $($rest)*)
    };

    ([$($operator_stack:tt)*] [$($value_stack:expr),*] $identifier:ident $($rest:tt)*) => {
        toq!([$($operator_stack)*] [crate::r(stringify!($identifier)) $(, $value_stack)*] $($rest)*)
    };

    ([$($operator_stack:tt)*] [$($value_stack:expr),*] @ $identifier:ident $($rest:tt)*) => {
        toq!([$($operator_stack),*] [crate::Element::Predicate(|c| c.$identifier()) $(, $value_stack)*] $($rest)*)
    };

    ([$($operator_stack:tt),*] [$($value_stack:expr),*] @ { $predicate:expr } $($rest:tt)*) => {
        toq!([$($operator_stack),*] [crate::Element::Predicate($predicate) $(, $value_stack)*] $($rest)*)
    };

    // Parentheses

    ([$($operator_stack:tt),*] [$($value_stack:expr),*] ( $($inner:tt)+ ) $($rest:tt)*) => {
        toq!([begin $($operator_stack),*] [$($value_stack),*] $($inner)+ end $($rest)*)
    };

    // Final rules

    ([] [ $value:expr ]) => {
        $value
    };

    // Start rule

    ($($tokens:tt)*) => {
        toq!([begin] [] $($tokens)* end)
    };
}


#[cfg(test)]
mod regex_should {
    use super::super::Element;

    #[test]
    fn parse_char_literal() {
        assert_eq!(Element::Char('a'), toq!('a'));
    }

    #[test]
    fn parse_string_literal() {
        assert_eq!(Element::String("foo"), toq!("foo"));
    }

    #[test]
    fn parse_and() {
        assert_eq!(
            Element::And(
                Box::new(Element::Char('a')),
                Box::new(Element::String("foo"))),
            toq!('a' "foo"));
    }

    #[test]
    fn parse_and_and() {
        assert_eq!(
            Element::And(
                Box::new(Element::And(
                    Box::new(Element::Reference("a")),
                    Box::new(Element::Reference("b")))),
                Box::new(Element::Reference("c"))),
            toq!(a b c));
    }

    #[test]
    fn parse_or() {
        assert_eq!(
            Element::Or(
                Box::new(Element::Char('a')),
                Box::new(Element::String("foo"))),
            toq!('a' | "foo"));
    }

    #[test]
    fn parse_question() {
        assert_eq!(Element::Repeat(Box::new(Element::String("foo")), 0..1), toq!("foo"?));
    }

    #[test]
    fn parse_asterisk() {
        assert_eq!(Element::Repeat(Box::new(Element::String("foo")), 0..u32::MAX), toq!("foo"*));
    }

    #[test]
    fn parse_plus() {
        assert_eq!(Element::Repeat(Box::new(Element::String("foo")), 1..u32::MAX), toq!("foo"+));
    }

    #[test]
    fn parse_curly_brackets() {
        assert_eq!(Element::Repeat(Box::new(Element::String("foo")), 3..5), toq!("foo"{3,5}));
    }

    #[test]
    fn parse_arrow() {
        assert_eq!(Element::Replace(Box::new(Element::Char('a')), "b"), toq!('a' => "b"));
    }

    #[test]
    fn parse_skip_in_middle() {
        assert_eq!(
            Element::And(
                Box::new(Element::And(
                    Box::new(Element::Char('a')),
                    Box::new(Element::Skip(Box::new(Element::Char('b'))))
                )),
                Box::new(Element::Char('c'))
            ),
            toq!('a' skip 'b' 'c'));
    }

    #[test]
    fn parse_skip_in_begin() {
        assert_eq!(
            Element::And(
                Box::new(Element::Skip(Box::new(Element::Char('b')))),
                Box::new(Element::Char('c'))
            ),
            toq!(skip 'b' 'c'));
    }

    #[test]
    fn parse_case_insensitive_in_middle() {
        assert_eq!(
            Element::And(
                Box::new(Element::And(
                    Box::new(Element::Char('a')),
                    Box::new(Element::CaseInsensitive(Box::new(Element::Char('b'))))
                )),
                Box::new(Element::Char('c'))
            ),
            toq!('a' case insensitive 'b' 'c'));
    }

    #[test]
    fn parse_case_insensitive_in_begin() {
        assert_eq!(
            Element::And(
                Box::new(Element::CaseInsensitive(Box::new(Element::Char('b')))),
                Box::new(Element::Char('c'))
            ),
            toq!(case insensitive 'b' 'c'));
    }
}