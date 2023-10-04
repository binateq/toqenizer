#[macro_export]
macro_rules! toq {
    // Postfix unary operators

    ([$($operator_stack:tt)*] [$last:expr $(, $value_stack:expr)*] ? $($rest:tt)*) => {
        toq!([ $($operator_stack)* ] [$last.opt() $(, $value_stack)*] $($rest)*)
    };

    ([$($operator_stack:tt)*] [$last:expr $(, $value_stack:expr)*] * $($rest:tt)*) => {
        toq!([$($operator_stack)*] [$last.rep0() $(, $value_stack)*] $($rest)*)
    };

    ([$($operator_stack:tt)*] [$last:expr $(, $value_stack:expr)*] + $($rest:tt)*) => {
        toq!([$($operator_stack)*] [$last.rep1() $(, $value_stack)*] $($rest)*)
    };

    ([$($operator_stack:tt)*] [$last:expr $(, $value_stack:expr)*] { $min:literal , $max:literal } $($rest:tt)*) => {
        toq!([$($operator_stack)*] [$last.rep($min..$max) $(, $value_stack)*] $($rest)*)
    };

    // Keywords

    ([begin $($operator_stack:tt)*] [$($value_stack:expr)*] end $($rest:tt)*) => {
        toq!([$($operator_stack)*] [$($value_stack)*] $($rest)*)
    };

    ([$($operator_stack:tt)*] [$($value_stack:expr)*] end $($rest:tt)*) => {
        compile_error!(concat!(
            "Unpair close parentheses ')'. Rest tokens: ",
            stringify!($($rest)*)
        ))
    };

    ([& $($operator_stack:tt)*] [$right:expr, $left:expr $(, $value_stack:expr)*] end $($rest:tt)*) => {
        toq!([$($operator_stack),*] [$left & $right $(, $value_stack)*] end $($rest)*)
    };

    ([| $($operator_stack:tt)*] [$right:expr, $left:expr $(, $value_stack:expr)*] end $($rest:tt)*) => {
        toq!([$($operator_stack),*] [$left | $right $(, $value_stack)*] end $($rest)*)
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
}