//trace_macros!(true);

#[macro_export]
macro_rules! toq {
    // Postfix unary operators

    ([ $($operator_stack:tt),* ] [ $last:expr $(, $value_stack:expr)* ] ? $($rest:tt)*) => {
        toq!([ $($operator_stack),* ] [ $last.opt() $(, $value_stack)* ] $($rest)*)
    };

    ([ $($operator_stack:tt),* ] [ $last:expr $(, $value_stack:expr)* ] * $($rest:tt)*) => {
        toq!([ $($operator_stack),* ] [ $last.rep0() $(, $value_stack)* ] $($rest)*)
    };

    ([ $($operator_stack:tt),* ] [ $last:expr $(, $value_stack:expr)* ] + $($rest:tt)*) => {
        toq!([ $($operator_stack),* ] [ $last.rep1() $(, $value_stack)* ] $($rest)*)
    };

    ([ $($operator_stack:tt),* ] [ $last:expr $(, $value_stack:expr)* ] { $min:literal , $max:literal } $($rest:tt)*) => {
        toq!([ $($operator_stack),* ] [ $last.rep($min..$max) $(, $value_stack)* ] $($rest)*)
    };

    // Binary &

    ([& $(, $operator_stack:tt)*] [$right:expr, $left:expr $(, $value_stack:expr)*] $constant:literal $($rest:tt)*) => {
        toq!([& $(, $operator_stack)*] [crate::Elem::elem($constant), $left & $right $(, $value_stack)*] $($rest)*)
    };

    ([& $(, $operator_stack:tt)*] [$right:expr, $left:expr $(, $value_stack:expr)*] $identifier:ident $($rest:tt)*) => {
        toq!([& $(, $operator_stack)*] [crate::r(stringify!($identifier)), $left & $right $(, $value_stack)*] $($rest)*)
    };

    // Binary |

    ([] [$($value_stack:expr),*] | $($rest:tt)*) => {
        toq!([|] [$($value_stack),*] $($rest)*)
    };

    ([& $(, $operator_stack:tt)*] [$right:expr, $left:expr $(, $value_stack:expr)*] | $($rest:tt)*) => {
        toq!([| $(, $operator_stack)*] [$left & $right $(, $value_stack)*] $($rest)*)
    };

    ([| $(, $operator_stack:tt)* ][$($value_stack:expr),*] $constant:literal $($rest:tt)*) => {
        toq!([| $(, $operator_stack)*] [crate::Elem::elem($constant), $($value_stack),*] $($rest)*)
    };

    ([| $(, $operator_stack:tt)* ][$($value_stack:expr),*] $identifier:ident $($rest:tt)*) => {
        toq!([| $(, $operator_stack)*] [crate::r(!stringify($identifier)), $($value_stack),*] $($rest)*)
    };

    ([|, $($operator_stack:tt),*] [$right:expr, $left:expr $(, $value_stack:expr)*]) => {
        toq!([$($operator_stack),*] [ $left | $right $(, $value_stack)* ] $($rest)*)
    };

    // Lambdas

    ([] [] @ $identifier:ident $($rest:tt)*) => {
        toq!([] [crate::Element::Predicate(|c| c.$identifier())] $($rest)*)
    };

    ([$($operator_stack:tt),*] [$($value_stack:expr),*] @ $identifier:ident $($rest:tt)*) => {
        toq!([$($operator_stack),*] [crate::Element::Predicate(|c| c.$identifier()), $($value_stack),*] $($rest)*)
    };

    ([$($operator_stack:tt),*] [] @ $predicate:block $($rest:tt)*) => {
        toq!([$($operator_stack),*] [crate::Element::Predicate(|c| $block)] $($rest)*)
    };

    ([$($operator_stack:tt),*] [$($value_stack:expr),*] @ $predicate:block $($rest:tt)*) => {
        toq!([$($operator_stack),*] [crate::Element::Predicate(|c| $block), $($value_stack),*] $($rest)*)
    };

    // Literals and identifiers

    ([] [] $constant:literal $($rest:tt)*) => {
        toq!([] [crate::Elem::elem($constant)]  $($rest)*)
    };

    ([] [] $identifier:ident $($rest:tt)*) => {
        toq!([] [crate::r(stringify!($identifier))]  $($rest)*)
    };

    ([] [$term:expr] $constant:literal $($rest:tt)*) => {
        toq!([&] [crate::Elem::elem($constant), $term] $($rest)*)
    };

    ([] [$term:expr] $identifier:ident $($rest:tt)*) => {
        toq!([&] [crate::r(stringify!($identifier)), $term] $($rest)*)
    };

    // Final rules

    ([&] [$right:expr, $left:expr $(, $value_stack:expr)*]) => {
        toq!([] [$left & $right $(, $value_stack)*])
    };

    ([|] [$right:expr, $left:expr $(, $value_stack:expr)*]) => {
        toq!([] [$left | $right $(, $value_stack)*])
    };

    ([&, $($operator_stack:tt),*] [$right:expr, $left:expr $(, $value_stack:expr)*]) => {
        toq!([$($operator_stack),*] [$left & $right $(, $value_stack)*])
    };

    ([|, $($operator_stack:tt),*] [$right:expr, $left:expr $(, $value_stack:expr)*]) => {
        toq!([$($operator_stack),*] [$left | $right $(, $value_stack)*])
    };

    ([] [ $value:expr ]) => {
        $value
    };

    // Start rule

    ($($tokens:tt)*) => {
        toq!([] [] $($tokens)*)
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