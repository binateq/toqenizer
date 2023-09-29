#[macro_export]
macro_rules! toq {
    ([ $last:expr $(, $stack:expr)* ] ? $($rest:tt)*) => {
        toq!([ $last.opt() $(, $stack)* ] $($rest)*)
    };

    ([ $last:expr $(, $stack:expr)* ] * $($rest:tt)*) => {
        toq!([ $last.rep0() $(, $stack)* ] $($rest)*)
    };

    ([ $last:expr $(, $stack:expr)* ] + $($rest:tt)*) => {
        toq!([ $last.rep1() $(, $stack)* ] $($rest)*)
    };

    ([ $last:expr $(, $stack:expr)* ] { $min:literal , $max:literal } $($rest:tt)*) => {
        toq!([ $last.rep($min..$max) $(, $stack)* ] $($rest)*)
    };

    ([] $lit:literal $($rest:tt)*) => {
        toq!([ crate::Elem::elem($lit) ] $($rest)*)
    };

    ([ $last:expr $(, $stack:expr)* ] $lit:literal $($rest:tt)*) => {
        toq!([ ($last & crate::Elem::elem($lit)) $(, $stack)* ] $($rest)*)
    };

    ([ $value:expr ]) => {
        $value
    };

    ($($tokens:tt)*) => {
        toq!([] $($tokens)*)
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
        assert_eq!(Element::And(Box::new(Element::Char('a')), Box::new(Element::String("foo"))), toq!('a' "foo"));
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