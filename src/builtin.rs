// NB. We use this type instead of storing `&'static [...]`s directly to make debugging easier.
#[derive(Debug, Clone, Copy)]
pub enum BuiltinCharRange {
    Alphabetic,
    Alphanumeric,
    Ascii,
    AsciiAlphabetic,
    AsciiAlphanumeric,
    AsciiControl,
    AsciiDigit,
    AsciiGraphic,
    AsciiHexdigit,
    AsciiLowercase,
    AsciiPunctuation,
    AsciiUppercase,
    AsciiWhitespace,
    Control,
    Lowercase,
    Numeric,
    Uppercase,
    Whitespace,
}

pub static BUILTIN_RANGES: [(&'static str, BuiltinCharRange); 18] = [
    ("alphabetic", BuiltinCharRange::Alphabetic),
    ("alphanumeric", BuiltinCharRange::Alphanumeric),
    ("ascii", BuiltinCharRange::Ascii),
    ("ascii_alphabetic", BuiltinCharRange::AsciiAlphabetic),
    ("ascii_alphanumeric", BuiltinCharRange::AsciiAlphanumeric),
    ("ascii_control", BuiltinCharRange::AsciiControl),
    ("ascii_digit", BuiltinCharRange::AsciiDigit),
    ("ascii_graphic", BuiltinCharRange::AsciiGraphic),
    ("ascii_hexdigit", BuiltinCharRange::AsciiHexdigit),
    ("ascii_lowercase", BuiltinCharRange::AsciiLowercase),
    ("ascii_punctuation", BuiltinCharRange::AsciiPunctuation),
    ("ascii_uppercase", BuiltinCharRange::AsciiUppercase),
    ("ascii_whitespace", BuiltinCharRange::AsciiWhitespace),
    ("control", BuiltinCharRange::Control),
    ("lowercase", BuiltinCharRange::Lowercase),
    ("numeric", BuiltinCharRange::Numeric),
    ("uppercase", BuiltinCharRange::Uppercase),
    ("whitespace", BuiltinCharRange::Whitespace),
];

impl BuiltinCharRange {
    pub fn get_ranges(&self) -> &'static [(u32, u32)] {
        use crate::char_ranges::*;

        match self {
            BuiltinCharRange::Alphabetic => &ALPHABETIC,
            BuiltinCharRange::Alphanumeric => &ALPHANUMERIC,
            BuiltinCharRange::Ascii => &ASCII,
            BuiltinCharRange::AsciiAlphabetic => &ASCII_ALPHABETIC,
            BuiltinCharRange::AsciiAlphanumeric => &ASCII_ALPHANUMERIC,
            BuiltinCharRange::AsciiControl => &ASCII_CONTROL,
            BuiltinCharRange::AsciiDigit => &ASCII_DIGIT,
            BuiltinCharRange::AsciiGraphic => &ASCII_GRAPHIC,
            BuiltinCharRange::AsciiHexdigit => &ASCII_HEXDIGIT,
            BuiltinCharRange::AsciiLowercase => &ASCII_LOWERCASE,
            BuiltinCharRange::AsciiPunctuation => &ASCII_PUNCTUATION,
            BuiltinCharRange::AsciiUppercase => &ASCII_UPPERCASE,
            BuiltinCharRange::AsciiWhitespace => &ASCII_WHITESPACE,
            BuiltinCharRange::Control => &CONTROL,
            BuiltinCharRange::Lowercase => &LOWERCASE,
            BuiltinCharRange::Numeric => &NUMERIC,
            BuiltinCharRange::Uppercase => &UPPERCASE,
            BuiltinCharRange::Whitespace => &WHITESPACE,
        }
    }
}
