use std::convert::TryFrom;

fn main() {
    for (f, name) in FNS.iter() {
        let ranges = generate_char_fn_ranges(*f);
        println!("pub static {}: [(u32, u32); {}] = [", name, ranges.len());
        for range in ranges {
            println!("    ({}, {}),", range.0, range.1);
        }
        println!("];");
    }
}

fn generate_char_fn_ranges(f: fn(char) -> bool) -> Vec<(u32, u32)> {
    let mut ranges: Vec<(u32, u32)> = vec![];
    let mut current_range_start: Option<u32> = None;

    for i in 0..=u32::from(char::MAX) {
        let c = match char::try_from(i) {
            Err(_) => continue,
            Ok(c) => c,
        };

        if f(c) {
            if current_range_start.is_none() {
                current_range_start = Some(i);
            }
        } else if let Some(current_range_start) = current_range_start.take() {
            ranges.push((current_range_start, i - 1));
        }
    }

    ranges
}

macro_rules! ascii_fn {
    ($x:ident) => {
        fn $x(c: char) -> bool {
            char::$x(&c)
        }
    };
}

ascii_fn!(is_ascii);
ascii_fn!(is_ascii_alphabetic);
ascii_fn!(is_ascii_alphanumeric);
ascii_fn!(is_ascii_control);
ascii_fn!(is_ascii_digit);
ascii_fn!(is_ascii_graphic);
ascii_fn!(is_ascii_hexdigit);
ascii_fn!(is_ascii_lowercase);
ascii_fn!(is_ascii_punctuation);
ascii_fn!(is_ascii_uppercase);
ascii_fn!(is_ascii_whitespace);

static FNS: [(fn(char) -> bool, &str); 20] = [
    (char::is_alphabetic, "ALPHABETIC"),
    (char::is_alphanumeric, "ALPHANUMERIC"),
    (is_ascii, "ASCII"),
    (is_ascii_alphabetic, "ASCII_ALPHABETIC"),
    (is_ascii_alphanumeric, "ASCII_ALPHANUMERIC"),
    (is_ascii_control, "ASCII_CONTROL"),
    (is_ascii_digit, "ASCII_DIGIT"),
    (is_ascii_graphic, "ASCII_GRAPHIC"),
    (is_ascii_hexdigit, "ASCII_HEXDIGIT"),
    (is_ascii_lowercase, "ASCII_LOWERCASE"),
    (is_ascii_punctuation, "ASCII_PUNCTUATION"),
    (is_ascii_uppercase, "ASCII_UPPERCASE"),
    (is_ascii_whitespace, "ASCII_WHITESPACE"),
    (char::is_control, "CONTROL"),
    (char::is_lowercase, "LOWERCASE"),
    (char::is_numeric, "NUMERIC"),
    (char::is_uppercase, "UPPERCASE"),
    (char::is_whitespace, "WHITESPACE"),
    (<char as unicode_xid::UnicodeXID>::is_xid_start, "XID_START"),
    (
        <char as unicode_xid::UnicodeXID>::is_xid_continue,
        "XID_CONTINUE",
    ),
];
