# lexer_gen: A fully-featured lexer generator, implemented as a proc macro

Here's a simple lexer definition for lexing identifiers:

```rust
lexer! {
    // Name of the lexer and the token type is specified as first thing
    Lexer -> Token;

    // Regular expressions can be named with `let` syntax
    let init = ['a'-'z'];
    let subseq = $init | ['A'-'Z' '0'-'9' '-' '_'];

    // Rules sets have names, to allow stateful lexing
    rule Init {
        // Rules without a right-hand sides for skipping whitespace, comments, etc.
        [' ' '\t' '\n']+,

        // Rule for matching identifiers
        $init $subseq* =>
            |lexer| {
                let token = Token::Id(lexer.match_().to_owned());
                lexer.return_(token)
            },
    }
}

// Generated lexers are initialized with a `&str` for the input
let mut lexer = Lexer::new(" abc123Q-t  z9_9");

// Lexers implement `Iterator<Item=Result<(usize, T, usize), LexerError>>`,
// where `T` is the token type specified in the lexer definition (`Token` in
// this case), and `usize`s indicate byte indices of beginning and end of the
// returned tokens.
assert_eq!(
    lexer.next(),
    Some(Ok((1, Token::Id("abc123Q-t".to_owned()), 10)))
);
assert_eq!(
    lexer.next(),
    Some(Ok((12, Token::Id("z9_9".to_owned()), 16)))
);
assert_eq!(lexer.next(), None);
```

You can see more examples [here][1], and a full Lua 5.1 lexer [here][2].

## Motivation

Implementing lexing is often (along with parsing) the most tedious part of
implementing a language. Lexer generators make this much easier, but in Rust
existing lexer generators miss essential features for practical use, and/or
require a pre-processing step when building.

My goal with lexer_gen is to have a feature-complete lexer generator that is
easy to use.

[1]: https://github.com/osa1/lexer_gen/blob/main/tests/tests.rs
[2]: https://github.com/osa1/lexer_gen/blob/main/examples/lua_5_1.rs
