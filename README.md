# lexgen: A fully-featured lexer generator, implemented as a proc macro

```rust
lexer! {
    // First line specifies name of the lexer and the token type returned by
    // user actions
    Lexer -> Token;

    // Regular expressions can be named with `let` syntax
    let init = ['a'-'z'];
    let subseq = $init | ['A'-'Z' '0'-'9' '-' '_'];

    // Rule sets have names. Each rule set is compiled to a separate DFA.
    // Switching between rule sets is done explicitly in user actions.
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

// The token type
#[derive(Debug, PartialEq, Eq)]
enum Token {
    // An identifier
    Id(String),
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

My goal with lexgen is to have a feature-complete and easy to use lexer
generator.

## Usage

lexgen doesn't require a build step. Just add it as a dependency in your
`Cargo.toml`.

## Lexer syntax

lexgen lexers start with type of the generated lexer struct, optional user state
part, and the token type (type of values returned by user actions). For example:

```rust
lexer! {
    Lexer(LexerState) -> Token;
    ...
}
```

Here the lexer struct is named `Lexer`. User state type is `LexerState` (this
type should be defined by the user). The token type is `Token`.

Next is let bindings for regular expressions. These are optional. The syntax is
`let <id> = <regex>;` where `<id>` is a Rust identifier and regex is as
described below.

```rust
let init = ['a'-'z'];
let subseq = $init | ['A'-'Z' '0'-'9' '-' '_'];
```

Finally we define the lexer rules:

```rust
rule Init {
    ...
}

rule SomeOtherRule {
    ...
}
```

The first rule set will be defining the initial state of the lexer and needs to
be named `Init`.

In the body of a `rule` block we define the rules for that lexer state. The
syntax for a rule is `<regex> => <user action>,`. Regex syntax is described
below. User action is any Rust code with type `fn(LexerHandle) -> LexerAction`
where `LexerHandle` and `LexerAction` are generated names derived from the lexer
name (`Lexer`). More on these types below.

In summary:

- First line is in form `<lexer name>(<user state type>) -> <token type name>`.
  The `(<user state type>)` part can be omitted for stateless lexers.

- Next we have let bindings for regexes. This part is optional.

- Next is the rule sets. There should be at least one rule set with the name
  `Init`, which is the name of the initial state.

## Regex syntax

Regex syntax can be used in right-hand side of let bindings and left-hand side
of rules. The syntax is:

- `$var` for variables. Variables need to be defined before used.
- Rust character syntax for characters, e.g. `'a'`.
- Rust string syntax for strings, e.g. `"abc"`.
- `[...]` for character sets. Inside the brackets you have have one or more of:
  - Characters
  - Character ranges: e.g. `'a'-'z'`
  Here's an example character set for ASCII alphanumerics: `['a'-'z' 'A'-'Z'
  '0'-'9']`
- `<regex>*` for zero or more repetitions of `<regex>`
- `<regex>+` for one or more repetitions of `<regex>`
- `<regex>?` for zero or one repetitions of `<regex>`
- `<regex> <regex>` for concatenation
- `<regex> | <regex>` for alternation (match the first one, or the second one)
- `_` can only appear at the top-level (in a LHS of a rule) and matches when
  none of the other rules match.

`*`, `+`, and `?` have the same binding power. `|` has the least binding power.
You can use parenthesis for grouping, e.g. `('a' | 'b')*`

## Rule syntax

A rule is just a `<regex> => <user action>,`. `<regex>` is as described above.
`<user action>` is any Rust code with type `fn(LexerHandle) -> LexerAction`.
More on these types below.

An alternative syntax without a right-hand side, `<regex>,`, can be used when
the user action is just "continue". (more on user actions below)

## Handle, rule, and action types

The `lexer` macro generates three types with names derived from the lexer name
specified by the user. If the lexer name is `Lexer`, then these types are:

- `LexerAction`: this is the type returned by user actions. You don't need to
  worry about the detail of this type as the handle type has methods for
  generating `LexerAction`s.

- `LexerRule`: see the `LexerHandle::switch` method below.

- `LexerHandle`: this type is the argument type of user actions. It provides
  methods for manipulating user and lexer states, and getting the current match.
  The API is:

  - `fn match_(&self) -> &str`: returns the current match
  - `fn peek(&mut self) -> Option<char>`: looks ahead one character
  - `fn state(&mut self) -> &mut <user state type>`: returns a mutable reference
    to the user state
  - `fn return_(self, token: <user token type>) -> LexerAction`: returns the
    passed token as a match.
  - `fn continue_(self)`: ignores the current match and continues lexing in the
    same lexer state. Useful for skipping whitespace and comments.
  - `fn switch(self, rule: LexerRule) -> LexerAction`: used for switching
    between lexer states. The `LexerRule` is an enum with a variant for each
    rule set name, for example, `LexerRule::Init`. See the stateful lexer
    example below.
  - `fn switch_and_return(self, rule: LexerRule, token: <user token type>)`:
    switches to the given lexer state and returns the given token.

## Stateful lexer example

Here's an example lexer that counts number of `=`s appear between two `[`s:

```rust
lexer! {
    Lexer(usize) -> usize;

    rule Init {
        ' ',

        '[' =>
            |mut lexer| {
                *lexer.state() = 0; // line 9
                lexer.switch(LexerRule::Count) // line 10
            },
    }

    rule Count {
        '=' =>
            |mut lexer| {
                let n = *lexer.state();
                *lexer.state() = n + 1; // line 18
                lexer.continue_() // line 19
            },

        '[' =>
            |mut lexer| {
                let n = *lexer.state();
                lexer.switch_and_return(LexerRule::Init, n) // line 25
            },
    }
}

let mut lexer = Lexer::new("[[ [=[ [==[");
assert_eq!(lexer.next(), Some(Ok((0, 0, 2))));
assert_eq!(lexer.next(), Some(Ok((3, 1, 6))));
assert_eq!(lexer.next(), Some(Ok((7, 2, 11))));
assert_eq!(lexer.next(), None);
```

Initially (the `Init` rule set) we skip spaces. When we see a `[` we initialize
the user state (line 9) and switch to the `Count` state (line 10). In `Count`,
each `=` increments the user state by one (line 18) and just skips the match
(line 19). A `[` in the `Count` states returns the current number and switches
to the `Init` state (line 25).

[1]: https://github.com/osa1/lexgen/blob/main/tests/tests.rs
[2]: https://github.com/osa1/lexgen/blob/main/examples/lua_5_1.rs
