# lexgen: A fully-featured lexer generator, implemented as a proc macro

```rust
lexer! {
    // First line specifies name of the lexer and the token type returned by
    // semantic actions
    Lexer -> Token;

    // Regular expressions can be named with `let` syntax
    let init = ['a'-'z'];
    let subseq = $init | ['A'-'Z' '0'-'9' '-' '_'];

    // Rule sets have names. Each rule set is compiled to a separate DFA.
    // Switching between rule sets is done explicitly in semantic actions.
    rule Init {
        // Rules without a right-hand side for skipping whitespace,
        // comments, etc.
        [' ' '\t' '\n']+,

        // Rule for matching identifiers
        $init $subseq* => |lexer| {
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

// Lexers implement `Iterator<Item=Result<(Loc, T, Loc), LexerError>>`,
// where `T` is the token type specified in the lexer definition (`Token` in
// this case), and `Loc`s indicate line, column, and byte indices of
// beginning and end of the lexemes.
assert_eq!(
    lexer.next(),
    Some(Ok((
        Loc { line: 0, col: 1, byte_idx: 1 },
        Token::Id("abc123Q-t".to_owned()),
        Loc { line: 0, col: 10, byte_idx: 10 }
    )))
);
assert_eq!(
    lexer.next(),
    Some(Ok((
        Loc { line: 0, col: 12, byte_idx: 12 },
        Token::Id("z9_9".to_owned()),
        Loc { line: 0, col: 16, byte_idx: 16 }
    )))
);
assert_eq!(lexer.next(), None);
```

See also:

- [Simple lexer definitions in tests][1]
- [A full Lua 5.1 lexer][2]
- [An example that uses lexgen with LALRPOP][3]
- [A lexer for a simpler version of OCaml][4]

## Motivation

Implementing lexing is often (along with parsing) the most tedious part of
implementing a language. Lexer generators make this much easier, but in Rust
existing lexer generators miss essential features for practical use, and/or
require a pre-processing step when building.

My goal with lexgen is to have a feature-complete and easy to use lexer
generator.

## Usage

lexgen doesn't require a build step. Add same versions of `lexgen` and
`lexgen_util` as dependencies in your `Cargo.toml`.

## Lexer syntax

lexgen lexers start with the name of the generated lexer struct, optional user
state part, and the token type (type of values returned by semantic actions).
Example:

```rust
lexer! {
    Lexer(LexerState) -> Token;
    ...
}
```

Here the generated lexer type will be named `Lexer`. User state type is
`LexerState` (this type should be defined by the user). The token type is
`Token`.

After the lexer name and user state and token types we define the rules:

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
syntax for a rule is `<regex> => <semantic action>,`. Regex syntax is described
below. A semantic action is any Rust code with the type `fn(LexerHandle) ->
LexerAction` where `LexerHandle` and `LexerAction` are generated names derived
from the lexer name (`Lexer` in our example). More on these types below.

Regular expressions can be named with `let <name> = <regex>;` syntax. Example:

```rust
let init = ['a'-'z'];
let subseq = $init | ['A'-'Z' '0'-'9' '-' '_'];

// Named regexes can be used with the `$` prefix
$init $subseq* => |lexer| { ... }
```

You can omit the `rule Init { ... }` part and have all of your rules at the top
level if you don't need rule sets.

In summary:

- First line is in form `<lexer name>(<user state type>) -> <token type name>`.
  The `(<user state type>)` part can be omitted for stateless lexers.

- Next is the rule sets. There should be at least one rule set with the name
  `Init`, which is the name of the initial state.

- `let` bindings can be added at the top-level or in `rule`s.

## Regex syntax

Regex syntax can be used in right-hand side of let bindings and left-hand side
of rules. The syntax is:

- `$var` for variables defined in the let binding section. Variables need to be
  defined before used.
- `$$var` for built-in regexes (see "Built-in regular expressions" section
  below).
- Rust character syntax for characters, e.g. `'a'`.
- Rust string syntax for strings, e.g. `"abc"`.
- `[...]` for character sets. Inside the brackets you can have one or more of:

  - Characters
  - Character ranges: e.g. `'a'-'z'`

  Here's an example character set for ASCII alphanumerics: `['a'-'z' 'A'-'Z'
  '0'-'9']`
- `_` for matching any character
- `$` for matching end-of-input
- `<regex>*` for zero or more repetitions of `<regex>`
- `<regex>+` for one or more repetitions of `<regex>`
- `<regex>?` for zero or one repetitions of `<regex>`
- `<regex> <regex>` for concatenation
- `<regex> | <regex>` for alternation: match the first one, or the second one.
- `<regex> # <regex>` for difference: match characters in the first regex that
  are not in the second regex. Note that regexes on the left and right of `#`
  should be "characters sets", i.e. `*`, `+`, `?`, `"..."`, `$`, and
  concatenation are not allowed. Variables that are bound to character sets are
  allowed.

Binding powers (precedences), from higher to lower:

- `*`, `+`, `?`
- `#`
- Concatenation
- `|`

You can use parenthesis for grouping, e.g. `('a' | 'b')*`.

Example: `'a' 'b' | 'c'+` is the same as `(('a' 'b') | ('c'+))`.

## Right context (lookahead)

A rule in a rule set can be followed by another regex using `> <regex>` syntax,
for right context. Right context is basically a limited form of lookahead: they
can only appear after a top-level regex for a rule. They cannot be used nested
in a regex.

For example, the rule left-hand side `'a' > (_ # 'b')` matches `'a'` as long as
it's not followed by `'b'`.

See also [right context tests] for more examples.

[right context tests]: https://github.com/osa1/lexgen/blob/main/crates/lexgen/tests/right_ctx.rs

## Built-in regular expressions

lexgen comes with a set of built-in regular expressions. Regular
expressions listed below match the same set of characters as their Rust
counterparts. For example, `$$alphabetic` matches the same set of characters as
Rust's [`char::is_alphabetic`]:

- `$$alphabetic`
- `$$alphanumeric`
- `$$ascii`
- `$$ascii_alphabetic`
- `$$ascii_alphanumeric`
- `$$ascii_control`
- `$$ascii_digit`
- `$$ascii_graphic`
- `$$ascii_hexdigit`
- `$$ascii_lowercase`
- `$$ascii_punctuation`
- `$$ascii_uppercase`
- `$$ascii_whitespace`
- `$$control`
- `$$lowercase`
- `$$numeric`
- `$$uppercase`
- `$$whitespace`

(Note that in the generated code we don't use Rust `char` methods. For simple
cases like `$$ascii` we generate simple range checks. For more complicated
cases like `$$lowercase` we generate a binary search table and run binary
search when checking a character)

In addition, these two built-in regular expressions match Unicode [XID_Start and
XID_Continue]:

- `$$XID_Start`
- `$$XID_Continue`

[`char::is_alphabetic`]: https://doc.rust-lang.org/std/primitive.char.html#method.is_alphabetic
[XID_Start and XID_Continue]: http://www.unicode.org/reports/tr31/

## Rule syntax

- `<regex> => <semantic action>,`: `<regex>` syntax is as described above.
  `<semantic action>` is any Rust code with type `fn(&mut Lexer) ->
  SemanticActionResult<Token>`. More on `SemanticActionResult` type in the next
  section.

- `<regex> =? <semantic action>,`: fallible actions. This syntax is similar to
  the syntax above, except `<semantic action>` has type `fn(&mut Lexer) ->
  LexerAction<Result<Token, UserError>>`. When using rules of this kind, the
  error type needs to be declared at the beginning of the lexer with the `type
  Error = UserError;` syntax.

  When a rule of this kind returns an error, the error is returned to the
  caller of the lexer's `next` method.

- `<regex>,`: Syntactic sugar for `<regex> => |lexer| { lexer.reset_match();
  lexer.continue_() },`. Useful for skipping characters (e.g. whitespace).

- `<regex> = <token>,`: Syntactic sugar for `<regex> => |lexer|
  lexer.return_(<token>),`. Useful for matching keywords, punctuation
  (operators) and delimiters (parens, brackets).

## End-of-input handling in rule sets

The `Init` rule set terminates lexing successfully on end-of-input (i.e.
`lexer.next()` returns `None`). Other rule sets fail on end-of-input (i.e.
return `Some(Err(...))`). This is because generally the states other than the
initial one are for complicated tokens (strings, raw strings, multi-line
comments) that need to be terminated and handled, and end-of-input in those
states usually means the token did not terminate properly.

(To handle end-of-input in a rule set you can use `$` as described in section
"Regex syntax" above.)

## Handle, rule, error, and action types

The `lexer` macro generates a struct with the name specified by the user in the
first line of the lexer definition. In the example at the beginning (`Lexer ->
Token;`), name of the struct is `Lexer`.

A mut reference to this type is passed to semantic action functions. In the
implementation of a semantic action, you should use one of the methods below
drive the lexer and return tokens:

- `fn match_(&self) -> &str`: returns the current match. Note that when the
  lexer is constructed with `new_from_iter` or `new_from_iter_with_state`, this
  method panics. It should only be called when the lexer is initialized with
  `new` or `new_with_state`.
- `fn match_loc(&self) -> (lexgen_util::Loc, lexgen_util::Loc)`: returns the
  bounds of the current match
- `fn peek(&mut self) -> Option<char>`: looks ahead one character
- `fn state(&mut self) -> &mut <user state type>`: returns a mutable reference
  to the user state
- `fn return_(&self, token: <user token type>) -> SemanticActionResult`:
  returns the passed token as a match.
- `fn continue_(&self) -> SemanticActionResult`: ignores the current match and
  continues lexing in the same lexer state. Useful for skipping characters.
- `fn switch(&mut self, rule: LexerRule) -> SemanticActionResult`: used for
  switching between lexer states. The `LexerRule` (where `Lexer` part is the
  name of the lexer as specified by the user) is an enum with a variant for
  each rule set name, for example, `LexerRule::Init`. See the stateful lexer
  example below.
- `fn switch_and_return(&mut self, rule: LexerRule, token: <user token type>)
  -> SemanticActionResult`: switches to the given lexer state and returns the
  given token.
- `fn reset_match(&mut self)`: resets the current match. E.g. if you call
  `match_()` right after `reset_match()` it will return an empty string.

Semantic action functions should return a `SemanticActionResult` value obtained
from one of the methods listed above.

## Initializing lexers

lexgen generates 4 constructors:

- `fn new(input: &str) -> Self`: Used when the lexer does not have user state,
  or user state implements `Default`.

- `fn new_with_state(input: &str, user_state: S) -> Self`: Used when the lexer
  has user state that does not implement `Default`, or you want to initialize
  the state with something other than the default. `S` is the user state type
  specified in lexer definition. See stateful lexer example below.

- `fn new_from_iter<I: Iterator<Item = char> + Clone>(iter: I) -> Self`: Used
  when the input isn't a flat string, but something like a rope or zipper. Note
  that the `match_` method panics when this constructor is used. Instead use
  `match_loc` to get the location of the current match.

- `fn new_from_iter_with_state<I: Iterator<Item = char> + Clone, S>(iter: I,
  user_state: S) -> Self`: Same as above, but doesn't require user state to
  implement `Default`.

## Stateful lexer example

Here's an example lexer that counts number of `=`s appear between two `[`s:

```rust
lexer! {
    // `usize` in parenthesis is the user state type, `usize` after the arrow
    // is the token type
    Lexer(usize) -> usize;

    rule Init {
        $$ascii_whitespace,                             // line 7

        '[' => |lexer| {
            *lexer.state() = 0;                         // line 10
            lexer.switch(LexerRule::Count)              // line 11
        },
    }

    rule Count {
        '=' => |lexer| {
            *lexer.state() += 1;                        // line 17
            lexer.continue_()                           // line 18
        },

        '[' => |lexer| {
            let n = *lexer.state();
            lexer.switch_and_return(LexerRule::Init, n) // line 23
        },
    }
}

let mut lexer = Lexer::new("[[ [=[ [==[");
assert_eq!(
    lexer.next(),
    Some(Ok((
        Loc { line: 0, col: 0, byte_idx: 0 },
        0,
        Loc { line: 0, col: 2, byte_idx: 2 },
    )))
);
assert_eq!(
    lexer.next(),
    Some(Ok((
        Loc { line: 0, col: 3, byte_idx: 3 },
        1,
        Loc { line: 0, col: 6, byte_idx: 6 },
    )))
);
assert_eq!(
    lexer.next(),
    Some(Ok((
        Loc { line: 0, col: 7, byte_idx: 7 },
        2,
        Loc { line: 0, col: 11, byte_idx: 11 },
    )))
);
assert_eq!(lexer.next(), None);
```

Initially (the `Init` rule set) we skip spaces (line 7). When we see a `[` we
initialize the user state (line 10) and switch to the `Count` state (line 11).
In `Count`, each `=` increments the user state by one (line 17) and skips the
match (line 18). A `[` in `Count` state returns the current number and switches
to the `Init` state (line 23).

[1]: https://github.com/osa1/lexgen/blob/main/crates/lexgen/tests/tests.rs
[2]: https://github.com/osa1/lexgen/blob/main/crates/lexgen/tests/lua_5_1.rs
[3]: https://github.com/osa1/lexgen/tree/main/crates/lexgen_lalrpop_example
[4]: https://github.com/osa1/mincaml/blob/master/src/lexer.rs
