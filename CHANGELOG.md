# 2023/04/10: 0.13.0

- Fix more `manual_range_contains` lints in generated code.

- `let` bindings can now appear inside `rule`s. Previously `let`s were only
  allowed at the top-level. (#28)

- You can now add `#[derive(Clone)]` before the lexer type name to implement
  `Clone` for the lexer type. This can be used to implement backtracking
  parsers. Example:
  ```rust
  lexer! {
      #[derive(Clone)]
      pub Lexer -> Token;
      // The struct `Lexer` will implement `Clone`

      ...
  }
  ```

# 2022/08/12: 0.12.0

- Fix `double_comparison`, `manual_range_contains` lints in generated code.
  (0ecb0b1)

- Lexer constructors `new_with_state` and `new_from_iter_with_state` no longer
  require user state to implement `Default`. (#54)

- User state can now have lifetime parameters other than `'input`. (#53)

# 2022/05/15: 0.11.0

- Lexer state is now reset on failure. (#48)

# 2022/02/20: 0.10.0

- Generated lexers now have two new constructors:

  - `new_from_iter<I: Iterator<Item = char> + Clone>(iter: I) -> Self`
  - `new_from_iter_with_state<I: Iterator<Item = char> + Clone, S>(iter: I, user_state: S) -> Self`

  These constructors allow running a lexer on a character iterator instead of a
  string slice. Generated lexers work exactly the same way, except the `match_`
  method panics when called.

  Locations of matches can be obtained with the `match_loc(&self) -> (Loc,
  Loc)` method.

  These constructors are useful when the input is not a flat unicode string,
  but something like a rope, gap array, zipper, etc. (#41)

- `lexgen_util::Loc` now implements `Default`. This makes it easier to use
  lexgen with [LALRPOP]. (#44)

[LALRPOP]: https://github.com/lalrpop/lalrpop

# 2022/01/31: 0.9.0

- New regex syntax `#` added for character set difference, e.g. `re1 # re2`
  matches characters in `re1` that are not in `re2`. `re1` and `re2` need to be
  "character sets", i.e. `*`, `+`, `?`, `"..."`, `$`, and concatenation are not
  allowed.

- **Breaking change:** `LexerError` type is refactored to add location
  information to all errors, not just `InvalidToken`. Previously the type was:

  ```rust
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub enum LexerError<E> {
      InvalidToken {
          location: Loc,
      },

      /// Custom error, raised by a semantic action
      Custom(E),
  }
  ```

  with this change, it is now:

  ```rust
  #[derive(Debug, Clone, PartialEq, Eq)]
  pub struct LexerError<E> {
      pub location: Loc,
      pub kind: LexerErrorKind<E>,
  }

  #[derive(Debug, Clone, PartialEq, Eq)]
  pub enum LexerErrorKind<E> {
      /// Lexer error, raised by lexgen-generated code
      InvalidToken,

      /// Custom error, raised by a semantic action
      Custom(E),
  }
  ```

- A new syntax added for right contexts. A right context is basically
  lookahead, but can only be used in rules and cannot be nested inside regexes.
  See README for details. (#29)

# 2021/11/30: 0.8.1

New version published to fix broken README pages for lexgen and lexgen_util in
crates.io.

# 2021/10/30: 0.8.0

- **Breaking change:** Starting with this release, lexgen-generated lexers now
  depend on `lexgen_util` package of the same version. If you are using lexgen
  version 0.8 or newer, make sure to add `lexgen_util = "..."` to your
  `Cargo.toml`, using the same version number as `lexgen`.

- Common code in generated code is moved to a new crate `lexgen_util`.
  lexgen-generated lexers now depend on `lexgen_util`.

- **Breaking change:** Line and column tracking implemented. Iterator
  implementation now yields `(Loc, Token, Loc)`, where `Loc` is defined in
  `lexgen_util` as `struct Loc { line: u32, col: u32, byte_idx: usize }`.

- Fixed a bug when initial state of a rule does not have any transitions (rule
  is empty). (#27, 001ea51)

- Fixed a bug in codegen that caused accidental backtracking in some cases.
  (#27, 001ea51)

- Fixed a bug that caused incorrect lexing when a lexer state has both range
  and any (`_`) transitions. (#31)

# 2021/10/21: 0.7.0

- Regex syntax updated to include "any character" (`_`) and "end of input"
  (`$`).

  Previously "any character" (`_`) could be used as a rule left-hand side, but
  was not allowed in regexes.

- Semantic action functions that use user state (`state` method of the lexer
  handle) no longer need `mut` modifier in the handle argument.

  This will generate warnings in old code with semantic actions that take a
  `mut` argument.

- New lexer method `reset_match` implemented to reset the current match.

# 2021/10/19: 0.6.0

- Fixed precedences of concatenation (juxtaposition) and alternation (`|`).

- Fixed lexing in lexers that require backtracking to implement longest match
  rule. (#16)

# 2021/10/07: 0.5.0

- Accepting states without transitions are now simplified in compile time and
  semantic actions of such states are inlined in the states that make a
  transition to such accepting states. In Lua 5.1 lexer this reduces a
  benchmark's runtime by 14.9%. (#7)

  Note that this potentially duplicates a lot of code in the generated code
  when some states have large semantic action codes and lots of incoming edges
  in the DFA. However in practice I haven't observed this yet. (#8)

- DFA states with one predecessor are now inlined in the predecessor states.
  This reduces code size and improves runtime performance. (33547ec)

- We now reset the current match after returning a token (with `return_` and
  `switch_and_return`). (#11)

# 2021/05/30: 0.4.0

- lexgen now comes with a set of built-in regular expressions for matching
  Unicode alphanumerics, uppercases, whitespaces etc. See README for details.

- Fixed a few issues with end-of-stream handling (cbaabe2)

# 2021/05/28: 0.3.0

- Fixed handling of overlapping ranges in a single NFA/DFA state. (#3)

# 2021/05/16: 0.2.2

- `LexerError` type now implements `Clone` and `Copy`.

# 2021/05/06: 0.2.1

- Fixed various bugs in `_` pattern handling.

# 2021/05/05: 0.2.0

- It is now possible to use the special lifetime `'input` in your token types
  to borrow from the input string. Example:

  ```rust
  enum Token<'input> {
      Id(&'input str),
  }

  lexer! {
      Lexer -> Token<'input>;

      rule Init {
          [' ' '\t' '\n']; // skip whitespace

          ['a'-'z']+ => |lexer| {
              let match_ = lexer.match_();
              lexer.return_(Token::Id(match_))
          },
      }
  }
  ```

  See also the Lua 5.1 lexer example, which is updated to use this feature.

- The `rule Init { ... }` syntax can now be omitted when you don't need named
  rule sets. For example, the example in the previous changelog entry can be
  simplified as:

  ```rust
  lexer! {
      Lexer -> Token<'input>;

      [' ' '\t' '\n'], // skip whitespace

      ['a'-'z']+ => |lexer| {
          let match_ = lexer.match_();
          lexer.return_(Token::Id(match_))
      },
  }
  ```

- `pub` keyword before a lexer name now generates the type as `pub`. Useful for
  using the generated lexer in other modules. Example:

  ```rust
  lexer! {
      pub Lexer -> Token;

      ...
  }
  ```

- Two new action kinds: "fallible" and "simple" added. The old ones defined
  with `=>` are now called "infallible".

  - "fallible" actions are defined with `=?` instead of `=>`. The difference
    from infallible actions is the return type is `Result<Token, UserError>`,
    instead of `Token`, where `UserError` is defined using `type Error = ...;`
    syntax. LHS can have a `<'input>` lifetime parameter when borrowing from
    the user input in the error values. When a user error type is defined, the
    lexer error struct becomes an enum, with two variants:

    ```rust
    enum LexerError {
        LexerError { char_idx: usize },
        UserError(UserError),
    }
    ```

  - "simple" actions are defined with `=` instead of `=>`. The RHS needs to be a
    value for a token, instead of a closure for a lexer action. This rule kind is
    useful when matching keywords and other simple tokens in a language. Example:

    ```rust
    lexer! {
        Lexer -> Token;

        '(' = Token::LParen,
        ')' = Token::RParen,
        ...
    }
    ```

    The syntax `<regex> = <expr>` is syntactic sugar for `<regex> => |lexer|
    lexer.return_(<expr>)`.

# 2021/04/22: 0.1.0

- Initial release
