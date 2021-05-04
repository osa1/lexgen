# Unreleased

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

# 2021/04/22: 0.1.0

- Initial release
