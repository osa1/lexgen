# Unreleased

- You can now use the special lifetime `'input` in your token types to borrow
  from the input string. Example:

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

- You can now omit the `rule Init { ... }` syntax when you don't need named
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

# 2021/04/22: 0.1.0

- Initial release
