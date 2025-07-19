# Test Syntax

Comments are `--` for single line and `{-` and `-}` for multi-line
The following charecters are restricted: `()[]{}@?_`, in addition to whitespace

Lexemes are combinations of any charecters that are not any of the afore, matching as large as possible
Note that for simplicity, these can have "nonesensical" forms
For instance, `כα+hi!ף` is a single lexeme

Lexemes are surronded by whitespace, or to a given side a special token
The forms are as follows:

- `( x ... )` introduces a `כ` list
- `[ x ... ]` introduces `ד`s
- `{ n }` introudces a `פ` name
- `@n` indroduces a `ף` reference
- `?` Introduces `ל`
- `_` introduces a fresh token
