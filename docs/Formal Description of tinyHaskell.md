Here, I am laying down the *formal description* of tinyHaskell's grammar using the Extended Backus-Naur Form (EBNF)—which is what is commonly used to formally describe the valid syntax for programming languages.

**The to-be-implemented Subset of Haskell (tinyHaskell)**:
- If then else
- Where expressions
- Let expressions
- Algebraic Data Types
- case expressions
- Strings, and concatenation
- Arithmetic operations (only on Int)
- Type Synonyms
- Conditional expressions

**Terminal symbols:**
- `let`
- `in`
- `where`
- `if`
- `then`
- `else`
- `case`
- `of`
- `::`
- `Int`
- `String`
- `+`
- `-`
- `*`
- `/`
- `(`
- `)`
- `,`
- `|`
- `_`
- `identifier` (covering both variable and constructor names)
- `"string literal"`
- `integer literal`

**Non-terminal symbols:**

- `expr`
- `pat`
- `type`
- `data_type_decl`
- `type_synonym`
- `func_decl`

**Grammar productions:**

**Expressions:**

```EBNF
expr ::= identifier ;
      | literal ;
      | `let` pat `=` expr `in` expr ;
      | expr `where` `binding_list` ;
      | `Int` ;
      | `(` expr `)` ;
      | expr `op` expr ;   // op is one of +, -, *, /
      | `String` ;
      | `"string literal"` ;
      | expr `++` expr ;
      | `if` expr `then` expr `else` expr ;
      | `case` expr `of` `matching_list` ;
      | func_call

binding_list ::= pat `=` expr { `,` pat `=` expr }

matching_list ::= pat `->` expr { "|" pat `->` expr }
```

**Patterns:**

```EBNF
pat ::= identifier ;
      | `_` ;
      | `constructor` `pat_list` ;   // constructor is an identifier

pat_list ::= pat { `,` pat }
```

**Function declarations:**

```EBNF
func_decl ::= `func_name` `pat_list` `::` type `where` `binding_list`

func_call ::= identifier `pat_list` ;
```

**Algebraic data types & Type synonyms:**

```EBNF
data_type_decl ::= `data` `data_type_name` `=` `constructor_list`

constructor_list ::= `constructor` | `constructor` `|` `constructor_list`

type_synonym ::= `type` `type_name` `::` type
```