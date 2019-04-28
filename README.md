# Carml
A minimal ml-like language with secret annotations.

# Example
```ml
let obfuscate msg: secret(string) -> string =
  let l: int = string_len msg in
  mk_string '*' l
;;


let log msg: string -> unit = print msg;;
let log msg: secret(string) -> unit = print (obfuscate msg);;

let main: unit =
  let token: secret(string) = load_secret_value () in
  log token;
  log "Loaded secret value"
;;
```

## Syntax
The program is a series of statements separated by double semicolons (`;;`). Every let binding requires explicit type annotations (no type inference).

### Statements
* variable binding `let ident: type = expression`
* function binding `let ident params: type = expression`
* type declaration `type ident = constructors`

### Literals

#### Simple literals
* integers -- `1`, `2`, `11223344`
* booleans -- `true`, `false`
* floats -- `1.234`, `0.123`
* strings -- `"hello, world"`
* characters -- `'a'`
* unit -- `()`

#### Compound literals
* tuples -- `(a, b, c)`
* lists -- `[a; b; c]`, `a :: b :: c :: []`
* records -- `A`, `B 1`, `C (B 1)`

### Types

### Simple types
* `int`
* `bool`
* `float`
* `string`
* `char`

### Compound types
* tuples -- `A * B`
* functions -- `A -> B`
* lists -- `A list`
* secrets -- `secret(A)`

### Expressions
* binary expressions (as you would expect, `> >= = < <= != !`)
* numeric expressions (as you would expect, `+ - * /`)
* let-in binding -- `let ident: type = expression in expression`
* sequencing -- `expression; expression`

#### Functions (closures)
* `fun params: type ->> expression`
A function expression uses a double-arrow (`->>`) to disambiguate the explicit type annotation, this will be removed when type inference is implemented. The function can take multiple parameters, separated by a space. See examples below:
```ml
fun a: int -> int ->> a + 1
fun a b: int -> int -> int ->> a + b
```
The top-level function statement is syntactic sugar for a variable-binding of a function expression.
```ml
let add1 a: int -> int = a + 1;;
let add1: int -> int = fun a: int -> int ->> a + 1;;
```
Function applications require explicit type annotations on arguments
```ml
let b: bool = (fun gt0 x: int -> bool ->> x > 0) (1: int);;
```
Partial application is supported
```ml
let b: int -> bool = (fun gt a b: int -> int ->> a > b) (1: int);;
```

#### Pattern matching
Each branch of a match expression requires a separate type annotation, to disambiguate a function type (single arrow), the match expression uses the double arrow, like the function expression.
```
match expression with
| match_expression: type ->> expression 
```
* Match expressions
```
_
a
(a, b)
a :: b
A
A 1
A (1, 2)
```