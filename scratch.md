# Carml

Minimal syntax
```carml
(* Simple Types *)
let i: int = 1234
let b: bool = true
let f: float = 1.234
let s: string = "Hello, world"
let c: char = 'a'

(* Tuples and lists *)
let t: int * int = (1234, 1234)
let t2: int * int * int * int = (1234, 4321, 1234, 4321)

let l: int list = [1234; 1234; 1234]
let l2: int list = []

(* Inductive Types *)
type Record = A of int * int | B of string
let r: Record = A(1234, 1234)
let r2: Record = B("abc")

(* Functions *)
let f a b: int -> int -> int = a + b

(* Secret annotation *)
let i: secret int = 1234
let f s: secret int -> secret int = s


(* Sample program *)
type log_lvl =
    | Debug
    | Info
    | Warn
    | Fatal

let log_lvl_to_int lvl: log_lvl -> int =
    match lvl with
    | Debug -> 0
    | Info -> 1
    | Warn -> 2
    | Fatal -> 3

let mk_log lvl: log_lvl -> string -> log_lvl -> unit =
    let l: int = log_lvl_to_int l in
    fun msg lvl': string -> log_lvl -> unit ->
        let l': int = log_lvl_to_int lvl' in
        match l' >= l with
        | true -> print_string msg
        | _ -> ()

let main: unit -> unit =
    let log = mk_log Info in
    log "debug" Debug;
    log "info" Info;
    log "warn" Warn;
    log "fatal" Fatal;
    () 
```

# Infix and Symbolic functions

Instead of any builtin operators Carml provides symbolic functions of the form:

```ml
let concat_string a b = ...

let (++) a b = concat_string a b
```

Only a "symbolic" function may use the infix notation, but it can also use the regular prefix notation:

```ml
a ++ b
++ a b


let (||) a b = Builtin.orb a b

a || b

|| a b

let or a b = Bultin.orb a b

(* a or b invalid *)
or a b (* valid *)
```

A symbolic function may only have one (unary) or two (binary) parameters, a unary symbolic function always uses the prefix notation and a binary symbolic function always uses infix notation:

```ml
let (!) a =
    match a with
    | true -> false
    | false -> true

let (!) actor message = Actor.message_send actor message

let a = ! true

let a = (spawn echo) ! Hello
```

Precedence is defined on symbolic functions in the standard library, although they are normal functions they have special treatment in the parser and all functions shadowing builtin symbolics do too:

```ml
open Prelude

let a = 1 + 2 / 3 * 4 - 6
(* is equivalent to *)
let a = (1 + (2 / (3 * 4))) - 6

let (+) (a: string) (b: string) = String.concat a b
let (/) (a: string) (b: string) = String.includes a b

let a = "this" + " that" / "hat"
(* is equivalent to *)
let a = "this" + (" that" / "hat")
```

All other symbolic functions will have the same precedence (lower than all builtins) and be left-associative.

# Multi-functions

A function is defined both by its name and signature, there can be multiple functions with the same name in scope.

```ml
let (+) (a: int) (b: int): int = VM.integer_add a b
let (+) (a: float) (b: float): float = VM.floating_add a b

let a: int = 1 + 2
let b: float = 1.0 + 2.0
```

Because functions can be partially applied, all functions may be partially shadowed by another function:

```ml
let f a b: int -> int -> int = a + b
let f a b: float -> float -> float = a + b

(* Here we have two f multi-functions in scope at the same time, *)
/* as well as partials f a: int -> int, f a: float -> float *) */


let f a: int -> bool = true

/*
    The new function f shadows a partial application of f: int -> int -> int
    with a single parameter  f a: int -> int with f a: bool
*/

```

TODO fix above behavior


# Next steps

<!-- 1. Parse symbolic function declarations -->
<!-- 2. Parse symbolic function invocations (postfix, infix, unary, binary) -->
<!-- 3. Assert precedence of builtins -->
<!-- 4. Remove binop, unop, listop nodes from ast, tast -->
<!-- 5. Remove binop, unop, listop from interpreter, inference -->
<!-- 6. Add support of multi-functions in interpreter (error on too many params) -->
7. Modify inference to work

# Modules

Simple collections of declarations, may be nested, open imports entire set of declarations.

```ml
module Prelude
module Numeric

open VM

let (+) a b = VM.integer_add a b
let (+) a b = VM.floating_add a b

end
open Numeric
end

open Prelude

let a = 1 + 2
```