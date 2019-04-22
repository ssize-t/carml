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