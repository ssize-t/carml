open Ast
open Core

type err =
  | SyntaxError of loc * string
  | TypeError of loc * string 

let print_err (e: err) =
  match e with
  | SyntaxError (line_no, msg) -> printf "Syntax error on line %d: %s\n" line_no msg
  | TypeError (line_no, msg) -> printf "[%d] Type error: %s\n" line_no msg

let print_errs (es: err list): unit = List.iter (List.rev es) ~f:print_err