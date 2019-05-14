open Core
open Carml

let parse_expr buf =
  let lexbuf = Lexing.from_string buf in
  try
    Ok (Parser.single_expression Lexer.micro lexbuf)
  with s -> Error (Exn.to_string s)

let parse_stmt buf =
  let lexbuf = Lexing.from_string buf in
  try
    Ok (Parser.single_statement Lexer.micro lexbuf)
  with s -> Error (Exn.to_string s)

let parse f =
  let lexbuf = Lexing.from_channel (In_channel.create f) in
  try
    let program = Parser.program Lexer.micro lexbuf in
    printf "%s" (Carml.Ast.show_program program)
  with s -> printf "%s\n" (Exn.to_string s)

let typecheck f =
  let lexbuf = Lexing.from_channel (In_channel.create f) in
  try
    let program = Parser.program Lexer.micro lexbuf in
    match Typecheck.typecheck program with
    | (program, Some err) -> Error.print_errs err
    | (program, None) -> printf "Typecheck ok"
  with s -> printf "%s\n" (Exn.to_string s)

let pretty f =
  let lexbuf = Lexing.from_channel (In_channel.create f) in
  try
    let program = Parser.program Lexer.micro lexbuf in
    printf "%s" (Carml.Pretty.pretty_program program)
  with s -> printf "%s\n" (Exn.to_string s)

let repl_header = "
Carml 0.0.1
"

type repl_mode = Expr | Stmt

let rec _repl st (mode: repl_mode) =
  printf ">>> ";
  Out_channel.flush Out_channel.stdout;
  let line = In_channel.input_line In_channel.stdin in
  match line with
  | None -> ()
  | Some "" -> _repl st mode
  | Some "exit" -> ()
  | Some ":expr" -> printf "Switched to expression mode\n"; _repl st Expr
  | Some ":stmt" -> printf "Switched to statement mode\n"; _repl st Stmt
  | Some l -> (
    match mode with
    | Expr -> (
      match parse_expr l with
      | Error e -> printf "Parse error: %s" e; _repl st mode
      | Ok s -> printf "%s\n" (Carml.Eval.show_value (Carml.Eval.eval_expr st s)); _repl st mode
    )
    | Stmt -> (
      match parse_stmt l with
      | Ok s -> _repl (Carml.Eval.eval_stmt st s) mode
      | Error e -> printf "Parse error: %s" e; _repl st mode
    )
  )

let repl st =
  printf "%s\n" repl_header;
  _repl st

let help = fun _ ->
  printf "
Usage: cl [command] ...
Options:
  parse <filename>            Parse filename and print AST
  typecheck <filename>        Typecheck filename and print errors
  pretty <filename>           Pretty-print filename to stdin
  repl                        Launch REPL

  -h/--help                   Print this message\n"

let () =
  if (Array.length Sys.argv) < 2 then printf "Too few arguments (-h/--help for help)\n" else
  match Array.nget Sys.argv 1 with
  | "parse" -> parse (Array.nget Sys.argv 2)
  | "typecheck" -> typecheck (Array.nget Sys.argv 2)
  | "pretty" -> pretty (Array.nget Sys.argv 2)
  | "repl"   -> repl Carml.Eval.empty_state Expr
  | "-h" -> help ()
  | "--help" -> help ()
  | s -> printf "Unknown command: %s (-h/--help for help)\n" s