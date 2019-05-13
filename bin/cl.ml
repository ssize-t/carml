open Core
open Carml

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

let help = fun _ ->
  printf "
Usage: cl [command] ...
Options:
  parse <filename>            Parse filename and print AST
  typecheck <filename>        Typecheck filename and print errors
  pretty <filename>           Pretty-print filename to stdin

  -h/--help                   Print this message\n"

let () =
  if (Array.length Sys.argv) < 2 then printf "Too few arguments (-h/--help for help)\n" else
  match Array.nget Sys.argv 1 with
  | "parse" -> parse (Array.nget Sys.argv 2)
  | "typecheck" -> typecheck (Array.nget Sys.argv 2)
  | "pretty" -> pretty (Array.nget Sys.argv 2)
  | "-h" -> help ()
  | "--help" -> help ()
  | s -> printf "Unknown command: %s (-h/--help for help)\n" s