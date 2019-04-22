open Core
open Carml

let parse f =
  printf "========= [%s] =========\n" f;
  let lexbuf = Lexing.from_channel (In_channel.create f) in
  try
    let program = Parser.program Lexer.micro lexbuf in
    printf "%s\n" (Carml.Ast.show_program program);
    printf "========================\n";
  with s -> printf "%s\n" (Exn.to_string s);
  printf "========================\n"

let () =
  Array.iter ~f:parse (Array.sub Sys.argv ~pos:1 ~len:(Array.length Sys.argv - 1))