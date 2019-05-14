open Core
open Carml

let run f =
  let lexbuf = Lexing.from_channel (In_channel.create f) in
  try
    let program = Parser.program Lexer.micro lexbuf in
    match Carml.Typecheck.typecheck program with
    | (program, Some errs) -> Error.print_errs errs; None
    | (program, None) -> Some (Carml.Eval.eval_program program)
  with s -> printf "%s\n" (Exn.to_string s); None

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
    | (program, Some err) -> Error.print_errs err; printf "%s" (Carml.Pretty.pretty_program program);
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
Type \":help\" for more information
"

type repl_mode = Expr | Stmt | Auto

let show_mode mode =
  match mode with
  | Expr -> "expression"
  | Stmt -> "statement"
  | Auto -> "auto"

let repl_help mode = sprintf "
Type \":stmt\" to enter statement mode
Type \":expr\" to enter expression mode
Type \":load <filename>\" to load a file into the current environment

You are in %s mode
" (show_mode mode)

let rec _repl st (mode: repl_mode) (buf: string) =
  printf "%s" (if buf = "" then ">>> " else "    ");
  Out_channel.flush Out_channel.stdout;
  let line = In_channel.input_line In_channel.stdin in
  match line with
  | None -> ()
  | Some "" -> _repl st mode buf
  | Some "exit" when buf = "" -> ()
  | Some ":auto" when buf = "" -> printf "Switched to auto mode\n"; _repl st Auto ""
  | Some ":expr" when buf = "" -> printf "Switched to expression mode\n"; _repl st Expr ""
  | Some ":stmt" when buf = "" -> printf "Switched to statement mode\n"; _repl st Stmt ""
  | Some ":help" when buf = "" -> printf "%s" (repl_help mode); _repl st mode ""
  | Some l when String.substr_index l ~pattern:":load" = Some 0 -> (
    let filename = String.strip (String.substr_replace_first l ~pattern:":load" ~pos:0 ~with_:"") in
    match run filename with
    | Some st' -> _repl st' mode ""
    | None -> _repl st mode ""
  )
  | Some l when String.substr_index l ~pattern:";;" = Some (String.length l - 2) -> (
    let l = buf ^ l in
    let pos = String.substr_index_all l ~pattern:";;" ~may_overlap:false in
    let lines, _ = List.fold pos ~f:(fun (acc, old_pos) new_pos -> (acc @ [(String.sub l ~pos:old_pos ~len:(new_pos-old_pos))], new_pos + 2)) ~init:(([], 0)) in
    let st' = List.fold lines ~f:(fun st line -> (
      match mode with
      | Expr -> (
        match parse_expr line with
        | Error e -> printf "Parse error: %s\n" e; st
        | Ok s -> printf "%s\n" (Carml.Eval.pretty_value (Carml.Eval.eval_expr st s)); st
      )
      | Stmt -> (
        match parse_stmt line with
        | Ok s -> Carml.Eval.eval_stmt st s
        | Error e -> printf "Parse error: %s\n" e; st
      )
      | Auto -> (
        match parse_stmt line with
        | Ok s -> Carml.Eval.eval_stmt st s
        | Error e -> (
          match parse_expr line with
          | Ok s -> printf "%s\n" (Carml.Eval.pretty_value (Carml.Eval.eval_expr st s)); st
          | Error e -> printf "Parse error: %s\n" e; st
        )
      )
    )) ~init:st
    in
    _repl st' mode ""
  )
  | Some l -> _repl st mode (buf ^ l)

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
  run <filename>              Interpret file filename

  -h/--help                   Print this message\n"

let () =
  if (Array.length Sys.argv) < 2 then printf "Too few arguments (-h/--help for help)\n" else
  match Array.nget Sys.argv 1 with
  | "parse" -> parse (Array.nget Sys.argv 2)
  | "typecheck" -> typecheck (Array.nget Sys.argv 2)
  | "pretty" -> pretty (Array.nget Sys.argv 2)
  | "repl"   -> repl Carml.Eval.empty_state Auto ""
  | "run"    -> run (Array.nget Sys.argv 2); ()
  | "-h" -> help ()
  | "--help" -> help ()
  | s -> printf "Unknown command: %s (-h/--help for help)\n" s