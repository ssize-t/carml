open Core
open Ast
open Pretty

type node =
  | S of stmt
  | E of expr
  | MB of match_branch

type err =
  | SyntaxError of loc * string
  | TypeError of loc * node * string

let indent_block (b: string): string =
  "\t" ^ String.concat (String.split b ~on:'\n') ~sep:"\n\t"

let pretty_node (n: node): string = ""
  (* match n with
  | S s -> pretty_stmt s
  | E e -> pretty_expr e
  | MB b -> pretty_branch b *)

let err_line (e: err): int =
  match e with
  | SyntaxError (l, _) -> l
  | TypeError (l, _, _) -> l

let print_err (e: err) =
  match e with
  | SyntaxError (line_no, msg) -> printf "Syntax error on line %d: %s\n" line_no msg
  | TypeError (_, n, msg) -> printf "\n\tType error: %s\n" msg; printf "%s\n" (pretty_node n |> indent_block |> indent_block)

let print_line_no (line_no: int) =
  printf "Line %d:\n" line_no

let rec print_errs (es: err list) (line_no: int) =
  match es with
  | e :: t -> (
    match err_line e with
    | l' when line_no = 0 -> (
      print_line_no l';
      print_err e;
      print_errs t l'
    )
    | l' when l' <> line_no -> (
      printf "\n";
      print_line_no l';
      print_err e;
      print_errs t l'
    )
    | _ -> print_err e; print_errs t line_no
  )
  | [] -> ()

let print_errs (es: err list) =
  print_errs (List.rev es) 0;
  printf "\n%d errors found\n" (List.length es)