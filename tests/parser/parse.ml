open Core
open Carml

(* let parse f =
  printf "========= [%s] =========\n" f;
  let lexbuf = Lexing.from_channel (In_channel.create f) in
  try
    let program = Parser.program Lexer.lexer lexbuf in
    printf "%s\n" (Carml.Pretty.pretty_program (Carml.Infer.infer_program program));
  with s -> printf "%s\n" (Exn.to_string s);
  printf "========================\n" *)

let test_infer () =
  let tests: (string * Carml.Tast.typ') list = [
    ("1", TInt);
    ("1.0", TFloat);
    ("\"three\"", TString);
    ("true", TBool);
    ("()", TUnit);
    ("(1,2,3)", TTuple [TInt; TInt; TInt]);
    ("(1, true, ())", TTuple [TInt; TBool; TUnit]);
    ("fun a -> a", TFun [TAny; TAny]);
    ("let a = \"this\" in true", TBool);
    ("let rec id = fun a -> a in true", TBool);
    ("let a = true in a", TBool);
    ("let a = true in let b = 1.0 in (a, b)", TTuple [TBool; TFloat]);
    ("let id = fun a -> a in id true", TBool)
    (* TODO not all TAnys are equal -- fix that e.g. above id 'a -> 'a applied to true has type bool, not TAny
       This also affects superfunction selection
    *)
  ] in
  List.iter tests ~f:(fun (expr_s, typ) -> (
    printf "-----------------------------------------------\n";
    printf "Testing: %s === %s\n" expr_s (Carml.Pretty.pretty_typ' typ);
    let expr = Carml.Parser.single_expression Lexer.lexer (Lexing.from_string expr_s) in
    let texpr = Carml.Infer.infer_expr Carml.Infer.empty_env expr in
    match Carml.Infer.expr_typ Carml.Infer.empty_env texpr with
    | Some t when t = typ -> printf "Ok\n";
    | Some t' -> printf "Fail: %s != %s" (Carml.Pretty.pretty_typ' t') (Carml.Pretty.pretty_typ' typ)
    | None -> printf "Fail: unknown"
  ))

let () =
  test_infer ()
  (* Array.iter ~f:parse (Array.sub Sys.argv ~pos:1 ~len:(Array.length Sys.argv - 1)) *)