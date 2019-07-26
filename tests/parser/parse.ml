open Core
open Carml

let parse f =
  printf "========= [%s] =========\n" f;
  let lexbuf = Lexing.from_channel (In_channel.create f) in
  try
    let program = Parser.program Lexer.lexer lexbuf in
    printf "%s\n" (Carml.Ast.show_program program);
  with s -> printf "%s\n" (Exn.to_string s);
  printf "========================\n"

let test_infer () =
  let num_success = ref 0 in
  let num_failed = ref 0 in
  let tests: (string * Carml.Tast.typ') list = [
    (* ("1", TInt);
    ("1.0", TFloat);
    ("\"three\"", TString);
    ("true", TBool);
    ("()", TUnit);
    ("(1,2,3)", TTuple (TInt, TTuple (TInt, TInt)));
    ("(1, true, ())", TTuple (TInt, TTuple (TBool, TUnit)));
    ("fun a -> a", TFun (TVar 1, TVar 1));
    ("let a = \"this\" in true", TBool);
    ("let rec id = fun a -> a in true", TBool);
    ("let a = true in a", TBool);
    ("let a = true in let b = 1.0 in (a, b)", TTuple (TBool, TFloat));
    ("let id = fun a -> a in id true", TBool);
    ("fun a -> (
      match a with
      | (b, _) -> b
      )", TFun (TTuple (TVar 0, TVar 1), TVar 0));
    ("fun a -> (
      match a with
      | (a, _) -> a
      )", TFun (TTuple (TVar 0, TVar 1), TVar 0));
    ("fun a -> (
      match a with
      | (_, a) -> a
      )", TFun (TTuple (TVar 0, TVar 1), TVar 1));
    ("fun a -> (
      match a with
      | (a, _, c) -> (a, c)
      )", TFun (TTuple (TVar 0, TTuple (TVar 1, TVar 2)), TTuple (TVar 0, TVar 2)));
    ("let fst = fun a -> (
      match a with
      | (a, _) -> a
      ) in fst (1, 2)", TInt);
    ("let fst_last = fun a -> (
        match a with
        | (a, _, c) -> (a, c)
      ) in fst_last (1, true, 2)", TTuple (TInt, TInt)); *)
    ("fun l -> (
        match l with
        | h :: _ -> h
        | _ -> 0
      )", TFun (TList TInt, TInt))
  ] in
  List.iter tests ~f:(fun (expr_s, typ) -> (
    printf "-----------------------------------------------\n";
    printf "Testing: %s === %s\n" expr_s (Carml.Pretty.pretty_typ typ);
    let expr = Carml.Parser.single_expression Lexer.lexer (Lexing.from_string expr_s) in
    let texpr = Carml.Infer.infer_expr Carml.Infer.empty_env expr in
    match Carml.Infer.expr_typ Carml.Infer.empty_env texpr with
    | Some t when t = typ -> num_success := !num_success + 1; printf "Ok\n";
    | Some t' -> num_failed := !num_failed + 1; printf "Fail: %s != %s" (Carml.Tast.show_typ' t') (Carml.Tast.show_typ' typ)
    | None -> num_failed := !num_failed + 1; printf "Fail: unknown"
  ));
  printf "\nRan %d tests: %d successes, %d failures" (!num_success + !num_failed) !num_success !num_failed


let test_unify () =
  let test = [
    ((Carml.Tast.TVar 6), (Carml.Tast.TVar 1));
    ((Carml.Tast.TVar 2), (Carml.Tast.TInt));
    ((Carml.Tast.TList (Carml.Tast.TVar 5)), (Carml.Tast.TVar 1));
    ((Carml.Tast.TVar 2), Carml.Tast.TVar 3);
    ((Carml.Tast.TVar 3), Carml.Tast.TVar 5);
    ((Carml.Tast.TVar 3), Carml.Tast.TVar 5);
  ] in
  let gamma = Carml.Infer.unify test in
  let show_opt t = 
    match t with
    | Some t -> Carml.Tast.show_typ' t
    | None -> "None"
  in
  printf "TVar %d --> %s\n" 1 (show_opt (gamma 1));
  printf "TVar %d --> %s\n" 2 (show_opt (gamma 2));
  printf "TVar %d --> %s\n" 3 (show_opt (gamma 3));
  printf "TVar %d --> %s\n" 4 (show_opt (gamma 4));
  printf "TVar %d --> %s\n" 5 (show_opt (gamma 5));
  printf "TVar %d --> %s\n" 6 (show_opt (gamma 6)); ()


let () =
  test_infer ();
  Array.iter ~f:parse (Array.sub Sys.argv ~pos:1 ~len:(Array.length Sys.argv - 1))