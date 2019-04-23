open Ast
open Core

let propagate_error (error_messages: string option) (maybe_error: string option): string option =
  match error_messages, maybe_error with
  | Some errors, Some error -> Some (errors ^ "\n" ^ error)
  | e, None -> e
  | None, e -> e

let typecheck_propagate (typecheck: 'a -> string option) (e: string option) (node: 'a): string option =
  let new_e = typecheck node in
  propagate_error e new_e

let rec match_typ (t: typ) (t': typ): (bool, string) result =
  match t, t' with
  | TAny, _ -> Ok true
  | _, TAny -> Ok true
  | TFun (t1', t2'), TFun (t1'', t2'') -> (
    match (match_typ t1' t1''), (match_typ t2' t2'') with
    | Ok e, Ok e' -> Ok (e && e')
    | Error e, Error e2 -> Error (e ^ "\n" ^ e2)
    | Error e, _ -> Error e
    | _, Error e -> Error e
  )
  | TTuple t, TTuple t' -> (
    match List.zip t t' with
    | Some tt -> Ok (List.fold_left tt ~f:(fun acc (t, t') -> t = t' && acc) ~init:true)
    | None -> Error "tuple type lengths do not match"
  )
  | TRecord t1', TRecord t2' -> Ok true (* TODO symbol table for this *)
  | TList t1', TList t2' -> match_typ t1' t2'
  | TSecret t1', TSecret t2' -> match_typ t1' t2'
  | t1', t2' -> Ok (t1' = t2')

let rec typecheck_literal (t: typ) (l: literal): string option =
  match l, t with
  | Unit, TUnit -> None
  | Int _, TInt -> None
  | Float _, TFloat -> None
  | String _, TString -> None
  | Char _, TChar -> None
  | Bool _, TBool -> None
  | l', TSecret t' -> typecheck_literal t' l'
  | l', t' -> Some (sprintf "Expected type %s, found %s" (show_typ t') (show_literal l'))

let rec typecheck_match (t: typ) (e: expr) (match_branches: (match_branch * expr) list): string option =
  match match_branches with
  | [] -> None
  | (branch, branch_expr) :: rest -> (
    let rec branch_to_typ (branch: match_branch): (typ, string) result =
      match branch with
      | ML l -> (
        match l with
        | Unit -> Ok TUnit
        | Int _ -> Ok TInt
        | Float _ -> Ok TFloat
        | String _ -> Ok TString
        | Char _ -> Ok TChar
        | Bool _ -> Ok TBool
      )
      | MVar name -> Ok TAny (* TODO symbol table for this *)
      | Blank -> Ok TAny
      | MTuple branches -> (
        let typs = List.fold branches ~f:(fun acc t ->
          match branch_to_typ t, acc with
          | Ok typ, Ok typs -> Ok (typ :: typs)
          | Error e, _ -> Error e
          | _, Error e -> Error e
        ) ~init:(Ok []) in
        match typs with
        | Ok typs -> Ok (TTuple typs)
        | Error e -> Error e 
      )
      | MRecord (constructor, branches) -> Ok (TRecord constructor) (* TODO symbol table for this *)
      | MList branches -> (
        let typs = List.map branches ~f:branch_to_typ in
        let typ = List.reduce typs ~f:(fun t t' -> (
          match t, t' with
          | Ok t, Ok t' -> (
            match match_typ t t' with
            | Ok true -> Ok t
            | Ok false -> Error (sprintf "%s does not match %s" (show_typ t) (show_typ t'))
            | Error e -> Error e
          )
          | Error e, Ok _ -> Error e
          | Ok _, Error e -> Error e
          | Error e, Error e' -> Error (e ^ "\n" ^ e') 
        )) in
          match typ with
          | Some Ok t -> Ok (TList t)
          | Some Error e -> Error e
          | _ -> Ok TAny
      )
    in
    let branch_typ = branch_to_typ branch in
    match branch_typ with
    | Ok branch_typ -> (
      let e1 = typecheck_expr branch_typ e in
      let e2 = typecheck_expr t branch_expr in
      let branch_e = propagate_error e1 e2 in
      let rest_e = typecheck_match t e rest in
      propagate_error branch_e rest_e
    )
    | Error e -> Some e
  )
and typecheck_complex (t: typ) (c: complex): string option =
  match c with
  | Tuple params when List.length params < 2 -> Some (sprintf "A tuple must have at least two members")
  | Tuple e -> (
    match t with
    | TTuple t -> (
      match List.zip e t with
      | Some expr_typs -> (
        List.fold expr_typs ~f:(fun acc (e, t) -> typecheck_propagate (typecheck_expr t) acc e) ~init:None
      )
      | None -> Some (sprintf "Tuple %s length does not match %s" (show_typ (TTuple t)) (show_expr (C (Tuple e))))
    )
    | t' -> Some (sprintf "Expected tuple, found %s" (show_typ t')) 
  )
  | Record (constructor, params) -> None (* TODO symbol table for this *)
  | List body -> (
    match t with
    | TList t' -> List.fold body ~f:(typecheck_propagate (typecheck_expr t')) ~init:None
    | t' -> Some (sprintf "expected list, found %s" (show_typ t'))
  )
and typecheck_expr (t: typ) (expr: expr): string option =
  match expr with
  | L l -> typecheck_literal t l
  | C c -> typecheck_complex t c
  | UnOp (op, e) -> typecheck_expr t e
  | BinOp (op, e1, e2) -> (
    let e1' = typecheck_expr t e1 in
    let e2' = typecheck_expr t e2 in
    propagate_error e1' e2'
  )
  | NumOp (op, e1, e2) -> (
    let e1' = typecheck_expr t e1 in
    let e2' = typecheck_expr t e2 in
    propagate_error e1' e2'
  )
  | Var name -> None (* TODO symbol table for this *)
  | LetIn (name, typ, body, rest) -> (
    let e1 = typecheck_expr typ body in
    let e2 = typecheck_expr t rest in
    propagate_error e1 e2
  )
  | Fun (params, typ, body) -> ( (* TODO symbol table for this *)
    let rec eat_param_typs (t: typ) (params: string list): (typ, string) result =
      match params with
      | _ :: rest -> (
        match t with
        | TFun (t', t'') -> eat_param_typs t'' rest
        | t' -> Error "type signature does not match function parameters"
      )
      | [] -> Ok t
    in
    let result_typ = eat_param_typs typ params in
    match result_typ with
    | Ok result_typ -> (
      match match_typ typ t with
      | Ok true ->  typecheck_expr result_typ body
      | Ok false -> Some (sprintf "%s does not match %s" (show_typ typ) (show_typ t))
      | Error e -> Some e
    )
    | Error e -> Some e
  )
  | Match (e, match_branches) -> typecheck_match t e match_branches
  | App (e, params) -> None (* TODO symbol table for this *)
  | Seq (_, e') -> typecheck_expr t e'

let typecheck_stmt (stmt: stmt): string option =
  match stmt with
  | Let (name, typ, body) -> typecheck_expr typ body
  | Type (name, constructors) -> None (* TODO symbol table for this *)

let typecheck (program: program) =
  List.fold_left program ~f:(typecheck_propagate typecheck_stmt) ~init:None