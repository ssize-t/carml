open Ast
open Core

type constructor =
  | Nullary of string
  | Nary of string * typ list

type ttyp =
  | TBinding of typ
  | TType of constructor list
  | TConstructor of constructor

type env = string -> ttyp option
let empty_env: env = fun _ -> None

let update (gamma: env) (x: string) (t: ttyp): env =
  fun i -> if i = x then Some t else gamma i

let propagate_error (error_messages: string option) (maybe_error: string option): string option =
  match error_messages, maybe_error with
  | Some errors, Some error -> Some (errors ^ "\n" ^ error)
  | e, None -> e
  | None, e -> e

let propagate_success (maybe_e1: string option) (maybe_e2: string option): string option =
  match maybe_e1, maybe_e2 with
  | Some e1, Some e2 -> Some e1
  | None, _ -> None
  | _, None -> None

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
    | Some tt -> List.fold_left tt ~f:(fun acc (t, t') -> (
      match acc, match_typ t t' with
      | Ok t, Ok t' -> Ok (t && t')
      | Ok _, Error e -> Error e
      | Error e, Ok _ -> Error e
      | Error e, Error e' -> Error (e ^ "\n" ^ e') 
    )) ~init:(Ok true)
    | None -> Error "tuple type lengths do not match"
  )
  | TRecord t1', TRecord t2' -> Ok (t1' = t2')
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

let rec typecheck_match (t: typ) (gamma: env) (e: expr) (match_branches: (match_branch * expr) list): string option =
  match match_branches with
  | [] -> None
  | (branch, branch_expr) :: rest -> (
    let rec branch_to_typ (branch: match_branch) (gamma: env): (env * typ, string) result =
      let rec branches_to_typs (branches: match_branch list) (gamma: env) =
        List.fold branches ~f:(fun acc t ->
          match acc with
          | Error e -> Error e
          | Ok (g, typs) -> (
            match branch_to_typ t g with
            | Ok (g', typ) -> Ok (g', typ :: typs)
            | Error e -> Error e
          )
        ) ~init:(Ok (gamma, []))
      in
      match branch with
      | ML l -> (
        match l with
        | Unit -> Ok (gamma, TUnit)
        | Int _ -> Ok (gamma, TInt)
        | Float _ -> Ok (gamma, TFloat)
        | String _ -> Ok (gamma, TString)
        | Char _ -> Ok (gamma, TChar)
        | Bool _ -> Ok (gamma, TBool)
      )
      | MVar name -> Ok ((update gamma name (TBinding TAny)), TAny) (* TODO this should have a real type once types are inferred *)
      | Blank -> Ok (gamma, TAny)
      | MTuple branches -> (
        let typs = branches_to_typs branches gamma in
        match typs with
        | Ok (g, typs) -> Ok (g, TTuple typs)
        | Error e -> Error e
      )
      | MRecord (constructor, branches) -> (
        match gamma constructor with
        | Some (TConstructor c) -> (
          let typs = branches_to_typs branches gamma in
          match typs with
          | Ok (g, typs) -> Ok(g, TRecord constructor)
          | Error e -> Error e
        )
        | Some (TBinding t) -> Error (sprintf "Name %s refers to a binding, not a constructor" constructor)
        | Some (TType t) -> Error (sprintf "Name %s refers to a type, not a constructor" constructor)
        | None -> Error (sprintf "Name %s not found in current environment" constructor)
      )
      | MList branches -> (
        let typs = List.map branches ~f:(fun b -> branch_to_typ b gamma) in
        let typ = List.reduce typs ~f:(fun t t' -> (
          match t, t' with
          | Ok (g, t), Ok (g', t') -> (
            match match_typ t t' with
            | Ok true -> Ok (g, t)
            | Ok false -> Error (sprintf "%s does not match %s" (show_typ t) (show_typ t'))
            | Error e -> Error e
          )
          | Error e, Ok _ -> Error e
          | Ok _, Error e -> Error e
          | Error e, Error e' -> Error (e ^ "\n" ^ e') 
        )) in
          match typ with
          | Some Ok (g, t) -> Ok (g, TList t)
          | Some Error e -> Error e
          | _ -> Error "a list must contain items of the same typs"
      )
    in
    let branch_typ = branch_to_typ branch gamma in
    match branch_typ with
    | Ok (gamma', branch_typ) -> (
      let e1 = typecheck_expr branch_typ gamma e in
      let e2 = typecheck_expr t gamma' branch_expr in
      let branch_e = propagate_error e1 e2 in
      let rest_e = typecheck_match t gamma e rest in
      propagate_error branch_e rest_e
    )
    | Error e -> Some e
  )
and typecheck_complex (t: typ) (gamma: env) (c: complex): string option =
  match c with
  | Tuple params when List.length params < 2 -> Some (sprintf "A tuple must have at least two members")
  | Tuple e -> (
    match t with
    | TTuple t -> (
      match List.zip e t with
      | Some expr_typs -> (
        List.fold expr_typs ~f:(fun acc (e, t) -> typecheck_propagate (typecheck_expr t gamma) acc e) ~init:None
      )
      | None -> Some (sprintf "Tuple %s length does not match %s" (show_typ (TTuple t)) (show_expr (C (Tuple e))))
    )
    | t' -> Some (sprintf "Expected tuple, found %s" (show_typ t')) 
  )
  | Record (constructor, params) -> (
    match gamma constructor with
    | Some (TConstructor (Nullary c')) -> (
      match params with
      | [] -> None
      | params -> Some (sprintf "Too many arguments for constructor %s (0 required)" constructor)
    )
    | Some (TConstructor (Nary (c', typs))) -> (
      let param_typs = List.zip params typs in
      match param_typs with
      | Some param_typs -> (
        List.fold param_typs ~f:(fun acc (param, typ) -> typecheck_propagate (typecheck_expr typ gamma) acc param) ~init:None
      )
      | None -> Some (sprintf "Incorrect number of parameters for constructor %s (%d required)" constructor (List.length typs))
    )
    | Some (TType _) -> Some (sprintf "Name %s refers to a type, not a constructor" constructor)
    | Some (TBinding _) -> Some (sprintf "Name %s refers to a binding, not a constructor" constructor)
    | None -> Some (sprintf "Name %s not found in current environment" constructor)
  )
  | List body -> (
    match t with
    | TList t' -> List.fold body ~f:(typecheck_propagate (typecheck_expr t' gamma)) ~init:None
    | t' -> Some (sprintf "expected list, found %s" (show_typ t'))
  )
and typecheck_expr (t: typ) (gamma: env) (expr: expr): string option =
  let rec construct_env (t: typ) (params: string list) (gamma: env): ((env * typ), string) result =
    match params with
    | param :: rest -> (
      match t with
      | TFun (t', t'') -> construct_env t'' rest (update gamma param (TBinding t'))
      | t' -> Error "type signature does not match function parameters"
    )
    | [] -> Ok (gamma, t)
  in
  match expr with
  | L l -> typecheck_literal t l
  | C c -> typecheck_complex t gamma c
  | UnOp (op, e) -> (
    match t with
    | TBool -> typecheck_expr TBool gamma e
    | t' -> Some (sprintf "attempted to assign the result of a unary expression to type %s, expected %s" (show_typ t') (show_typ TBool))
  )
  | BinOp (op, e1, e2) -> (
    match op, t with
    | op', TBool when (
      (op' = Lt) || 
      (op' = Lte) ||
      (op' = Eq) ||
      (op' = Gt) ||
      (op' = Gte) ||
      (op' = Neq)
    ) -> ( 
      let e1' = typecheck_expr TInt gamma e1 in
      let e1'' = typecheck_expr TFloat gamma e1 in
      let ee1 = propagate_success e1' e1'' in
      let e2' = typecheck_expr TInt gamma e2 in
      let e2'' = typecheck_expr TFloat gamma e2 in
      let ee2 = propagate_success e2' e2'' in
      propagate_error ee1 ee2
    )
    | op', TBool -> (
      let e1' = typecheck_expr TBool gamma e1 in
      let e2' = typecheck_expr TBool gamma e2 in
      propagate_error e1' e2'
    )
    | op', t -> Some (sprintf "attempted to assign the result of a binary expression to type %s, expected %s" (show_typ t) (show_typ TBool))
  )
  | NumOp (op, e1, e2) -> (
    match t with
    | t' when t' = TInt || t' = TFloat -> (
      let e1' = typecheck_expr t' gamma e1 in
      let e2' = typecheck_expr t' gamma e2 in
      propagate_error e1' e2'
    )
    | t' -> Some (sprintf "attempted to assign the result of an arithmetic expression to type %s, %s or %s expected" (show_typ t') (show_typ TInt) (show_typ TFloat))
  )
  | Var name -> (
    match gamma name with
    | None -> Some (sprintf "Name %s not found in current environment" name)
    | Some (TBinding t') -> (
      match match_typ t t' with
      | Ok true -> None
      | Ok false -> Some (sprintf "Type of variable %s (%s) does not match required %s" name (show_typ t') (show_typ t))
      | Error e -> Some e
    )
    | Some (TType t) -> Some (sprintf "Name %s refers to a type, it cannot be used in this context" name)
    | Some (TConstructor t) -> Some (sprintf "Name %s refers to a type constructor, it cannot be used in this context" name)
  )
  | LetIn (name, typ, body, rest) -> (
    let gamma' = update gamma name (TBinding typ) in
    let e1 = typecheck_expr typ gamma body in
    let e2 = typecheck_expr t gamma' rest in
    propagate_error e1 e2
  )
  | Fun (params, typ, body) -> (
    match construct_env typ params gamma with
    | Ok (gamma', result_typ) -> (
      match match_typ typ t with
      | Ok true ->  typecheck_expr result_typ gamma' body
      | Ok false -> Some (sprintf "%s does not match %s" (show_typ typ) (show_typ t))
      | Error e -> Some e
    )
    | Error e -> Some e
  )
  | Match (e, match_branches) -> typecheck_match t gamma e match_branches
  | App (e, params) -> (
    match e with
    | Var name -> (
      match gamma name with
      | Some (TBinding t') -> (
        let rec match_params (params: expr list) (typ: typ) =
          match params, typ with
          | param :: rest, TFun (t1, t1') -> (
            let e1 = typecheck_expr t1 gamma param in
            let e2 = match_params rest t1' in
            propagate_error e1 e2
          )
          | [], t' -> (
            match match_typ t t' with
            | Ok true -> None
            | Ok false -> Some (sprintf "%s does not match %s" (show_typ t) (show_typ t'))
            | Error e -> Some e
          )
          | params, t' -> Some (sprintf "%s is not a function, it cannot be applied" (show_typ t'))
        in
        match_params params t'
      )
      | Some (TType t') -> Some (sprintf "%s is a type, it cannot be applied" name)
      | Some (TConstructor t') -> Some (sprintf "%s is a constructor, it cannot be applied" name)
      | None -> Some (sprintf "%s not found in the current environment" name)
    )
    | Fun (pparams, typ, body) -> (
      match construct_env typ pparams gamma with
      | Ok (gamma', result_typ) -> (
        match match_typ result_typ t with
        | Ok true ->  typecheck_expr result_typ gamma' body
        | Ok false -> Some (sprintf "%s does not match %s" (show_typ result_typ) (show_typ t))
        | Error e -> Some e
      )
      | Error e -> Some e
    )
    | e' -> Some (sprintf "%s is not a function" (show_expr e'))
  )
  | Seq (_, e') -> typecheck_expr t gamma e'

let typecheck_stmt (gamma: env) (stmt: stmt): (env, (env * string)) result =
  match stmt with
  | Let (name, typ, body) -> (
    let gamma' = update gamma name (TBinding typ) in
    match typecheck_expr typ gamma body with
    | None -> Ok gamma'
    | Some e -> Error (gamma', e)
  )
  | Type (name, constructors) -> (
    let type_constructors = List.map constructors ~f:(fun (name, typs) -> (
      match typs with
      | None -> Nullary name
      | Some typs -> Nary (name, typs)
    )) in
    let gamma' = List.fold type_constructors ~f:(fun acc c -> (
      match c with
      | Nullary name -> update acc name (TConstructor (Nullary name))
      | Nary (name, typs) -> update acc name (TConstructor (Nary (name, typs)))
    )) ~init:gamma in
    let gamma'' = update gamma' name (TType type_constructors) in
    Ok gamma''
  )

let typecheck (program: program) =
  let maybe_error = List.fold_left program ~f:(fun gamma stmt -> (
    match gamma with
    | Ok gamma -> (
      match typecheck_stmt gamma stmt with
      | Ok gamma' -> Ok gamma'
      | Error (gamma', e) -> Error (gamma', e)
    )
    | Error (gamma, e) -> (
      match typecheck_stmt gamma stmt with
      | Ok gamma' -> Error (gamma', e)
      | Error (gamma', e') -> Error (gamma', (e ^ "\n" ^ e'))
    )
  )) ~init:(Ok empty_env)
  in
  match maybe_error with
  | Ok _ -> Ok true
  | Error (_, e) -> Error e