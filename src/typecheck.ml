open Ast
open Core

type constructor =
  | Nullary of string * string (* constructor * parent *)
  | Nary of string * string * typ list (* constructor * parent * typs *)

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

let rec match_typ (t: typ) (t': typ) (gamma: env): (bool, string) result =
  let rec match_typs (t: typ list) (t': typ list) (gamma: env): (bool, string) result =
    match List.zip t t' with
    | Some tt -> List.fold_left tt ~f:(fun acc (t, t') -> (
      match acc, match_typ t t' gamma with
      | Ok t, Ok t' -> Ok (t && t')
      | Ok _, Error e -> Error e
      | Error e, Ok _ -> Error e
      | Error e, Error e' -> Error (e ^ "\n" ^ e') 
    )) ~init:(Ok true)
    | None -> Error "type lengths do not match"
  in
  match t, t' with
  | TFun t, TFun t' -> match_typs t t' gamma
  | TTuple t, TTuple t' -> match_typs t t' gamma
  | TRecord t1', TRecord t2' -> (
    match gamma t1' with
    | Some (TConstructor Nullary (_, parent_typ)) when parent_typ = t2' -> Ok true
    | Some (TConstructor Nary (_, parent_typ, _)) when parent_typ = t2' -> Ok true
    | Some (TConstructor _) -> Error (sprintf "constructor %s does not belong to type %s" t1' t2')
    | Some (TType _) -> Ok (t1' = t2')
    | Some (TBinding _) -> Error (sprintf "%s refers to a binding, not a constructor" t1')
    | None -> Error (sprintf "%s not found in current environment" t1')
  )
  | TList t1', TList t2' -> match_typ t1' t2' gamma
  | TSecret t1', TSecret t2' -> match_typ t1' t2' gamma
  | TSecret _, _ -> Ok false
  | _, TSecret _ -> Ok false
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

let rec typecheck_match (t: typ) (gamma: env) (e: expr) (match_branches: (match_branch * typ * expr) list): string option =
  match match_branches with
  | [] -> None
  | (branch, branch_typ, branch_expr) :: rest -> (
    let rec match_typ_shape (branch: match_branch) (branch_typ: typ) (gamma: env) =
      let rec match_typ_shapes (branches: match_branch list) (branch_typs: typ list) (gamma: env) =
        let branch_typs = List.zip branches branch_typs in
        match branch_typs with
        | Some branch_typs -> (
          List.fold branch_typs ~f:(fun acc (b, t) ->
            match acc with
            | Error e -> Error e
            | Ok g -> (
              match match_typ_shape b t g with
              | Ok g' -> Ok g'
              | Error e -> Error e
            )
          ) ~init:(Ok gamma)
        )
        | None -> Error (sprintf ("number of types does not match number of expressions"))
      in
      match branch, branch_typ with
      | ML l, t' when typecheck_literal t' l = None -> Ok gamma
      | MVar name, t' -> Ok ((update gamma name (TBinding t')))
      | Blank, _ -> Ok gamma
      | MTuple branches, TTuple typs -> (
        match match_typ_shapes branches typs gamma with
        | Ok gamma' -> Ok gamma'
        | Error e -> Error ("Tuple shape does not match type: " ^ e)
      )
      | MRecord (constructor, argument_branches), TRecord c -> (
        match gamma constructor with
        | Some (TConstructor c) -> (
          match c with
          | Nullary (c, parent_typ) -> (
            match List.length argument_branches with
            | 0 -> Ok gamma
            | n -> Error (sprintf "constructor %s does not take arguments, %d supplied" c n)
          )
          | Nary (c, parent_typ, argument_typs) -> (
            match match_typ_shapes argument_branches argument_typs gamma with
            | Ok gamma' -> Ok gamma'
            | Error e -> Error ("Constructor arguments do not match required types: " ^ e)
          )
        )
        | Some (TBinding t) -> Error (sprintf "Name %s refers to a binding, not a constructor" constructor)
        | Some (TType t) -> Error (sprintf "Name %s refers to a type, not a constructor" constructor)
        | None -> Error (sprintf "Name %s not found in current environment" constructor)
      )
      | MList branches, TList t' -> (
        let branch_typs = List.init (List.length branches) ~f:(fun _ -> t') in
        match match_typ_shapes branches branch_typs gamma with
        | Ok gamma' -> Ok gamma'
        | Error e -> Error (sprintf "Expected list of type %s: %s" (show_typ t') e)
      )
      | l, t' -> Error (sprintf "Branch shape does not match specified type: %s found, %s expected" (show_match_branch l) (show_typ t'))
    in
    match match_typ_shape branch branch_typ gamma with
    | Error e -> Some e
    | Ok gamma' -> (
      let e1 = typecheck_expr branch_typ gamma e in
      let e2 = typecheck_expr t gamma' branch_expr in
      let branch_e = propagate_error e1 e2 in
      let rest_e = typecheck_match t gamma e rest in
      propagate_error branch_e rest_e
    )
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
    | Some (TConstructor (Nullary (c', _))) -> (
      match params with
      | [] -> None
      | params -> Some (sprintf "Too many arguments for constructor %s (0 required)" constructor)
    )
    | Some (TConstructor (Nary (c', _, typs))) -> (
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
    match t with
    | TFun typs -> (
      (*
        Below allows partial application
      *)
      let supplied_typs = List.sub typs ~len:(List.length params) ~pos:0 in
      match List.zip supplied_typs params with
      | None -> Error "number of parameters does not match number of types"
      | Some typ_params -> Ok (
        List.fold typ_params ~f:(fun (gamma, _) (typ, param) -> (update gamma param (TBinding typ), typ)) ~init:(gamma, TInt)
      )
    )
    | t' -> Error (sprintf "function type expected, found %s" (show_typ t'))
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
      match match_typ t t' gamma with
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
  | LetRecIn (name, typ, body, rest) -> (
    let gamma' = update gamma name (TBinding typ) in
    let e1 = typecheck_expr typ gamma' body in
    let e2 = typecheck_expr t gamma' rest in
    propagate_error e1 e2
  )
  | Fun (params, typ, body) -> (
    match construct_env typ params gamma with
    | Ok (gamma', result_typ) -> (
      match match_typ typ t gamma with
      | Ok true ->  typecheck_expr result_typ gamma' body
      | Ok false -> Some (sprintf "%s does not match %s" (show_typ typ) (show_typ t))
      | Error e -> Some e
    )
    | Error e -> Some e
  )
  | Match (e, match_branches) -> typecheck_match t gamma e match_branches
  | App (e, param_typs) -> (
    (*
      We flatten the function to allow for partial application

      let a: int -> int = (fun (int -> int -> int) ...) (a: int) -> TFun int -> int -> int
      not
      let a: int -> int = (fun (int -> int -> int) ...) (a: int) -> TFun (TFun int -> int) -> int
    *)
    let _, typs = List.unzip param_typs in
    let fun_typ = (
      match t with
      | TFun return_typs -> TFun (typs @ return_typs)
      | t -> TFun (typs @ [t])
    ) in
    let e1 = typecheck_expr fun_typ gamma e in
    let e2 = List.fold param_typs ~f:(fun acc (param, typ) -> (
      let e = typecheck_expr typ gamma param in
      match acc, e with
      | Some e, Some e' -> Some (e ^ "\n" ^ e')
      | Some e, None -> Some e
      | None, Some e -> Some e
      | None, None -> None
    )) ~init:None
    in
    propagate_error e1 e2
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
  | LetRec (name, typ, body) -> (
    let gamma' = update gamma name (TBinding typ) in
    match typecheck_expr typ gamma' body with
    | None -> Ok gamma'
    | Some e -> Error (gamma', e)
  )
  | Type (parent_name, constructors) -> (
    let type_constructors = List.map constructors ~f:(fun (name, typs) -> (
      match typs with
      | None -> Nullary (name, parent_name)
      | Some typs -> Nary (name, parent_name, typs)
    )) in
    let gamma' = List.fold type_constructors ~f:(fun acc c -> (
      match c with
      | Nullary (name, parent_name) -> update acc name (TConstructor (Nullary (name, parent_name)))
      | Nary (name, parent_name, typs) -> update acc name (TConstructor (Nary (name, parent_name, typs)))
    )) ~init:gamma in
    let gamma'' = update gamma' parent_name (TType type_constructors) in
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