open Ast
open Error
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

let propagate_error (error_messages: err list option) (maybe_error: err list option): err list option =
  match error_messages, maybe_error with
  | Some errors, Some error -> Some (errors @ error)
  | e, None -> e
  | None, e -> e

let propagate_success (maybe_e1: err list option) (maybe_e2: err list option): err list option =
  match maybe_e1, maybe_e2 with
  | Some e1, Some e2 -> Some e1
  | None, _ -> None
  | _, None -> None

let typecheck_propagate (typecheck: 'a -> err list option) (e: err list option) (node: 'a): err list option =
  let new_e = typecheck node in
  propagate_error e new_e

let typ_loc (t: typ): loc =
  match t with
  | TUnit l -> l
  | TInt l -> l
  | TFloat l -> l
  | TBool l -> l
  | TString l -> l
  | TChar l -> l
  | TList (l, _) -> l
  | TSecret (l, _) -> l
  | TTuple (l, _) -> l
  | TFun (l, _) -> l
  | TRecord (l, _) -> l

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
  | TFun (loc, t), TFun (loc', t') -> match_typs t t' gamma
  | TTuple (loc, t), TTuple (loc', t') -> match_typs t t' gamma
  | TRecord (loc, t1'), TRecord (loc', t2') -> (
    match gamma t1' with
    | Some (TConstructor Nullary (_, parent_typ)) when parent_typ = t2' -> Ok true
    | Some (TConstructor Nary (_, parent_typ, _)) when parent_typ = t2' -> Ok true
    | Some (TConstructor _) -> Error (sprintf "constructor %s does not belong to type %s" t1' t2')
    | Some (TType _) -> Ok (t1' = t2')
    | Some (TBinding _) -> Error (sprintf "%s refers to a binding, not a constructor" t1')
    | None -> Error (sprintf "%s not found in current environment" t1')
  )
  | TList (loc, t1'), TList (loc', t2') -> match_typ t1' t2' gamma
  | TSecret (loc, t1'), TSecret (loc', t2') -> match_typ t1' t2' gamma
  | TSecret _, _ -> Ok false
  | _, TSecret _ -> Ok false
  | TUnit _, TUnit _ -> Ok true
  | TInt _, TInt _ -> Ok true
  | TFloat _, TFloat _ -> Ok true
  | TChar _, TChar _ -> Ok true
  | TBool _, TBool _ -> Ok true
  | TString _, TString _ -> Ok true
  | t1', t2' -> Ok (t1' = t2')

let rec typecheck_literal (t: typ) (l: literal): err option =
  match l, t with
  | Unit, TUnit _ -> None
  | Int _, TInt _ -> None
  | Float _, TFloat _ -> None
  | String _, TString _ -> None
  | Char _, TChar _ -> None
  | Bool _, TBool _ -> None
  | l', TSecret (_, t') -> typecheck_literal t' l'
  | l', t' -> Some (TypeError ((typ_loc t'), (sprintf "Expected type %s, found %s" (pretty_typ t') (show_literal l'))))

let rec typecheck_match (t: typ) (expr_typ: typ) (gamma: env) (e: expr) (match_branches: (match_branch * expr) list): err list option =
  match match_branches with
  | [] -> None
  | (branch, branch_expr) :: rest -> (
    let rec match_typ_shape (branch: match_branch) (branch_typ: typ) (gamma: env): (env, err) result =
      let rec match_typ_shapes (branches: match_branch list) (branch_typs: typ list) (gamma: env): (env, string) result =
        let branch_typs = List.zip branches branch_typs in
        match branch_typs with
        | Some branch_typs -> (
          List.fold branch_typs ~f:(fun acc (b, t) ->
            match acc with
            | Error e -> Error e
            | Ok g -> (
              match match_typ_shape b t g with
              | Ok g' -> Ok g'
              | Error (TypeError (_, e)) -> Error e
              | Error (SyntaxError (_, e)) -> Error e
            )
          ) ~init:(Ok gamma)
        )
        | None -> Error (sprintf ("number of types does not match number of expressions"))
      in
      match branch, branch_typ with
      | ML (loc, l), t' when typecheck_literal t' l = None -> Ok gamma
      | MVar (loc, name), t' -> Ok ((update gamma name (TBinding t')))
      | Blank loc, _ -> Ok gamma
      | MTuple (loc, branches), TTuple (loc', typs) -> (
        match match_typ_shapes branches typs gamma with
        | Ok gamma' -> Ok gamma'
        | Error e -> Error (TypeError (loc, "Tuple shape does not match type: " ^ e))
      )
      | MRecord (loc, constructor, argument_branches), TRecord (loc', c) -> (
        match gamma constructor with
        | Some (TConstructor c) -> (
          match c with
          | Nullary (c, parent_typ) -> (
            match List.length argument_branches with
            | 0 -> Ok gamma
            | n -> Error (TypeError (loc, sprintf "constructor %s does not take arguments, %d supplied" c n))
          )
          | Nary (c, parent_typ, argument_typs) -> (
            match match_typ_shapes argument_branches argument_typs gamma with
            | Ok gamma' -> Ok gamma'
            | Error e -> Error (TypeError (loc, "Constructor arguments do not match required types: " ^ e))
          )
        )
        | Some (TBinding t) -> Error (TypeError (loc, sprintf "Name %s refers to a binding, not a constructor" constructor))
        | Some (TType t) -> Error (TypeError (loc, (sprintf "Name %s refers to a type, not a constructor" constructor)))
        | None -> Error (TypeError (loc, (sprintf "Name %s not found in current environment" constructor)))
      )
      | MNil _, TList (_, _) -> Ok gamma
      | MCons (loc, mb1, MNil _), TList (loc', t') -> match_typ_shape mb1 (TList (loc', t')) gamma
      | MCons (loc, mb1, mb2), TList (loc', t') -> (
        match match_typ_shape mb1 t' gamma with
        | Error e -> Error e
        | Ok gamma' -> (
          match_typ_shape mb2 (TList (loc', t')) gamma'
        )
      )
      | l, t' -> Error (TypeError ((typ_loc t'), (sprintf "Branch shape does not match specified type: %s found, %s expected" (pretty_branch l) (pretty_typ t'))))
    in
    match match_typ_shape branch expr_typ gamma with
    | Error err -> (
      let rest_e = typecheck_match t expr_typ gamma e rest in
      propagate_error (Some [err]) rest_e
    )
    | Ok gamma' -> (
      let e1 = typecheck_expr expr_typ gamma e in
      let e2 = typecheck_expr t gamma' branch_expr in
      let branch_e = propagate_error e1 e2 in
      let rest_e = typecheck_match t expr_typ gamma e rest in
      propagate_error branch_e rest_e
    )
  )
and typecheck_complex (t: typ) (gamma: env) (c: complex) (loc: loc): err list option =
  match c with
  | Tuple params when List.length params < 2 -> Some [TypeError (loc, sprintf "A tuple must have at least two members")]
  | Tuple e -> (
    match t with
    | TTuple (loc, t) -> (
      match List.zip e t with
      | Some expr_typs -> (
        List.fold expr_typs ~f:(fun acc (e, t) -> typecheck_propagate (typecheck_expr t gamma) acc e) ~init:None
      )
      | None -> Some [TypeError (loc, (sprintf "Tuple %s length does not match %s" (pretty_typ (TTuple (loc, t))) (show_expr (C (loc, (Tuple e))))))]
    )
    | TSecret (loc, (TTuple (loc', t))) -> (
      match List.zip e t with
      | Some expr_typs -> (
        List.fold expr_typs ~f:(fun acc (e, t) -> typecheck_propagate (typecheck_expr t gamma) acc e) ~init:None
      )
      | None -> Some [TypeError (loc, (sprintf "Tuple %s length does not match %s" (pretty_typ (TTuple (loc, t))) (show_expr (C (loc, (Tuple e))))))]
    )
    | t' -> Some [TypeError (loc, sprintf "Expected tuple, found %s" (pretty_typ t'))]
  )
  | Record (constructor, params) -> (
    match gamma constructor with
    | Some (TConstructor (Nullary (c', _))) -> (
      match params with
      | [] -> None
      | params -> Some [TypeError (loc, (sprintf "Too many arguments for constructor %s (0 required)" constructor))]
    )
    | Some (TConstructor (Nary (c', _, typs))) -> (
      let param_typs = List.zip params typs in
      match param_typs with
      | Some param_typs -> (
        List.fold param_typs ~f:(fun acc (param, typ) -> typecheck_propagate (typecheck_expr typ gamma) acc param) ~init:None
      )
      | None -> Some [TypeError (loc, sprintf "Incorrect number of parameters for constructor %s (%d required)" constructor (List.length typs))]
    )
    | Some (TType _) -> Some [TypeError (loc, (sprintf "Name %s refers to a type, not a constructor" constructor))]
    | Some (TBinding _) -> Some [TypeError (loc, (sprintf "Name %s refers to a binding, not a constructor" constructor))]
    | None -> Some [TypeError (loc, (sprintf "Name %s not found in current environment" constructor))]
  )
  | Nil -> (
    match t with
    | TList (_, _) -> None
    | TSecret (_, (TList (_, _))) -> None
    | t' -> Some [TypeError ((typ_loc t'), (sprintf "List expected, %s found" (pretty_typ t')))]
  )
  | Cons (e1, e2) -> (
    match t with
    | TList (loc, t') -> (
      let e1 = typecheck_expr t' gamma e1 in
      let e2 = typecheck_expr (TList (loc, t')) gamma e2 in
      propagate_error e1 e2
    )
    | TSecret (loc, TList (loc', t')) -> (
      let e1 = typecheck_expr t' gamma e1 in
      let e2 = typecheck_expr (TList (loc, t')) gamma e2 in
      propagate_error e1 e2
    )
    | t' -> Some [TypeError ((typ_loc t'), (sprintf "List expected, %s found" (pretty_typ t')))]
  )
and typecheck_expr (t: typ) (gamma: env) (expr: expr): err list option =
  let rec construct_env (t: typ) (params: string list) (gamma: env): ((env * typ), err) result =
    match t with
    | TFun (loc, typs) -> (
      (*
        Below allows partial application
      *)
      let supplied_typs = List.sub typs ~len:(List.length params) ~pos:0 in
      let result_typ = List.last typs in
      match result_typ with
      | None -> Error (TypeError (loc, "unable to identify return type"))
      | Some result_typ -> (
        match List.zip supplied_typs params with
        | None -> Error (TypeError (loc, "number of parameters does not match number of types"))
        | Some typ_params -> Ok (
          ((List.fold typ_params ~f:(fun gamma (typ, param) -> update gamma param (TBinding typ)) ~init:(gamma)), result_typ)
        )
      )
    )
    | t' -> Error (TypeError ((typ_loc t'), sprintf "function type expected, found %s" (pretty_typ t')))
  in
  match expr with
  | L (_, l) -> (
    match typecheck_literal t l with
    | None -> None
    | Some e -> Some [e]
  )
  | C (loc, c) -> typecheck_complex t gamma c loc
  | UnOp (loc, op, e) -> (
    match t with
    | TBool loc' -> typecheck_expr (TBool loc') gamma e
    | t' -> Some [(TypeError ((typ_loc t'), sprintf "attempted to assign the result of a unary expression to type %s, expected %s" (pretty_typ t') (pretty_typ (TBool loc))))]
  )
  | BinOp (loc, op, e1, e2) -> (
    let try_pair (t: typ) (e1: expr) (e2: expr): err list option =
      let e1' = typecheck_expr t gamma e1 in
      let e2' = typecheck_expr t gamma e2 in
      propagate_error e1' e2'
    in
    match op, t with
    | op', TBool loc' when (
      (op' = Lt) || 
      (op' = Lte) ||
      (op' = Gt) ||
      (op' = Gte)
    ) -> (
      let ee1 = try_pair (TInt loc') e1 e2 in
      let ee2 = try_pair (TFloat loc') e1 e2 in
      propagate_success ee1 ee2
    )
    | op', TBool loc' when (
      (op' = Eq) ||
      (op' = Neq)
    ) -> (
      (*
      Since there is no type inference have to rely on exhaustive search,
      Comparing complex types not supported
      *)
      let rec try_pairs (ts: typ list) (e1: expr) (e2: expr): err list option =
        match ts with
        | h :: t -> (
          let e1' = try_pair h e1 e2 in
          let e2' = try_pairs t e1 e2 in
          propagate_success e1' e2'
        )
        | [] -> None
      in
      try_pairs [
        TInt loc;
        TFloat loc;
        TString loc;
        TChar loc;
        TUnit loc;
      ] e1 e2
    )
    | op', TBool loc' -> (
      let e1' = typecheck_expr (TBool loc') gamma e1 in
      let e2' = typecheck_expr (TBool loc') gamma e2 in
      propagate_error e1' e2'
    )
    | op', t -> Some [(TypeError (loc, (sprintf "attempted to assign the result of a binary expression to type %s, expected %s" (pretty_typ t) (pretty_typ (TBool loc)))))]
  )
  | NumOp (loc, op, e1, e2) -> (
    match t with
    | TInt loc' -> (
      let e1' = typecheck_expr (TInt loc') gamma e1 in
      let e2' = typecheck_expr (TInt loc') gamma e2 in
      propagate_error e1' e2'
    )
    | TFloat loc' -> (
      let e1' = typecheck_expr (TFloat loc') gamma e1 in
      let e2' = typecheck_expr (TFloat loc') gamma e2 in
      propagate_error e1' e2'
    )
    | t' -> Some [(TypeError (loc, sprintf "attempted to assign the result of an arithmetic expression to type %s, %s or %s expected" (pretty_typ t') (pretty_typ (TInt loc)) (pretty_typ (TFloat loc))))]
  )
  | ListOp (loc, op, e1, e2) -> (
    match t with
    | TList (loc', t') -> (
      let e1' = typecheck_expr (TList (loc', t')) gamma e1 in
      let e2' = typecheck_expr (TList (loc', t')) gamma e2 in
      propagate_error e1' e2'
    )
    | t' -> Some [(TypeError (loc, sprintf "attempted to assign the result of a list operator expression (%s) to type %s, list type expected" (pretty_list_op op) (pretty_typ t')))]
  )
  | Var (loc, name) -> (
    match gamma name with
    | None -> Some [(TypeError (loc, sprintf "Name %s not found in current environment" name))]
    | Some (TBinding t') -> (
      match match_typ t t' gamma with
      | Ok true -> None
      | Ok false -> Some [(TypeError (loc, (sprintf "Type of variable %s (%s) does not match required %s" name (pretty_typ t') (pretty_typ t))))]
      | Error e -> Some [(TypeError (loc, e))]
    )
    | Some (TType t) -> Some [TypeError (loc, sprintf "Name %s refers to a type, it cannot be used in this context" name)]
    | Some (TConstructor t) -> Some [TypeError (loc, sprintf "Name %s refers to a type constructor, it cannot be used in this context" name)]
  )
  | LetIn (loc, name, typ, body, rest) -> (
    let gamma' = update gamma name (TBinding typ) in
    let e1 = typecheck_expr typ gamma body in
    let e2 = typecheck_expr t gamma' rest in
    propagate_error e1 e2
  )
  | LetRecIn (loc, name, typ, body, rest) -> (
    let gamma' = update gamma name (TBinding typ) in
    let e1 = typecheck_expr typ gamma' body in
    let e2 = typecheck_expr t gamma' rest in
    propagate_error e1 e2
  )
  | Fun (loc, params, typ, body) -> (
    match construct_env typ params gamma with
    | Ok (gamma', result_typ) -> (
      match match_typ typ t gamma with
      | Ok true ->  typecheck_expr result_typ gamma' body
      | Ok false -> Some [TypeError (loc, sprintf "%s does not match %s" (pretty_typ typ) (pretty_typ t))]
      | Error e -> Some [TypeError (loc, e)]
    )
    | Error e -> Some [e]
  )
  | Match (loc, e, typ, match_branches) -> typecheck_match t typ gamma e match_branches
  | App (loc, e, param_typs) -> (
    (*
      We flatten the function to allow for partial application

      let a: int -> int = (fun (int -> int -> int) ...) (a: int) -> TFun int -> int -> int
      not
      let a: int -> int = (fun (int -> int -> int) ...) (a: int) -> TFun (TFun int -> int) -> int
    *)
    let _, typs = List.unzip param_typs in
    let fun_typ = (
      match t with
      | TFun (loc', return_typs) -> TFun (loc', typs @ return_typs)
      | t -> TFun (loc, typs @ [t])
    ) in
    let e1 = typecheck_expr fun_typ gamma e in
    let e2 = List.fold param_typs ~f:(fun acc (param, typ) -> (
      let e = typecheck_expr typ gamma param in
      match acc, e with
      | Some e, Some e' -> Some (e @ e')
      | Some e, None -> Some e
      | None, Some e -> Some e
      | None, None -> None
    )) ~init:None
    in
    propagate_error e1 e2
  )
  | Seq (loc, _, e') -> typecheck_expr t gamma e'

let typecheck_stmt (gamma: env) (stmt: stmt): (env, (env * err list)) result =
  match stmt with
  | Let (loc, name, typ, body) -> (
    let gamma' = update gamma name (TBinding typ) in
    match typecheck_expr typ gamma body with
    | None -> Ok gamma'
    | Some e -> Error (gamma', e)
  )
  | LetRec (loc, name, typ, body) -> (
    let gamma' = update gamma name (TBinding typ) in
    match typecheck_expr typ gamma' body with
    | None -> Ok gamma'
    | Some e -> Error (gamma', e)
  )
  | Type (loc, parent_name, constructors) -> (
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

let typecheck (program: program): err list option =
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
      | Error (gamma', e') -> Error (gamma', e' @ e)
    )
  )) ~init:(Ok empty_env)
  in
  match maybe_error with
  | Ok _ -> None
  | Error (_, e) -> Some e