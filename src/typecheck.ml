open Ast
open Error
open Pretty
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

let propagate_error (error_messages: (err * ('a -> 'a)) list option) (maybe_error: (err * ('a -> 'a)) list option): (err * ('a -> 'a)) list option =
  match error_messages, maybe_error with
  | Some errors, Some error -> Some (errors @ error)
  | e, None -> e
  | None, e -> e

let propagate_success (maybe_e1: (err * ('a -> 'a)) list option) (maybe_e2: (err * ('a -> 'a)) list option): (err * ('a -> 'a)) list option =
  match maybe_e1, maybe_e2 with
  | Some e1, Some e2 -> Some e1
  | None, _ -> None
  | _, None -> None

let typecheck_propagate (typecheck: 'a -> (err * ('a -> 'a)) list option) (e: (err * ('a -> 'a)) list option) (node: 'a): (err * ('a -> 'a)) list option =
  let new_e = typecheck node in
  propagate_error e new_e

let tag_expr (e: expr): expr =
  match e with
  | L (Loc (l, _), a) -> L (Loc (l, TypeErrorLoc), a)
  | C (Loc (l, _), a) -> C (Loc (l, TypeErrorLoc), a)
  | UnOp (Loc (l, _), a, b) -> UnOp (Loc (l, TypeErrorLoc), a, b)
  | BinOp (Loc (l, _), a, b, c) -> BinOp (Loc (l, TypeErrorLoc), a, b, c)
  | NumOp (Loc (l, _), a, b, c) -> NumOp (Loc (l, TypeErrorLoc), a, b, c)
  | ListOp (Loc (l, _), a, b, c) -> ListOp (Loc (l, TypeErrorLoc), a, b, c)
  | Var (Loc (l, _), a) -> Var (Loc (l, TypeErrorLoc), a)
  | LetIn (Loc (l, _), a, b, c, d) -> LetIn (Loc (l, TypeErrorLoc), a, b, c, d)
  | LetRecIn (Loc (l, _), a, b, c, d) -> LetRecIn (Loc (l, TypeErrorLoc), a, b, c, d)
  | Fun (Loc (l, _), a, b, c) -> Fun (Loc (l, TypeErrorLoc), a, b, c)
  | Match (Loc (l, _), a, b, c) -> Match (Loc (l, TypeErrorLoc), a, b, c)
  | App (Loc (l, _), a, b) -> App (Loc (l, TypeErrorLoc), a, b)
  | Seq (Loc (l, _), a, b) -> Seq (Loc (l,TypeErrorLoc), a, b)

let tag_match_branch (mb: match_branch): match_branch =
  match (mb : match_branch) with
  | ML (Loc (l, _), a) -> ML (Loc (l, TypeErrorLoc), a)
  | MVar (Loc (l, _), a) -> MVar (Loc (l, TypeErrorLoc), a)
  | Blank (Loc (l, _)) -> Blank (Loc (l, TypeErrorLoc))
  | MTuple (Loc (l, _), a) -> MTuple (Loc (l, TypeErrorLoc), a)
  | MRecord (Loc (l, _), a, b) -> MRecord (Loc (l, TypeErrorLoc), a, b)
  | MNil (Loc (l, _)) -> MNil (Loc (l, TypeErrorLoc))
  | MCons (Loc (l, _), a, b) -> MCons (Loc (l, TypeErrorLoc), a, b)

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
  | TPublic (l, _) -> l
  | TTuple (l, _) -> l
  | TFun (l, _) -> l
  | TRecord (l, _) -> l

let is_simple_typ (t: typ): bool =
  match t with
  | TUnit _ -> true
  | TInt _ -> true
  | TFloat _ -> true
  | TBool _ -> true
  | TString _ -> true
  | TChar _ -> true
  | _ -> false

let unwrap_typ (t: typ): typ =
  match t with
  | TSecret (_, t') -> t'
  | TPublic (_, t') -> t'
  | t' -> t'

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
  | TPublic (loc, t1'), TPublic (loc', t2') -> match_typ t1' t2' gamma
  | TPublic (loc, t1'), t2' -> match_typ t1' t2' gamma
  | t1', TPublic (loc', t2') -> match_typ t1' t2' gamma
  | TSecret _, _ -> Ok false
  | _, TSecret _ -> Ok false
  | TUnit _, TUnit _ -> Ok true
  | TInt _, TInt _ -> Ok true
  | TFloat _, TFloat _ -> Ok true
  | TChar _, TChar _ -> Ok true
  | TBool _, TBool _ -> Ok true
  | TString _, TString _ -> Ok true
  | t1', t2' -> Ok (t1' = t2')

let rec typecheck_literal (t: typ) (l: literal): string option =
  match l, t with
  | Unit, TUnit _ -> None
  | Int _, TInt _ -> None
  | Float _, TFloat _ -> None
  | String _, TString _ -> None
  | Char _, TChar _ -> None
  | Bool _, TBool _ -> None
  | l', TSecret (_, t') -> typecheck_literal t' l'
  | l', t' -> Some (sprintf "Expected type %s, found %s" (pretty_typ t') (pretty_lit l'))

let rec _typecheck_match (t: typ) (expr_typ: typ) (gamma: env) (e: expr) (match_branches: (int * match_branch * expr) list):
(int * err * ((match_branch * expr) -> (match_branch * expr))) list =
  match match_branches with
  | [] -> []
  | (i, branch, branch_expr) :: rest -> (
    let rec match_typ_shape (branch: match_branch) (branch_typ: typ) (gamma: env): (env, err) result =
      let rec match_typ_shapes (branches: match_branch list) (branch_typs: typ list) (gamma: env): (env, (int * string)) result =
        let branch_typs = List.zip branches branch_typs in
        match branch_typs with
        | Some branch_typs -> (
          List.foldi branch_typs ~f:(fun i acc (b, t) ->
            match acc with
            | Error e -> Error e
            | Ok g -> (
              match match_typ_shape b t g with
              | Ok g' -> Ok g'
              | Error (TypeError (_, _, e)) -> Error (i, e)
              | Error (SyntaxError (_, e)) -> Error (i, e)
            )
          ) ~init:(Ok gamma)
        )
        | None -> Error (0, sprintf ("number of types does not match number of expressions"))
      in
      match branch, branch_typ with
      | ML (loc, l), t' when typecheck_literal t' l = None -> Ok gamma
      | MVar (loc, name), t' -> Ok ((update gamma name (TBinding t')))
      | Blank loc, _ -> Ok gamma
      | MTuple (loc, branches), TTuple (loc', typs) -> (
        match match_typ_shapes branches typs gamma with
        | Ok gamma' -> Ok gamma'
        | Error (i, e) -> Error (TypeError (loc, MB (MTuple (loc, branches)), "Tuple shape does not match type: " ^ e))
      )
      | MRecord (loc, constructor, argument_branches), TRecord (loc', c) -> (
        match gamma constructor with
        | Some (TConstructor c) -> (
          match c with
          | Nullary (c, parent_typ) -> (
            match List.length argument_branches with
            | 0 -> Ok gamma
            | n -> Error (TypeError (loc, MB (MRecord (loc, constructor, argument_branches)), sprintf "constructor %s does not take arguments, %d supplied" c n))
          )
          | Nary (c, parent_typ, argument_typs) -> (
            match match_typ_shapes argument_branches argument_typs gamma with
            | Ok gamma' -> Ok gamma'
            | Error (i, e) -> Error (TypeError (loc, MB (MRecord (loc, constructor, argument_branches)), "Constructor arguments do not match required types: " ^ e))
          )
        )
        | Some (TBinding t) -> Error (TypeError (loc, MB (MRecord (loc, constructor, argument_branches)), sprintf "Name %s refers to a binding, not a constructor" constructor))
        | Some (TType t) -> Error (TypeError (loc, MB (MRecord (loc, constructor, argument_branches)), (sprintf "Name %s refers to a type, not a constructor" constructor)))
        | None -> Error (TypeError (loc, MB (MRecord (loc, constructor, argument_branches)), (sprintf "Name %s not found in current environment" constructor)))
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
      | l, t' -> Error (TypeError ((typ_loc t'), MB l, (sprintf "%s found, %s expected" (pretty_branch l) (pretty_typ t'))))
    in
    match match_typ_shape branch expr_typ gamma with
    | Error err -> (
      let tr_mb (match_branch, expr) = (tag_match_branch match_branch, expr) in
      (i, err, tr_mb) :: (_typecheck_match t expr_typ gamma e rest)
    )
    | Ok gamma' -> []
  )

and typecheck_match (t: typ) (expr_typ: typ) (gamma: env) (e: expr) (match_branches: (match_branch * expr) list):
(int * err * ((match_branch * expr) -> (match_branch * expr))) list =
  _typecheck_match t expr_typ gamma e (List.mapi match_branches ~f:(fun i (mb, expr) -> (i, mb, expr)))

and typecheck_complex (t: typ) (gamma: env) (c: complex) (loc: loc): (err * (expr -> expr)) list option =
  match c with
  | Tuple params when List.length params < 2 -> (
    Some [(TypeError (loc, E (C (loc, (Tuple params))), sprintf "A tuple must have at least two members"), tag_expr)]
  )
  | Tuple e -> (
    let tag_tuple_element i tr_e e =
      match e with
      | C (l, Tuple es) -> C (l, Tuple (List.foldi es ~f:(fun i' acc e -> (
        let e' = match i' with
        | i' when i = i' -> tr_e e
        | _ -> e in
        acc @ [e']
      )) ~init:[]))
      | e' -> e'
    in
    match t with
    | TTuple (loc, t) -> (
      match List.zip e t with
      | Some expr_typs -> (
        List.foldi expr_typs ~f:(fun i acc (e, t) -> (
          let er = typecheck_expr t gamma e in
          let er' = match er with
          | None -> None
          | Some ((TypeError (loc, n, er), tr_e) :: t) -> (
            Some ((TypeError (loc, n, er), tag_tuple_element i tr_e) :: t)
          )
          | Some e -> Some e
          in
          propagate_error acc er')
      )~init:None)
      | None -> Some [(TypeError (loc, E (C (loc, (Tuple e))), (sprintf "Tuple %s length does not match %s" (pretty_typ (TTuple (loc, t))) (pretty_expr (C (loc, (Tuple e)))))), tag_expr)]
    )
    | TSecret (loc, (TTuple (loc', t))) -> (
      match List.zip e t with
      | Some expr_typs -> (
        List.fold expr_typs ~f:(fun acc (e, t) -> typecheck_propagate (typecheck_expr t gamma) acc e) ~init:None
      )
      | None -> Some [(TypeError (loc, E (C (loc, (Tuple e))), (sprintf "Tuple %s length does not match %s" (pretty_typ (TTuple (loc, t))) (pretty_expr (C (loc, (Tuple e)))))), tag_expr)]
    )
    | t' -> Some [(TypeError (loc, E (C (loc, (Tuple e))), sprintf "Expected tuple, found %s" (pretty_typ t')), tag_expr)]
  )
  | Record (constructor, params) -> (
    match gamma constructor with
    | Some (TConstructor (Nullary (c', _))) -> (
      match params with
      | [] -> None
      | params -> Some [(TypeError (loc, E (C (loc, Record (constructor, params))), (sprintf "Too many arguments for constructor %s (0 required)" constructor)), tag_expr)]
    )
    | Some (TConstructor (Nary (c', _, typs))) -> (
      let param_typs = List.zip params typs in
      match param_typs with
      | Some param_typs -> (
        List.fold param_typs ~f:(fun acc (param, typ) -> typecheck_propagate (typecheck_expr typ gamma) acc param) ~init:None
      )
      | None -> Some [(TypeError (loc, E (C (loc, Record (constructor, params))), sprintf "Incorrect number of parameters for constructor %s (%d required)" constructor (List.length typs)), tag_expr)]
    )
    | Some (TType _) -> Some [(TypeError (loc, E (C (loc, Record (constructor, params))), (sprintf "Name %s refers to a type, not a constructor" constructor)), tag_expr)]
    | Some (TBinding _) -> Some [(TypeError (loc, E (C (loc, Record (constructor, params))), (sprintf "Name %s refers to a binding, not a constructor" constructor)), tag_expr)]
    | None -> Some [(TypeError (loc, E (C (loc, Record (constructor, params))), (sprintf "Name %s not found in current environment" constructor)), tag_expr)]
  )
  | Nil -> (
    match t with
    | TList (_, _) -> None
    | TSecret (_, (TList (_, _))) -> None
    | t' -> Some [(TypeError ((typ_loc t'), E (C (loc, Nil)), (sprintf "List expected, %s found" (pretty_typ t'))), tag_expr)]
  )
  | Cons (e1, e2) -> (
    let tag_cons_expr tr_e e =
      match e with
      | C (l, Cons (e, e')) -> (
        printf "Tagging cons expr"; C (l, Cons (tr_e e, e'))
      )
      | e' -> e'
    in
    let tag_cons_rest tr_e e =
      match e with
      | C (l, Cons (e, e')) -> (
        printf "Tagging cons rest"; C (l, Cons (e, tr_e e'))
      )
      | e' -> e'
    in
    match t with
    | TList (loc, t') -> (
      (*
      Cons e1 (Cons e2 Nil)
      e1 -> TypeError tr_e e1 -> (...)
      Cons (e2, Nil) -> (...)

      ----
      e2 -> TypeError tr_e1 e2 -> (...)
      Nil -> None

      typecheck e2 ->
        fun c -> Cons e2, Nil -> tr_e1 e2
      
      fun c -> Cons e2, Nil -> tr_e1 e2
      ----

      typecheck e1 ->
        fun c -> Cons e1, e2 -> tr_e e1
      
      typecheck Cons (e2, Nil) ->
        fun c -> Cons e1, Cons (e2, Nil) -> tr_e1 e2
      
      
      *)
      let er1 = typecheck_expr t' gamma e1 in
      let er1' = match er1 with
      | None -> None
      | Some errors -> (
        printf "List.length errors %d\n" (List.length errors);
        Some (List.map errors ~f:(fun e -> (
          match e with
          | (TypeError (loc, n, er), tr_e) -> (TypeError (loc, E (C (loc, Cons (e1, e2))), er), tag_cons_expr tr_e)
          | e' -> e'
        )))
      )
      in
      let er2 = typecheck_expr (TList (loc, t')) gamma e2 in
      let er2' = match er2 with
      | None -> None
      | Some errors -> (
        printf "List.length errors cons: %d\n" (List.length errors);
        Some (List.map errors ~f:(fun e -> (
          match e with
          | (TypeError (loc, n, er), tr_e) -> (TypeError (loc, E (C (loc, Cons (e1, e2))), er), tag_cons_rest tr_e)
          | e' -> e'
        )))
      )
      in
      propagate_error er1' er2'
    )
    | TSecret (loc, TList (loc', t')) -> (
      let e1 = typecheck_expr t' gamma e1 in
      let e2 = typecheck_expr (TList (loc, t')) gamma e2 in
      propagate_error e1 e2
    )
    | t' -> Some [(TypeError ((typ_loc t'), E (C (loc, Cons (e1, e2))), (sprintf "List expected, %s found" (pretty_typ t'))), tag_expr)]
  )
and typecheck_expr (t: typ) (gamma: env) (expr: expr): (err * (expr -> expr)) list option =
  let rec construct_env (t: typ) (params: string list) (gamma: env): ((env * typ), string) result =
    match t with
    | TFun (loc, typs) -> (
      (*
        Below allows partial application
      *)
      let supplied_typs = List.sub typs ~len:(List.length params) ~pos:0 in
      let result_typ = List.last typs in
      match result_typ with
      | None -> Error "unable to identify return type"
      | Some result_typ -> (
        match List.zip supplied_typs params with
        | None -> Error "number of parameters does not match number of types"
        | Some typ_params -> Ok (
          ((List.fold typ_params ~f:(fun gamma (typ, param) -> update gamma param (TBinding typ)) ~init:(gamma)), result_typ)
        )
      )
    )
    | t' -> Error (sprintf "function type expected, found %s" (pretty_typ t'))
  in
  match expr with
  | L (loc, l) -> (
    match typecheck_literal t l with
    | None -> None
    | Some e -> Some [(TypeError (loc, E (L (loc, l)), e), tag_expr)]
  )
  | C (loc, c) -> typecheck_complex t gamma c loc
  | UnOp (loc, op, e) -> (
    match t with
    | TBool loc' -> typecheck_expr (TBool loc') gamma e
    | t' -> (
      Some [(TypeError ((typ_loc t'), E (UnOp (loc, op, e)), sprintf "attempted to assign the result of a unary expression to type %s, expected %s" (pretty_typ t') (pretty_typ (TBool loc))), tag_expr)]
    )
  )
  | BinOp (loc, op, e1, e2) -> (
    let try_pair (t: typ) (e1: expr) (e2: expr): (err * ('a -> 'a)) list option =
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
      let rec try_pairs (ts: typ list) (e1: expr) (e2: expr): (err * ('a -> 'a)) list option =
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
    | op', t -> (
      Some [(TypeError (loc, E (BinOp (loc, op, e1, e2)), (sprintf "attempted to assign the result of a binary expression to type %s, expected %s" (pretty_typ t) (pretty_typ (TBool loc)))), tag_expr)]
    )
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
    | t' -> (
      Some [(TypeError (loc, E (NumOp (loc, op, e1, e2)), sprintf "attempted to assign the result of an arithmetic expression to type %s, %s or %s expected" (pretty_typ t') (pretty_typ (TInt loc)) (pretty_typ (TFloat loc))), tag_expr)]
    )
  )
  | ListOp (loc, op, e1, e2) -> (
    match t with
    | TList (loc', t') -> (
      let e1' = typecheck_expr (TList (loc', t')) gamma e1 in
      let e2' = typecheck_expr (TList (loc', t')) gamma e2 in
      propagate_error e1' e2'
    )
    | t' -> (
      Some [(TypeError (loc, E (ListOp (loc, op, e1, e2)), sprintf "attempted to assign the result of a list operator expression (%s) to type %s, list type expected" (pretty_list_op op) (pretty_typ t')), tag_expr)]
    )
  )
  | Var (loc, name) -> (
    match gamma name with
    | None -> Some [(TypeError (loc, E (Var (loc, name)), sprintf "Name %s not found in current environment" name), tag_expr)]
    | Some (TBinding t') -> (
      match match_typ t t' gamma with
      | Ok true -> None
      | Ok false -> Some [(TypeError (loc, E (Var (loc, name)), (sprintf "Type of variable %s (%s) does not match required %s" name (pretty_typ t') (pretty_typ t))), tag_expr)]
      | Error e -> Some [(TypeError (loc, E (Var (loc, name)), e), tag_expr)]
    )
    | Some (TType t) -> Some [(TypeError (loc, E (Var (loc, name)), sprintf "Name %s refers to a type, it cannot be used in this context" name), tag_expr)]
    | Some (TConstructor t) -> Some [(TypeError (loc, E (Var (loc, name)), sprintf "Name %s refers to a type constructor, it cannot be used in this context" name), tag_expr)]
  )
  | LetIn (loc, name, typ, body, rest) -> (
    let gamma' = update gamma name (TBinding typ) in
    let e1 = typecheck_expr typ gamma body in
    let e1' = match e1 with
    | None -> None
    | Some ((TypeError (loc, n, e), tr) :: t) -> (
      Some ((TypeError (loc, E (LetIn (loc, name, typ, body, rest)), e), tag_expr) :: t)
    )
    | e -> e
    in
    let e2 = typecheck_expr t gamma' rest in
    propagate_error e1' e2
  )
  | LetRecIn (loc, name, typ, body, rest) -> (
    let gamma' = update gamma name (TBinding typ) in
    let e1 = typecheck_expr typ gamma' body in
    let e1' = match e1 with
    | None -> None
    | Some ((TypeError (loc, n, e), tr) :: t) -> (
      Some ((TypeError (loc, E (LetRecIn (loc, name, typ, body, rest)), e), tag_expr) :: t)
    )
    | e -> e
    in
    let e2 = typecheck_expr t gamma' rest in
    propagate_error e1' e2
  )
  | Fun (loc, params, typ, body) -> (
    match construct_env typ params gamma with
    | Ok (gamma', result_typ) -> (
      match match_typ typ t gamma with
      | Ok true ->  typecheck_expr result_typ gamma' body
      | Ok false -> Some [(TypeError (loc, E (Fun (loc, params, typ, body)), sprintf "%s does not match %s" (pretty_typ typ) (pretty_typ t)), tag_expr)]
      | Error e -> Some [(TypeError (loc, E (Fun (loc, params, typ, body)), e), tag_expr)]
    )
    | Error e -> Some [(TypeError (loc, E (Fun (loc, params, typ, body)), e), tag_expr)]
  )
  | Match (loc, e, typ, match_branches) -> (
    let match_expr_err = typecheck_expr typ gamma e in
    let match_branch_errs = typecheck_match t typ gamma e match_branches in
    let tr_mb i tr e =
      match e with
      | Match (loc, e, typ, match_branches) -> (
        let new_branches = List.mapi match_branches ~f:(fun i' mb -> (
          match i' with
          | i' when i' = i -> tr mb
          | _ -> mb
        )) in
        Match (loc, e, typ, new_branches)
      )
      | e' -> e'
    in
    let mes = match match_expr_err with
    | Some l -> l
    | None -> []
    in
    let mbs = List.map match_branch_errs ~f:(fun (i, e, tr) -> (e, tr_mb i tr)) in
    match mes @ mbs with
    | [] -> None
    | mebs -> Some mebs
  )
  | App (loc, e, param_typs) -> (
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

let typecheck_stmt (gamma: env) (stmt: stmt): (env, (env * (err * (stmt -> stmt)) list)) result =
  let expr_tr_to_stmt tr s =
    match s with
    | Let (loc, name, typ, body) -> Let (loc, name, typ, tr body)
    | LetRec (loc, name, typ, body) -> LetRec (loc, name, typ, tr body)
    | s' -> s'
  in
  let err_expr_tr_to_stmt ((e: err), tr) = (e, expr_tr_to_stmt tr)
  in
  match stmt with
  | Let (loc, name, typ, body) -> (
    let gamma' = update gamma name (TBinding typ) in
    match typecheck_expr typ gamma body with
    | None -> Ok gamma'
    | Some ((TypeError (loc, e', e), transform) :: t) -> (
      Error (gamma',((TypeError (loc, S (Let (loc, name, typ, body)), e), expr_tr_to_stmt transform) :: (List.map t ~f:err_expr_tr_to_stmt)))
    )
    | Some e -> (
      let transform ((e: err), tr) = (e, fun s ->
        match s with
        | Let (loc, name, typ, body) -> Let (loc, name, typ, tr body)
        | s' -> s'
      )
      in
      Error (gamma', List.map e ~f:transform)
    )
  )
  | LetRec (loc, name, typ, body) -> (
    let gamma' = update gamma name (TBinding typ) in
    match typecheck_expr typ gamma' body with
    | None -> Ok gamma'
    | Some ((TypeError (loc, e', e), transform) :: t) -> (
      Error (gamma', ((TypeError (loc, S (Let (loc, name, typ, body)), e), expr_tr_to_stmt transform) :: (List.map t ~f:err_expr_tr_to_stmt)))
    )
    | Some e -> (
      Error (gamma', List.map e ~f:err_expr_tr_to_stmt)
    )
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

let typecheck (program: program): (int * err * (stmt -> stmt)) list option =
  let maybe_error = List.foldi program ~f:(fun i gamma stmt -> (
    match gamma with
    | Ok gamma -> (
      match typecheck_stmt gamma stmt with
      | Ok gamma' -> Ok gamma'
      | Error (gamma', etrs) -> Error (gamma', List.map etrs ~f:(fun (e, tr) -> (i, e, tr)))
    )
    | Error (gamma, e) -> (
      match typecheck_stmt gamma stmt with
      | Ok gamma' -> Error (gamma', e)
      | Error (gamma', e') -> Error (gamma', (List.map e' ~f:(fun (e, tr) -> (i, e, tr))) @ e)
    )
  )) ~init:(Ok empty_env)
  in
  match maybe_error with
  | Ok _ -> None
  | Error (_, e) -> Some e

let rec patch_program (program: (int * stmt) list) (patches: (int * err * (stmt -> stmt)) list): program =
  match program, patches with
  | (i, s) :: rest_program, (i', _, patch) :: rest_patches when i = i' -> (patch_program ((i, (patch s)) :: rest_program) rest_patches)
  | (i, s) :: rest_program, patches -> s :: (patch_program rest_program patches)
  | program, [] -> List.map program ~f:(fun (i, s) -> s)
  | [], _ -> []
let patch_program (program: stmt list) (patches: (int * err * (stmt -> stmt)) list): program =
  patch_program (List.mapi program ~f:(fun i s -> (i, s))) patches

let typecheck (program: program): program * err list option =
  match typecheck program with
  | None -> program, None
  | Some etrs ->  (
    let new_prog = patch_program program (List.rev etrs) in
    let es = List.map etrs ~f:(fun (_, e, _) -> e) in
    (new_prog, Some es)
  )