open Core
open Tast
open Ast

(*
  TODO explicit type annotations
*)

let rec typ_to_typ' (t: typ): typ' =
  match t with
  | TInt _ -> TInt
  | TFloat _ -> TFloat
  | TBool _ -> TBool
  | TString _ -> TString
  | TChar _ -> TChar
  | TUnit _ -> TUnit
  | TFun (_, typs) -> TFun (List.map typs typ_to_typ')
  | TTuple (_, typs) -> TTuple (List.map typs typ_to_typ')
  | TRecord (_, name) -> TRecord name
  | TList (_, typ) -> TList (typ_to_typ' typ) 
  | TSecret (_, typ) -> TSecret (typ_to_typ' typ)
  | TPublic (_, typ) -> TPublic (typ_to_typ' typ)

type constr = typ' * typ'

type constructor =
  | Nullary of string * string (* constructor * parent *)
  | Nary of string * string * typ' list (* constructor * parent * typs *)

type ttyp =
  | TBinding of typ'
  | TType of constructor list
  | TConstructor of constructor

type env = string -> ttyp option
let empty_env: env = fun _ -> None

let update (gamma: env) (x: string) (t: ttyp): env =
  fun i -> if i = x then Some t else gamma i

let merge (gamma: env) (gamma': env): env =
  fun i -> match gamma i with | Some x -> Some x | None -> gamma' i

let lit_to_typ (l: literal): typ' =
  match l with
  | Unit -> TUnit
  | Int _ -> TInt
  | Float _ -> TFloat
  | String _ -> TString
  | Char _ -> TChar
  | Bool _ -> TBool

let lit_to_tast (l: literal): Tast.literal =
  match l with
  | Unit -> Tast.Unit
  | Int i -> Tast.Int i
  | Float f -> Tast.Float f
  | String s -> Tast.String s
  | Char c -> Tast.Char c
  | Bool b -> Tast.Bool b

let unop_op_to_tast (op: unop): Tast.unop =
  match op with
  | Not -> Tast.Not

let binop_op_to_tast (op: binop): Tast.binop =
  match op with
  | Lt -> Tast.Lt
  | Lte -> Tast.Lte
  | Eq -> Tast.Eq
  | Gt -> Tast.Gt
  | Gte -> Tast.Gte
  | Neq -> Tast.Neq
  | And -> Tast.And
  | Or -> Tast.Or

let numop_op_to_tast (op: numop): Tast.numop =
  match op with
  | Sub -> Tast.Sub
  | Add -> Tast.Add
  | Mult -> Tast.Mult
  | Div -> Tast.Div

let listop_op_to_tast (op: listop): Tast.listop =
  match op with
  | Concat -> Tast.Concat

let rec match_mb_t (gamma: env) (t: typ') (mb: match_branch): Tast.match_branch * env * constr list =
  match mb with
  | ML (loc, l) -> (Tast.ML (loc, lit_to_tast l), gamma, [(t, lit_to_typ l)])
  | MVar (loc, name) -> (Tast.MVar (loc, name), update gamma name (TBinding t), [])
  | Blank loc -> (Tast.Blank loc, gamma, [])
  | MTuple (loc, mbs) -> (
    match t with
    | TTuple typs -> (
      match List.zip typs mbs with
      | Some typ_mbs -> (
        let (mbs', gamma', typs', cs') = List.fold typ_mbs ~f:(fun (mbs, gamma', typs, cs) (typ, mb) -> (
          let (mb', gamma'', cs') = match_mb_t gamma typ mb in
          (mb' :: mbs, merge gamma' gamma'', typ :: typs, cs' @ cs)
        )) ~init:(([], gamma, [], [])) in
        (Tast.MTuple (loc, mbs'), gamma', (t, TTuple typs') :: cs')
      )
      | None -> (Tast.MTuple (loc, []), empty_env, []) (* TODO error handling *)
    )
    | _ -> (Tast.MTuple (loc, []), empty_env, []) (* TODO error handling *)
  )
  | MRecord (loc, constructor, mbs) -> (
    match gamma constructor with
    | Some (TConstructor c) -> (
      match c with
      | Nullary (_, parent) -> (
        match mbs with
        | [] -> (Tast.MRecord (loc, constructor, []), gamma, [])
        | _ -> (Tast.MRecord (loc, constructor, []), empty_env, []) (* TODO error handling *)
      )
      | Nary (_, parent, args) -> (
        match List.zip args mbs with
        | Some argmbs -> (
          let (mbs', gamma', css) = List.fold argmbs ~f:(fun (mbs, gamma, cs) (typ, mb) -> (
            let (mb', gamma', cs') = match_mb_t gamma typ mb in
            (mb' :: mbs, merge gamma gamma', cs' @ cs)
          )) ~init:([], gamma, []) in
          (Tast.MRecord (loc, constructor, mbs'), gamma', css)
        )
        | None -> (Tast.MRecord (loc, constructor, []), empty_env, []) (* TODO error handling *)
      )
    )
    | _ -> (Tast.MRecord (loc, constructor, []), empty_env, []) (* TODO error handling *)
  )
  | MNil loc -> (Tast.MNil loc, gamma, [t, (TList (TVar (fresh_tvar ())))])
  | MCons (loc, mb, mb') -> (
    match t with
    | TList typ -> (
      let (mb1', gamma', cs) = match_mb_t gamma typ mb in
      let (mb1'', gamma'', cs') = match_mb_t gamma typ mb' in
      (Tast.MCons (loc, mb1', mb1''), merge gamma'' (merge gamma' gamma), cs @ cs')
    )
    | _ -> (Tast.MNil loc, empty_env, []) (* TODO error handling *)
  )

and match_mb (gamma: env) (e: expr) (mb: match_branch): Tast.match_branch * Tast.expr * env * constr list =
  let (e', t, cs) = gather_expr gamma e in
  let (mb', gamma', cs') = match_mb_t gamma t mb in
  (mb', e', gamma', cs' @ cs)

and complex_to_typ (gamma: env) (c: complex): Tast.complex * typ' * constr list =
  match c with
  | Tuple ts -> (
    let (e', ts', cs) = List.fold ts ~f:(fun (es, t, cs) e -> (
      let (e', t', cs') = gather_expr gamma e in
      (e' :: es, t' :: t, cs' @ cs)
    )) ~init:([], [], []) in
    (Tast.Tuple e', TTuple ts', cs)
  )
  | Record (constructor, params) -> (
    match gamma constructor with
    | Some (TConstructor c) -> (
      match c with
      | Nullary (_, parent) -> (
        match params with
        | [] -> (Tast.Record (constructor, []), TRecord parent, [])
        | _ -> (Tast.Record (constructor, []), TInt, []) (* TODO error handling *)
      )
      | Nary (_, parent, args) -> (
        match List.zip args params with
        | Some argparams -> (
          let (es', cs) = List.fold argparams ~f:(fun (es, acc) (typ, e) -> (
            let (e', t, cs') = gather_expr gamma e in
            (e' :: es, (typ, t) :: cs' @ acc)
          )) ~init:([], []) in
          (Tast.Record (constructor, es'), TRecord parent, cs)
        )
        | None -> (Tast.Record (constructor, []), TInt, []) (* TODO error handling *)
      )
    ) 
    | _ -> (Tast.Record ("", []), TInt, []) (* TODO error handling *)
  )
  | Nil -> (Tast.Nil, TList (TVar (fresh_tvar ())), [])
  | Cons (e, e') -> (
    let list_typ = TVar (fresh_tvar ()) in
    let (e1', t, cs) = gather_expr gamma e in
    let (e1'', t', cs') = gather_expr gamma e' in
    (Tast.Cons (e1', e1''), TList list_typ, (list_typ, t) :: (list_typ, t') :: cs @ cs')
  )

and gather_expr (gamma: env) (e: expr): Tast.expr * typ' * constr list =
  match e with
  | L (loc, l) -> (Tast.L (loc, lit_to_tast l), lit_to_typ l, [])
  | C (loc, c) -> (
    let (c', t, cs) = complex_to_typ gamma c in
    (Tast.C (loc, c'), t, cs)
  )
  | UnOp (loc, op, e') -> (
    match op with
    | Not -> (
      let (e', t', cs) = gather_expr gamma e' in
      (Tast.UnOp (loc, unop_op_to_tast op, e'), TBool, (TBool, t') :: cs) (* Optimistic? *)
    )
  )
  | BinOp (loc, op, e1, e2) -> (
    match op with
    | op' when (
      (op' = Lt) ||
      (op' = Lte) ||
      (op' = Gt) ||
      (op' = Gte)
    ) -> ((* Numerical *)
      let (e1', e1t, cs1) = gather_expr gamma e1 in
      let (e2', e2t, cs2) = gather_expr gamma e2 in
      (Tast.BinOp (loc, binop_op_to_tast op, e1', e2'), Tast.TBool, (TOr [TInt; TFloat], e1t) :: (TOr [TInt; TFloat], e2t) :: (e1t, e2t) :: cs1 @ cs2)
    )
    | op' when (
      (op' = And) ||
      (op' = Or)
    ) -> ((* Binary *)
      let (e1', e1t, cs1) = gather_expr gamma e1 in
      let (e2', e2t, cs2) = gather_expr gamma e2 in
      (Tast.BinOp (loc, binop_op_to_tast op, e1', e2'), TBool, (Tast.TBool, e1t) :: (TBool, e2t) :: cs1 @ cs2)
    )
    | op' -> ((* Any matching *)
      let (e1', e1t, cs1) = gather_expr gamma e1 in
      let (e2', e2t, cs2) = gather_expr gamma e2 in
      (Tast.BinOp (loc, binop_op_to_tast op, e1', e2'), TBool, (e1t, e2t) :: cs1 @ cs2)
    )
  ) 
  | NumOp (loc, op, e1, e2) -> (
    let (e1', e1t, cs1) = gather_expr gamma e1 in
    let (e2', e2t, cs2) = gather_expr gamma e2 in
    (Tast.NumOp (loc, numop_op_to_tast op, e1', e2'), TOr [TInt; TFloat], (TOr [TInt; TFloat], e1t) :: (TOr [TInt; TFloat], e2t) :: (e1t, e2t) :: cs1 @ cs2)
  )
  | ListOp (loc, op, e1, e2) -> (
    let list_typ = Tast.TList (TVar (fresh_tvar ())) in
    let (e1', e1t, cs1) = gather_expr gamma e1 in
    let (e2', e2t, cs2) = gather_expr gamma e2 in
    (Tast.ListOp (loc, listop_op_to_tast op, e1', e2'), list_typ, (list_typ, e1t) :: (list_typ, e2t) :: cs1 @ cs2)
  )
  | Var (loc, name) -> (
    match gamma name with
    | Some (TBinding t') -> (Tast.Var (loc, name), t', [])
    (* TODO handle errors *)
    | _ -> (Tast.Var (loc, name), TInt, [])
  )
  | LetIn (loc, name, typ, body, rest) -> (
    let (e1', e1t, cs1) = gather_expr gamma body in
    let gamma' = update gamma name (TBinding e1t) in
    let (e2', e2t, cs2) = gather_expr gamma' rest in
    (Tast.LetIn (loc, name, e2t, e1', e2'), e2t, cs1 @ cs2)
  )
  | LetRecIn (loc, name, typ, body, rest) -> (
    let expr_typ = TVar (fresh_tvar ()) in
    let gamma' = update gamma name (TBinding expr_typ) in
    let (e1', e1t, cs1) = gather_expr gamma' body in
    let gamma' = update gamma name (TBinding e1t) in
    let (e2', e2t, cs2) = gather_expr gamma' rest in
    (Tast.LetRecIn (loc, name, e2t, e1', e2'), e2t, (e1t, expr_typ) :: cs1 @ cs2)
  ) 
  | Fun (loc, params, body) -> (
    let (typs, gamma') = List.fold params ~f:(fun (typs, gamma) name -> (
      let t = TVar (fresh_tvar ()) in
      (t :: typs, update gamma name (TBinding t))
    )) ~init:(([], gamma)) in
    let (e', t', cs) = gather_expr gamma' body in
    (Tast.Fun (loc, params, t', e'), TFun (typs @ [t']), cs)
  )
  | Match (loc, e, branches) -> (
    let t = TVar (fresh_tvar ()) in
    let (e', t', cs) = gather_expr gamma e in
    let (mbs', cs') = List.fold branches ~f:(fun (mbs, css) (mb, e) -> (
      let (mb', e', gamma', cs) = match_mb gamma e mb in
      ((mb', e') :: mbs, (t, t') :: cs @ css)
    )) ~init:([], []) in
    (Tast.Match (loc, e', t', mbs'), t, cs' @ cs)
  )
  | App (loc, e, es) -> (
    (*
      et' = [A, B]...
      et = [A, B, C]
      C
    *)
    let (e', et, cs) = gather_expr gamma e in
    let (es', ets, cs') = List.fold es ~f:(fun (es, ets, cs') e' -> (
      let (e', et, cs) = gather_expr gamma e' in
      ((e', et) :: es, et :: ets, cs @ cs')
    )) ~init:([], [], []) in
    let ret_typ = TVar (fresh_tvar ()) in
    (Tast.App (loc, e', es'), ret_typ, (et, Tast.TFun (ets @ [ret_typ])) :: cs @ cs')
  )
  | Seq (_, e, e') -> (
    (* TODO warn if typ e is not unit *)
    gather_expr gamma e'
  )

let gather_stmt (gamma: env) (s: stmt): Tast.stmt * env * constr list =
  match s with
  | Let (l, name, typ, body) -> (
    let body, t', cs = gather_expr gamma body in
    (Let (l, name, t', body), update gamma name (TBinding t'), cs)
  )
  | LetRec (l, name, typ, body) -> (
    let stmt_typ = TVar (fresh_tvar ()) in
    let gamma' = update gamma name (TBinding stmt_typ) in
    let body, t', cs = gather_expr gamma' body in
    (Tast.LetRec (l, name, t', body), update gamma name (TBinding t'), (t', stmt_typ) :: cs)
  )
  | Type (l, name, constructors) -> (
    let (constructors, gamma', cs) = List.fold constructors ~f:(fun (cts, gamma, cs) (c, typs) -> (
      match typs with
      | None -> (
        let c' = Nullary (c, name) in
        ((name, None) :: cts, update gamma c (TConstructor c'), cs)
      )
      | Some typs -> (
        let tts = List.map typs typ_to_typ' in
        let c' = Nary (c, name, tts) in
        ((name, Some tts) :: cts, update gamma c (TConstructor c'), cs)
      )
    )) ~init:([], gamma, []) in
    (Tast.Type (l, name, constructors), update gamma' name (TType cs), [])
  )

let gather_program (p: program): Tast.program * constr list =
  let (prog, _, cs) = List.fold p ~f:(fun (prog, gamma, acc) s -> (
    let (stmt, gamma', cs) = gather_stmt gamma s in
    (prog @ [stmt], gamma', cs @ acc)
  )) ~init:([], empty_env, []) in
  (prog, cs)

(* Unification *)

type env' = int -> typ' option (* TVar -> typ *)

let empty_env' = fun _ -> None

let update (gamma: env') (tvar: int) (t: typ') =
  fun i -> if i = tvar then Some t else gamma i

let merge (gamma: env') (gamma': env') =
  fun i -> match gamma i with | Some t -> Some t | None -> gamma' i

let mkenv (x: int) (t: typ'): env' =
  fun i -> if i = x then Some t else None

let merge_maybe (gamma: env' option) (gamma': env' option): env' option =
  match gamma, gamma' with
  | Some g, Some g' -> Some (merge g g')
  | None, Some g' -> Some g'
  | Some g, None -> Some g
  | None, None -> None

let rec tvars (t: typ'): int list =
   match t with
  | TFun ts -> List.fold ts ~f:(fun acc t -> (tvars t) @ acc) ~init:[]
  | TTuple ts -> List.fold ts ~f:(fun acc t -> (tvars t) @ acc) ~init:[]
  | TList t -> tvars t
  | TSecret t -> tvars t
  | TPublic t -> tvars t
  | TVar x -> [x]
  | TOr ts -> List.fold ts ~f:(fun acc t -> (tvars t) @ acc) ~init:[]
  | _ -> []

let rec apply_env' (gamma: env') (t: typ'): typ' =
  match t with
  | TVar x -> (
    match gamma x with
    | Some t -> t
    | None -> TVar x
  )
  | TFun ts -> TFun (List.map ts ~f:(fun t -> apply_env' gamma t))
  | TTuple ts -> TTuple (List.map ts ~f:(fun t -> apply_env' gamma t))
  | TList t -> TList (apply_env' gamma t)
  | TSecret t -> TSecret (apply_env' gamma t)
  | TPublic t -> TPublic (apply_env' gamma t)
  | TOr ts -> TOr (List.map ts ~f:(fun t -> apply_env' gamma t))
  | t' -> t'

let subst_all (gamma: env') (cs: constr list) =
  List.map cs ~f:(fun (t, t') -> (apply_env' gamma t, apply_env' gamma t'))

let rec unify (gamma: env') (cs: constr list): env' =
  match cs with
  | c :: rest -> (
    match c with
    | TFun ts, TFun ts' -> (
      match List.zip ts ts' with
      | Some tts' -> unify gamma (tts' @ rest)
      | None -> unify gamma rest (* TODO error handling *)
    )
    | TTuple ts, TTuple ts' -> (
      match List.zip ts ts' with
      | Some tts' -> unify gamma (tts' @ rest)
      | None -> unify gamma rest (* TODO error handling *)
    )
    | TVar x, t -> (
      (* if List.exists (tvars t) ~f:(fun i -> i = x) then merge (update gamma x t) (unify gamma rest) else *)
      let gamma' = mkenv x t in
      merge (update gamma x t) (unify gamma (subst_all gamma' rest)) 
    )
    | t, TVar x -> (
      (* if List.exists (tvars t) ~f:(fun i -> i = x) then merge (update gamma x t) (unify gamma rest) else *)
      let gamma' = mkenv x t in
      merge (update gamma x t) (unify gamma (subst_all gamma' rest))
    )
    | t, t' when t = t' -> unify gamma rest
    (* 
      Below is a bit suspicious
    *)
    (* | TOr ts, TOr ts' -> (
      let maybe_gamma' = List.fold (List.cartesian_product ts ts') ~f:(fun gamma' c -> (
        match gamma' with
        | Some g -> Some g
        | None -> (
          match unify gamma c with
          | Some gamma -> Some gamma
          | None -> None
        )
      )) ~init:None in
      gamma
    ) *)
    | _, _ -> unify gamma rest (* TODO error handling *)
  )
  | [] -> gamma

let unify (cs: constr list): env' = unify empty_env' cs

let rec subst_expr (gamma: env') (e: Tast.expr): Tast.expr =
  match (e : Tast.expr) with
  | L (loc, l) -> L (loc, l)
  | C (loc, c) -> C  (loc, c)
  | UnOp (loc, op, e) -> UnOp (loc, op, subst_expr gamma e)
  | BinOp (loc, op, e, e') -> BinOp (loc, op, subst_expr gamma e, subst_expr gamma e')
  | NumOp (loc, op, e, e') -> NumOp (loc, op, subst_expr gamma e, subst_expr gamma e')
  | ListOp (loc, op, e, e') -> ListOp (loc, op, subst_expr gamma e, subst_expr gamma e')
  | Var (loc, name) -> Var (loc, name)
  | LetIn (loc, name, typ, body, rest) -> LetIn (loc, name, apply_env' gamma typ, subst_expr gamma body, subst_expr gamma rest)
  | LetRecIn (loc, name, typ, body, rest) -> LetRecIn (loc, name, apply_env' gamma typ, subst_expr gamma body, subst_expr gamma rest)
  | Fun (loc, params, typ, body) -> Fun (loc, params, apply_env' gamma typ, subst_expr gamma body)
  | Match (loc, e, typ, mbs) -> Match (loc, subst_expr gamma e, apply_env' gamma typ, List.map mbs ~f:(fun (mb, e) -> (mb, subst_expr gamma e)))
  | App (loc, e, et') -> App (loc, subst_expr gamma e, List.map et' ~f:(fun (e, t) -> (subst_expr gamma e, apply_env' gamma t)))
  | Seq (loc, e, e') -> Seq (loc, subst_expr gamma e, subst_expr gamma e')

let subst_stmt (gamma: env') (s: Tast.stmt): Tast.stmt =
  match s with
  | Let (loc, name, typ, body) -> Let (loc, name, apply_env' gamma typ, subst_expr gamma body)
  | LetRec (loc, name, typ, body) -> LetRec (loc, name, apply_env' gamma typ, subst_expr gamma body)
  | Type (loc, name, constructors) -> Type (loc, name, constructors)

let subst_program (gamma: env') (p: Tast.program): Tast.program =
  List.map p ~f:(subst_stmt gamma)

let test (p: program) =
  let p, cs = gather_program p in
  let gamma = unify cs in
  let p' = subst_program gamma p in
  printf "%s" (Tast.show_program p')