open Core
open Tast
open Ast

(*
  TODO explicit type annotations

  TODO: polymorphism?
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
[@@deriving show]

let next = ref 0

let fresh_tvar () = next := !next + 1; !next

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

let rec gather_mb (gamma: env) (mb: match_branch): Tast.match_branch * typ' * env * constr list =
  match mb with
  | ML (loc, l) -> (Tast.ML (loc, lit_to_tast l), lit_to_typ l, gamma, [])
  | MVar (loc, name) -> (
    let t = TVar (fresh_tvar ()) in
    (Tast.MVar (loc, name), t, update gamma name (TBinding t), [])
  )
  | Blank loc -> (Tast.Blank loc, TVar (fresh_tvar ()), gamma, [])
  | MTuple (loc, mbs) -> (
    let (mbs', ts, gamma', cs) = List.fold mbs ~f:(fun (mbs, ts, gamma, cs) mb -> (
      let mb', t, gamma', cs' = gather_mb gamma mb in
      (mbs @ [mb'], ts @ [t], gamma', cs' @ cs)
    )) ~init:([], [], gamma, []) in
    (Tast.MTuple (loc, mbs'), TTuple ts, gamma', cs)
  )
  | MRecord (loc, constructor, mbs) -> (
    let (mbs', ts, gamma', cs) = List.fold mbs ~f:(fun (mbs, ts, gamma, cs) mb -> (
      let mb', t, gamma', cs' = gather_mb gamma mb in
      (mbs @ [mb'], ts @ [t], gamma', cs' @ cs)
    )) ~init:([], [], gamma, []) in
    match gamma constructor with
    | Some (TConstructor (Nullary (_, parent))) when List.length mbs = 0 -> (
      (Tast.MRecord (loc, constructor, mbs'), TRecord parent, gamma', cs)
    )
    | Some (TConstructor (Nullary (_, parent))) -> ( (* TODO error handing -- too many arguments *)
      (Tast.MRecord (loc, constructor, mbs'), TRecord parent, gamma', cs)
    )
    | Some (TConstructor (Nary (_, parent, typs))) -> (
      match List.zip typs ts with
      | Some tts -> (Tast.MRecord (loc, constructor, mbs'), TRecord parent, gamma', tts @ cs)
      | None -> (Tast.MRecord (loc, constructor, []), TInt, empty_env, []) (* TODO error handling *)
    )
    | _ -> (Tast.MRecord (loc, constructor, []), TRecord "", empty_env, []) (* TODO error handling *)
  )
  | MNil loc -> (Tast.MNil loc, TList (TVar (fresh_tvar ())), gamma, [])
  | MCons (loc, mb, mb') -> (
    let (mb1', t, gamma', cs) = gather_mb gamma mb in
    let (mb1'', t', gamma'', cs') = gather_mb gamma mb' in
    let list_typ = TVar (fresh_tvar ()) in
    (Tast.MCons (loc, mb1', mb1''), TList list_typ, (merge gamma (merge gamma' gamma'')), (t, list_typ) :: (t, list_typ) :: cs @ cs')
  )

and complex_to_typ (gamma: env) (c: complex): Tast.complex * typ' * constr list =
  match c with
  | Tuple ts -> (
    let (e', ts', cs) = List.fold ts ~f:(fun (es, t, cs) e -> (
      let (e', t', cs') = gather_expr gamma e in
      (es @ [e'], t @ [t'], cs' @ cs)
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
            (es @ [e'], (typ, t) :: cs' @ acc)
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
      (typs @ [t], update gamma name (TBinding t))
    )) ~init:(([], gamma)) in
    let (e', t', cs) = gather_expr gamma' body in
    (Tast.Fun (loc, params, TFun (typs @ [t']), e'), TFun (typs @ [t']), cs)
  )
  | Match (loc, e, branches) -> (
    let result_typ = TVar (fresh_tvar ()) in
    let (match_expr, match_expr_typ, cs) = gather_expr gamma e in
    let (mbs', cs') = List.fold branches ~f:(fun (mbs, css) (mb, e) -> (
      let (mb', t, gamma', cs) = gather_mb gamma mb in
      let (e', t', cs') = gather_expr gamma e in
      (mbs @ [(mb', e')], (t, match_expr_typ) :: (result_typ, t') :: cs @ css)
    )) ~init:([], []) in
    (Tast.Match (loc, match_expr, match_expr_typ, mbs'), result_typ, cs' @ cs)
  )
  | App (loc, e, es) -> (
    (*
      e: A -> B -> C -> D
      es: [A; B]

      t: C -> D
    *)
    let (e', et, cs) = gather_expr gamma e in
    let (es', ets, cs') = List.fold es ~f:(fun (es, ets, cs') e' -> (
    let (e', et, cs) = gather_expr gamma e' in
      (es @ [(e', et)], ets @ [et], cs @ cs')
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

let rec tvars (t: typ'): int list =
   match t with
  | TFun ts -> List.fold ts ~f:(fun acc t -> (tvars t) @ acc) ~init:[]
  | TTuple ts -> List.fold ts ~f:(fun acc t -> (tvars t) @ acc) ~init:[]
  | TList t -> tvars t
  | TSecret t -> tvars t
  | TPublic t -> tvars t
  | TVar x -> [x]
  | _ -> []

let rec apply_env' (gamma: env') (t: typ'): typ' =
  match t with
  | TVar x -> (
    match gamma x with
    | Some t -> t
    | None -> TAny
  )
  | TFun ts -> TFun (List.map ts ~f:(fun t -> apply_env' gamma t))
  | TTuple ts -> TTuple (List.map ts ~f:(fun t -> apply_env' gamma t))
  | TList t -> TList (apply_env' gamma t)
  | TSecret t -> TSecret (apply_env' gamma t)
  | TPublic t -> TPublic (apply_env' gamma t)
  | t' -> t'

let subst_all (gamma: env') (cs: constr list) =
  List.map cs ~f:(fun (t, t') -> (apply_env' gamma t, apply_env' gamma t'))

let rec unify (gamma: env') (cs: constr list): env' =
  match cs with
  | c :: rest -> (
    match c with
    | t, t' when t = t' -> unify gamma rest
    | TVar x, t' when not (List.exists (tvars t') ~f:(fun t -> t = x)) -> (
      let gamma' = update gamma x t' in
      let rest' = subst_all gamma' rest in
      merge gamma' (unify gamma' rest')
    )
    | t, TVar x when not (List.exists (tvars t) ~f:(fun t -> t = x)) -> (
      let gamma' = update gamma x t in
      let rest' = subst_all gamma' rest in
      merge gamma' (unify gamma' rest')
    )
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
    | a, b -> printf "Failed: %s = %s\n" (show_typ' a) (show_typ' b); unify gamma rest (* TODO error handling *)
  )
  | [] -> gamma

let unify (cs: constr list): env' = unify empty_env' cs

let rec subst_expr (gamma: env') (e: Tast.expr): Tast.expr =
  match (e : Tast.expr) with
  | L (loc, l) -> L (loc, l)
  | C (loc, c) -> C  (loc, c)
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

open Pretty

let infer_expr (gamma: env) (e: Ast.expr): Tast.expr =
  let e, t, cs = gather_expr gamma e in
  let gamma = unify cs in
  printf "Constraints:\n";
  List.iter cs ~f:(fun (t, t') -> printf "%s == %s\n" (show_typ' t) (show_typ' t'));
  printf "Gamma:\n";
  List.iter (Array.to_list (Array.mapi (Array.create 0 ~len:!next) ~f:(fun i _ -> (i + 1)))) ~f:(fun a -> (
    match gamma a with
    | Some t -> printf "%d: %s\n" a (show_typ' t)
    | None -> printf "%d None\n" a));
  printf "%s\n" (pretty_expr e);
  next := 0;
  subst_expr gamma e

let infer_stmt (gamma: env) (s: Ast.stmt): env * Tast.stmt =
  let s, gamma', cs = gather_stmt gamma s in
  let gamma'' = unify cs in
  printf "Constraints:\n";
  List.iter cs ~f:(fun (t, t') -> printf "%s == %s\n" (show_typ' t) (show_typ' t'));
  printf "Gamma:\n";
  List.iter (Array.to_list (Array.mapi (Array.create 0 ~len:!next) ~f:(fun i _ -> (i + 1)))) ~f:(fun a -> (
    match gamma'' a with
    | Some t -> printf "%d: %s\n" a (show_typ' t)
    | None -> printf "%d None\n" a));
  printf "%s\n" (pretty_stmt s);
  next := 0;
  (gamma', subst_stmt gamma'' s)

let infer_program (p: Ast.program): Tast.program =
  let p, cs = gather_program p in
  next := 0;
  let gamma = unify cs in
  subst_program gamma p



let lit_to_typ' (l: Tast.literal): typ' =
  match l with
  | Unit -> TUnit
  | Int _ -> TInt
  | Float _ -> TFloat
  | String _ -> TString
  | Char _ -> TChar
  | Bool _ -> TBool

let rec complex_to_typ' (gamma: env) (c: Tast.complex): typ' option =
  match c with
  | Tuple es -> (
    let es' = List.fold es ~f:(fun acc e -> (
      match acc, expr_typ gamma e with
      | Some t, Some t' -> Some (t @ [t'])
      | _, _ -> None
    )) ~init:(Some []) in
    match es' with
    | Some ets -> Some (TTuple ets)
    | None -> None
  )
  | Record (name, params) -> (
    match gamma name with
    | Some (TConstructor (Nullary (c, p))) when c = name -> Some (TRecord p)
    | Some (TConstructor (Nary (c, p, typs))) when c = name -> (
      match List.zip typs params with
      | Some typ_params -> List.fold typ_params ~f:(fun acc (t, e) -> (
        match expr_typ gamma e with
        | Some t' when t = t' -> acc
        | _ -> None
      )) ~init:(Some (Tast.TRecord p)) 
      | None -> None
    )
    | _ -> None
  )
  | Nil -> Some (Tast.TList Tast.TAny) (* ? *)
  | Cons (e, e') -> (
    let t = expr_typ gamma e in
    let t' = expr_typ gamma e' in
    match t with
    | Some t1 -> (
      match t' with
      | Some t1' when t1 = t1' -> Some (Tast.TList t1')
      | _ -> None
    )
    | None -> None
  )

and expr_typ (gamma: env) (e: Tast.expr): typ' option =
  match e with
  | L (_, l) -> Some (lit_to_typ' l) 
  | C (_, c) -> complex_to_typ' gamma c
  | Var (_, name) -> (
    match gamma name with
    | Some (TBinding t) -> Some t
    | _ -> None
  )
  | LetIn (_, _, t, _, _) -> Some t (* Type of binding, not resulting expression *)
  | LetRecIn (_, _, t, _, _) -> Some t (* Type of binding, not resulting expression *)
  | Fun (_, _, t, _) -> Some t
  | Match (_, _, _, mbs) -> ( (* What is the type of a match expression? *)
    match List.hd mbs with
    | Some (_, fst_e) -> (
      let t = expr_typ gamma fst_e in
      List.fold (List.slice mbs 1 (List.length mbs)) ~f:(fun acc (_, e) -> (
        let t' = expr_typ gamma e in
        match t, t' with
        | Some t, Some t' when t = t' -> Some t
        | _, _ -> None 
      )) ~init:t
    )
    | None -> None
  )
  | App (_, e, e') -> (
    let t = expr_typ gamma e in
    let (_, supplied_param_typs) = List.unzip e' in
    match t with
    | Some (TFun param_typs) -> (
      if List.length param_typs < List.length supplied_param_typs then None else
      match List.zip (List.slice param_typs 0 (List.length supplied_param_typs)) supplied_param_typs with
      | Some as_t -> (
        List.fold as_t ~f:(fun acc (a, s) -> (
          if a = s then acc else None
        )) ~init:(Some (Tast.TFun (List.slice param_typs (List.length supplied_param_typs) (List.length param_typs))))
      )
      | None -> None
    )
    | _ -> None
  )
  | Seq (_, e, e') -> expr_typ gamma e'