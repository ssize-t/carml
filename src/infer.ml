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
  | TFun (_, t, t') -> TFun (typ_to_typ' t, typ_to_typ' t')
  | TTuple (_, t, t') -> TTuple (typ_to_typ' t, typ_to_typ' t')
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
  | TForAll of int list * typ'
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

type env' = int -> typ' option (* TVar -> typ *)

let empty_env' = fun _ -> None

let update' (gamma: env') (tvar: int) (t: typ') =
  fun i -> if i = tvar then Some t else gamma i

let merge' (gamma: env') (gamma': env') =
  fun i -> match gamma i with | Some t -> Some t | None -> gamma' i

type tvarenv = int -> int option
let update tau x y = fun i -> if i = x then Some y else tau i
let empty_tvarenv = fun _ -> None

type context =
  | Empty
  | Application of typ'

let update_application (delta: context) (t: typ'): context =
  match delta with
  | Empty -> Application t
  | Application t' -> Application (TFun (t, t'))

let rec specialize (t: typ') (tid: int) (t': typ'): typ' =
  match (t : typ') with
  | TInt -> TInt
  | TFloat -> TFloat
  | TBool -> TBool
  | TString -> TString
  | TChar -> TChar
  | TUnit -> TUnit
  | TFun (t, t1) -> TFun (specialize t tid t', specialize t1 tid t')
  | TTuple (t, t1) -> TTuple (specialize t tid t', specialize t1 tid t')
  | TRecord name -> TRecord name
  | TList t -> TList (specialize t tid t')     
  | TSecret t -> TSecret (specialize t tid t')
  | TPublic t -> TPublic (specialize t tid t')
  | TVar tid' when tid = tid' -> t'
  | TVar tid' -> TVar tid'

let rec gather_mb (gamma: env) (mb: match_branch): Tast.match_branch * typ' * env * constr list =
  match mb with
  | ML (loc, l) -> (Tast.ML (loc, lit_to_tast l), lit_to_typ l, gamma, [])
  | MVar (loc, name) -> (
    let t = TVar (fresh_tvar ()) in
    (Tast.MVar (loc, name), t, update gamma name (TBinding t), [])
  )
  | Blank loc -> (Tast.Blank loc, TVar (fresh_tvar ()), gamma, [])
  | MTuple (loc, mb, mb') -> (
    let (mb1', t1, gamma', cs1') = gather_mb gamma mb in
    let (mb2', t2, gamma', cs2') = gather_mb gamma' mb' in
    (Tast.MTuple (loc, mb1', mb2'), TTuple (t1, t2), gamma', cs1' @ cs2')
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
    let (mb1', t, gamma, cs) = gather_mb gamma mb in
    let (mb1'', t', gamma, cs') = gather_mb gamma mb' in
    (Tast.MCons (loc, mb1', mb1''), TList t, gamma, (t', Tast.TList t) :: cs @ cs')
  )

and complex_to_typ (gamma: env) (delta: context) (c: complex): Tast.complex * typ' * constr list =
  match c with
  | Tuple (e, e') -> (
    let (e1', t1', cs1') = gather_expr gamma delta e in
    let (e2', t2', cs2') = gather_expr gamma delta e' in
    (Tast.Tuple (e1', e2'), TTuple (t1', t2'), cs1' @ cs2')
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
            let (e', t, cs') = gather_expr gamma delta e in
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
    let (e1', t, cs) = gather_expr gamma delta e in
    let (e1'', t', cs') = gather_expr gamma delta e' in
    (Tast.Cons (e1', e1''), TList list_typ, (list_typ, t) :: (list_typ, t') :: cs @ cs')
  )
  
and gather_expr (gamma: env) (delta: context) (e: expr): Tast.expr * typ' * constr list =
  match e with
  | L (loc, l) -> (Tast.L (loc, lit_to_tast l), lit_to_typ l, [])
  | C (loc, c) -> (
    let (c', t, cs) = complex_to_typ gamma delta c in
    (Tast.C (loc, c'), t, cs)
  )
  | Var (loc, name) -> (
    match gamma name with
    | Some (TBinding t') -> (Tast.Var (loc, name), t', [])
    | Some (TForAll (tids, t')) -> (
      match delta with
      | Empty -> printf "Var fail\n"; (Tast.Var (loc, name), TInt, [])
      | Application typ -> (
        printf "\n\nApplication: %s\n\n" (show_typ' typ);
        printf "\n\nForAll %s, t': %s\n\n" (String.concat (List.map tids ~f:(sprintf "%d")) ~sep:",") (show_typ' t');
        let rec assign_subs (actual: typ') (given: typ'): (int * typ') list =
          match actual, given with
          | TFun (a, a'), TFun (g, g') -> (assign_subs a g) @ (assign_subs a' g')
          | TTuple (a, a'), TTuple (g, g') -> (assign_subs a g) @ (assign_subs a' g')
          | TList a, TList g -> assign_subs a g 
          | TSecret a, TSecret g -> assign_subs a g
          | TPublic a, TPublic g -> assign_subs a g
          | TVar x, g -> [(x, g)]
          | _, _ -> []
        in
        let rec match_typ (actual: typ') (given: typ'): typ' * constr list =
          let get_subs a g =
            assign_subs a g
            |> List.filter ~f:(fun (tid, _) -> List.mem tids tid ~equal:(fun a b -> a = b))
          in
          match actual, given with
          | TFun (a, a'), TFun (g, g') when get_subs a g |> List.length > 0 -> (
            printf "\n\nPerforming substitution\n\n";
            let a'' = get_subs a g
            |> List.fold ~f:(fun a' (tid, t) -> specialize a' tid t) ~init:a' in
            match_typ a'' g'
          )
          | TFun (at, at'), TFun (gt, gt') -> (
            let (ret_typ, cs) = match_typ at' gt' in
            (ret_typ, (at, gt) :: cs)
          )
          | TFun (a, a'), g when get_subs a g |> List.length > 0 -> (
            printf "\n\nReached end or args, got ret_typ\n\n";
            printf "TFun (%s, %s), %s\n" (show_typ' a) (show_typ' a') (show_typ' g);
            printf "a': %s\n" (show_typ' a');
            get_subs a g
            |> List.iter ~f:(fun (tid, t) -> printf "Sub: %d --> %s\n" tid (show_typ' t));
            let a'' = get_subs a g
            |> List.fold ~f:(fun a' (tid, t) -> specialize a' tid t) ~init:a' in
            (a'', [])
          )
          | TFun (at, at'), gt -> (
            (at', [(at, gt)])
          )
          | t, t' -> raise Not_found
        in
        let (ret_typ, cs) = match_typ t' typ in
        printf "Got ret_typ: %s\n" (show_typ' ret_typ);
        (Tast.Var (loc, name), ret_typ, cs)        
      )
    )
    (* TODO handle errors *)
    | _ -> (Tast.Var (loc, name), TInt, [])
  )
  | LetIn (loc, name, typ, body, rest) -> (
    (*
    Let-Polymorphism, infer let body locally
    *)
    let old_next = !next in
    let body', t, cs = gather_expr gamma delta body in
    let gamma' = unify cs in
    let body'' = subst_expr gamma' body' in
    let body_typ = expr_typ gamma body'' in
    next := old_next;

    let gamma' =
      match body_typ with
      | Some body_typ -> (
        let body_tvars = tvars body_typ in
        let new_tvars = List.filter body_tvars ~f:(fun i -> i >= !next) in
        match new_tvars with
        | [] -> update gamma name (TBinding t)
        | new_tvars -> printf "New tvars: %s\n\n" (String.concat (List.map new_tvars ~f:(sprintf "%d")) ~sep:","); printf "\n\n LetIn inner typ: %s\n\n" (show_typ' body_typ); update gamma name (TForAll (new_tvars, body_typ))
      )
      |  None -> gamma (* TODO error handling *) in
    
    let (e2', e2t, cs2) = gather_expr gamma' delta rest in
    (Tast.LetIn (loc, name, e2t, body'', e2'), e2t, cs @ cs2)
  )
  | LetRecIn (loc, name, typ, body, rest) -> (
    let expr_typ = TVar (fresh_tvar ()) in
    let gamma' = update gamma name (TBinding expr_typ) in
    let (e1', e1t, cs1) = gather_expr gamma' delta body in
    let gamma' = update gamma name (TBinding e1t) in
    let (e2', e2t, cs2) = gather_expr gamma' delta rest in
    (Tast.LetRecIn (loc, name, e2t, e1', e2'), e2t, (e1t, expr_typ) :: cs1 @ cs2)
  ) 
  | Fun (loc, param, body) -> (
    let t = TVar (fresh_tvar ()) in
    printf "First t: %s\n" (show_typ' t); 
    let gamma' = update gamma param (TBinding t) in
    let (e', t', cs) = gather_expr gamma' delta body in
    (Tast.Fun (loc, param, TFun (t, t'), e'), TFun (t, t'), cs)
  )
  | Match (loc, e, (hm, he) :: branches) -> (
    (* let result_typ = TVar (fresh_tvar ()) in
    printf "Result_typ: %s\n" (show_typ' result_typ); *)
    let (match_expr, match_expr_typ, cs) = gather_expr gamma delta e in
    printf "Match_expr_typ: %s\n" (show_typ' match_expr_typ);

    let (mb', match_expr_typ', gamma', cs) = gather_mb gamma hm in
    let (e', result_typ, cs') = gather_expr gamma' delta he in
    printf "Match_expr_typ': %s\n" (show_typ' match_expr_typ');
    printf "Result_typ: %s\n" (show_typ' result_typ);

    let (mbs', cs') = List.fold branches ~f:(fun (mbs, css) (mb, e) -> (
      let (mb', t, gamma', cs) = gather_mb gamma mb in
      let (e', t', cs') = gather_expr gamma' delta e in
      let _ = match gamma' "h" with
      | Some (TBinding t) -> printf "h has typ %s\n" (show_typ' t)
      | _ -> printf "h not found\n"
       in
      printf "t: %s\n" (show_typ' t);
      printf "t': %s\n" (show_typ' t');
      printf "CS: %s\n" (cs |> List.map ~f:(fun (t, t') -> sprintf "(%s ==== %s)" (show_typ' t) (show_typ' t')) |> String.concat ~sep:", ");
      printf "CS': %s\n" (cs' |> List.map ~f:(fun (t, t') -> sprintf "(%s ==== %s)" (show_typ' t) (show_typ' t')) |> String.concat ~sep:", ");
      (mbs @ [(mb', e')], (t, match_expr_typ) :: (result_typ, t') :: cs @ cs' @ css)
    )) ~init:([], []) in
    (Tast.Match (loc, match_expr, match_expr_typ, mbs'), result_typ, (match_expr_typ, match_expr_typ') :: cs' @ cs)
  )
  | Match (loc, e, []) -> raise Not_found
  | App (loc, e, e') -> (
    let (e'', t', cs') = gather_expr gamma delta e' in
    let delta' = update_application delta t' in
    let (e1, t1, cs1) = gather_expr gamma delta' e in
    (App (loc, e1, (e'', t')), t1, cs1 @ cs')
    (**
    e e'
    e: t1
    e': t'
    e: t' -> ret_typ

    t1 = t' -> ret_typ

    substitute 
    *)
    (* (App (loc, e1, (e'', t')), ret_typ, (t1, Tast.TFun (t', ret_typ)) :: cs1 @ cs') *)
    (*
      e: A -> B -> C -> D
      es: A

      t: B -> C -> D
    *)
    (* let (e', et, cs) = gather_expr gamma delta e in
    let (es', ets, cs') = List.fold es ~f:(fun (es, ets, cs') e' -> (
      let (e', et, cs) = gather_expr gamma delta e' in
        (es @ [(e', et)], ets @ [et], cs @ cs')
    )) ~init:([], [], []) in
    (* The fact that function nodes are flat is causing a problem here -- a -> b cannot be unified with a -> b -> c where a is a -> b  *)
    let ret_typ = TVar (fresh_tvar ()) in
    (Tast.App (loc, e', es'), ret_typ, (et, Tast.TFun (ets @ [ret_typ])) :: cs @ cs') *)
  )
  | Seq (_, e, e') -> (
    (* TODO warn if typ e is not unit *)
    gather_expr gamma delta e'
  )

and gather_stmt (gamma: env) (s: stmt): Tast.stmt * env * constr list =
  match s with
  | Let (l, name, typ, body) -> (
    let body, t', cs = gather_expr gamma Empty body in
    (Let (l, name, t', body), update gamma name (TBinding t'), cs)
  )
  | LetRec (l, name, typ, body) -> (
    let stmt_typ = TVar (fresh_tvar ()) in
    let gamma' = update gamma name (TBinding stmt_typ) in
    let body, t', cs = gather_expr gamma' Empty body in
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

and gather_program (p: program): Tast.program * constr list =
  let (prog, _, cs) = List.fold p ~f:(fun (prog, gamma, acc) s -> (
    let (stmt, gamma', cs) = gather_stmt gamma s in
    (prog @ [stmt], gamma', cs @ acc)
  )) ~init:([], empty_env, []) in
  (prog, cs)

(* Unification *)

and normalize_tvars (gamma': env'): env' =
  let rec normalize_tvar (tau: tvarenv) (ti: int) (t: typ' option): tvarenv * int * typ' option =
    match t with
    | Some (TVar x) -> (
      match tau x with
      | Some x' -> (tau, ti, Some (TVar x'))
      | None -> (update tau x ti, ti + 1, Some (TVar ti))
    )
    | Some (TFun (t, t')) -> (
      let (tau', ti', topt') = normalize_tvar tau ti (Some t) in
      let (tau'', ti'', topt'') = normalize_tvar tau' ti' (Some t') in
      match topt', topt'' with
      | Some t, Some t' -> (tau'', ti'', Some (TFun (t, t')))
      | Some t, None -> (tau'', ti'', Some (TFun (t, t')))
      | None, Some t' -> (tau'', ti'', Some (TFun (t, t')))
      | None, None -> (tau'', ti'', Some (TFun (t, t')))
      )
    | Some (TTuple (t, t')) -> (
      let (tau', ti', topt') = normalize_tvar tau ti (Some t) in
      let (tau'', ti'', topt'') = normalize_tvar tau' ti' (Some t') in
      match topt', topt'' with
      | Some t, Some t' -> (tau'', ti'', Some (TTuple (t, t')))
      | Some t, None -> (tau'', ti'', Some (TTuple (t, t')))
      | None, Some t' -> (tau'', ti'', Some (TTuple (t, t')))
      | None, None -> (tau'', ti'', Some (TTuple (t, t')))
      )
    | Some (TList t') -> (
      match normalize_tvar tau ti (Some t') with
      | (tau', ti', Some t'') -> (tau', ti', Some (TList (t'')))
      | _ -> (tau, ti, Some (TList t'))
    )
    | Some (TSecret t') -> (
      match normalize_tvar tau ti (Some t') with
      | (tau', ti', Some t'') -> (tau', ti', Some (TSecret (t'')))
      | _ -> (tau, ti, Some (TSecret t'))
    )
    | Some (TPublic t') -> (
      match normalize_tvar tau ti (Some t') with
      | (tau', ti', Some t'') -> (tau', ti', Some (TPublic (t'')))
      | _ -> (tau, ti, Some (TPublic t'))
    )
    | _ -> (tau, ti, t)
  in
  let maybe_update (gamma: env') (tvar: int) (t: typ' option) =
    match t with
    | Some t' -> update gamma tvar t'
    | None -> gamma
  in
  Array.create 0 ~len:!next
  |> Array.foldi ~init:(empty_env', empty_tvarenv, 1) ~f:( fun x (gamma'', tau, ti) _ -> (
    let t' = gamma' x in
    let (tau', ti', t'') = normalize_tvar tau ti t' in
    (maybe_update gamma'' x t'', tau', ti')
  ))
  |> fst3

and tvars (t: typ'): int list =
   match t with
  | TFun (t, t') -> (tvars t) @ (tvars t')
  | TTuple (t, t') -> (tvars t) @ (tvars t')
  | TList t -> tvars t
  | TSecret t -> tvars t
  | TPublic t -> tvars t
  | TVar x -> [x]
  | _ -> []

and apply_env' (gamma: env') (t: typ'): typ' =
  match t with
  | TVar x -> (
    match gamma x with
    | Some t -> t
    | None -> TVar x
  )
  | TFun (t, t') -> TFun (apply_env' gamma t, apply_env' gamma t')
  | TTuple (t, t') -> TTuple (apply_env' gamma t, apply_env' gamma t')
  | TList t -> printf "Applying env' to list: %s\n" (show_typ' t); printf "RR: %s\n" (show_typ' (apply_env' gamma t)); printf "Result: %s\n" (show_typ' (TList (apply_env' gamma t))); TList (apply_env' gamma t)
  | TSecret t -> TSecret (apply_env' gamma t)
  | TPublic t -> TPublic (apply_env' gamma t)
  | t' -> t'

and subst_all (gamma: env') (cs: constr list) =
  List.map cs ~f:(fun (t, t') -> (apply_env' gamma t, apply_env' gamma t'))

and _unify (gamma: env') (cs: constr list): env' =
  match cs with
  | c :: rest -> (
    match c with
    | t, t' when t = t' -> _unify gamma rest
    | TVar x, t' when not (List.exists (tvars t') ~f:(fun t -> t = x)) -> (
      let gamma' = update gamma x t' in
      let rest' = subst_all gamma' rest in
      merge' gamma' (_unify gamma' rest')
    )
    | t, TVar x when not (List.exists (tvars t) ~f:(fun t -> t = x)) -> (
      let gamma' = update gamma x t in
      let rest' = subst_all gamma' rest in
      merge' gamma' (_unify gamma' rest')
    )
    | TFun (t1, t1'), TFun (t2, t2') -> _unify gamma ((t1, t2) :: (t1', t2') :: cs)
    | TTuple (t1, t1'), TTuple (t2, t2') -> _unify gamma ((t1, t2) :: (t1', t2') :: cs)
    | a, b -> printf "Failed: %s = %s\n" (show_typ' a) (show_typ' b); raise Not_found; _unify gamma rest (* TODO error handling *)
  )
  | [] -> gamma

and unify (cs: constr list): env' = _unify empty_env' cs

and subst_expr (gamma: env') (e: Tast.expr): Tast.expr =
  match (e : Tast.expr) with
  | L (loc, l) -> L (loc, l)
  | C (loc, c) -> C  (loc, c)
  | Var (loc, name) -> Var (loc, name)
  | LetIn (loc, name, typ, body, rest) -> LetIn (loc, name, apply_env' gamma typ, subst_expr gamma body, subst_expr gamma rest)
  | LetRecIn (loc, name, typ, body, rest) -> LetRecIn (loc, name, apply_env' gamma typ, subst_expr gamma body, subst_expr gamma rest)
  | Fun (loc, params, typ, body) -> Fun (loc, params, apply_env' gamma typ, subst_expr gamma body)
  | Match (loc, e, typ, mbs) -> Match (loc, subst_expr gamma e, apply_env' gamma typ, List.map mbs ~f:(fun (mb, e) -> (mb, subst_expr gamma e)))
  | App (loc, e, (e', t)) -> App (loc, subst_expr gamma e, (subst_expr gamma e', apply_env' gamma t))
  | Seq (loc, e, e') -> Seq (loc, subst_expr gamma e, subst_expr gamma e')

and subst_stmt (gamma: env') (s: Tast.stmt): Tast.stmt =
  match s with
  | Let (loc, name, typ, body) -> Let (loc, name, apply_env' gamma typ, subst_expr gamma body)
  | LetRec (loc, name, typ, body) -> LetRec (loc, name, apply_env' gamma typ, subst_expr gamma body)
  | Type (loc, name, constructors) -> Type (loc, name, constructors)

and subst_program (gamma: env') (p: Tast.program): Tast.program =
  List.map p ~f:(subst_stmt gamma)

and lit_to_typ' (l: Tast.literal): typ' =
  match l with
  | Unit -> TUnit
  | Int _ -> TInt
  | Float _ -> TFloat
  | String _ -> TString
  | Char _ -> TChar
  | Bool _ -> TBool

and complex_to_typ' (gamma: env) (c: Tast.complex): typ' option =
  match c with
  | Tuple (e, e') -> (
    let t = expr_typ gamma e in
    let t' = expr_typ gamma e' in
    match t, t' with
    | Some t, Some t' -> Some (Tast.TTuple (t, t'))
    | _, _ -> None
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
  | Nil -> raise Not_found; Some (Tast.TList (Tast.TVar 1)) (* ? *)
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
  printf "Expr typ: %s\n" (Tast.show_expr e);
  match e with
  | L (_, l) -> Some (lit_to_typ' l) 
  | C (_, c) -> complex_to_typ' gamma c
  | Var (_, name) -> (
    match gamma name with
    | Some (TBinding t) -> printf "Found type of %s: %s\n" name (show_typ' t); Some t
    | _ -> printf "Type of %s not found\n" name; None (* TODO error handling *)
  )
  | LetIn (_, _, t, _, _) -> printf "Type of letin is %s\n" (show_typ' t); Some t (* Type of binding, not resulting expression *)
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
  | App (_, e, (e', typ)) -> ( (*  *)
    let t = expr_typ gamma e in
    match t with
    | Some (TFun (t, t')) when t = typ -> Some t'
    | Some (TFun (t, t')) -> (
      match t with
      | TVar tid -> Some (specialize t tid typ)
      | _ -> printf "Unable to specialize function\n"; None (* TODO error handling *)
    )
    | _ -> None
  )
  | Seq (_, e, e') -> expr_typ gamma e'

open Pretty

let infer_expr (gamma: env) (e: Ast.expr): Tast.expr =
  let e, t, cs = gather_expr gamma Empty e in
  printf "Inferred type: %s\n" (show_typ' t);
  printf "Constraints: %s\n" (cs |> List.map ~f:(fun (t, t') -> sprintf "(%s == %s)\n" (show_typ' t) (show_typ' t')) |> String.concat ~sep:",");
  let gamma = unify cs in
  next := 0;
  let show_opt t = 
    match t with
    | Some t -> show_typ' t
    | None -> "None"
  in
  printf "TVar %d --> %s\n" 1 (show_opt (gamma 1));
  printf "TVar %d --> %s\n" 2 (show_opt (gamma 2));
  printf "TVar %d --> %s\n" 3 (show_opt (gamma 3));
  printf "TVar %d --> %s\n" 4 (show_opt (gamma 4));
  printf "TVar %d --> %s\n" 5 (show_opt (gamma 5));
  printf "TVar %d --> %s\n" 6 (show_opt (gamma 6));
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