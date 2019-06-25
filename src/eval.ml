open Tast
open Pretty
open Core

let vars = ref []

type value =
  | Int of int
  | Float of float
  | Bool of bool
  | Char of char
  | String of string
  | Unit
  | Fun of state * typ' list * string list * expr
  | Tuple of value list
  | Record of string * value list
  | Nil
  | Cons of value * value
  | Error of string
  | Rec
[@@deriving show]

and state = string -> value option
let empty_state = fun _ -> None
let update st n v =
  vars := n :: !vars;
  fun n' -> if n' = n then v else st n'

let dump st =
  printf "-----------------------------\n";
  List.iter !vars ~f:(fun name -> (
    match st name with
    | Some v -> printf "| %s:  %s\n" name (show_value v)
    | None -> printf "| %s: None\n" name
  ));
  printf "-----------------------------\n"

(* let rec mangle_mfun (params: typ' list): string list =
  match params with
  | TVar _ -> 
  | h :: t -> ""
  | [] -> "" *)
let mangle_mfun (name: string) (params: typ' list): string =
  name ^ (String.concat (List.map params ~f:show_typ'))
  (* name ^  "" (mangle_mfun params) *)

let rec pretty_value (v: value): string =
  match v with
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Bool b -> string_of_bool b
  | Char c -> (sprintf "%c" c)
  | String s -> (sprintf "\"%s\"" s)
  | Unit -> "()"
  | Fun (state, typs, params, expr) -> (sprintf "fun %s -> %s" (String.concat params ~sep:" ") (pretty_expr expr))
  | Tuple vs -> (sprintf "(%s)" (String.concat (List.map vs ~f:pretty_value) ~sep:", "))
  | Record (name, vs) -> (sprintf "%s(%s)" name (String.concat (List.map vs ~f:pretty_value) ~sep:", "))
  | Nil -> "[]"
  | Cons (v, v') -> (sprintf "%s::%s" (pretty_value v) (pretty_value v'))
  | Error e -> e
  | Rec -> "rec"

type context =
  | Empty
  | Application of typ' list
[@@deriving show]

let rec compare (v: value) (v': value) (inv: bool): bool =
  let res = match v, v' with
  | Int i, Int i' -> i = i'
  | Float f, Float f' -> f = f'
  | Char c, Char c' -> c = c'
  | String s, String s' -> s = s'
  | Unit, Unit -> true
  (* All functions are distinct *)
  | Fun (_, _, _, _), Fun (_, _, _, _) -> false
  | Tuple vs, Tuple vs' -> (
    match List.zip vs vs' with
    | None -> false
    | Some vss -> List.fold vss ~f:(fun acc (v, v') -> acc && (compare v v' false)) ~init:true
  )
  | Record (c, vs), Record (c', vs') -> (
    match c = c' with
    | false -> false
    | true -> (
      match List.zip vs vs' with
      | None -> false
      | Some vss -> List.fold vss ~f:(fun acc (v, v') -> acc && (compare v v' false)) ~init:true
    )
  )
  | Nil, Nil -> true
  | Cons (v1, v1'), Cons (v2, v2') -> (compare v1 v2 false) && (compare v1' v2' false)
  | _, _ -> false
  in
  if inv then not res else res

let bool_and (v: value) (v': value): value =
  match v, v' with
  | Bool v, Bool v' -> Bool (v && v')
  | _, _ -> Error (sprintf "Cannot take a boolean and of %s and %s" (show_value v) (show_value v'))

let bool_or (v: value) (v': value): value =
  match v, v' with
  | Bool v, Bool v' -> Bool (v && v')
  | _, _ -> Error (sprintf "Cannot take a boolean or of %s and %s" (show_value v) (show_value v'))

let rec concat_lists (v: value) (v': value): value =
  match v with
  | Nil -> v'
  | Cons (e, e') -> Cons (e, (concat_lists e' v'))
  | v -> Error (sprintf "Cannot concatenate %s to a list" (show_value v))

let eval_literal (l: literal): value =
  match l with
  | Unit -> Unit
  | Int i -> Int i
  | Float f -> Float f
  | String s -> String s
  | Char c -> Char c
  | Bool b -> Bool b

let rec eval_complex (st: state) (omega: context) (c: complex): value =
  match c with
  | Tuple es -> Tuple (List.map es ~f:(eval_expr st omega))
  | Record (name, es) -> Record (name, List.map es ~f:(eval_expr st omega))
  | Nil -> Nil
  | Cons (e, e') -> Cons (eval_expr st omega e, eval_expr st omega e')

and match_value (v: value) (mb: match_branch) (st: state): (state, unit) result =
  match mb with
  | ML (_, lit) -> (
    match v, lit with
    | Unit, Unit -> Ok st
    | Int i, Int i' when i = i' -> Ok st
    | Float f, Float f' when f = f' -> Ok st
    | String s, String s' when s = s' -> Ok st
    | Char c, Char c' when c = c' -> Ok st
    | Bool b, Bool b' when b = b' -> Ok st
    | _ -> Error ()
  )
  | MVar (_, name) -> Ok (update st name (Some v))
  | Blank _ -> Ok st
  | MTuple (_, mbs') -> (
    match v with
    | Tuple vs -> (
      let vmbs = List.zip vs mbs' in
      match vmbs with
      | None -> Error ()
      | Some vmbs -> (
        List.fold vmbs ~f:(fun acc (v, mb) -> (
          match acc with
          | Error _ -> Error ()
          | Ok st -> match_value v mb st
        )) ~init:(Ok st)
      )
    )
    | _ -> Error ()
  )
  | MNil _ -> (
    match v with
    | Nil -> Ok st
    | _ -> Error ()
  )
  | MCons (_, mb, mb') -> (
    match v with
    | Cons (v, v') -> (
      match match_value v mb st with
      | Error _ -> Error ()
      | Ok st' -> match_value v' mb' st'
    )
    | _ -> Error ()
  )
  | MRecord (_, name, mbs') -> (
    match v with
    | Record (name', vs) when name = name' -> (
      let vmbs = List.zip vs mbs' in
      match vmbs with
      | Some vmbs -> (
        List.fold vmbs ~f:(fun acc (v, mb) -> (
          match acc with
          | Error _ -> Error ()
          | Ok st -> match_value v mb st
        )) ~init:(Ok st)
      )
      | None -> Error () 
    )
    | _ -> Error ()
  )

and eval_match_expr (st: state) (omega: context) (v: value) (mbs: (match_branch * expr) list): value =
  match mbs with
  | [] -> Error "No match found, please add exhaustiveness checks"
  | (h, e) :: t -> (
    match match_value v h st with
    | Error _ -> eval_match_expr st omega v t
    | Ok st' -> eval_expr st' omega e
  )

and eval_expr (st: state) (omega: context) (e: expr): value =
  match e with
  | L (_, l) -> eval_literal l
  | C (_, c) -> eval_complex st omega c
  | Var (l, name) -> (
    let full_name = match omega with
    | Empty -> name
    | Application t' -> mangle_mfun name t'
    in
    match st full_name with
    | None -> Error (sprintf "Line %d: name %s not found" l name)
    | Some v -> v
  )
  | LetIn (l, name, _, e, rest) -> (
    let v = eval_expr st omega e in
    let st' = update st name (Some v) in
    eval_expr st' omega rest
  )
  | LetRecIn (l, name, _, e, rest) -> (
    let st' = update st name (Some Rec) in
    let v = eval_expr st' omega e in
    let st' = update st name (Some v) in
    eval_expr st' omega rest
  )
  | Fun (l, params, t, body) -> (
    match t with
    | TFun typs -> Fun (st, typs, params, body)
    | _ -> Error (sprintf "Line %d: function type mismatch" l)
  )
  | Match (l, e, t, branches) -> (
    let v = eval_expr st omega e in
    eval_match_expr st omega v branches
  )
  | App (l, e, pparams) -> (
    let (pparams, typs) = List.unzip pparams in

    let find_fun (typs: typ' list): (state * string list * expr) option =
      let omega' = Application typs in
      let f = eval_expr st omega' e in
      match f with
      | Fun (st', typs, params, body) -> Some (st', params, body)
      | s -> None
    in

    let apply_fun (st: state) (params: string list) (body: expr): value =
      let vs = List.map pparams ~f:(eval_expr st omega) in
      let v_names = List.zip params vs in
      match v_names with
      | None -> Error (sprintf "Line %d: incorrect number of function arguments supplied (expected %d)" l (List.length params))
      | Some v_names -> (
        let st' = List.fold v_names ~f:(fun st (name, v) -> update st name (Some v)) ~init:st in
        eval_expr st' omega body
      )
    in

    (* TODO expand multifunction in new variable bindings *)

    let rec try_subtyps (typs: typ' list): ((state * string list * expr) * typ' list) option =
      match typs, find_fun typs with
      | _, Some f -> Some (f, typs)
      | [], None -> None
      | _, None -> try_subtyps (List.filteri typs ~f:(fun i _ -> i <> (List.length typs - 1)))
    in
    match try_subtyps typs with
    | Some ((st, p, e), subtyps) when List.length subtyps = List.length typs -> printf "Found perfect match!\n"; apply_fun st p e
    | Some (_, subtyps) -> (
      printf "Found partial match: %s\n" (String.concat (List.map subtyps ~f:pretty_typ'));
      let rec try_grow_subtyps (typs: typ' list) (supplied_typs: typ' list) =
        match List.nth supplied_typs (List.length typs) with
        | Some next_supplied_typ -> (
          let next_supplied_typs = typs @ [next_supplied_typ] in
          printf "%s; %s\n" (String.concat (List.map next_supplied_typs ~f:pretty_typ')) (String.concat (List.map supplied_typs ~f:pretty_typ'));
          match find_fun next_supplied_typs with
          | Some f when List.length next_supplied_typs = List.length supplied_typs -> Some f
          | Some f -> printf "Found next match with actual param: %s\n" (String.concat (List.map next_supplied_typs ~f:pretty_typ')); try_grow_subtyps next_supplied_typs supplied_typs
          | None -> (
            let next_any_typs = typs @ [TAny] in
            printf "%s; %s\n" (String.concat (List.map next_any_typs ~f:pretty_typ')) (String.concat (List.map supplied_typs ~f:pretty_typ'));
            match find_fun next_any_typs with
            | Some f when List.length next_supplied_typs = List.length supplied_typs -> Some f
            | Some f -> printf "Found next match with TAny: %s\n" (String.concat (List.map next_supplied_typs ~f:pretty_typ')); try_grow_subtyps next_any_typs supplied_typs
            | None -> None
          )
        )
        | None -> None
      in
      match try_grow_subtyps subtyps typs with
      | Some (st, p, e) -> apply_fun st p e
      | None -> Error "Could not find suitable function for supplied parameters"
    )
    | None -> Error "Could not find suitable function for supplied parameters"
  ) 
  | Seq (l, e, e') -> (
    eval_expr st omega e;
    eval_expr st omega e'
  )

(*


How do super-functions apply to AST nodes besides functions?

- Simple and Complex literals -- N/A
- Variables -- depends on what is bound to variable
- LetIn -- depends on rest expression
- LetRecIn -- depends on rest expression
- Fun -- duh
- Match -- N/A
- App -- depends on result of application
- Seq -- depends on rest expression


*)

let rec update_partials (st: state) (name: string) (l: loc) (v: value) (applied_typs: typ' list) (applied_params: string list): state =
  match v with
  | Fun (st', last_typ :: return_typ, last_param :: [], e) -> (
    (* Fully applied --> do not generate another curry *)
    let new_applied_typs = applied_typs @ [last_typ] in
    let new_applied_params = applied_params @ [last_param] in
    let full = Fun (st', new_applied_typs @ return_typ, new_applied_params, e) in
    update st (mangle_mfun name new_applied_typs) (Some full)
  )
  | Fun (st', t :: restt, param :: rest, e) -> (
    let new_applied_typs = applied_typs @ [t] in
    let new_applied_params = applied_params @ [param] in


    (* Given new_applied_typs, new_applied_params, return function that takes those and returns curry of rest *)
    (* This is not viable since anything can return a function which may be a multifunction?
       No! Multi-functions only matter in shadowing -- but the expansion to multi-functions must be extended to every AST node on the RHS of let-bindings *)

    let next = Tast.Fun (l, rest, TFun restt, e) in

    let curried = Fun (st', new_applied_typs @ restt, new_applied_params, next) in

    let st' = update st (mangle_mfun name new_applied_typs) (Some curried) in

    update_partials st' name l (Fun (st', restt, rest, e)) new_applied_typs new_applied_params
  )
  | _ -> st
let update_partials (st: state) (name: string) (l: loc) (v: value): state = update_partials st name l v [] []

let rec expand_partials (st: state) (name: string) (l: loc) (e: expr): state =
  let maybe_update_partials (e: expr): state =
    match eval_expr st Empty e with
    | Fun (st', typs, params, body) -> update_partials st name l (Fun (st', typs, params, body))
    | _ -> st
  in
  match e with
  | Var (_, _) -> maybe_update_partials e
  | LetIn (_, _, _, _, _) -> maybe_update_partials e
  | LetRecIn (_, _, _, _, _) -> maybe_update_partials e
  | Fun (_, _, _, _) -> maybe_update_partials e
  | App (_, _, _) -> maybe_update_partials e
  | Seq (_, _, _) ->maybe_update_partials e
  | _ -> st


(* let rec update_partials (st: state) (name: string) (e: expr) (applied_typs: typ' list) (applied_params: string list): state =
  match e with
  | Fun (l, last_param :: [], TFun (last_typ :: return_typ), e) -> (
    (* Fully applied --> do not generate another curry *)
    let new_applied_typs = applied_typs @ [last_typ] in
    let new_applied_params = applied_params @ [last_param] in
    let full = Tast.Fun (l, new_applied_params, TFun (new_applied_typs @ return_typ), e) in
    update st (mangle_mfun name new_applied_typs) (Some (eval_expr st Empty full))
  )
  | Fun (l, param :: rest, TFun (t :: restt), e) -> (
    let new_applied_typs = applied_typs @ [t] in
    let new_applied_params = applied_params @ [param] in


    (* Given new_applied_typs, new_applied_params, return function that takes those and returns curry of rest *)
    (* This is not viable since anything can return a function which may be a multifunction?
       No! Multi-functions only matter in shadowing -- but the expansion to multi-functions must be extended to every AST node on the RHS of let-bindings *)

    let next = Tast.Fun (l, rest, TFun restt, e) in


    let curried = Tast.Fun (l, new_applied_params, TFun (new_applied_typs @ [TFun restt]), next) in

    let st' = update st (mangle_mfun name new_applied_typs) (Some (eval_expr st Empty curried)) in

    update_partials st' name next new_applied_typs new_applied_params
  )
  | _ -> st
let update_partials (st: state) (name: string) (e: expr): state = update_partials st name e [] [] *)

let eval_stmt (st: state) (s: stmt): state =
  match s with
  | Let (l, name, t, e) -> (
    let st' = update st name (Some (eval_expr st Empty e)) in
    expand_partials st' name l e
  )
  | LetRec (l, name, _, e) -> (
    let st' = update st name (Some Rec) in
    update st name (Some (eval_expr st' Empty e))
  )
  | _ -> st

let eval_program (p: program): state =
  List.fold p ~f:(fun st s -> eval_stmt st s) ~init:empty_state