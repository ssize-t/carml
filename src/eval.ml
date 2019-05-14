open Ast
open Core

type value =
  | Int of int
  | Float of float
  | Bool of bool
  | Char of char
  | String of string
  | Unit
  | Fun of string list * expr
  | Tuple of value list
  | Record of string * value list
  | Nil
  | Cons of value * value
  | Error of string
  | Rec
[@@deriving show]

type state = string -> value option
let empty_state = fun _ -> None
let update st n v =
  fun n' -> if n' = n then v else st n'

let rec compare (v: value) (v': value) (inv: bool): bool =
  let res = match v, v' with
  | Int i, Int i' -> i = i'
  | Float f, Float f' -> f = f'
  | Char c, Char c' -> c = c'
  | String s, String s' -> s = s'
  | Unit, Unit -> true
  (* All functions are distinct *)
  | Fun (_, _), Fun (_, _) -> false
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

let rec eval_complex (st: state) (c: complex): value =
  match c with
  | Tuple es -> Tuple (List.map es ~f:(eval_expr st))
  | Record (name, es) -> Record (name, List.map es ~f:(eval_expr st))
  | Nil -> Nil
  | Cons (e, e') -> Cons (eval_expr st e, eval_expr st e')

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

and eval_match_expr (st: state) (v: value) (mbs: (match_branch * expr) list): value =
  match mbs with
  | [] -> Error "No match found, please add exhaustiveness checks"
  | (h, e) :: t -> (
    match match_value v h st with
    | Error _ -> eval_match_expr st v t
    | Ok st' -> eval_expr st' e
  )

and eval_expr (st: state) (e: expr): value =
  match e with
  | L (_, l) -> eval_literal l
  | C (_, c) -> eval_complex st c
  | UnOp (Loc (l, _), op, e) -> (
    let v = eval_expr st e in
    match v with
    | Bool b -> (
      match op with
      | Not -> Bool (not b)
    )
    | v' -> Error (sprintf "Line %d: expected boolean value, found %s" l (show_value v'))
  )
  | BinOp (Loc (l, _), op, e, e') -> (
    let v = eval_expr st e in
    let v' = eval_expr st e' in
    match op with
    | Eq -> Bool (compare v v' false)
    | Neq -> Bool (compare v v' true)
    | And -> bool_and v v'
    | Or -> bool_or v v'
    | op' -> (
      let v', v'' = match v, v' with
      (*
        Comparing floats and ints is supported,
        ints are coerced to floats
      *)
      | Int i, Int i' -> (Int i, Int i')
      | Float f, Float f' -> (Float f, Float f')
      | Int i, Float f -> (Float (float_of_int i), Float f)
      | Float f, Int i -> (Float f, Float (float_of_int i))
      | v, v' -> (Error (
            sprintf "Line: %d: cannot compare %s and %s" l (show_value v) (show_value v')
          ), Error (
            sprintf "Line: %d: cannot compare %s and %s" l (show_value v) (show_value v')
          )
        )
      in
      match v', v'' with
      | Int i, Int i' -> (
        match op with
        | Lt -> Bool (i < i')
        | Lte -> Bool (i <= i')
        | Gt -> Bool (i > i')
        | Gte -> Bool (i >= i')
        | op' -> Error "Shouldn't happen"
      )
      | Float f, Float f' -> (
        match op with
        | Lt -> Bool (f < f')
        | Lte -> Bool (f <= f')
        | Gt -> Bool (f > f')
        | Gte -> Bool (f >= f')
        | op' -> Error "shouldn't happen"
      )
      | e, _ -> e
      )
  )
  | NumOp (Loc (l, _), op, e, e') -> (
    let v = eval_expr st e in
    let v' = eval_expr st e' in
    let v', v'' = match v, v' with
      (*
        Numerical operations between floats and ints are supported,
        ints are coerced to floats
      *)
      | Int i, Int i' -> (Int i, Int i')
      | Float f, Float f' -> (Float f, Float f')
      | Int i, Float f -> (Float (float_of_int i), Float f)
      | Float f, Int i -> (Float f, Float (float_of_int i))
      | v, v' -> (Error (
          sprintf "Line: %d: cannot compare %s and %s" l (show_value v) (show_value v')
        ), Error (
          sprintf "Line: %d: cannot compare %s and %s" l (show_value v) (show_value v')
        )
      )
    in
    match v', v'' with
    | Int i, Int i' -> (
      match op with
      | Sub -> Int (i - i')
      | Add -> Int (i + i')
      | Mult -> Int (i * i')
      | Div -> Int (i / i')
    )
    | Float f, Float f' -> (
      match op with
      | Sub -> Float (f -. f')
      | Add -> Float (f +. f')
      | Mult -> Float (f *. f')
      | Div -> Float (f /. f')
    )
    | e, _ -> e
  ) 
  | ListOp (Loc (l, _), op, e, e') -> (
    let v = eval_expr st e in
    let v' = eval_expr st e' in
    let v', v'' = match v, v' with
    | Nil, v' -> (
      match v' with
      | Nil -> (Nil,  Nil)
      | Cons (v, v') -> (Nil, Cons (v, v'))
      | v' -> (
        Error (sprintf "Line %d: expected list, found %s" l (show_value v')),
        Error (sprintf "Line %d: expected list, found %s" l (show_value v'))
      )
    )
    | Cons (v, v''), v' -> (
      match v' with
      | Nil -> (Cons (v, v''), Nil)
      | Cons (v'', v''') -> (Cons (v, v''), Cons (v'', v'''))
      | v' -> (
        Error (sprintf "Line %d: expected list, found %s" l (show_value v')),
        Error (sprintf "Line %d: expected list, found %s" l (show_value v'))
      )
    )
    | v', _ -> (
      Error (sprintf "Line %d: expected list, found %s" l (show_value v')),
      Error (sprintf "Line %d: expected list, found %s" l (show_value v'))
    )
    in
    match v', v'' with
    | Error msg, _ -> Error msg
    | l, l' -> (
      match op with
      | Concat -> concat_lists l l'
    )
  )
  | Var (Loc (l, _), name) -> (
    match st name with
    | None -> Error (sprintf "Line %d: name %s not found" l name)
    | Some v -> v
  )
  | LetIn (Loc (l, _), name, _, e, rest) -> (
    let v = eval_expr st e in
    let st' = update st name (Some v) in
    eval_expr st' rest
  )
  | LetRecIn (Loc (l, _), name, _, e, rest) -> (
    let st' = update st name (Some Rec) in
    let v = eval_expr st' e in
    let st' = update st name (Some v) in
    eval_expr st' rest
  )
  | Fun (Loc (l, _), params, _, body) -> Fun (params, body)
  | Match (Loc (l, _), e, _, branches) -> (
    let v = eval_expr st e in
    eval_match_expr st v branches
  )
  | App (Loc (l, _), e, ets) -> (
    let f = eval_expr st e in
    match f with
    | Fun (params, body) -> (
      let pparams, _ = List.unzip ets in
      let vs = List.map pparams ~f:(eval_expr st) in
      let v_names = List.zip params vs in
      match v_names with
      | None -> Error (sprintf "Line %d: incorrect number of function arguments supplied (expected %d)" l (List.length params))
      | Some v_names -> (
        let st' = List.fold v_names ~f:(fun st (name, v) -> update st name (Some v)) ~init:st in
        eval_expr st' body
      )
    )
    | _ -> Error (sprintf "Line %d: the left hand side expression of an application is not a function" l)
  ) 
  | Seq (Loc (l, _), e, e') -> (
    eval_expr st e;
    eval_expr st e'
  )

let eval_stmt (st: state) (s: stmt): state =
  match s with
  | Let (l, name, _, e) -> update st name (Some (eval_expr st e))
  | LetRec (l, name, _, e) -> (
    let st' = update st name (Some Rec) in
    update st name (Some (eval_expr st' e))
  )
  | _ -> st

let eval_program (p: program): state =
  List.fold p ~f:(fun st s -> eval_stmt st s) ~init:empty_state