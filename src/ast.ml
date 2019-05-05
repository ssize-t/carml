open Core

type loc = int
[@@deriving show]

type typ =
  | TInt of loc
  | TFloat of loc
  | TBool of loc
  | TString of loc
  | TChar of loc
  | TUnit of loc
  | TFun of loc * typ list
  | TTuple of loc * typ list
  | TRecord of loc * string
  | TList of loc * typ
  | TSecret of loc * typ
  | TPublic of loc * typ
[@@deriving show]

let rec pretty_typ (t: typ): string =
  match t with
  | TInt _ -> "int"
  | TFloat _ -> "float"
  | TBool _ -> "bool"
  | TChar _ -> "char"
  | TUnit _ -> "unit"
  | TString _ -> "string"
  | TFun (_, typs) -> String.concat (List.map typs ~f:pretty_typ) ~sep:" -> "
  | TTuple (_, typs) -> String.concat (List.map typs ~f:pretty_typ) ~sep:" * "
  | TRecord (_, constructor) -> constructor
  | TList (_, t') -> sprintf "%s list" (pretty_typ t')
  | TSecret (_, t') -> sprintf "secret(%s)" (pretty_typ t')
  | TPublic (_, t') -> sprintf "public(%s)" (pretty_typ t')
  

type binop = Lt | Lte | Eq | Gt | Gte | Neq | And | Or
[@@deriving show]

type unop = Not
[@@deriving show]

type numop = Sub | Add | Mult | Div
[@@deriving show]

type listop = Concat
[@@deriving show]

let rec pretty_list_op (op: listop): string =
  match op with
  | Concat -> "@"

type expr =
  | L of loc * literal
  | C of loc * complex
  | UnOp of loc * unop * expr
  | BinOp of loc * binop * expr * expr
  | NumOp of loc * numop * expr * expr
  | ListOp of loc * listop * expr * expr
  | Var of loc * string
  | LetIn of loc * string * typ * expr * expr
  | LetRecIn of loc * string * typ * expr * expr
  | Fun of loc * string list * typ * expr
  | Match of loc * expr * typ * (match_branch * expr) list
  | App of loc * expr * (expr * typ) list
  | Seq of loc * expr * expr

and literal =
  (* Simple literals *)
  | Unit
  | Int of int
  | Float of float
  | String of string
  | Char of char
  | Bool of bool

and complex =
  | Tuple of expr list
  | Record of string * expr list
  | Nil
  | Cons of expr * expr

and match_branch =
  | ML of loc * literal
  | MVar of loc * string
  | Blank of loc
  (* Simplify complex types for binding *)
  | MTuple of loc * match_branch list
  | MRecord of loc * string * match_branch list
  | MNil of loc
  | MCons of loc * match_branch * match_branch
[@@deriving show]

let pretty_lit (l: literal): string =
  match l with
  | Unit -> "()"
  | Int i -> sprintf "%d" i
  | Float f -> sprintf "%f" f
  | String s -> sprintf "\"%s\"" s
  | Char c -> sprintf "'%c'" c
  | Bool b -> if b then "true" else "false"

let rec pretty_branch (mb: match_branch): string =
  match mb with
  | ML (_, l) -> pretty_lit l
  | MVar (_, s) -> s
  | Blank _ -> "_"
  | MTuple (_, mbs) -> sprintf "(%s)" (String.concat (List.map mbs ~f:pretty_branch) ~sep:",")
  | MRecord (_, constructor, mbs) -> sprintf "%s(%s)" constructor (String.concat (List.map mbs ~f:pretty_branch) ~sep:",")
  | MNil _ -> "[]"
  | MCons (_, mb, mb') -> sprintf "%s::%s" (pretty_branch mb) (pretty_branch mb')

let rec pretty_comp (c: complex): string =
  match c with
  | Tuple es -> (
    let ses = String.concat (List.map es ~f:pretty_expr) ~sep:"," in
    sprintf "(%s)" ses
  )
  | Record (constructor, params) -> (
    let sparams = List.fold params ~f:(fun acc e -> (
      let se = pretty_expr e in
      acc ^ sprintf ", %s" se
    )) ~init:"" in
    sprintf "%s(%s)" constructor sparams
  )
  | Nil -> "[]"
  | Cons (e1, e2) -> (
    let se1 = pretty_expr e1 in
    let se2 = pretty_expr e2 in
    sprintf "%s::%s" se1 se2
  )
and pretty_expr (e: expr): string =
  match e with
  | L (_, l') -> pretty_lit l'
  | C (_, c') -> pretty_comp c'
  | UnOp (_, op, e') -> (
    match op with
    | Not -> sprintf "!(%s)" (pretty_expr e')
  )
  | BinOp (_, op, e1', e2') -> (
    let se1' = pretty_expr e1' in
    let se2' = pretty_expr e2' in
    let sop = match op with
    | Lt -> "<"
    | Lte -> "<="
    | Eq -> "="
    | Gt -> ">"
    | Gte -> ">="
    | Neq -> "!="
    | And -> "&&"
    | Or -> "||" in
    sprintf "%s %s %s" se1' sop se2'
  )
  | NumOp (_, op, e1', e2') -> (
    let se1' = pretty_expr e1' in
    let se2' = pretty_expr e2' in
    let sop = match op with
    | Sub -> "-"
    | Add -> "+"
    | Mult -> "*"
    | Div -> "/" in
    sprintf "%s %s %s" se1' sop se2'
  )
  | ListOp (_, op, e1', e2') -> (
    let se1' = pretty_expr e1' in
    let se2' = pretty_expr e2' in
    let sop = match op with
    | Concat -> "@" in
    sprintf "%s %s %s" se1' sop se2'
  )
  | Var (_, name) -> name
  | LetIn (_, name, typ, e1', e2') -> (
    let styp = pretty_typ typ in
    let body = pretty_expr e1' in
    let rest = pretty_expr e2' in
    sprintf "let %s: %s = %s in\n%s" name styp body rest
  )
  | LetRecIn (_, name, typ, e1', e2') -> (
    let styp = pretty_typ typ in
    let body = pretty_expr e1' in
    let rest = pretty_expr e2' in
    sprintf "let rec %s: %s = %s in\n%s" name styp body rest
  )
  | Fun (_, params, typ, e') -> (
    let sparams = String.concat params ~sep:" " in
    let styp = pretty_typ typ in
    let body = pretty_expr e' in
    sprintf "fun %s: %s ->> %s" sparams styp body
  )
  | Match (_, e', typ, mbs) -> (
    let se' = pretty_expr e' in
    let styp = pretty_typ typ in
    let smbs = List.fold mbs ~f:(fun acc (mb, e'') -> (
      let smb = pretty_branch mb in
      let se'' = pretty_expr e'' in
      acc ^ (sprintf "| %s -> %s\n" smb se'')
    )) ~init:"" in
    sprintf "match (%s: %s) with\n%s" se' styp smbs
  )
  | App (_, e1', est') -> (
    let se1' = pretty_expr e1' in
    let sest' = List.fold est' ~f:(fun acc (es', t') -> (
      let ses' = pretty_expr es' in
      let st' = pretty_typ t' in
      acc ^ sprintf "(%s: %s) " ses' st'
    )) ~init:"" in
    sprintf "%s %s" se1' sest'
  )
  | Seq (_, e1', e2') -> (
    sprintf "%s;\n%s" (pretty_expr e1') (pretty_expr e2')
  )

type stmt =
  | Let of loc * string * typ * expr
  | LetRec of loc * string * typ * expr
  | Type of loc * string * (string * typ list option) list
[@@deriving show]

let rec pretty_stmt (s: stmt): string =
  match s with
  | Let (_, name, typ, body) -> (
    let styp = pretty_typ typ in
    let sbody = pretty_expr body in
    sprintf "let %s: %s = %s" name styp sbody
  )
  | LetRec (_, name, typ, body) -> (
    let styp = pretty_typ typ in
    let sbody = pretty_expr body in
    sprintf "let rec %s: %s = %s" name styp sbody
  )
  | Type (_, name, constructors) -> (
    let sconstructors = List.fold constructors ~f:(fun acc (constructor, typs) -> (
      match typs with
      | None -> acc ^ (sprintf "\n| %s" constructor)
      | Some ts -> (
        let sts = List.fold ts ~f:(fun acc t -> (
          let st = pretty_typ t in
          acc ^ (sprintf "* %s" st)
        )) ~init:"" in
        acc ^ (sprintf "\n| %s of %s" constructor sts)
      )
    )) ~init:"" in
    sprintf "typ %s = %s" name sconstructors
  )

type program = stmt list
[@@deriving show]
