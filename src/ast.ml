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
  

type binop = Lt | Lte | Eq | Gt | Gte | Neq | And | Or
[@@deriving show]

type unop = Not
[@@deriving show]

type numop = Sub | Add | Mult | Div
[@@deriving show]

type expr =
  | L of loc * literal
  | C of loc * complex
  | UnOp of loc * unop * expr
  | BinOp of loc * binop * expr * expr
  | NumOp of loc * numop * expr * expr
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

type stmt =
  | Let of loc * string * typ * expr
  | LetRec of loc * string * typ * expr
  | Type of loc * string * (string * typ list option) list
[@@deriving show]

type program = stmt list
[@@deriving show]
