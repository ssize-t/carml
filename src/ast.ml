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

type binop = Lt | Lte | Eq | Gt | Gte | Neq | And | Or
[@@deriving show]

type unop = Not
[@@deriving show]

type numop = Sub | Add | Mult | Div
[@@deriving show]

type listop = Concat
[@@deriving show]

type expr =
  | L of loc * literal
  | C of loc * complex
  | UnOp of loc * unop * expr
  | BinOp of loc * binop * expr * expr
  | NumOp of loc * numop * expr * expr
  | ListOp of loc * listop * expr * expr
  | Var of loc * string
  | LetIn of loc * string * typ option * expr * expr
  | LetRecIn of loc * string * typ option * expr * expr
  | Fun of loc * string list * expr
  | Match of loc * expr * (match_branch * expr) list
  | App of loc * expr * expr list
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
  | Let of loc * string * typ option * expr
  | LetRec of loc * string * typ option * expr
  | Type of loc * string * (string * typ list option) list
[@@deriving show]

type program = stmt list
[@@deriving show]
