type typ =
  | TInt
  | TFloat
  | TBool
  | TString
  | TChar
  | TUnit
  (* Mutlivariate functions desugar into currying *)
  | TFun of typ * typ
  | TTuple of typ * typ
  (* Name * optional constructor *)
  | TRecord of string
  | TList of typ
  | TSecret of typ
[@@deriving show]

type binop = Lt | Lte | Eq | Gt | Gte | Neq | And | Or
[@@deriving show]

type unop = Not
[@@deriving show]

type numop = Sub | Add | Mult | Div
[@@deriving show]

type expr =
  | L of literal
  | C of complex
  | UnOp of unop * expr
  | BinOp of binop * expr * expr
  | NumOp of numop * expr * expr
  | Var of string
  | LetIn of string * typ * expr * expr
  | Fun of string list * typ * expr
  | Match of expr * (match_branch * expr) list
  | App of expr * expr list
  | Seq of expr * expr

and literal =
  (* Simple literals *)
  | Unit
  | Int of int
  | Float of float
  | String of string
  | Char of char
  | Bool of bool

and complex =
  (* Complex literals *)
  | Tuple of expr list
  (* Inductive type name * constructor values *)
  | Record of string * expr list
  | List of expr list

and match_branch =
  | ML of literal
  | MVar of string
  | Blank
  (* Simplify complex types for binding *)
  | MTuple of match_branch list
  | MRecord of string * match_branch list
  | MList of match_branch list
[@@deriving show]

type stmt =
  | Let of string * typ * expr
  | Type of string * (string * typ list option) list
[@@deriving show]

type program = stmt list
[@@deriving show]
