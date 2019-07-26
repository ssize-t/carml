type loc = int
[@@deriving show]

type typ =
  | TInt of loc
  | TFloat of loc
  | TBool of loc
  | TString of loc
  | TChar of loc
  | TUnit of loc
  | TFun of loc * typ * typ
  | TTuple of loc * typ * typ
  | TRecord of loc * string
  | TList of loc * typ
  | TSecret of loc * typ
  | TPublic of loc * typ
[@@deriving show]

type expr =
  | L of loc * literal
  | C of loc * complex
  | Var of loc * string
  | LetIn of loc * string * typ option * expr * expr
  | LetRecIn of loc * string * typ option * expr * expr
  | Fun of loc * string * expr
  | Match of loc * expr * (match_branch * expr) list
  | App of loc * expr * expr
  | Seq of loc * expr * expr

and literal =
  | Unit
  | Int of int
  | Float of float
  | String of string
  | Char of char
  | Bool of bool

and complex =
  | Tuple of expr * expr
  | Record of string * expr list
  | Nil
  | Cons of expr * expr

and match_branch =
  | ML of loc * literal
  | MVar of loc * string
  | Blank of loc
  (* Simplify complex types for binding *)
  | MTuple of loc * match_branch * match_branch
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