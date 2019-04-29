type loc = int
[@@deriving show]

type typ =
  | TInt of loc
  | TFloat of loc
  | TBool of loc
  | TString of loc
  | TChar of loc
  | TUnit of loc
  (* Mutlivariate functions desugar into currying *)
  | TFun of loc * typ list
  | TTuple of loc * typ list
  (* Name * optional constructor *)
  | TRecord of loc * string
  | TList of loc * typ
  | TSecret of loc * typ
[@@deriving show]

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
  | Match of loc * expr * (match_branch * typ * expr) list
  | App of loc * expr * (expr * typ) list
  | Seq of loc * expr * expr

and literal =
  (* Simple literals *)
  | Unit of loc
  | Int of loc * int
  | Float of loc * float
  | String of loc * string
  | Char of loc * char
  | Bool of loc * bool

and complex =
  (* Complex literals *)
  | Tuple of loc * expr list
  (* Inductive type name * constructor values *)
  | Record of loc * string * expr list
  | List of loc * expr list

and match_branch =
  | ML of loc * literal
  | MVar of loc * string
  | Blank of loc
  (* Simplify complex types for binding *)
  | MTuple of loc * match_branch list
  | MRecord of loc * string * match_branch list
  | MList of loc * match_branch list
[@@deriving show]

type stmt =
  | Let of loc * string * typ * expr
  | LetRec of loc * string * typ * expr
  | Type of loc * string * (string * typ list option) list
[@@deriving show]

type program = stmt list
[@@deriving show]
