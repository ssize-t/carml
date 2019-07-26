open Tast
(* open Ast *)
open Core

let mb_line_no (e: match_branch): int =
  match e with
  | ML (l, _) -> l
  | MVar (l, _) -> l
  | Blank l -> l
  | MTuple (l, _, _) -> l
  | MRecord (l, _, _) -> l
  | MNil l -> l
  | MCons (l, _,_) -> l

let rec expr_line_no (e: expr): int =
  match e with
  | L (l', _) -> l'
  | C (l', _) -> l'
  | Var (l', _) -> l'
  | LetIn (l', _, _, _, _) -> l'
  | LetRecIn (l', _, _, _, _) -> l'
  | Fun (l', _, _,  _) -> l'
  | Match (l', _, _, _) -> l'
  | App (l', _, _) -> l'
  | Seq (l', _, _) -> l'

let is_complex_typ (t: typ'): bool =
  match t with
  | TFun _ -> true
  | TTuple _ -> true
  | TRecord _ -> true
  | TList _ -> true
  | _ -> false

let make_indent (lvl: int): string =
  let rec repeat (s: string) (n: int) (acc: string): string =
    match n with
    | 0 -> acc
    | n' -> repeat s (n' - 1) (acc ^ s)
  in
  repeat "    " lvl ""

let rec pretty_typ (t: typ') (nested: bool): string =
  match t with
  | TInt -> "int"
  | TFloat -> "float"
  | TBool -> "bool"
  | TChar -> "char"
  | TUnit -> "unit"
  | TString -> "string"
  | TFun (t, t') when nested -> sprintf "(%s -> %s)" (pretty_typ t true) (pretty_typ t' false)
  | TFun (t, t') -> sprintf "%s -> %s" (pretty_typ t true) (pretty_typ t' false)
  | TTuple (t, t') when nested -> sprintf "(%s * %s)" (pretty_typ t true) (pretty_typ t' false)
  | TTuple (t, t') -> sprintf "%s * %s" (pretty_typ t true) (pretty_typ t' false)
  | TRecord constructor -> constructor
  | TList t' -> sprintf "%s list" (pretty_typ t' false)
  | TSecret t' -> sprintf "secret(%s)" (pretty_typ t' false)
  | TPublic t' -> sprintf "public(%s)" (pretty_typ t' false)
  | TVar id -> sprintf "t%d" id
let pretty_typ (t: typ'): string = pretty_typ t false

let pretty_lit (l: literal) (line_no: int): int * string =
  match l with
  | Unit -> (line_no, "()")
  | Int i -> (line_no, sprintf "%d" i)
  | Float f -> (line_no, sprintf "%g" f)
  | String s -> (line_no, sprintf "\"%s\"" s)
  | Char c -> (line_no, sprintf "'%c'" c)
  | Bool b -> (line_no, if b then "true" else "false")

let rec pretty_branch (mb: match_branch) (line_no: int) (nested: bool): int * string =
  match mb with
  | ML (l', l) -> (
    let (line_no, sl) = pretty_lit l line_no in
    (line_no, sprintf "%s" sl)
  )
  | MVar (_, s) -> (line_no, s)
  | Blank _ -> (line_no, "_")
  | MTuple (_, mb, mb') when nested-> (
    let (line_no, smb) = pretty_branch mb line_no true in
    let (line_no, smb') = pretty_branch mb' line_no false in
    (line_no, sprintf "(%s, %s)" smb smb')
  )
  | MTuple (_, mb, mb') -> (
    let (line_no, smb) = pretty_branch mb line_no true in
    let (line_no, smb') = pretty_branch mb' line_no false in
    (line_no, sprintf "%s, %s" smb smb')
  )
  | MRecord (_, constructor, mbs) -> (
    let (line_no, smbs) = List.fold mbs ~f:(fun (line_no, acc) mb -> (
      let (line_no, smb) = pretty_branch mb line_no false in
      (line_no, if String.length acc > 0 then sprintf "%s, %s" acc smb else smb)
    )) ~init:(0, "") in
    (line_no, sprintf "%s(%s)" constructor smbs)
  )
  | MNil _ -> (line_no, sprintf "[]")
  | MCons (_, mb, mb') -> (
    let (line_no, smb) = pretty_branch mb line_no false in
    let (line_no, smb') = pretty_branch mb' line_no false in
    (line_no, sprintf "%s :: %s" smb smb')
  )
let pretty_branch (mb: match_branch) (line_no: int): int * string = pretty_branch mb line_no false

let rec pretty_comp (c: complex) (line_no: int) (nested: bool): int * string =
  match c with
  | Tuple (e, e') when nested -> (
    let (line_no, se) = pretty_expr line_no 0 e true in
    let (line_no, se') = pretty_expr line_no 0 e' false in
    (line_no, sprintf "(%s, %s)" se se')
  )
  | Tuple (e, e') -> (
    let (line_no, se) = pretty_expr line_no 0 e true in
    let (line_no, se') = pretty_expr line_no 0 e' false in
    (line_no, sprintf "(%s, %s)" se se')
  )
  | Record (constructor, params) -> (
    let (line_no, sparams) = List.fold params ~f:(fun (line_no, acc) e -> (
      let (line_no, se) = pretty_expr 0 0 e nested in
      (line_no, acc ^ sprintf ", %s" se)
    )) ~init:(line_no, "") in
    (line_no, sprintf "%s(%s)" constructor sparams)
  )
  | Nil -> (line_no, "[]")
  | Cons (e1, e2) -> (
    let (line_no, se1) = pretty_expr line_no 0 e1 nested in
    let (line_no, se2) = pretty_expr line_no 0 e2 nested in
    (line_no, sprintf "%s::%s" se1 se2)
  )
and pretty_expr (line_no: int) (indent_lvl: int) (e: expr) (nested: bool): int * string =
  let (maybe_newline, line_no) = match expr_line_no e with
  | l' when l' <> line_no -> ("\n", l')
  | l' when line_no = 0 -> ("", l')
  | _ -> ("", line_no)
  in
  let pre = match maybe_newline with
  | "" -> ""
  | nl -> nl ^ (make_indent indent_lvl) in
  match e with
  | L (_, l') -> pretty_lit l' line_no
  | C (_, c') -> pretty_comp c' line_no nested
  | Var (_, name) -> (line_no, name)
  | LetIn (_, name, typ, e1', e2') -> (
    let styp = sprintf ": %s" (pretty_typ typ) in
    let (line_no, body) = pretty_expr line_no (indent_lvl + 1) e1' nested in
    let (line_no, rest) = pretty_expr line_no (indent_lvl + 1) e2' nested in
    (line_no, sprintf "%slet %s%s = %s in\n%s" pre name styp body rest)
  )
  | LetRecIn (_, name, typ, e1', e2') -> (
    let styp = sprintf ": %s" (pretty_typ typ) in
    let (line_no, body) = pretty_expr line_no (indent_lvl + 1) e1' nested in
    let (line_no, rest) = pretty_expr line_no (indent_lvl + 1) e2' nested in
    (line_no, sprintf "%slet rec %s%s = %s in\n%s" pre name styp body rest)
  )
  | Fun (_, param, typ, e') -> (
    let styp = pretty_typ typ in
    let (line_no, body) = pretty_expr line_no (indent_lvl + 1) e' nested in
    (line_no, sprintf "%sfun: (%s) %s ->> %s" pre styp param body)
  )
  | Match (_, e', typ, mbs) -> (
    let styp = pretty_typ typ in
    let (line_no, se') = pretty_expr line_no (indent_lvl + 1) e' nested in
    let (line_no, smbs) = List.fold mbs ~f:(fun (line_no, acc) (mb, e'') -> (
      let (line_no, smb) = pretty_branch mb line_no in
      let (line_no, se'') = pretty_expr line_no (indent_lvl + 1) e'' nested in
      (line_no, acc ^ (sprintf "%s| %s -> %s" pre smb se''))
    )) ~init:(line_no, "") in
    (line_no, sprintf "%smatch (%s: %s) with%s" pre styp se' smbs)
  )
  | App (_, e1', (et', t)) -> (
    let (line_no, se1') = pretty_expr line_no (indent_lvl + 1) e1' nested in
    let se1'' = match e1' with
    | L (_, _) -> se1'
    | _ -> sprintf "(%s)" se1' in
    let (line_no, set') = pretty_expr line_no (indent_lvl + 1) et' nested in
    (line_no, sprintf "%s(%s %s)" pre se1'' set')
  )
  | Seq (_, e1', e2') -> (
    let (line_no, se1') = pretty_expr line_no (indent_lvl + 1) e1' nested in
    let (line_no, se2') = pretty_expr line_no (indent_lvl + 1) e2' nested in
    (line_no, sprintf "%s%s;%s" pre se1' se2')
  )
let pretty_expr (line_no: int) (indent_lvl: int) (e: expr): int * string = pretty_expr line_no indent_lvl e false

let rec pretty_stmt (s: stmt) (line_no: int): int * string =
  match s with
  | Let (_, name, typ, body) -> (
    let styp = sprintf ": %s" (pretty_typ typ) in
    let (line_no, sbody) = pretty_expr line_no 1 body in
    (line_no, sprintf "let %s%s = %s" name styp sbody)
  )
  | LetRec (_, name, typ, body) -> (
    let styp = sprintf ": %s" (pretty_typ typ) in
    let (line_no, sbody) = pretty_expr line_no 1 body in
    (line_no, sprintf "let rec %s%s = %s" name styp sbody)
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
    (line_no, sprintf "type %s = %s" name sconstructors)
  )
  

let pretty_expr (e: expr): string = let (_, pexpr) = pretty_expr 0 0 e in pexpr
let pretty_lit (l: literal): string = let (_, sl) = pretty_lit l 0 in sl
let pretty_branch (mb: match_branch): string = let (_, smb) = pretty_branch mb 0 in smb
let pretty_stmt (s: stmt): string = let (_, ss) = pretty_stmt s 0 in ss
let pretty_program (p: program): string = String.concat (List.map p ~f:pretty_stmt) ~sep:"\n"
