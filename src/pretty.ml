open Ast
open Core

let mb_line_no (e: match_branch): int =
  match e with
  | ML (Loc (l, _), _) -> l
  | MVar (Loc (l, _), _) -> l
  | Blank (Loc (l, _)) -> l
  | MTuple (Loc (l, _), _) -> l
  | MRecord (Loc (l, _), _, _) -> l
  | MNil (Loc (l, _)) -> l
  | MCons (Loc (l, _), _,_) -> l

let expr_line_no (e: expr): int =
  match e with
  | L (Loc (l', _), _) -> l'
  | C (Loc (l', _), _) -> l'
  | UnOp (Loc (l', _), _, _) -> l'
  | BinOp (Loc (l', _), _, _, _) -> l'
  | NumOp (Loc (l', _), _, _, _) -> l'
  | ListOp (Loc (l', _), _, _, _) -> l'
  | Var (Loc (l', _), _) -> l'
  | LetIn (Loc (l', _), _, _, _, _) -> l'
  | LetRecIn (Loc (l', _), _, _, _, _) -> l'
  | Fun (Loc (l', _), _, _, _) -> l'
  | Match (Loc (l', _), _, _, _) -> l'
  | App (Loc (l', _), _, _) -> l'
  | Seq (Loc (l', _), _, _) -> l'

let mb_meta (e: match_branch): meta =
  match e with
  | ML (Loc (_, m), _) -> m
  | MVar (Loc (_, m), _) -> m
  | Blank (Loc (_, m)) -> m
  | MTuple (Loc (_, m), _) -> m
  | MRecord (Loc (_, m), _, _) -> m
  | MNil (Loc (_, m)) -> m
  | MCons (Loc (_, m), _,_) -> m

let expr_meta (e: expr): meta =
  match e with
  | L (Loc (_, m), _) -> m
  | C (Loc (_, m), _) -> m
  | UnOp (Loc (_, m), _, _) -> m
  | BinOp (Loc (_, m), _, _, _) -> m
  | NumOp (Loc (_, m), _, _, _) -> m
  | ListOp (Loc (_, m), _, _, _) -> m
  | Var (Loc (_, m), _) -> m
  | LetIn (Loc (_, m), _, _, _, _) -> m
  | LetRecIn (Loc (_, m), _, _, _, _) -> m
  | Fun (Loc (_, m), _, _, _) -> m
  | Match (Loc (_, m), _, _, _) -> m
  | App (Loc (_, m), _, _) -> m
  | Seq (Loc (_, m), _, _) -> m

let stmt_meta (s: stmt): meta =
  match s with
  | Let (Loc (_, m), _, _, _) -> m
  | LetRec (Loc (_, m), _, _, _) -> m
  | Type (Loc (_, m), _, _) -> m

let is_complex_typ (t: typ): bool =
  match t with
  | TFun (_, _) -> true
  | TTuple (_, _) -> true
  | TRecord (_, _) -> true
  | TList (_, _) -> true
  | _ -> false

let make_indent (lvl: int): string =
  let rec repeat (s: string) (n: int) (acc: string): string =
    match n with
    | 0 -> acc
    | n' -> repeat s (n' - 1) (acc ^ s)
  in
  repeat "    " lvl ""

let pretty_list_op (op: listop): string =
  match op with
  | Concat -> "@"

let pretty_binop (op: binop): string =
  match op with
  | Lt -> "<"
  | Lte -> "<="
  | Eq -> "="
  | Gt -> ">"
  | Gte -> ">="
  | Neq -> "!="
  | And -> "&&"
  | Or -> "||"

let pretty_numop (op: numop): string =
  match op with
  | Sub -> "-"
  | Add -> "+"
  | Mult -> "*"
  | Div -> "/"

let rec pretty_typ (t: typ): string =
  match t with
  | TInt _ -> "int"
  | TFloat _ -> "float"
  | TBool _ -> "bool"
  | TChar _ -> "char"
  | TUnit _ -> "unit"
  | TString _ -> "string"
  | TFun (_, typs) -> (
    List.fold typs ~f:(fun acc typ -> (
      match is_complex_typ typ, String.length acc with
      | true, 0 -> sprintf "(%s)" (pretty_typ typ)
      | true, _ -> sprintf "%s -> (%s)" acc (pretty_typ typ)
      | false, 0 -> pretty_typ typ
      | false, _ -> sprintf "%s -> %s" acc (pretty_typ typ)
    )) ~init:""
  )
  | TTuple (_, typs) -> (
    List.fold typs ~f:(fun acc typ -> (
      match is_complex_typ typ, String.length acc with
      | true, 0 -> sprintf "(%s)" (pretty_typ typ)
      | true, _ -> sprintf "%s * (%s)" acc (pretty_typ typ)
      | false, 0 -> pretty_typ typ
      | false, _ -> sprintf "%s * %s" acc (pretty_typ typ)
    )) ~init:""
  )
  | TRecord (_, constructor) -> constructor
  | TList (_, t') -> sprintf "%s list" (pretty_typ t')
  | TSecret (_, t') -> sprintf "secret(%s)" (pretty_typ t')
  | TPublic (_, t') -> sprintf "public(%s)" (pretty_typ t')

let pretty_lit (l: literal) (line_no: int) (color: ANSITerminal.style option): int * ANSITerminal.style option * string =
  match l with
  | Unit -> (line_no, color, "()")
  | Int i -> (line_no, color, sprintf "%d" i)
  | Float f -> (line_no, color, sprintf "%g" f)
  | String s -> (line_no, color, sprintf "\"%s\"" s)
  | Char c -> (line_no, color, sprintf "'%c'" c)
  | Bool b -> (line_no, color, if b then "true" else "false")

let rec pretty_branch (mb: match_branch) (line_no: int) (color: ANSITerminal.style option): int * ANSITerminal.style option * string =
  let color = match (mb_meta mb, color) with
  | Empty, None -> []
  | TypeErrorLoc, _ -> [ANSITerminal.red]
  | _, Some c -> [c]
  in
  match mb with
  | ML (l', l) -> (
    let (line_no, color, sl) = pretty_lit l line_no (List.hd color) in
    let ccolor = match color with | Some c -> [c] | None -> [] in
    (line_no, color, ANSITerminal.sprintf ccolor "%s" sl)
  )
  | MVar (_, s) -> (line_no, (List.hd color), s)
  | Blank _ -> (line_no, (List.hd color), "_")
  | MTuple (_, mbs) -> (
    let (line_no, color, smbs) = List.fold mbs ~f:(fun (line_no, color, acc) mb -> (
      let (line_no, color, smb) = pretty_branch mb line_no color in
      let ccolor = match color with | Some c -> [c] | None -> [] in
      (line_no, color, if String.length acc > 0 then ANSITerminal.sprintf ccolor "%s,%s" acc smb else smb)
    )) ~init:(0, (List.hd color), "") in
    let ccolor = match color with | Some c -> [c] | None -> [] in
    (line_no, color, ANSITerminal.sprintf ccolor "(%s)" smbs)
  )
  | MRecord (_, constructor, mbs) -> (
    let (line_no, color, smbs) = List.fold mbs ~f:(fun (line_no, color, acc) mb -> (
      let (line_no, color, smb) = pretty_branch mb line_no color in
      let ccolor = match color with | Some c -> [c] | None -> [] in
      (line_no, color, if String.length acc > 0 then ANSITerminal.sprintf ccolor "%s, %s" acc smb else smb)
    )) ~init:(0, (List.hd color), "") in
    let ccolor = match color with | Some c -> [c] | None -> [] in
    (line_no, color, ANSITerminal.sprintf ccolor "%s(%s)" constructor smbs)
  )
  | MNil _ -> (line_no, (List.hd color), ANSITerminal.sprintf color "[]")
  | MCons (_, mb, mb') -> (
    let (line_no, color, smb) = pretty_branch mb line_no (List.hd color) in
    let (line_no, color, smb') = pretty_branch mb' line_no color in
    let ccolor = match color with | Some c -> [c] | None -> [] in
    (line_no, color, ANSITerminal.sprintf ccolor "%s :: %s" smb smb')
  )

let rec pretty_comp (c: complex) (line_no: int) (color: ANSITerminal.style option): int * ANSITerminal.style option * string =
  match c with
  | Tuple es -> (
    let (line_no, color, ses) = List.fold es ~f:(fun (line_no, color, acc) e -> (
      let (line_no, color, es) = pretty_expr line_no 0 e color in
      (line_no, color, if String.length acc > 0 then sprintf "%s, %s" acc es else es)
    )) ~init:(line_no, color, "") in
    let ccolor = match color with | Some c -> [c] | None -> [] in
    (line_no, color, ANSITerminal.sprintf ccolor "(%s)" ses)
  )
  | Record (constructor, params) -> (
    let (line_no, color, sparams) = List.fold params ~f:(fun (line_no, color, acc) e -> (
      let (line_no, color, se) = pretty_expr 0 0 e color in
      (line_no, color, acc ^ sprintf ", %s" se)
    )) ~init:(line_no, color, "") in
    (line_no, color, sprintf "%s(%s)" constructor sparams)
  )
  | Nil -> (line_no, color, "[]")
  | Cons (e1, e2) -> (
    let (line_no, color, se1) = pretty_expr line_no 0 e1 color in
    let (line_no, color, se2) = pretty_expr line_no 0 e2 color in
    (line_no, color, sprintf "%s::%s" se1 se2)
  )
and pretty_expr (line_no: int) (indent_lvl: int) (e: expr) (color: ANSITerminal.style option): int * (ANSITerminal.style option) * string =
  let color = match (expr_meta e, color) with
  | Empty, None -> []
  | TypeErrorLoc, _ -> [ANSITerminal.red]
  | _, Some c -> [c]
  in
  let (maybe_newline, line_no) = match expr_line_no e with
  | l' when l' <> line_no -> ("\n", l')
  | l' when line_no = 0 -> ("", l')
  | _ -> ("", line_no)
  in
  let pre = match maybe_newline with
  | "" -> ""
  | nl -> nl ^ (make_indent indent_lvl) in
  match e with
  | L (_, l') -> (
    let (line_no, color, sl') = pretty_lit l' line_no (List.hd color) in
    let ccolor = match color with | Some c -> [c] | None -> [] in
    (line_no, color, ANSITerminal.sprintf ccolor "%s" sl')
  )
  | C (_, c') -> (
    let (line_no, color, sc') = pretty_comp c' line_no (List.hd color) in
    let ccolor = match color with | Some c -> [c] | None -> [] in
    (line_no, color, ANSITerminal.sprintf ccolor "%s" sc')
  )
  | UnOp (_, op, e') -> (
    match op with
    | Not -> (
      let (line_no, color, es) = pretty_expr line_no (indent_lvl + 1) e' (List.hd color) in
      let ccolor = match color with | Some c -> [c] | None -> [] in
      (line_no, color, ANSITerminal.sprintf ccolor "%s!(%s)" pre es)
    )
  )
  | BinOp (_, op, e1', e2') -> (
    let (line_no, color, se1') = pretty_expr line_no (indent_lvl + 1) e1' (List.hd color) in
    let (line_no, color, se2') = pretty_expr line_no (indent_lvl + 1) e2' color in
    let ccolor = match color with | Some c -> [c] | None -> [] in
    let sop = pretty_binop op in
    (line_no, color, ANSITerminal.sprintf ccolor "%s%s %s %s" pre se1' sop se2')
  )
  | NumOp (_, op, e1', e2') -> (
    let (line_no, color, se1') = pretty_expr line_no (indent_lvl + 1) e1' (List.hd color) in
    let (line_no, color, se2') = pretty_expr line_no (indent_lvl + 1) e2' color in
    let ccolor = match color with | Some c -> [c] | None -> [] in
    let sop = pretty_numop op in
    (line_no, color, ANSITerminal.sprintf ccolor "%s%s %s %s" pre se1' sop se2')
  )
  | ListOp (_, op, e1', e2') -> (
    let (line_no, color, se1') = pretty_expr line_no (indent_lvl + 1) e1' (List.hd color) in
    let (line_no, color, se2') = pretty_expr line_no (indent_lvl + 1) e2' color in
    let ccolor =  match color with | Some c -> [c] | None -> [] in
    let sop = match op with
    | Concat -> "@" in
    (line_no, color, ANSITerminal.sprintf ccolor "%s%s %s %s" pre se1' sop se2')
  )
  | Var (_, name) -> (line_no, (List.hd color), name)
  | LetIn (_, name, typ, e1', e2') -> (
    let styp = pretty_typ typ in
    let (line_no, color, body) = pretty_expr line_no (indent_lvl + 1) e1' (List.hd color) in
    let (line_no, color, rest) = pretty_expr line_no (indent_lvl + 1) e2' color in
    let ccolor = match color with | Some c -> [c] | None -> [] in
    (line_no, color, ANSITerminal.sprintf ccolor "%slet %s: %s = %s in\n%s" pre name styp body rest)
  )
  | LetRecIn (_, name, typ, e1', e2') -> (
    let styp = pretty_typ typ in
    let (line_no, color, body) = pretty_expr line_no (indent_lvl + 1) e1' (List.hd color) in
    let (line_no, color, rest) = pretty_expr line_no (indent_lvl + 1) e2' color in
    let ccolor = match color with | Some c -> [c] | None -> [] in
    (line_no, color, ANSITerminal.sprintf ccolor "%slet rec %s: %s = %s in\n%s" pre name styp body rest)
  )
  | Fun (_, params, typ, e') -> (
    let sparams = String.concat params ~sep:" " in
    let (line_no, styp) = (line_no, pretty_typ typ) in
    let (line_no, color, body) = pretty_expr line_no (indent_lvl + 1) e' (List.hd color) in
    let ccolor = match color with | Some c -> [c] | None -> [] in
    (line_no, color, ANSITerminal.sprintf ccolor "%sfun %s: %s ->> %s" pre sparams styp body)
  )
  | Match (_, e', typ, mbs) -> (
    let (line_no, color, se') = pretty_expr line_no (indent_lvl + 1) e' (List.hd color) in
    let styp = pretty_typ typ in
    let (line_no, color, smbs) = List.fold mbs ~f:(fun (line_no, color, acc) (mb, e'') -> (
      let (line_no, color, smb) = pretty_branch mb line_no color in
      let (line_no, color, se'') = pretty_expr line_no (indent_lvl + 1) e'' color in
      let ccolor = match color with | Some c -> [c] | None -> [] in
      (line_no, color, acc ^ (ANSITerminal.sprintf ccolor "%s| %s -> %s" pre smb se''))
    )) ~init:(line_no, color, "") in
    let ccolor = match color with | Some c -> [c] | None -> [] in
    (line_no, color, ANSITerminal.sprintf ccolor "%smatch (%s: %s) with%s" pre se' styp smbs)
  )
  | App (_, e1', est') -> (
    let (line_no, color, se1') = pretty_expr line_no (indent_lvl + 1) e1' (List.hd color) in
    let ccolor = match color with | Some c -> [c] | None -> [] in
    let se1'' = match e1' with
    | L (_, _) -> se1'
    | _ -> ANSITerminal.sprintf ccolor "(%s)" se1' in
    let (line_no, sest') = List.fold est' ~f:(fun (line_no, acc) (es', t') -> (
      let (line_no, color, ses') = pretty_expr line_no (indent_lvl + 1) es' color in
      let ccolor = match color with | Some c -> [c] | None -> [] in
      let st' = pretty_typ t' in
      (line_no, acc ^ ANSITerminal.sprintf ccolor "(%s: %s) " ses' st')
    )) ~init:(0, "") in
    (line_no, color, ANSITerminal.sprintf ccolor "%s%s %s" pre se1'' sest')
  )
  | Seq (_, e1', e2') -> (
    let (line_no, color, se1') = pretty_expr line_no (indent_lvl + 1) e1' (List.hd color) in
    let (line_no, color, se2') = pretty_expr line_no (indent_lvl + 1) e2' color in
    let ccolor = match color with | Some c -> [c] | None -> [] in
    (line_no, color, ANSITerminal.sprintf ccolor "%s%s;%s" pre se1' se2')
  )

let rec pretty_stmt (s: stmt) (line_no: int): int * string =
  let color = match stmt_meta s with
    | Empty -> []
    | TypeErrorLoc -> [ANSITerminal.red]
  in
  match s with
  | Let (_, name, typ, body) -> (
    let styp = pretty_typ typ in
    let (line_no, _, sbody) = pretty_expr line_no 1 body (List.hd color) in
    (line_no, ANSITerminal.sprintf color "let %s: %s = %s" name styp sbody)
  )
  | LetRec (_, name, typ, body) -> (
    let styp = pretty_typ typ in
    let (line_no, _, sbody) = pretty_expr line_no 1 body (List.hd color) in
    (line_no, ANSITerminal.sprintf color "let rec %s: %s = %s" name styp sbody)
  )
  | Type (_, name, constructors) -> (
    let sconstructors = List.fold constructors ~f:(fun acc (constructor, typs) -> (
      match typs with
      | None -> acc ^ (ANSITerminal.sprintf color "\n| %s" constructor)
      | Some ts -> (
        let sts = List.fold ts ~f:(fun acc t -> (
          let st = pretty_typ t in
          acc ^ (ANSITerminal.sprintf color "* %s" st)
        )) ~init:"" in
        acc ^ (ANSITerminal.sprintf color "\n| %s of %s" constructor sts)
      )
    )) ~init:"" in
    (line_no, ANSITerminal.sprintf color "type %s = %s" name sconstructors)
  )
  
let pretty_program (p: program): string =
  String.concat (List.map (List.mapi p ~f:(fun i s -> pretty_stmt s i)) ~f:(fun (i, s) -> s)) ~sep:"\n"

let pretty_expr (e: expr): string = let (_, _, pexpr) = pretty_expr 0 0 e None in pexpr
let pretty_lit (l: literal): string = let (_, _, sl) = pretty_lit l 0 None in sl
let pretty_branch (mb: match_branch): string = let (_, _, smb) = pretty_branch mb 0 None in smb
let pretty_stmt (s: stmt): string = let (_, ss) = pretty_stmt s 0 in ss