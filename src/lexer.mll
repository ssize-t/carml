{

open Parser

let line_num = ref 1

exception Syntax_error of string
module B = Buffer
module L = Lexing
let get      = L.lexeme
let sprintf  = Printf.sprintf

let syntax_error msg = raise (Syntax_error (msg ^ " on line " ^ (string_of_int !line_num)))

}

let blank = [' ' '\r' '\t']
let digit = ['0'-'9']
let nonzero_digit = ['1'-'9']
let number = '0' | (nonzero_digit + (digit*))
let float = number + '.' + digit+ 
let lower_alpha = ['a'-'z'] | '_' | '\''
let upper_alpha = ['A'-'Z']
let alpha = lower_alpha | upper_alpha
let char = '\'' + alpha + '\''
let value_ident = lower_alpha (alpha | digit)*
let type_ident = upper_alpha (alpha | digit)*

rule micro = parse
  | '='         { EQ }
  | "let"       { LET }
  | "in"        { IN }
  | "fun"       { FUN }
  | "type"      { TYPE }
  | "match"     { MATCH }
  | "of"        { OF }
  | "with"      { WITH }
  | "->>"       { DOUBLE_ARROW }
  | "->"        { ARROW }
  | '|'         { BAR }
  | '_'         { UNDERSCORE }

  | '('         { LPAREN }
  | ')'         { RPAREN }
  | '['         { LBRACKET }
  | ']'         { RBRACKET }
  | ','         { COMMA }

  | ':'         { COLON }
  | "::"        { DOUBLE_COLON }
  | ";;"        { DOUBLE_SEMI }
  | ';'         { SEMI }

  | "()"        { UNIT }
  | "true"      { BOOL(true) }
  | "false"     { BOOL(false) }
  | char as c   { CHAR(String.get c 1) }
  | float as f  { FLOAT(float_of_string f) }
  | number as n { INT(int_of_string n) }
  | '"'         { STRING (string (B.create 100) lexbuf) }

  | "int"       { TINT }
  | "bool"      { TBOOL }
  | "float"     { TFLOAT }
  | "string"    { TSTRING }
  | "char"      { TCHAR }
  | "secret"    { TSECRET }
  | "unit"      { TUNIT }
  | "list"      { TLIST }

  | value_ident as i  { VALUE_IDENT(i) }
  | type_ident as i  { TYPE_IDENT(i) }

  | "<="        { LTE }
  | '<'         { LT }
  | ">="        { GTE }
  | '>'         { GT }
  | "&&"        { AND }
  | "||"        { OR }
  | "!="        { NEQ }
  | '!'         { NOT }

  | '+'         { ADD }
  | '-'         { SUB }
  | '*'         { MULT }
  | '/'         { DIV }

  | '\n'        { incr line_num; micro lexbuf }
  | blank       { micro lexbuf }
  | _           { syntax_error "couldn't identify the token" }
  | eof         { EOF }
and string buf = parse 
  | [^'"' '\n' '\\']+  
    { B.add_string buf @@ get lexbuf
    ; string buf lexbuf 
    }
  | '\n'      { B.add_string buf @@ get lexbuf
    ; L.new_line lexbuf
    ; string buf lexbuf
    }
  | '\\' '"'  { B.add_char buf '"'
    ; string buf lexbuf
    }
  | '\\'      { B.add_char buf '\\'
    ; string buf lexbuf
    }
  | '"'       { B.contents buf } (* return *)