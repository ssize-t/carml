{

open Parser

let line_num = ref 1

exception SyntaxError of string
module B = Buffer
module L = Lexing
let get      = L.lexeme
let sprintf  = Printf.sprintf

let syntax_error msg = raise (SyntaxError (msg ^ " on line " ^ (string_of_int !line_num)))

}

let blank = [' ' '\r' '\t']
let digit = ['0'-'9']
let nonzero_digit = ['1'-'9']
let number = '0' | (nonzero_digit + (digit*))
let float = number + '.' + digit+ 
let lower_alpha = ['a'-'z'] | '_' | '\''
let upper_alpha = ['A'-'Z']
let alpha = lower_alpha | upper_alpha
let value_ident = lower_alpha (alpha | digit)*
let type_ident = upper_alpha (alpha | digit)*
let char = "'" [^ '\\' '\'' '\010' '\013'] "'"

rule micro = parse
  | '='         { EQ }
  | "let"       { LET }
  | "in"        { IN }
  | "fun"       { FUN }
  | "rec"       { REC }
  | "type"      { TYPE }
  | "match"     { MATCH }
  | "of"        { OF }
  | "with"      { WITH }
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
  | '@'         { AT }
  | ';'         { SEMI }

  | "()"        { UNIT }
  | "true"      { BOOL(true) }
  | "false"     { BOOL(false) }
  | float as f  { FLOAT(float_of_string f) }
  | number as n { INT(int_of_string n) }
  | char        { CHAR (Lexing.lexeme_char lexbuf 1) }
  | "'\\''"     { CHAR ('\'') }
  | "'\\n'"     { CHAR ('\n') }
  | "'\\t'"     { CHAR ('\t') }
  | "'\\r'"     { CHAR ('\r') }
  | '"'         { STRING (read_string (B.create 100) lexbuf) }

  | "int"       { TINT }
  | "bool"      { TBOOL }
  | "float"     { TFLOAT }
  | "string"    { TSTRING }
  | "char"      { TCHAR }
  | "secret"    { TSECRET }
  | "public"    { TPUBLIC }
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

  | "(*"        { comment lexbuf }

  | '\n'        { incr line_num; L.new_line lexbuf; micro lexbuf }
  | blank       { micro lexbuf }
  | _           { syntax_error "couldn't identify the token" }
  | eof         { EOF }
and comment =
  parse
  | "*)"        { micro lexbuf }
  | '\n'        { L.new_line lexbuf; comment lexbuf }
  | _           { comment lexbuf }
and read_string buf =
  parse
  | '"'       { Buffer.contents buf }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }