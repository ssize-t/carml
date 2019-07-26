%{

open Ast
open Core

%}

/* Type declarations */
%token TINT TFLOAT TBOOL TSTRING TCHAR TUNIT TLIST
%token TSECRET TPUBLIC
%token ARROW

/* Simple Literals */
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token <string> STRING
%token <char> CHAR
%token UNIT

/* List */
%token LBRACKET RBRACKET SEMI

/* BinOps */
%token <string> LT LTE EQ GT GTE AND OR NEQ NOT

/* NumOps */
%token <string> SUB ADD MULT DIV

/* ListOps */
%token <string> DOUBLE_COLON AT

/* Identifiers */
%token <string> VALUE_IDENT TYPE_IDENT SYMBOLIC_IDENT

/* Keywords */
%token LET IN FUN REC
%token MATCH TYPE BAR OF WITH UNDERSCORE

/* Operators */
%right DOUBLE_COLON AT

%token LPAREN RPAREN
%token COMMA COLON

%token EOF

%left SYMBOLIC_IDENT
%left AND OR
%right EQ NEQ
%left GT GTE LTE LT
%left ADD SUB
%left MULT DIV
%left NOT

%start program single_expression single_statement
%type <Ast.program> program
%type <Ast.expr> single_expression
%type <Ast.stmt> single_statement

%%

program:
| statements EOF { $1 }
;

statements:
| statement { [$1] }
| statement statements { $1 :: $2 }
;

single_statement:
| statement EOF { $1 }

statement:
| LET VALUE_IDENT COLON styp EQ eexpression { Let ($symbolstartpos.Lexing.pos_lnum, $2, Some $4, $6) }
| LET VALUE_IDENT EQ eexpression { Let ($symbolstartpos.Lexing.pos_lnum, $2, None, $4) }
| LET REC VALUE_IDENT COLON styp EQ eexpression { LetRec ($symbolstartpos.Lexing.pos_lnum, $3, Some $5, $7) }
| LET REC VALUE_IDENT EQ eexpression { LetRec ($symbolstartpos.Lexing.pos_lnum, $3, None, $5) }

| LET VALUE_IDENT params COLON styp EQ eexpression {
  let f = List.rev $3 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$7 in
  Let ($symbolstartpos.Lexing.pos_lnum, $2, Some $5, f)
}
| LET VALUE_IDENT params EQ eexpression {
  let f = List.rev $3 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$5 in
  Let ($symbolstartpos.Lexing.pos_lnum, $2, None, f)
}
| LET LPAREN SYMBOLIC_IDENT RPAREN params EQ expression {
  let f = List.rev $5 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$7 in
  Let ($symbolstartpos.Lexing.pos_lnum, $3, None, f)
}
| LET LPAREN SYMBOLIC_IDENT RPAREN params COLON styp EQ expression {
  let f = List.rev $5 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$9 in
  Let ($symbolstartpos.Lexing.pos_lnum, $3, Some $7, f)
}
| LET REC VALUE_IDENT params COLON styp EQ eexpression {
  let f = List.rev $4 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$8 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $3, Some $6, f)
}
| LET REC VALUE_IDENT params EQ eexpression {
  let f = List.rev $4 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$6 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $3, None, f)
}
| LET REC LPAREN SYMBOLIC_IDENT RPAREN params EQ expression {
  let f = List.rev $6 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$8 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $4, None, f)
}
| LET REC LPAREN SYMBOLIC_IDENT RPAREN params COLON styp EQ expression {
  let f = List.rev $6 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$10 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $4, Some $8, f)
}

| overridden_default_symbolic_statement { $1}

| TYPE VALUE_IDENT EQ typedecls { Type ($symbolstartpos.Lexing.pos_lnum, $2, $4) }
;

overridden_default_symbolic_statement:
| LET LPAREN AT RPAREN params EQ expression {
  let f = List.rev $5 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$7 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $3, None, f)
}
| LET LPAREN AND RPAREN params EQ expression {
  let f = List.rev $5 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$7 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $3, None, f)
}
| LET LPAREN OR RPAREN params EQ expression {
  let f = List.rev $5 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$7 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $3, None, f)
}
| LET LPAREN LT RPAREN params EQ expression {
  let f = List.rev $5 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$7 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $3, None, f)
}
| LET LPAREN LTE RPAREN params EQ expression {
  let f = List.rev $5 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$7 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $3, None, f)
}
| LET LPAREN GT RPAREN params EQ expression {
  let f = List.rev $5 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$7 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $3, None, f)
}
| LET LPAREN GTE RPAREN params EQ expression {
  let f = List.rev $5 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$7 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $3, None, f)
}
| LET LPAREN EQ RPAREN params EQ expression {
  let f = List.rev $5 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$7 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $3, None, f)
}
| LET LPAREN NEQ RPAREN params EQ expression {
  let f = List.rev $5 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$7 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $3, None, f)
}
| LET LPAREN ADD RPAREN params EQ expression {
  let f = List.rev $5 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$7 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $3, None, f)
}
| LET LPAREN SUB RPAREN params EQ expression {
  let f = List.rev $5 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$7 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $3, None, f)
}
| LET LPAREN MULT RPAREN params EQ expression {
  let f = List.rev $5 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$7 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $3, None, f)
}
| LET LPAREN DIV RPAREN params EQ expression {
  let f = List.rev $5 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$7 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $3, None, f)
}
| LET LPAREN NOT RPAREN params EQ expression {
  let f = List.rev $5 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$7 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $3, None, f)
}
| LET LPAREN AT RPAREN params COLON styp EQ expression {
  let f = List.rev $5 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$9 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $3, Some $7, f)
}
| LET LPAREN AND RPAREN params COLON styp EQ expression {
  let f = List.rev $5 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$9 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $3, Some $7, f)
}
| LET LPAREN OR RPAREN params COLON styp EQ expression {
  let f = List.rev $5 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$9 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $3, Some $7, f)
}
| LET LPAREN LT RPAREN params COLON styp EQ expression {
  let f = List.rev $5 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$9 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $3, Some $7, f)
}
| LET LPAREN LTE RPAREN params COLON styp EQ expression {
  let f = List.rev $5 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$9 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $3, Some $7, f)
}
| LET LPAREN GT RPAREN params COLON styp EQ expression {
  let f = List.rev $5 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$9 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $3, Some $7, f)
}
| LET LPAREN GTE RPAREN params COLON styp EQ expression {
  let f = List.rev $5 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$9 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $3, Some $7, f)
}
| LET LPAREN EQ RPAREN params COLON styp EQ expression {
  let f = List.rev $5 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$9 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $3, Some $7, f)
}
| LET LPAREN NEQ RPAREN params COLON styp EQ expression {
  let f = List.rev $5 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$9 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $3, Some $7, f)
}
| LET LPAREN ADD RPAREN params COLON styp EQ expression {
  let f = List.rev $5 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$9 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $3, Some $7, f)
}
| LET LPAREN SUB RPAREN params COLON styp EQ expression {
  let f = List.rev $5 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$9 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $3, Some $7, f)
}
| LET LPAREN MULT RPAREN params COLON styp EQ expression {
  let f = List.rev $5 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$9 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $3, Some $7, f)
}
| LET LPAREN DIV RPAREN params COLON styp EQ expression {
  let f = List.rev $5 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$9 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $3, Some $7, f)
}
| LET LPAREN NOT RPAREN params COLON styp EQ expression {
  let f = List.rev $5 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$9 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $3, Some $7, f)
}
| LET REC LPAREN AT RPAREN params EQ expression {
  let f = List.rev $6 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$8 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $4, None, f)
}
| LET REC LPAREN AND RPAREN params EQ expression {
  let f = List.rev $6 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$8 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $4, None, f)
}
| LET REC LPAREN OR RPAREN params EQ expression {
  let f = List.rev $6 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$8 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $4, None, f)
}
| LET REC LPAREN LT RPAREN params EQ expression {
  let f = List.rev $6 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$8 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $4, None, f)
}
| LET REC LPAREN LTE RPAREN params EQ expression {
  let f = List.rev $6 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$8 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $4, None, f)
}
| LET REC LPAREN GT RPAREN params EQ expression {
  let f = List.rev $6 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$8 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $4, None, f)
}
| LET REC LPAREN GTE RPAREN params EQ expression {
  let f = List.rev $6 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$8 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $4, None, f)
}
| LET REC LPAREN EQ RPAREN params EQ expression {
  let f = List.rev $6 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$8 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $4, None, f)
}
| LET REC LPAREN NEQ RPAREN params EQ expression {
  let f = List.rev $6 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$8 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $4, None, f)
}
| LET REC LPAREN ADD RPAREN params EQ expression {
  let f = List.rev $6 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$8 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $4, None, f)
}
| LET REC LPAREN SUB RPAREN params EQ expression {
  let f = List.rev $6 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$8 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $4, None, f)
}
| LET REC LPAREN MULT RPAREN params EQ expression {
  let f = List.rev $6 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$8 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $4, None, f)
}
| LET REC LPAREN DIV RPAREN params EQ expression {
  let f = List.rev $6 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$8 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $4, None, f)
}
| LET REC LPAREN NOT RPAREN params EQ expression {
  let f = List.rev $6 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$8 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $4, None, f)
}
| LET REC LPAREN AT RPAREN params COLON styp EQ expression {
  let f = List.rev $6 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$10 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $4, Some $8, f)
}
| LET REC LPAREN AND RPAREN params COLON styp EQ expression {
  let f = List.rev $6 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$10 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $4, Some $8, f)
}
| LET REC LPAREN OR RPAREN params COLON styp EQ expression {
  let f = List.rev $6 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$10 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $4, Some $8, f)
}
| LET REC LPAREN LT RPAREN params COLON styp EQ expression {
  let f = List.rev $6 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$10 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $4, Some $8, f)
}
| LET REC LPAREN LTE RPAREN params COLON styp EQ expression {
  let f = List.rev $6 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$10 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $4, Some $8, f)
}
| LET REC LPAREN GT RPAREN params COLON styp EQ expression {
  let f = List.rev $6 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$10 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $4, Some $8, f)
}
| LET REC LPAREN GTE RPAREN params COLON styp EQ expression {
  let f = List.rev $6 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$10 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $4, Some $8, f)
}
| LET REC LPAREN EQ RPAREN params COLON styp EQ expression {
  let f = List.rev $6 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$10 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $4, Some $8, f)
}
| LET REC LPAREN NEQ RPAREN params COLON styp EQ expression {
  let f = List.rev $6 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$10 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $4, Some $8, f)
}
| LET REC LPAREN ADD RPAREN params COLON styp EQ expression {
  let f = List.rev $6 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$10 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $4, Some $8, f)
}
| LET REC LPAREN SUB RPAREN params COLON styp EQ expression {
  let f = List.rev $6 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$10 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $4, Some $8, f)
}
| LET REC LPAREN MULT RPAREN params COLON styp EQ expression {
  let f = List.rev $6 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$10 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $4, Some $8, f)
}
| LET REC LPAREN DIV RPAREN params COLON styp EQ expression {
  let f = List.rev $6 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$10 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $4, Some $8, f)
}
| LET REC LPAREN NOT RPAREN params COLON styp EQ expression {
  let f = List.rev $6 |> List.fold ~f:(fun e param -> Fun ($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$10 in
  LetRec ($symbolstartpos.Lexing.pos_lnum, $4, Some $8, f)
}
;

typedecls:
| typedecl { [$1] }
| typedecl BAR typedecls { $1 :: $3 }
;

typedecl:
| TYPE_IDENT { ($1, None) }
| TYPE_IDENT OF styps { ($1, Some $3) }
;

styps:
| simple_typ { [$1] }
| simple_typ MULT styps { $1 :: $3 }
;

eexpression:
| expression { $1 }
| expression SEMI eexpression { Seq ($symbolstartpos.Lexing.pos_lnum, $1, $3) }
;

single_expression:
| expression EOF { $1 }

expression:
| primary_expression { $1 }

| LET VALUE_IDENT COLON styp EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $2, Some $4, $6, $8) }
| LET VALUE_IDENT EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $2, None, $4, $6) }
| LET LPAREN SYMBOLIC_IDENT RPAREN COLON styp EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $3, Some $6, $8, $10) }
| LET LPAREN SYMBOLIC_IDENT RPAREN EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $3, None, $6, $8) }

| LET REC VALUE_IDENT COLON styp EQ expression IN expression { LetRecIn ($symbolstartpos.Lexing.pos_lnum, $3, Some $5, $7, $9) }
| LET REC VALUE_IDENT EQ expression IN expression { LetRecIn ($symbolstartpos.Lexing.pos_lnum, $3, None, $5, $7) }
| LET REC LPAREN SYMBOLIC_IDENT RPAREN COLON styp EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $4, Some $7, $9, $11) }
| LET REC LPAREN SYMBOLIC_IDENT RPAREN EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $4, None, $7, $9) }

| overridden_default_symbolic_expression { $1 }

| FUN params ARROW expression { List.fold $2 ~f:(fun e param -> Fun($symbolstartpos.Lexing.pos_lnum, param, e)) ~init:$4 }

| MATCH primary_expression WITH match_branches { Match ($symbolstartpos.Lexing.pos_lnum, $2, $4) }
;

overridden_default_symbolic_expression:
| LET LPAREN AT RPAREN COLON styp EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $3, Some $6, $8, $10) }
| LET LPAREN AND RPAREN COLON styp EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $3, Some $6, $8, $10) }
| LET LPAREN OR RPAREN COLON styp EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $3, Some $6, $8, $10) }
| LET LPAREN LT RPAREN COLON styp EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $3, Some $6, $8, $10) }
| LET LPAREN LTE RPAREN COLON styp EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $3, Some $6, $8, $10) }
| LET LPAREN GT RPAREN COLON styp EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $3, Some $6, $8, $10) }
| LET LPAREN GTE RPAREN COLON styp EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $3, Some $6, $8, $10) }
| LET LPAREN EQ RPAREN COLON styp EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $3, Some $6, $8, $10) }
| LET LPAREN NEQ RPAREN COLON styp EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $3, Some $6, $8, $10) }
| LET LPAREN ADD RPAREN COLON styp EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $3, Some $6, $8, $10) }
| LET LPAREN SUB RPAREN COLON styp EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $3, Some $6, $8, $10) }
| LET LPAREN MULT RPAREN COLON styp EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $3, Some $6, $8, $10) }
| LET LPAREN DIV RPAREN COLON styp EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $3, Some $6, $8, $10) }
| LET LPAREN NOT RPAREN COLON styp EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $3, Some $6, $8, $10) }

| LET LPAREN AT RPAREN EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $3, None, $6, $8) }
| LET LPAREN AND RPAREN EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $3, None, $6, $8) }
| LET LPAREN OR RPAREN EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $3, None, $6, $8) }
| LET LPAREN LT RPAREN EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $3, None, $6, $8) }
| LET LPAREN LTE RPAREN EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $3, None, $6, $8) }
| LET LPAREN GT RPAREN EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $3, None, $6, $8) }
| LET LPAREN GTE RPAREN EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $3, None, $6, $8) }
| LET LPAREN EQ RPAREN EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $3, None, $6, $8) }
| LET LPAREN NEQ RPAREN EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $3, None, $6, $8) }
| LET LPAREN ADD RPAREN EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $3, None, $6, $8) }
| LET LPAREN SUB RPAREN EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $3, None, $6, $8) }
| LET LPAREN MULT RPAREN EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $3, None, $6, $8) }
| LET LPAREN DIV RPAREN EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $3, None, $6, $8) }
| LET LPAREN NOT RPAREN EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $3, None, $6, $8) }

| LET REC LPAREN AT RPAREN COLON styp EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $4, Some $7, $9, $11) }
| LET REC LPAREN AND RPAREN COLON styp EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $4, Some $7, $9, $11) }
| LET REC LPAREN OR RPAREN COLON styp EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $4, Some $7, $9, $11) }
| LET REC LPAREN LT RPAREN COLON styp EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $4, Some $7, $9, $11) }
| LET REC LPAREN LTE RPAREN COLON styp EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $4, Some $7, $9, $11) }
| LET REC LPAREN GT RPAREN COLON styp EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $4, Some $7, $9, $11) }
| LET REC LPAREN GTE RPAREN COLON styp EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $4, Some $7, $9, $11) }
| LET REC LPAREN EQ RPAREN COLON styp EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $4, Some $7, $9, $11) }
| LET REC LPAREN NEQ RPAREN COLON styp EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $4, Some $7, $9, $11) }
| LET REC LPAREN ADD RPAREN COLON styp EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $4, Some $7, $9, $11) }
| LET REC LPAREN SUB RPAREN COLON styp EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $4, Some $7, $9, $11) }
| LET REC LPAREN MULT RPAREN COLON styp EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $4, Some $7, $9, $11) }
| LET REC LPAREN DIV RPAREN COLON styp EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $4, Some $7, $9, $11) }
| LET REC LPAREN NOT RPAREN COLON styp EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $4, Some $7, $9, $11) }

| LET REC LPAREN AT RPAREN EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $4, None, $7, $9) }
| LET REC LPAREN AND RPAREN EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $4, None, $7, $9) }
| LET REC LPAREN OR RPAREN EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $4, None, $7, $9) }
| LET REC LPAREN LT RPAREN EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $4, None, $7, $9) }
| LET REC LPAREN LTE RPAREN EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $4, None, $7, $9) }
| LET REC LPAREN GT RPAREN EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $4, None, $7, $9) }
| LET REC LPAREN GTE RPAREN EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $4, None, $7, $9) }
| LET REC LPAREN EQ RPAREN EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $4, None, $7, $9) }
| LET REC LPAREN NEQ RPAREN EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $4, None, $7, $9) }
| LET REC LPAREN ADD RPAREN EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $4, None, $7, $9) }
| LET REC LPAREN SUB RPAREN EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $4, None, $7, $9) }
| LET REC LPAREN MULT RPAREN EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $4, None, $7, $9) }
| LET REC LPAREN DIV RPAREN EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $4, None, $7, $9) }
| LET REC LPAREN NOT RPAREN EQ expression IN expression { LetIn ($symbolstartpos.Lexing.pos_lnum, $4, None, $7, $9) }
;

primary_expression:
| simple_expression { $1 }
| complex_expression { $1 }
| symbolic_application { $1 }
| applicative_expression applicable_expressions { List.fold $2 ~f:(fun f arg -> App ($symbolstartpos.Lexing.pos_lnum, f, arg)) ~init:$1 }
| LPAREN eexpression RPAREN { $2 }
;

symbolic_application:
| binop { $1 }
| numop { $1 }
| unop { $1 }
| listop { $1 }
| SYMBOLIC_IDENT primary_expression { App ($symbolstartpos.Lexing.pos_lnum, Var ($symbolstartpos.Lexing.pos_lnum, $1), $2) }
| primary_expression SYMBOLIC_IDENT primary_expression { App ($symbolstartpos.Lexing.pos_lnum, App ($symbolstartpos.Lexing.pos_lnum, Var ($symbolstartpos.Lexing.pos_lnum, $2), $1), $3) }
;

applicable_expressions:
| applicable_expression { [$1] }
| applicable_expression applicable_expressions { $1 :: $2 }

applicative_expression:
| VALUE_IDENT { Var ($symbolstartpos.Lexing.pos_lnum, $1) }
| LPAREN eexpression RPAREN { $2 }
;

applicable_expression:
| simple_expression { $1 }
/* Below is not great */
| LBRACKET RBRACKET { C ($symbolstartpos.Lexing.pos_lnum, Nil) }
| LBRACKET semi_sep_primary_expr  RBRACKET { $2 }
| TYPE_IDENT { C ($symbolstartpos.Lexing.pos_lnum, (Record ($1, []))) }
| LPAREN expression COMMA comma_sep_expr RPAREN { List.fold $4 ~f:(fun e' e -> C ($symbolstartpos.Lexing.pos_lnum, Tuple (e, e'))) ~init:$2 }
| LPAREN eexpression RPAREN { $2 }
;

simple_expression:
| literal { L ($symbolstartpos.Lexing.pos_lnum, $1) }
| VALUE_IDENT { Var ($symbolstartpos.Lexing.pos_lnum, $1) }
;

complex_expression:
| LBRACKET semi_sep_primary_expr  RBRACKET { $2 }
| complex_literal { C ($symbolstartpos.Lexing.pos_lnum, $1) }
;

literal:
| UNIT { Unit }
| INT { Int $1 }
| FLOAT { Float $1 }
| STRING { String $1 }
| CHAR { Char $1 }
| BOOL { Bool $1 }
;

match_branches:
| BAR match_branch { [$2] }
| BAR match_branch match_branches { $2 :: $3 }
;

match_branch:
| primary_match_branch ARROW primary_expression { ($1, $3) }
;

primary_match_branch:
| simple_branch { $1 }
| compund_branch { $1 }
;

simple_branch:
| literal { ML ($symbolstartpos.Lexing.pos_lnum, $1) }
| VALUE_IDENT { MVar ($symbolstartpos.Lexing.pos_lnum, $1) }
| UNDERSCORE { Blank ($symbolstartpos.Lexing.pos_lnum) }
| TYPE_IDENT { MRecord ($symbolstartpos.Lexing.pos_lnum, $1, []) }
;

compund_branch:
                                                                                 
| LPAREN primary_match_branch COMMA mtuple RPAREN { MTuple ($symbolstartpos.Lexing.pos_lnum, $2, $4) }
| TYPE_IDENT simple_branch { MRecord ($symbolstartpos.Lexing.pos_lnum, $1, [$2]) }
| TYPE_IDENT LPAREN comma_sep_match_branch RPAREN { MRecord ($symbolstartpos.Lexing.pos_lnum, $1, $3) }
| LBRACKET RBRACKET { let p =  $symbolstartpos in MNil ($symbolstartpos.Lexing.pos_lnum) }
| primary_match_branch DOUBLE_COLON primary_match_branch { MCons ($symbolstartpos.Lexing.pos_lnum, $1, $3) }
;

mtuple:
| primary_match_branch { $1 }
| primary_match_branch COMMA mtuple { MTuple ($symbolstartpos.Lexing.pos_lnum, $1, $3) }

comma_sep_match_branch:
| primary_match_branch { [$1] }
| primary_match_branch COMMA comma_sep_match_branch { $1 :: $3 }
;

complex_literal:
| LPAREN expression COMMA tuple RPAREN { Tuple ($2, $4) }
| TYPE_IDENT { Record ($1, []) }
| TYPE_IDENT LPAREN comma_sep_expr RPAREN { Record ($1, $3) }
| LBRACKET RBRACKET { Nil }
| primary_expression DOUBLE_COLON primary_expression { Cons ($1, $3) }
;

tuple:
| expression { $1 }
| expression COMMA tuple { C ($symbolstartpos.Lexing.pos_lnum, Tuple ($1, $3)) }

unop:
| NOT primary_expression { App ($symbolstartpos.Lexing.pos_lnum, Var ($symbolstartpos.Lexing.pos_lnum, $1), $2) }
;

listop:
| primary_expression AT primary_expression { App ($symbolstartpos.Lexing.pos_lnum, App ($symbolstartpos.Lexing.pos_lnum, Var ($symbolstartpos.Lexing.pos_lnum, $2), $1), $3) }
;

binop:
| primary_expression AND primary_expression { App ($symbolstartpos.Lexing.pos_lnum, App ($symbolstartpos.Lexing.pos_lnum, Var ($symbolstartpos.Lexing.pos_lnum, $2), $1), $3) }
| primary_expression OR primary_expression { App ($symbolstartpos.Lexing.pos_lnum, App ($symbolstartpos.Lexing.pos_lnum, Var ($symbolstartpos.Lexing.pos_lnum, $2), $1), $3) }
| primary_expression LT primary_expression { App ($symbolstartpos.Lexing.pos_lnum, App ($symbolstartpos.Lexing.pos_lnum, Var ($symbolstartpos.Lexing.pos_lnum, $2), $1), $3) }
| primary_expression LTE primary_expression { App ($symbolstartpos.Lexing.pos_lnum, App ($symbolstartpos.Lexing.pos_lnum, Var ($symbolstartpos.Lexing.pos_lnum, $2), $1), $3) }
| primary_expression GT primary_expression { App ($symbolstartpos.Lexing.pos_lnum, App ($symbolstartpos.Lexing.pos_lnum, Var ($symbolstartpos.Lexing.pos_lnum, $2), $1), $3) }
| primary_expression GTE primary_expression { App ($symbolstartpos.Lexing.pos_lnum, App ($symbolstartpos.Lexing.pos_lnum, Var ($symbolstartpos.Lexing.pos_lnum, $2), $1), $3) }
| primary_expression EQ primary_expression { App ($symbolstartpos.Lexing.pos_lnum, App ($symbolstartpos.Lexing.pos_lnum, Var ($symbolstartpos.Lexing.pos_lnum, $2), $1), $3) }
| primary_expression NEQ primary_expression { App ($symbolstartpos.Lexing.pos_lnum, App ($symbolstartpos.Lexing.pos_lnum, Var ($symbolstartpos.Lexing.pos_lnum, $2), $1), $3) }
;

numop:
| primary_expression ADD primary_expression { App ($symbolstartpos.Lexing.pos_lnum, App ($symbolstartpos.Lexing.pos_lnum, Var ($symbolstartpos.Lexing.pos_lnum, $2), $1), $3) }
| primary_expression SUB primary_expression { App ($symbolstartpos.Lexing.pos_lnum, App ($symbolstartpos.Lexing.pos_lnum, Var ($symbolstartpos.Lexing.pos_lnum, $2), $1), $3) }
| primary_expression MULT primary_expression { App ($symbolstartpos.Lexing.pos_lnum, App ($symbolstartpos.Lexing.pos_lnum, Var ($symbolstartpos.Lexing.pos_lnum, $2), $1), $3) }
| primary_expression DIV primary_expression { App ($symbolstartpos.Lexing.pos_lnum, App ($symbolstartpos.Lexing.pos_lnum, Var ($symbolstartpos.Lexing.pos_lnum, $2), $1), $3) }
;

comma_sep_expr:
| expression { [$1] }
| expression COMMA comma_sep_expr { $1 :: $3 }
;

semi_sep_primary_expr:
| primary_expression { C ($symbolstartpos.Lexing.pos_lnum, Cons ($1, C ($symbolstartpos.Lexing.pos_lnum, Nil))) }
| primary_expression SEMI semi_sep_primary_expr { C ($symbolstartpos.Lexing.pos_lnum, Cons ($1, $3)) }
;

params:
| VALUE_IDENT { [$1] }
| VALUE_IDENT params { $1 :: $2 }

styp:
| typ { $1 }
;

ssimple_typ:
| TSECRET LPAREN ssimple_typ RPAREN { TSecret ($symbolstartpos.Lexing.pos_lnum, $3) }
| TPUBLIC LPAREN ssimple_typ RPAREN { TPublic ($symbolstartpos.Lexing.pos_lnum, $3) }
| simple_typ { $1 }
| ssimple_typ TLIST { TList ($symbolstartpos.Lexing.pos_lnum, $1) }
;

simple_typ:
| TINT { TInt ($symbolstartpos.Lexing.pos_lnum) }
| TFLOAT { TFloat ($symbolstartpos.Lexing.pos_lnum) }
| TBOOL { TBool ($symbolstartpos.Lexing.pos_lnum) }
| TSTRING { TString ($symbolstartpos.Lexing.pos_lnum) }
| TUNIT { TUnit ($symbolstartpos.Lexing.pos_lnum) }
| TCHAR { TChar ($symbolstartpos.Lexing.pos_lnum) }
| VALUE_IDENT { TRecord ($symbolstartpos.Lexing.pos_lnum, $1) }
| LPAREN styp RPAREN { $2 }
;

typ:
| ssimple_typ { $1 }
| compound_typ { $1 }
;

compound_typ:
| TSECRET LPAREN compound_typ RPAREN { TSecret ($symbolstartpos.Lexing.pos_lnum, $3) }
| TPUBLIC LPAREN compound_typ RPAREN { TPublic ($symbolstartpos.Lexing.pos_lnum, $3) }
| ssimple_typ ARROW arrow_separated_ssimple_typs { List.fold $3 ~f:(fun t t' -> TFun ($symbolstartpos.Lexing.pos_lnum, t, t')) ~init:$1 }
| ssimple_typ MULT mult_separated_ssimple_typs { List.fold $3 ~f:(fun t t' -> TTuple ($symbolstartpos.Lexing.pos_lnum, t, t')) ~init:$1 }
;

arrow_separated_ssimple_typs:
| ssimple_typ { [$1] }
| TSECRET LPAREN compound_typ RPAREN { [TSecret ($symbolstartpos.Lexing.pos_lnum, $3)] }
| TPUBLIC LPAREN compound_typ RPAREN { [TPublic ($symbolstartpos.Lexing.pos_lnum, $3)] }
| ssimple_typ ARROW arrow_separated_ssimple_typs { $1 :: $3 }
;

mult_separated_ssimple_typs:
| ssimple_typ { [$1] }
| TSECRET LPAREN compound_typ RPAREN { [TSecret ($symbolstartpos.Lexing.pos_lnum, $3)] }
| TPUBLIC LPAREN compound_typ RPAREN { [TPublic ($symbolstartpos.Lexing.pos_lnum, $3)] }
| ssimple_typ MULT mult_separated_ssimple_typs { $1 :: $3 }
;

%%
