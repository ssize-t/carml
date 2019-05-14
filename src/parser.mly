%{

open Ast

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
%token LT LTE EQ GT GTE AND OR NEQ NOT

/* NumOps */
%token SUB ADD MULT DIV

/* Identifiers */
%token <string> VALUE_IDENT TYPE_IDENT

/* Keywords */
%token LET IN FUN REC
%token MATCH TYPE BAR OF WITH DOUBLE_ARROW UNDERSCORE

/* Operators */
%right DOUBLE_COLON AT

%token LPAREN RPAREN
%token COMMA COLON DOUBLE_COLON AT

%token EOF

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
| LET VALUE_IDENT COLON styp EQ eexpression { let p = $symbolstartpos in Let (Loc (p.Lexing.pos_lnum, Empty), $2, $4, $6) }
| LET REC VALUE_IDENT COLON styp EQ eexpression { let p = $symbolstartpos in LetRec (Loc (p.Lexing.pos_lnum, Empty), $3, $5, $7) }
| LET VALUE_IDENT params COLON styp EQ eexpression { let p = $symbolstartpos in Let (Loc (p.Lexing.pos_lnum, Empty), $2, $5, Fun (Loc (p.Lexing.pos_lnum, Empty), $3, $5, $7)) }
| LET REC VALUE_IDENT params COLON styp EQ eexpression { let p = $symbolstartpos in LetRec (Loc (p.Lexing.pos_lnum, Empty), $3, $6, Fun (Loc (p.Lexing.pos_lnum, Empty), $4, $6, $8)) }
| TYPE VALUE_IDENT EQ typedecls { let p = $symbolstartpos in Type (Loc (p.Lexing.pos_lnum, Empty), $2, $4) }
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
| expression SEMI eexpression { let p = $symbolstartpos in Seq (Loc (p.Lexing.pos_lnum, Empty), $1, $3) }
;

single_expression:
| expression EOF { $1 }

expression:
| primary_expression { $1 }
| LET VALUE_IDENT COLON styp EQ expression IN expression { let p = $symbolstartpos in LetIn (Loc (p.Lexing.pos_lnum, Empty), $2, $4, $6, $8) }
| LET REC VALUE_IDENT COLON styp EQ expression IN expression { let p = $symbolstartpos in LetRecIn (Loc (p.Lexing.pos_lnum, Empty), $3, $5, $7, $9) }
| FUN params COLON styp DOUBLE_ARROW expression { let p = $symbolstartpos in Fun (Loc (p.Lexing.pos_lnum, Empty), $2, $4, $6) }
| MATCH LPAREN primary_expression COLON styp RPAREN WITH match_branches { let p = $symbolstartpos in Match (Loc (p.Lexing.pos_lnum, Empty), $3, $5, $8) }
;

primary_expression:
| simple_expression { $1 }
| complex_expression { $1 }
| applicative_expression typed_applicable_expressions { let p = $symbolstartpos in App (Loc (p.Lexing.pos_lnum, Empty), $1, $2) }
| LPAREN eexpression RPAREN { $2 }
;

typed_applicable_expression:
| LPAREN applicable_expression COLON styp RPAREN { ($2, $4) }
;

typed_applicable_expressions:
| typed_applicable_expression { [$1] }
| typed_applicable_expression typed_applicable_expressions { $1 :: $2 }

applicative_expression:
| VALUE_IDENT { let p = $symbolstartpos in Var (Loc (p.Lexing.pos_lnum, Empty), $1) }
| LPAREN eexpression RPAREN { $2 }
;

applicable_expression:
| simple_expression { $1 }
/* Below is not great */
| TYPE_IDENT { let p = $symbolstartpos in C (Loc (p.Lexing.pos_lnum, Empty), (Record ($1, []))) }
| LPAREN expression COMMA comma_sep_expr RPAREN { let p = $symbolstartpos in C (Loc (p.Lexing.pos_lnum, Empty), (Tuple ($2 :: $4))) }
| LPAREN eexpression RPAREN { $2 }
;

simple_expression:
| literal { let p = $symbolstartpos in L (Loc (p.Lexing.pos_lnum, Empty), $1) }
| VALUE_IDENT { let p = $symbolstartpos in Var (Loc (p.Lexing.pos_lnum, Empty), $1) }
;

complex_expression:
| binop { $1 }
| numop { $1 }
| unop { $1 }
| listop { $1 }
| LBRACKET semi_sep_primary_expr  RBRACKET { $2 }
| complex_literal { let p = $symbolstartpos in C (Loc (p.Lexing.pos_lnum, Empty), $1) }
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
| literal { let p = $symbolstartpos in ML (Loc (p.Lexing.pos_lnum, Empty), $1) }
| VALUE_IDENT { let p = $symbolstartpos in MVar (Loc (p.Lexing.pos_lnum, Empty), $1) }
| UNDERSCORE { let p = $symbolstartpos in Blank (Loc (p.Lexing.pos_lnum, Empty)) }
| TYPE_IDENT { let p = $symbolstartpos in MRecord (Loc (p.Lexing.pos_lnum, Empty), $1, []) }
;

compund_branch:
| LPAREN comma_sep_match_branch RPAREN { let p = $symbolstartpos in MTuple (Loc (p.Lexing.pos_lnum, Empty), $2) }
| TYPE_IDENT simple_branch { let p = $symbolstartpos in MRecord (Loc (p.Lexing.pos_lnum, Empty), $1, [$2]) }
| TYPE_IDENT LPAREN comma_sep_match_branch RPAREN { let p = $symbolstartpos in MRecord (Loc (p.Lexing.pos_lnum, Empty), $1, $3) }
| LBRACKET RBRACKET { let p =  $symbolstartpos in MNil (Loc (p.Lexing.pos_lnum, Empty)) }
| primary_match_branch DOUBLE_COLON primary_match_branch { let p = $symbolstartpos in MCons (Loc (p.Lexing.pos_lnum, Empty), $1, $3) }
;

comma_sep_match_branch:
| primary_match_branch { [$1] }
| primary_match_branch COMMA comma_sep_match_branch { $1 :: $3 }
;

complex_literal:
| LPAREN expression COMMA comma_sep_expr RPAREN { Tuple ($2 :: $4) }
| TYPE_IDENT { Record ($1, []) }
| TYPE_IDENT LPAREN comma_sep_expr RPAREN { Record ($1, $3) }
| LBRACKET RBRACKET { Nil }
| primary_expression DOUBLE_COLON primary_expression { Cons ($1, $3) }
;

unop:
| NOT primary_expression { let p = $symbolstartpos in UnOp (Loc (p.Lexing.pos_lnum, Empty), Not, $2) }
;

listop:
| primary_expression AT primary_expression { let p = $symbolstartpos in ListOp (Loc (p.Lexing.pos_lnum, Empty), Concat, $1, $3)}
;

binop:
| primary_expression AND primary_expression { let p = $symbolstartpos in BinOp (Loc (p.Lexing.pos_lnum, Empty), And, $1, $3) }
| primary_expression OR primary_expression { let p = $symbolstartpos in BinOp (Loc (p.Lexing.pos_lnum, Empty), Or, $1, $3) }
| primary_expression LT primary_expression { let p = $symbolstartpos in BinOp (Loc (p.Lexing.pos_lnum, Empty), Lt, $1, $3) }
| primary_expression LTE primary_expression { let p = $symbolstartpos in BinOp (Loc (p.Lexing.pos_lnum, Empty), Lte, $1, $3) }
| primary_expression GT primary_expression { let p = $symbolstartpos in BinOp (Loc (p.Lexing.pos_lnum, Empty), Gt, $1, $3) }
| primary_expression GTE primary_expression { let p = $symbolstartpos in BinOp (Loc (p.Lexing.pos_lnum, Empty), Gte, $1, $3) }
| primary_expression EQ primary_expression { let p = $symbolstartpos in BinOp (Loc (p.Lexing.pos_lnum, Empty), Eq, $1, $3) }
| primary_expression NEQ primary_expression { let p = $symbolstartpos in BinOp (Loc (p.Lexing.pos_lnum, Empty), Neq, $1, $3) }
;

numop:
| primary_expression ADD primary_expression { let p = $symbolstartpos in NumOp (Loc (p.Lexing.pos_lnum, Empty), Add, $1, $3) }
| primary_expression SUB primary_expression { let p = $symbolstartpos in NumOp (Loc (p.Lexing.pos_lnum, Empty), Sub, $1, $3) }
| primary_expression MULT primary_expression { let p = $symbolstartpos in NumOp (Loc (p.Lexing.pos_lnum, Empty), Mult, $1, $3) }
| primary_expression DIV primary_expression { let p = $symbolstartpos in NumOp (Loc (p.Lexing.pos_lnum, Empty), Div, $1, $3) }
;

comma_sep_expr:
| expression { [$1] }
| expression COMMA comma_sep_expr { $1 :: $3 }
;

semi_sep_primary_expr:
| primary_expression { let p = $symbolstartpos in C (Loc (p.Lexing.pos_lnum, Empty), Cons ($1, C (Loc (p.Lexing.pos_lnum, Empty), Nil))) }
| primary_expression SEMI semi_sep_primary_expr { let p = $symbolstartpos in C (Loc (p.Lexing.pos_lnum, Empty), Cons ($1, $3)) }
;

params:
| VALUE_IDENT { [$1] }
| VALUE_IDENT params { $1 :: $2 }

styp:
| typ { $1 }
;

ssimple_typ:
| TSECRET LPAREN ssimple_typ RPAREN { let p = $symbolstartpos in TSecret (Loc (p.Lexing.pos_lnum, Empty), $3) }
| TPUBLIC LPAREN ssimple_typ RPAREN { let p = $symbolstartpos in TPublic (Loc (p.Lexing.pos_lnum, Empty), $3) }
| simple_typ { $1 }
| ssimple_typ TLIST { let p = $symbolstartpos in TList (Loc (p.Lexing.pos_lnum, Empty), $1) }
;

simple_typ:
| TINT { let p = $symbolstartpos in TInt (Loc (p.Lexing.pos_lnum, Empty)) }
| TFLOAT { let p = $symbolstartpos in TFloat (Loc (p.Lexing.pos_lnum, Empty)) }
| TBOOL { let p = $symbolstartpos in TBool (Loc (p.Lexing.pos_lnum, Empty)) }
| TSTRING { let p = $symbolstartpos in TString (Loc (p.Lexing.pos_lnum, Empty)) }
| TUNIT { let p = $symbolstartpos in TUnit (Loc (p.Lexing.pos_lnum, Empty)) }
| TCHAR { let p = $symbolstartpos in TChar (Loc (p.Lexing.pos_lnum, Empty)) }
| VALUE_IDENT { let p = $symbolstartpos in TRecord (Loc (p.Lexing.pos_lnum, Empty), $1) }
| LPAREN styp RPAREN { $2 }
;

typ:
| ssimple_typ { $1 }
| compound_typ { $1 }
;

compound_typ:
| TSECRET LPAREN compound_typ RPAREN { let p = $symbolstartpos in TSecret (Loc (p.Lexing.pos_lnum, Empty), $3) }
| TPUBLIC LPAREN compound_typ RPAREN { let p = $symbolstartpos in TPublic (Loc (p.Lexing.pos_lnum, Empty), $3) }
| ssimple_typ ARROW arrow_separated_ssimple_typs { let p = $symbolstartpos in TFun (Loc (p.Lexing.pos_lnum, Empty), $1 :: $3) }
| ssimple_typ MULT mult_separated_ssimple_typs { let p = $symbolstartpos in TTuple (Loc (p.Lexing.pos_lnum, Empty), $1 :: $3) }
;

arrow_separated_ssimple_typs:
| ssimple_typ { [$1] }
| TSECRET LPAREN compound_typ RPAREN { let p = $symbolstartpos in [TSecret (Loc (p.Lexing.pos_lnum, Empty), $3)] }
| TPUBLIC LPAREN compound_typ RPAREN { let p = $symbolstartpos in [TPublic (Loc (p.Lexing.pos_lnum, Empty), $3)] }
| ssimple_typ ARROW arrow_separated_ssimple_typs { $1 :: $3 }
;

mult_separated_ssimple_typs:
| ssimple_typ { [$1] }
| TSECRET LPAREN compound_typ RPAREN { let p = $symbolstartpos in [TSecret (Loc (p.Lexing.pos_lnum, Empty), $3)] }
| TPUBLIC LPAREN compound_typ RPAREN { let p = $symbolstartpos in [TPublic (Loc (p.Lexing.pos_lnum, Empty), $3)] }
| ssimple_typ MULT mult_separated_ssimple_typs { $1 :: $3 }
;

%%
