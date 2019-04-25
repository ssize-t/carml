%{

open Ast

%}

/* Type declarations */
%token TINT TFLOAT TBOOL TSTRING TCHAR TUNIT TLIST TSECRET
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

%token <string> VALUE_IDENT TYPE_IDENT

%token LET IN FUN

%token LPAREN RPAREN
%token COMMA COLON DOUBLE_COLON

%token MATCH TYPE BAR OF WITH DOUBLE_ARROW UNDERSCORE DOUBLE_SEMI

%token EOF

%right ARROW

%left AND OR
%right EQ NEQ
%left GT GTE LTE LT
%left ADD SUB
%left MULT DIV
%left NOT

%right DOUBLE_COLON

%start program
%type <Ast.program> program

%%

program:
| statements EOF { $1 }
;

statements:
| statement DOUBLE_SEMI { [$1] }
| statement DOUBLE_SEMI statements { $1 :: $3 }
;

statement:
| LET VALUE_IDENT COLON styp EQ eexpression { Let ($2, $4, $6) }
| LET VALUE_IDENT params COLON styp EQ eexpression { Let ($2, $5, Fun ($3, $5, $7)) }
| TYPE VALUE_IDENT EQ typedecls { Type ($2, $4) }
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
| expression SEMI eexpression { Seq ($1, $3) }
;

expression:
| primary_expression { $1 }
| LET VALUE_IDENT COLON styp EQ expression IN expression { LetIn ($2, $4, $6, $8) }
| FUN params COLON styp DOUBLE_ARROW expression { Fun ($2, $4, $6) }
| MATCH primary_expression WITH match_branches { Match ($2, $4) }
;

primary_expression:
| simple_expression { $1 }
| complex_expression { $1 }
| applicative_expression applicable_expressions { App ($1, $2) }
| LPAREN eexpression RPAREN { $2 }
;

applicative_expression:
| VALUE_IDENT { Var $1 }
| LPAREN eexpression RPAREN { $2 }
;

applicable_expression:
| simple_expression { $1 }
/* Below is not great */
| TYPE_IDENT { C (Record ($1, [])) }
| LPAREN expression COMMA comma_sep_expr RPAREN { C (Tuple ($2 :: $4)) }
| LPAREN eexpression RPAREN { $2 }
;

applicable_expressions:
| applicable_expression { [$1] }
| applicable_expression applicable_expressions { $1 :: $2 }
;

simple_expression:
| literal { L $1 }
| VALUE_IDENT { Var $1 }
;

complex_expression:
| binop { $1 }
| numop { $1 }
| unop { $1 }
| complex_literal { C $1 }
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
| primary_match_branch COLON styp DOUBLE_ARROW primary_expression { ($1, $3, $5) }
;

primary_match_branch:
| simple_branch { $1 }
| compund_branch { $1 }
;

simple_branch:
| literal { ML $1 }
| VALUE_IDENT { MVar $1 }
| UNDERSCORE { Blank }
| TYPE_IDENT { MRecord ($1, []) }
;

compund_branch:
| LPAREN comma_sep_match_branch RPAREN { MTuple $2 }
| TYPE_IDENT simple_branch { MRecord ($1, [$2]) }
| TYPE_IDENT LPAREN comma_sep_match_branch RPAREN { MRecord ($1, $3) }
| LBRACKET RBRACKET { MList [] }
| LBRACKET semi_sep_match_branch RBRACKET { MList $2 }
/* Flatten me before use */
| primary_match_branch DOUBLE_COLON primary_match_branch { MList [$1; $3] }
;

comma_sep_match_branch:
| primary_match_branch { [$1] }
| primary_match_branch COMMA comma_sep_match_branch { $1 :: $3 }
;

semi_sep_match_branch:
| primary_match_branch { [$1] }
| primary_match_branch SEMI semi_sep_match_branch { $1 :: $3 }
;

complex_literal:
| LPAREN expression COMMA comma_sep_expr RPAREN { Tuple ($2 :: $4) }
| TYPE_IDENT { Record ($1, []) }
| TYPE_IDENT LPAREN comma_sep_expr RPAREN { Record ($1, $3) }
| LBRACKET RBRACKET { List [] }
| LBRACKET semi_sep_primary_expr  RBRACKET { List $2 }
/* Beware of nesting per https://caml.inria.fr/pub/docs/tutorial-camlp4/tutorial005.html */
| applicable_expression DOUBLE_COLON applicable_expression { List [$1; $3] }
;

unop:
| NOT primary_expression { UnOp (Not, $2) }
;

binop:
| primary_expression AND primary_expression { BinOp (And, $1, $3) }
| primary_expression OR primary_expression { BinOp (Or, $1, $3) }
| primary_expression LT primary_expression { BinOp (Lt, $1, $3) }
| primary_expression LTE primary_expression { BinOp (Lte, $1, $3) }
| primary_expression GT primary_expression { BinOp (Gt, $1, $3) }
| primary_expression GTE primary_expression { BinOp (Gte, $1, $3) }
| primary_expression EQ primary_expression { BinOp (Eq, $1, $3) }
| primary_expression NEQ primary_expression { BinOp (Neq, $1, $3) }
;

numop:
| primary_expression ADD primary_expression { NumOp (Add, $1, $3) }
| primary_expression SUB primary_expression { NumOp (Sub, $1, $3) }
| primary_expression MULT primary_expression { NumOp (Mult, $1, $3) }
| primary_expression DIV primary_expression { NumOp (Div, $1, $3) }
;

comma_sep_expr:
| expression { [$1] }
| expression COMMA comma_sep_expr { $1 :: $3 }
;

semi_sep_primary_expr:
| primary_expression { [$1] }
| primary_expression SEMI semi_sep_primary_expr { $1 :: $3 }
;

params:
| VALUE_IDENT { [$1] }
| VALUE_IDENT params { $1 :: $2 }

styp:
| typ { $1 }
;

ssimple_typ:
| TSECRET LPAREN ssimple_typ RPAREN { TSecret $3 }
| simple_typ { $1 }
| ssimple_typ TLIST { TList $1 }
;

simple_typ:
| TINT { TInt }
| TFLOAT { TFloat }
| TBOOL { TBool }
| TSTRING { TString }
| TUNIT { TUnit }
| TCHAR { TChar }
| VALUE_IDENT { TRecord $1 }
| LPAREN styp RPAREN { $2 }
;

typ:
| compound_typ { $1 }
;

compound_typ:
| TSECRET LPAREN ccompound_typ RPAREN { TSecret $3 }
| ssimple_typ { $1 }
| compound_typ ARROW compound_typ { TFun ($1, $3) }
| ssimple_typ MULT mult_separated_compund_typs { TTuple ($1 :: $3) }
;

mult_separated_compund_typs:
| ssimple_typ { [$1] }
| ssimple_typ MULT mult_separated_compund_typs { $1 :: $3 }
;

ccompound_typ:
| ssimple_typ ARROW compound_typ { TFun ($1, $3) }
| ssimple_typ MULT mult_separated_compund_typs { TTuple ($1 :: $3) }
;

%%
