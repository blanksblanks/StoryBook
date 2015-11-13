%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA PERIOD APOST
%token PLUS MINUS TIMES DIVIDE ASSIGN MOD
%token EQ NOT AND OR NEQ LT LEQ GT GEQ
/*%token ENDWIDTH ELIF*/
%token RETURNS IF ELSE FOR WHILE
/*%token LIST NULL */
%token NUMBER BOOL TRUE FALSE STRING CHAR FUNCTION
/*%token SUBTYPE MAIN CLASS METHOD IVAR NEW SAY*/
%token CHARACTER METHOD TRAIT NEW
%token <int> LIT_INT
%token <bool> LIT_BOOL
%token <string> LIT_STRING
%token <char> LIT_CHAR
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
/* %right NEW */
%right NOT
%left COMMA APOST /* function call and member access */

%start program
%type <Ast.program> program

%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { [], [] }
 | decls cdecl { ($2 :: fst $1), snd $1 } /* change to class decl */
 | decls fdecl { fst $1, ($2 :: snd $1) }

fdecl:
   FUNCTION ID LPAREN formals_opt RPAREN RETURNS type_label LBRACE stmt_list RBRACE
     { { fname = $2;
	       formals = $4;
	       body = List.rev $9 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    type_label ID                   { [Parameter($1, $2)] } /* tuple (type, id) */
  | formal_list SEMI type_label ID { Parameter($3, $4) :: $1 } /* params are separated by ';' */

type_label:
   NUMBER  { Number }
 | BOOL    { Boolean }
 | STRING  { String }
 | CHAR    { Char }
 | CHARACTER ID    { Object($2) }


vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
  type_label ID PERIOD { U_Var($1, $2) } /* uninitialized variable of primitive type */
| type_label TRAIT ID PERIOD { U_Var($1, $3) } /* uninitialized trait of primitive type */
/*| ID ID PERIOD {U_Var(Object($1), $2)}  uninitialized variable of character type */
| type_label ID ASSIGN expr PERIOD { I_Var($1, $2, $4)} /*initialized variable of primitive type */
| type_label TRAIT ID ASSIGN expr PERIOD { I_Var($1, $3, $5) } /* initialized trait */
/*| ID ID ASSIGN expr PERIOD {I_Var(Object($1), $2, $4)}  initialized object */

cdecl:
  CHARACTER ID LPAREN formals_opt RPAREN LBRACE vdecl_list action_list RBRACE
  {{  cname = $2;
      traits = $4;
      actions = $8;
  }}  /* traits = instance variables */

action_list:
  /* nothing */ {[]}
  | action_list adecl {$2::$1}

adecl:
  METHOD ID LPAREN formals_opt RPAREN RETURNS type_label LBRACE stmt_list RBRACE
  {{
     action_name = $2;
     formals = $4;
     body = List.rev $9;
  }}

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr PERIOD { Expr($1) }
  | vdecl {Var_Decl($1)}
  | RETURNS expr SEMI { Return($2) }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    LIT_INT          {Lit_int($1)}
  | LIT_BOOL         {Lit_bool($1)}
  | LIT_STRING       {Lit_string($1)}
  | LIT_CHAR         {Lit_char($1)}
  | ID               {Id($1)}
  | expr PLUS   expr {Binop($1, Add, $3)}
  | expr MINUS  expr {Binop($1, Sub, $3)}
  | expr TIMES  expr {Binop($1, Mult, $3)}
  | expr DIVIDE expr {Binop($1, Div, $3)}
  | expr MOD    expr {Binop($1, Mod, $3)}
  | expr EQ     expr {Binop($1, Equal, $3)}
  | expr NEQ    expr {Binop($1, Neq, $3)}
  | expr LT     expr {Binop($1, Less, $3)}
  | expr LEQ    expr {Binop($1, Leq, $3)}
  | expr GT     expr {Binop($1, Greater, $3)}
  | expr GEQ    expr {Binop($1, Geq, $3)}
  | expr OR     expr {Binop($1, OR, $3)}
  | expr AND    expr {Binop($1, AND, $3)}
  | NOT expr {Unop(NOT, $2)}
  | ID ASSIGN expr   {Assign($1, $3)} /* variable assign */
  | ID APOST ID      {Access($1, $3)} /* member access */
  | ID APOST ID ASSIGN expr {Trait_Assign($1, $3, $5)} /* member assign */
  | ID LPAREN actuals_opt RPAREN {FCall($1, $3)} /* function call */
  | ID COMMA ID LPAREN actuals_opt RPAREN {ACall($1, $3, $5)} /* action call */
 /* | NEW ID LPAREN actuals_opt RPAREN {0} object declaration  */
  | LPAREN expr RPAREN {$2}


actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list SEMI expr { $3 :: $1 }
