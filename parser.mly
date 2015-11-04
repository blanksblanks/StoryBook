%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA PERIOD APOST
%token PLUS MINUS TIMES DIVIDE ASSIGN MOD
token EQ NOT AND OR NEQ LT LEQ GT GEQ
/*%token ENDWIDTH ELIF*/
%token RETURNS IF ELSE FOR WHILE 
/*%token LIST NULL */
%token NUMBER BOOL TRUE FALSE STRING CHAR FUNCTION
/*%token SUBTYPE MAIN CLASS METHOD IVAR NEW SAY*/
%token CLASS METHOD IVAR
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
   /* nothing */ { [], [], [] }
 | decls classdecl { ($2 :: fst $1), snd $1 } /* change to class decl */
 | decls fdecl { fst $1, ($2 :: snd $1) }

fdecl:
   FUNCTION ID LPAREN formals_opt RPAREN RETURNS type_label LBRACE vdecl_list stmt_list RBRACE
     { { fname = $2;
	 formals = $4;
	 locals = List.rev $9;
	 body = List.rev $10 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    type_label ID                   { [($1, $2)] } /* tuple (type, id) */
  | formal_list SEMI type_label ID { ($3, $4) :: $1 } /* params are separated by ';' */

type_label:
   NUMBER  { Int }
 | BOOL    { Bool }
 | STRING  { String }
 | CHAR    { Char }
 
vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }
 
vdecl:
   type_label ID PERIOD { ($1, $2) } /* tuple (type, id) */

classdecl:
  CLASS ID LPAREN formals_opt RPAREN LBRACE vdecl_list action_list RBRACE
  { { cname: $2;
      ivars : $4;
      actions: $8; }}

action_list:
  /* nothing */ {[]}
  | action_list actiondecl {$2::$1}

actiondecl:
  METHOD ID LPAREN formals_opt RPAREN RETURNS type_label LBRACE vdecl_list stmt_list RBRACE
  {{
     action_name: $2;
     formals: $4;
     locals: List.rev $9;
     body: List.rev $10;
  }}

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr PERIOD { Expr($1) }
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
    LIT_INT          {0}
  | LIT_BOOL         {0}
  | LIT_STRING       {0}
  | LIT_CHAR         {0}
  | ID               {0}
  | expr PLUS   expr {0}
  | expr MINUS  expr {0}
  | expr TIMES  expr {0}
  | expr DIVIDE expr {0}
  | expr MOD    expr {0}
  | expr EQ     expr {0}
  | expr NEQ    expr {0}
  | expr LT     expr {0}
  | expr LEQ    expr {0}
  | expr GT     expr {0}
  | expr GEQ    expr {0}
  | expr OR     expr {0}
  | expr AND    expr {0}
  | NOT expr {0}
  | ID ASSIGN expr   {0} /* variable assign */
  | ID APOST ID      {0} /* member access */
  | ID APOST ID ASSIGN expr {0} /* member assign */
  | ID LPAREN actuals_opt RPAREN {0} /* function call */
  | ID COMMA ID LPAREN acuals_opt RPAREN {0} /* action call */
  | LPAREN expr RPAREN {0}

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list SEMI expr { $3 :: $1 }
