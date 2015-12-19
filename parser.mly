%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA PERIOD APOST
%token PLUS MINUS TIMES DIVIDE ASSIGN MOD
%token EQ NOT AND OR NEQ LT LEQ GT GEQ
%token ENDWITH
%token RETURNS IF ELSE FOR WHILE
%token LIST  /*NULL */
%token VOID NUMBER BOOL TRUE FALSE STRING CHAR FUNCTION
/*%token SUBTYPE */
%token CHARACTER METHOD TRAIT NEW MY
%token <float> LIT_NUM
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
%right NEW
%right NOT
%left COMMA APOST /* function call and member access */

%start program
%type <Ast.program> program

%%

/* Program is comprised of class declarations and function declarations */
program:
  decls EOF { $1 }

decls:
   /* nothing */ { [], [] }
 | decls cdecl { ($2 :: fst $1), snd $1 } /* class decl */
 | decls fdecl { fst $1, ($2 :: snd $1) } /* func decl */

/* Function declarations */
fdecl:
   FUNCTION ID LPAREN formals_opt RPAREN RETURNS type_label LBRACE stmt_list RBRACE
     { { fname = $2;
	       fformals = $4;
         freturn = $7;
	       fbody = List.rev $9 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

/* Formal param list. */
/* Params are represented as variable declarations with no expr for assignment */
formal_list:
    type_label ID
      { [ { vtype = $1;
            vname = $2;
            vexpr = Noexpr } ] }
  | formal_list SEMI type_label ID
      { { vtype = $3;
          vname = $4;
          vexpr = Noexpr } :: $1}

/* Data type names */
type_label:
   VOID    { Void }
 | NUMBER  { Number }
 | BOOL    { Boolean }
 | STRING  { String }
 | CHAR    { Char }
 | CHARACTER ID    { Object($2) }
 | ID LIST {List($1)}

/* Variable Declarations */
vdecl_list:
  /* nothing */ { [] }
  | vdecl_list vdecl { $2 :: $1}

vdecl:
    /* Uninitialized regular variable */
    type_label ID PERIOD
      { { vtype=$1;
          vname=$2;
          vexpr = Noexpr } }
  /* Uninitialized instance variable */
  | type_label TRAIT ID PERIOD
      { { vtype = $1;
          vname = $3;
          vexpr = Noexpr } }

  /* Initialized regular variable */
  | type_label ID ASSIGN expr PERIOD
      { { vtype = $1;
          vname = $2;
          vexpr = $4 } }
  /* Uninitialized instance variable */
  | type_label TRAIT ID ASSIGN expr PERIOD
      { { vtype = $1;
          vname = $3;
          vexpr = $5 } }

/* Character (Class) Declarations */
cdecl:
    CHARACTER ID LPAREN formals_opt RPAREN LBRACE vdecl_list action_list RBRACE
    {{  cname = $2;
        cparent = $2;
        cformals = $4;
        cinstvars = $7;
        cactions = $8;
    }}

  | CHARACTER ID ASSIGN ID LPAREN formals_opt RPAREN LBRACE vdecl_list action_list RBRACE
    {{  
        cname = $2;
        cparent = $4;
        cformals = $6;
        cinstvars = $9;
        cactions = $10;
    }}

/* Action (Method) Declarations */
action_list:
  /* nothing */ {[]}
  | action_list adecl {$2::$1}

adecl:
  METHOD ID LPAREN formals_opt RPAREN RETURNS type_label LBRACE stmt_list RBRACE
  {{
     aname = $2;
     aformals = $4;
     areturn = $7;
     abody = List.rev $9;
  }}

/* Statements */
stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

/* added vdecl to statements so that stmt list could include vdecls */
stmt:
    expr PERIOD { Expr($1) }
  | vdecl {VarDecl($1)}
  | ENDWITH LPAREN expr RPAREN PERIOD { Return($3) }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN stmt SEMI expr SEMI expr RPAREN stmt 
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt{ While($3, $5) }

/* Expressions */
expr:
    LIT_NUM          {LitNum($1)}
  | LIT_BOOL         {LitBool($1)}
  | LIT_STRING       {LitString($1)}
  | LIT_CHAR         {LitChar($1)}
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
  | MY ID            {Access("my", $2)}
  | ID LBRACK expr RBRACK {ListAccess($1, $3)} /* myList [1 + 1] */
  | ID APOST ID ASSIGN expr {TraitAssign($1, $3, $5)} /* member assign */
  | ID LBRACK expr RBRACK ASSIGN expr {ListAssign($1, $3, $6)}
  | ID LPAREN actuals_opt RPAREN {FCall($1, $3)} /* function call */
  | ID COMMA ID LPAREN actuals_opt RPAREN {ACall($1, $3, $5)} /* action call */
  | NEW ID LPAREN actuals_opt RPAREN {Instantiate($2, $4)} /*object declaration  */
  | NEW ID LIST LBRACK expr RBRACK {ListInstantiate($2, $5)} /* new int list[5 + 3]  -> ListInstantiate (int, 8) */
  | LPAREN expr RPAREN {$2}

/* Actual Parameters */
actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list SEMI expr { $3 :: $1 }
