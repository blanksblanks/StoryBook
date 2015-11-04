type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | COMMA
  | PERIOD
  | APOST
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | MOD
  | token
  | EQ
  | NOT
  | AND
  | OR
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | RETURNS
  | IF
  | ELSE
  | FOR
  | WHILE
  | NUMBER
  | BOOL
  | TRUE
  | FALSE
  | STRING
  | CHAR
  | FUNCTION
  | CLASS
  | METHOD
  | IVAR
  | LIT_INT of (int)
  | LIT_BOOL of (bool)
  | LIT_STRING of (string)
  | LIT_CHAR of (char)
  | ID of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
