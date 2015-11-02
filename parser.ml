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
  | LIT_INT of (int)
  | LIT_BOOL of (bool)
  | LIT_STRING of (string)
  | LIT_CHAR of (char)
  | ID of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 51 "parser.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* LBRACK *);
  263 (* RBRACK *);
  264 (* COMMA *);
  265 (* PERIOD *);
  266 (* PLUS *);
  267 (* MINUS *);
  268 (* TIMES *);
  269 (* DIVIDE *);
  270 (* ASSIGN *);
  271 (* MOD *);
  272 (* token *);
  273 (* EQ *);
  274 (* NOT *);
  275 (* AND *);
  276 (* OR *);
  277 (* NEQ *);
  278 (* LT *);
  279 (* LEQ *);
  280 (* GT *);
  281 (* GEQ *);
  282 (* RETURNS *);
  283 (* IF *);
  284 (* ELSE *);
  285 (* FOR *);
  286 (* WHILE *);
  287 (* NUMBER *);
  288 (* BOOL *);
  289 (* TRUE *);
  290 (* FALSE *);
  291 (* STRING *);
  292 (* CHAR *);
  293 (* FUNCTION *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  294 (* LIT_INT *);
  295 (* LIT_BOOL *);
  296 (* LIT_STRING *);
  297 (* LIT_CHAR *);
  298 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\005\000\005\000\009\000\
\009\000\006\000\006\000\006\000\006\000\007\000\007\000\003\000\
\008\000\008\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\012\000\012\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\013\000\013\000\014\000\014\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\011\000\000\000\001\000\002\000\
\004\000\001\000\001\000\001\000\001\000\000\000\002\000\003\000\
\000\000\002\000\002\000\003\000\003\000\005\000\007\000\009\000\
\005\000\000\000\001\000\001\000\001\000\001\000\001\000\001\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\002\000\003\000\004\000\
\003\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\054\000\000\000\010\000\011\000\012\000\013\000\
\000\000\001\000\003\000\004\000\000\000\000\000\000\000\000\000\
\016\000\000\000\000\000\000\000\000\000\008\000\000\000\000\000\
\000\000\000\000\009\000\014\000\000\000\015\000\000\000\000\000\
\017\000\005\000\000\000\000\000\000\000\000\000\000\000\028\000\
\029\000\030\000\031\000\000\000\018\000\000\000\000\000\000\000\
\046\000\000\000\000\000\000\000\000\000\000\000\000\000\019\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\049\000\021\000\020\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\035\000\036\000\037\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\048\000\000\000\000\000\000\000\025\000\000\000\000\000\000\000\
\023\000\000\000\000\000\024\000"

let yydgoto = "\002\000\
\003\000\004\000\011\000\012\000\018\000\013\000\029\000\031\000\
\020\000\045\000\046\000\075\000\078\000\079\000"

let yysindex = "\011\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\231\254\000\000\000\000\000\000\232\254\043\255\041\255\022\255\
\000\000\052\255\005\255\059\255\049\255\000\000\022\255\022\255\
\027\255\077\255\000\000\000\000\022\255\000\000\038\255\100\255\
\000\000\000\000\100\255\100\255\107\255\108\255\111\255\000\000\
\000\000\000\000\000\000\000\255\000\000\184\000\095\000\044\255\
\000\000\182\255\100\255\100\255\100\255\100\255\100\255\000\000\
\100\255\100\255\100\255\100\255\100\255\100\255\100\255\100\255\
\100\255\100\255\100\255\100\255\100\255\000\000\000\000\000\000\
\118\000\200\000\087\255\141\000\200\000\103\255\120\255\200\000\
\026\255\026\255\000\000\000\000\000\000\248\000\232\000\216\000\
\248\000\086\255\086\255\086\255\086\255\131\255\100\255\131\255\
\000\000\100\255\094\255\133\255\000\000\200\000\131\255\100\255\
\000\000\134\255\131\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\140\255\
\000\000\000\000\000\000\142\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\085\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\165\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\135\255\000\000\144\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\008\255\000\000\000\000\058\255\000\000\145\255\007\255\
\207\255\232\255\000\000\000\000\000\000\166\000\143\255\004\255\
\171\000\002\000\038\000\047\000\072\000\000\000\135\255\000\000\
\000\000\000\000\090\255\000\000\000\000\104\255\000\000\147\255\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\122\000\000\000\000\000\028\000\000\000\120\000\
\000\000\253\255\224\255\167\255\000\000\000\000"

let yytablesize = 529
let yytable = "\047\000\
\010\000\054\000\049\000\050\000\044\000\100\000\044\000\047\000\
\027\000\047\000\027\000\001\000\044\000\055\000\106\000\047\000\
\014\000\015\000\073\000\074\000\076\000\077\000\080\000\044\000\
\081\000\082\000\083\000\084\000\085\000\086\000\087\000\088\000\
\089\000\090\000\091\000\092\000\093\000\059\000\060\000\032\000\
\061\000\033\000\034\000\019\000\016\000\032\000\022\000\033\000\
\071\000\017\000\025\000\026\000\005\000\006\000\021\000\035\000\
\007\000\008\000\052\000\023\000\052\000\035\000\074\000\036\000\
\037\000\102\000\038\000\039\000\027\000\036\000\037\000\074\000\
\038\000\039\000\024\000\040\000\041\000\042\000\043\000\044\000\
\028\000\040\000\041\000\042\000\043\000\044\000\017\000\095\000\
\017\000\017\000\099\000\022\000\101\000\022\000\022\000\057\000\
\058\000\059\000\060\000\105\000\061\000\032\000\017\000\108\000\
\053\000\097\000\053\000\022\000\051\000\052\000\017\000\017\000\
\053\000\017\000\017\000\022\000\022\000\035\000\022\000\022\000\
\098\000\103\000\017\000\017\000\017\000\017\000\017\000\022\000\
\022\000\022\000\022\000\022\000\032\000\104\000\033\000\026\000\
\107\000\040\000\041\000\042\000\043\000\044\000\006\000\045\000\
\007\000\045\000\050\000\051\000\035\000\026\000\030\000\045\000\
\048\000\000\000\000\000\000\000\036\000\037\000\000\000\038\000\
\039\000\045\000\045\000\000\000\000\000\032\000\000\000\032\000\
\040\000\041\000\042\000\043\000\044\000\032\000\032\000\032\000\
\032\000\032\000\000\000\032\000\000\000\032\000\072\000\032\000\
\032\000\032\000\032\000\032\000\032\000\032\000\000\000\057\000\
\058\000\059\000\060\000\000\000\061\000\000\000\062\000\000\000\
\063\000\064\000\065\000\066\000\067\000\068\000\069\000\033\000\
\000\000\033\000\000\000\000\000\000\000\000\000\000\000\033\000\
\033\000\033\000\000\000\000\000\000\000\000\000\000\000\033\000\
\000\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
\034\000\000\000\034\000\000\000\000\000\000\000\000\000\000\000\
\034\000\034\000\034\000\000\000\000\000\000\000\000\000\000\000\
\034\000\000\000\034\000\034\000\034\000\034\000\034\000\034\000\
\034\000\000\000\040\000\000\000\040\000\000\000\000\000\000\000\
\000\000\000\000\040\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\040\000\000\000\040\000\040\000\040\000\040\000\
\040\000\040\000\040\000\000\000\000\000\000\000\000\000\005\000\
\006\000\000\000\000\000\007\000\008\000\009\000\041\000\000\000\
\041\000\000\000\000\000\000\000\000\000\000\000\041\000\042\000\
\000\000\042\000\000\000\000\000\000\000\000\000\041\000\042\000\
\041\000\041\000\041\000\041\000\041\000\041\000\041\000\042\000\
\000\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
\043\000\000\000\043\000\000\000\000\000\000\000\000\000\000\000\
\043\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\043\000\000\000\043\000\043\000\043\000\043\000\043\000\043\000\
\043\000\070\000\000\000\000\000\000\000\000\000\000\000\000\000\
\057\000\058\000\059\000\060\000\000\000\061\000\000\000\062\000\
\000\000\063\000\064\000\065\000\066\000\067\000\068\000\069\000\
\094\000\000\000\000\000\000\000\000\000\000\000\000\000\057\000\
\058\000\059\000\060\000\000\000\061\000\000\000\062\000\000\000\
\063\000\064\000\065\000\066\000\067\000\068\000\069\000\096\000\
\000\000\000\000\000\000\000\000\000\000\000\000\057\000\058\000\
\059\000\060\000\000\000\061\000\000\000\062\000\000\000\063\000\
\064\000\065\000\066\000\067\000\068\000\069\000\038\000\000\000\
\038\000\000\000\000\000\039\000\000\000\039\000\038\000\000\000\
\000\000\000\000\000\000\039\000\000\000\000\000\038\000\000\000\
\038\000\038\000\038\000\039\000\000\000\039\000\039\000\039\000\
\056\000\057\000\058\000\059\000\060\000\000\000\061\000\000\000\
\062\000\000\000\063\000\064\000\065\000\066\000\067\000\068\000\
\069\000\057\000\058\000\059\000\060\000\000\000\061\000\000\000\
\062\000\000\000\063\000\064\000\065\000\066\000\067\000\068\000\
\069\000\057\000\058\000\059\000\060\000\000\000\061\000\000\000\
\062\000\000\000\063\000\000\000\065\000\066\000\067\000\068\000\
\069\000\057\000\058\000\059\000\060\000\000\000\061\000\000\000\
\062\000\000\000\000\000\000\000\065\000\066\000\067\000\068\000\
\069\000\057\000\058\000\059\000\060\000\000\000\061\000\000\000\
\000\000\000\000\000\000\000\000\000\000\066\000\067\000\068\000\
\069\000"

let yycheck = "\032\000\
\000\000\002\001\035\000\036\000\001\001\095\000\003\001\001\001\
\001\001\003\001\003\001\001\000\009\001\014\001\104\000\009\001\
\042\001\042\001\051\000\052\000\053\000\054\000\055\000\020\001\
\057\000\058\000\059\000\060\000\061\000\062\000\063\000\064\000\
\065\000\066\000\067\000\068\000\069\000\012\001\013\001\002\001\
\015\001\004\001\005\001\016\000\002\001\002\001\042\001\004\001\
\005\001\009\001\023\000\024\000\031\001\032\001\003\001\018\001\
\035\001\036\001\001\001\001\001\003\001\018\001\095\000\026\001\
\027\001\098\000\029\001\030\001\042\001\026\001\027\001\104\000\
\029\001\030\001\026\001\038\001\039\001\040\001\041\001\042\001\
\004\001\038\001\039\001\040\001\041\001\042\001\002\001\001\001\
\004\001\005\001\094\000\002\001\096\000\004\001\005\001\010\001\
\011\001\012\001\013\001\103\000\015\001\002\001\018\001\107\000\
\001\001\003\001\003\001\018\001\002\001\002\001\026\001\027\001\
\002\001\029\001\030\001\026\001\027\001\018\001\029\001\030\001\
\001\001\028\001\038\001\039\001\040\001\041\001\042\001\038\001\
\039\001\040\001\041\001\042\001\002\001\001\001\004\001\001\001\
\003\001\038\001\039\001\040\001\041\001\042\001\003\001\001\001\
\003\001\003\001\003\001\003\001\018\001\003\001\029\000\009\001\
\033\000\255\255\255\255\255\255\026\001\027\001\255\255\029\001\
\030\001\019\001\020\001\255\255\255\255\001\001\255\255\003\001\
\038\001\039\001\040\001\041\001\042\001\009\001\010\001\011\001\
\012\001\013\001\255\255\015\001\255\255\017\001\001\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\255\255\010\001\
\011\001\012\001\013\001\255\255\015\001\255\255\017\001\255\255\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\001\001\
\255\255\003\001\255\255\255\255\255\255\255\255\255\255\009\001\
\010\001\011\001\255\255\255\255\255\255\255\255\255\255\017\001\
\255\255\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\001\001\255\255\003\001\255\255\255\255\255\255\255\255\255\255\
\009\001\010\001\011\001\255\255\255\255\255\255\255\255\255\255\
\017\001\255\255\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\255\255\001\001\255\255\003\001\255\255\255\255\255\255\
\255\255\255\255\009\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\017\001\255\255\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\255\255\255\255\255\255\255\255\031\001\
\032\001\255\255\255\255\035\001\036\001\037\001\001\001\255\255\
\003\001\255\255\255\255\255\255\255\255\255\255\009\001\001\001\
\255\255\003\001\255\255\255\255\255\255\255\255\017\001\009\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\017\001\
\255\255\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\001\001\255\255\003\001\255\255\255\255\255\255\255\255\255\255\
\009\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\017\001\255\255\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\003\001\255\255\255\255\255\255\255\255\255\255\255\255\
\010\001\011\001\012\001\013\001\255\255\015\001\255\255\017\001\
\255\255\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\003\001\255\255\255\255\255\255\255\255\255\255\255\255\010\001\
\011\001\012\001\013\001\255\255\015\001\255\255\017\001\255\255\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\003\001\
\255\255\255\255\255\255\255\255\255\255\255\255\010\001\011\001\
\012\001\013\001\255\255\015\001\255\255\017\001\255\255\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\001\001\255\255\
\003\001\255\255\255\255\001\001\255\255\003\001\009\001\255\255\
\255\255\255\255\255\255\009\001\255\255\255\255\017\001\255\255\
\019\001\020\001\021\001\017\001\255\255\019\001\020\001\021\001\
\009\001\010\001\011\001\012\001\013\001\255\255\015\001\255\255\
\017\001\255\255\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\010\001\011\001\012\001\013\001\255\255\015\001\255\255\
\017\001\255\255\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\010\001\011\001\012\001\013\001\255\255\015\001\255\255\
\017\001\255\255\019\001\255\255\021\001\022\001\023\001\024\001\
\025\001\010\001\011\001\012\001\013\001\255\255\015\001\255\255\
\017\001\255\255\255\255\255\255\021\001\022\001\023\001\024\001\
\025\001\010\001\011\001\012\001\013\001\255\255\015\001\255\255\
\255\255\255\255\255\255\255\255\255\255\022\001\023\001\024\001\
\025\001"

let yynames_const = "\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  LBRACK\000\
  RBRACK\000\
  COMMA\000\
  PERIOD\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  ASSIGN\000\
  MOD\000\
  token\000\
  EQ\000\
  NOT\000\
  AND\000\
  OR\000\
  NEQ\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  RETURNS\000\
  IF\000\
  ELSE\000\
  FOR\000\
  WHILE\000\
  NUMBER\000\
  BOOL\000\
  TRUE\000\
  FALSE\000\
  STRING\000\
  CHAR\000\
  FUNCTION\000\
  EOF\000\
  "

let yynames_block = "\
  LIT_INT\000\
  LIT_BOOL\000\
  LIT_STRING\000\
  LIT_CHAR\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 38 "parser.mly"
            ( _1 )
# 370 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "parser.mly"
                 ( [], [] )
# 376 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 42 "parser.mly"
               ( (_2 :: fst _1), snd _1 )
# 384 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 43 "parser.mly"
               ( fst _1, (_2 :: snd _1) )
# 392 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 7 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 4 : 'type_label) in
    let _9 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 47 "parser.mly"
     ( { fname = _2;
	 formals = _4;
	 locals = List.rev _9;
	 body = List.rev _10 } )
# 406 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
                  ( [] )
# 412 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 54 "parser.mly"
                  ( List.rev _1 )
# 419 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'type_label) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 57 "parser.mly"
                                    ( [(_1, _2)] )
# 427 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'type_label) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 58 "parser.mly"
                                   ( (_3, _4) :: _1 )
# 436 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
           ( Int )
# 442 "parser.ml"
               : 'type_label))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "parser.mly"
           ( Bool )
# 448 "parser.ml"
               : 'type_label))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "parser.mly"
           ( String )
# 454 "parser.ml"
               : 'type_label))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "parser.mly"
           ( Char )
# 460 "parser.ml"
               : 'type_label))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "parser.mly"
                     ( [] )
# 466 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 68 "parser.mly"
                     ( _2 :: _1 )
# 474 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'type_label) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 71 "parser.mly"
                        ( (_1, _2) )
# 482 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "parser.mly"
                   ( [] )
# 488 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 75 "parser.mly"
                   ( _2 :: _1 )
# 496 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 78 "parser.mly"
                ( Expr(_1) )
# 503 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 79 "parser.mly"
                      ( Return(_2) )
# 510 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 80 "parser.mly"
                            ( Block(List.rev _2) )
# 517 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 81 "parser.mly"
                                            ( If(_3, _5, Block([])) )
# 525 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 82 "parser.mly"
                                            ( If(_3, _5, _7) )
# 534 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 84 "parser.mly"
     ( For(_3, _5, _7, _9) )
# 544 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 85 "parser.mly"
                                  ( While(_3, _5) )
# 552 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "parser.mly"
                  ( Noexpr )
# 558 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 89 "parser.mly"
                  ( _1 )
# 565 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 92 "parser.mly"
                     (0)
# 572 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 93 "parser.mly"
                     (0)
# 579 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 94 "parser.mly"
                     (0)
# 586 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 95 "parser.mly"
                     (0)
# 593 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 96 "parser.mly"
                     (0)
# 600 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 97 "parser.mly"
                     (0)
# 608 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 98 "parser.mly"
                     (0)
# 616 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "parser.mly"
                     (0)
# 624 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "parser.mly"
                     (0)
# 632 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "parser.mly"
                     (0)
# 640 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
                     (0)
# 648 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 103 "parser.mly"
                     (0)
# 656 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "parser.mly"
                     (0)
# 664 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "parser.mly"
                     (0)
# 672 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "parser.mly"
                     (0)
# 680 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 107 "parser.mly"
                     (0)
# 688 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 108 "parser.mly"
                     (0)
# 696 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "parser.mly"
                     (0)
# 704 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "parser.mly"
             (0)
# 711 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "parser.mly"
                     (0)
# 719 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 112 "parser.mly"
                                 (0)
# 727 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 113 "parser.mly"
                       (0)
# 734 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 116 "parser.mly"
                  ( [] )
# 740 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 117 "parser.mly"
                  ( List.rev _1 )
# 747 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "parser.mly"
                            ( [_1] )
# 754 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "parser.mly"
                           ( _3 :: _1 )
# 762 "parser.ml"
               : 'actuals_list))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
