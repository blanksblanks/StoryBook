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

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 55 "parser.ml"
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
  266 (* APOST *);
  267 (* PLUS *);
  268 (* MINUS *);
  269 (* TIMES *);
  270 (* DIVIDE *);
  271 (* ASSIGN *);
  272 (* MOD *);
  273 (* token *);
  274 (* EQ *);
  275 (* NOT *);
  276 (* AND *);
  277 (* OR *);
  278 (* NEQ *);
  279 (* LT *);
  280 (* LEQ *);
  281 (* GT *);
  282 (* GEQ *);
  283 (* RETURNS *);
  284 (* IF *);
  285 (* ELSE *);
  286 (* FOR *);
  287 (* WHILE *);
  288 (* NUMBER *);
  289 (* BOOL *);
  290 (* TRUE *);
  291 (* FALSE *);
  292 (* STRING *);
  293 (* CHAR *);
  294 (* FUNCTION *);
  295 (* CLASS *);
  296 (* METHOD *);
  297 (* IVAR *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  298 (* LIT_INT *);
  299 (* LIT_BOOL *);
  300 (* LIT_STRING *);
  301 (* LIT_CHAR *);
  302 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\005\000\005\000\009\000\
\009\000\006\000\006\000\006\000\006\000\007\000\007\000\010\000\
\003\000\011\000\011\000\012\000\008\000\008\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\015\000\015\000\014\000\
\014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
\014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
\014\000\014\000\014\000\014\000\014\000\014\000\014\000\016\000\
\016\000\017\000\017\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\011\000\000\000\001\000\002\000\
\004\000\001\000\001\000\001\000\001\000\000\000\002\000\003\000\
\009\000\000\000\002\000\011\000\000\000\002\000\002\000\003\000\
\003\000\005\000\007\000\009\000\005\000\000\000\001\000\001\000\
\001\000\001\000\001\000\001\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\002\000\003\000\003\000\005\000\004\000\003\000\000\000\
\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\060\000\000\000\000\000\000\000\001\000\003\000\
\004\000\000\000\000\000\000\000\000\000\010\000\011\000\012\000\
\013\000\000\000\000\000\000\000\000\000\000\000\008\000\000\000\
\000\000\000\000\000\000\014\000\000\000\009\000\000\000\014\000\
\000\000\015\000\000\000\000\000\000\000\017\000\000\000\019\000\
\000\000\016\000\000\000\000\000\021\000\005\000\000\000\000\000\
\000\000\000\000\000\000\032\000\033\000\034\000\035\000\000\000\
\022\000\000\000\000\000\000\000\000\000\050\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\023\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\055\000\025\000\024\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\039\000\040\000\041\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\054\000\000\000\000\000\000\000\000\000\000\000\029\000\
\000\000\000\000\000\000\000\000\000\000\014\000\027\000\000\000\
\000\000\000\000\000\000\028\000\020\000"

let yydgoto = "\002\000\
\003\000\004\000\008\000\009\000\018\000\019\000\031\000\041\000\
\020\000\034\000\035\000\040\000\057\000\058\000\090\000\093\000\
\094\000"

let yysindex = "\012\000\
\000\000\000\000\000\000\001\000\225\254\227\254\000\000\000\000\
\000\000\020\255\047\255\066\255\066\255\000\000\000\000\000\000\
\000\000\061\255\029\255\077\255\082\255\069\255\000\000\066\255\
\096\255\066\255\055\255\000\000\107\255\000\000\066\255\000\000\
\068\255\000\000\005\255\066\255\109\255\000\000\075\255\000\000\
\064\255\000\000\121\255\012\255\000\000\000\000\012\255\012\255\
\124\255\130\255\138\255\000\000\000\000\000\000\000\000\014\255\
\000\000\214\000\066\255\137\000\085\255\000\000\250\255\012\255\
\012\255\012\255\012\255\091\255\012\255\000\000\012\255\012\255\
\012\255\012\255\012\255\012\255\012\255\012\255\012\255\012\255\
\012\255\012\255\012\255\146\255\000\000\000\000\000\000\153\000\
\230\000\155\255\169\000\230\000\164\255\172\255\156\255\230\000\
\049\255\049\255\000\000\000\000\000\000\022\001\006\001\246\000\
\022\001\007\255\007\255\007\255\007\255\148\255\170\255\012\255\
\170\255\000\000\012\255\012\255\066\255\147\255\178\255\000\000\
\230\000\230\000\179\255\170\255\012\255\000\000\000\000\181\255\
\066\255\170\255\115\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\182\255\182\255\000\000\000\000\000\000\
\000\000\000\000\000\000\183\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\006\255\000\000\
\000\000\000\000\000\000\120\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\208\255\
\000\000\000\000\182\255\000\000\000\000\000\000\000\000\000\000\
\186\255\000\000\185\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\078\255\000\000\000\000\081\255\000\000\187\255\234\255\071\255\
\023\000\049\000\000\000\000\000\000\000\195\000\050\255\132\255\
\200\000\075\000\085\000\111\000\121\000\000\000\000\000\186\255\
\000\000\000\000\000\000\000\000\000\000\150\255\000\000\000\000\
\167\255\135\255\000\000\000\000\188\255\000\000\000\000\000\000\
\120\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\249\255\232\255\227\255\215\255\
\000\000\000\000\000\000\000\000\193\255\217\255\191\255\000\000\
\000\000"

let yytablesize = 560
let yytable = "\027\000\
\007\000\029\000\036\000\061\000\060\000\021\000\033\000\062\000\
\063\000\038\000\018\000\033\000\001\000\044\000\010\000\067\000\
\011\000\071\000\072\000\073\000\074\000\012\000\075\000\068\000\
\088\000\089\000\091\000\092\000\069\000\096\000\047\000\097\000\
\098\000\099\000\100\000\101\000\102\000\103\000\104\000\105\000\
\106\000\107\000\108\000\109\000\039\000\018\000\119\000\118\000\
\013\000\120\000\049\000\084\000\049\000\052\000\053\000\054\000\
\055\000\056\000\049\000\128\000\127\000\073\000\074\000\022\000\
\075\000\044\000\132\000\045\000\046\000\049\000\049\000\051\000\
\089\000\051\000\023\000\121\000\122\000\024\000\031\000\051\000\
\031\000\058\000\047\000\058\000\025\000\089\000\044\000\131\000\
\045\000\086\000\048\000\049\000\123\000\050\000\051\000\026\000\
\129\000\014\000\015\000\028\000\030\000\016\000\017\000\047\000\
\033\000\052\000\053\000\054\000\055\000\056\000\032\000\048\000\
\049\000\037\000\050\000\051\000\044\000\042\000\045\000\133\000\
\043\000\021\000\059\000\021\000\021\000\064\000\052\000\053\000\
\054\000\055\000\056\000\065\000\048\000\047\000\048\000\053\000\
\095\000\053\000\021\000\066\000\048\000\048\000\049\000\053\000\
\050\000\051\000\021\000\021\000\110\000\021\000\021\000\026\000\
\048\000\026\000\026\000\112\000\052\000\053\000\054\000\055\000\
\056\000\021\000\021\000\021\000\021\000\021\000\114\000\059\000\
\026\000\059\000\116\000\044\000\115\000\045\000\117\000\124\000\
\026\000\026\000\125\000\026\000\026\000\000\000\126\000\130\000\
\006\000\007\000\030\000\056\000\047\000\057\000\030\000\026\000\
\026\000\026\000\026\000\026\000\048\000\049\000\000\000\050\000\
\051\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\036\000\000\000\036\000\052\000\053\000\054\000\055\000\056\000\
\036\000\000\000\036\000\036\000\036\000\036\000\000\000\036\000\
\000\000\036\000\000\000\036\000\036\000\036\000\036\000\036\000\
\036\000\036\000\052\000\000\000\052\000\000\000\000\000\000\000\
\000\000\000\000\052\000\000\000\052\000\052\000\052\000\052\000\
\000\000\052\000\087\000\052\000\000\000\052\000\052\000\052\000\
\052\000\052\000\052\000\052\000\071\000\072\000\073\000\074\000\
\000\000\075\000\000\000\076\000\000\000\077\000\078\000\079\000\
\080\000\081\000\082\000\083\000\000\000\000\000\000\000\037\000\
\000\000\037\000\000\000\000\000\000\000\000\000\000\000\037\000\
\000\000\037\000\037\000\000\000\000\000\000\000\005\000\006\000\
\037\000\000\000\037\000\037\000\037\000\037\000\037\000\037\000\
\037\000\038\000\000\000\038\000\000\000\000\000\000\000\000\000\
\000\000\038\000\000\000\038\000\038\000\000\000\000\000\000\000\
\000\000\000\000\038\000\000\000\038\000\038\000\038\000\038\000\
\038\000\038\000\038\000\044\000\000\000\044\000\000\000\000\000\
\000\000\000\000\000\000\044\000\000\000\045\000\000\000\045\000\
\000\000\000\000\000\000\000\000\044\000\045\000\044\000\044\000\
\044\000\044\000\044\000\044\000\044\000\000\000\045\000\000\000\
\045\000\045\000\045\000\045\000\045\000\045\000\045\000\046\000\
\000\000\046\000\000\000\000\000\000\000\000\000\000\000\046\000\
\000\000\047\000\000\000\047\000\000\000\000\000\000\000\000\000\
\046\000\047\000\046\000\046\000\046\000\046\000\046\000\046\000\
\046\000\000\000\047\000\085\000\047\000\047\000\047\000\047\000\
\047\000\047\000\047\000\071\000\072\000\073\000\074\000\000\000\
\075\000\000\000\076\000\111\000\077\000\078\000\079\000\080\000\
\081\000\082\000\083\000\071\000\072\000\073\000\074\000\000\000\
\075\000\000\000\076\000\113\000\077\000\078\000\079\000\080\000\
\081\000\082\000\083\000\071\000\072\000\073\000\074\000\000\000\
\075\000\000\000\076\000\000\000\077\000\078\000\079\000\080\000\
\081\000\082\000\083\000\042\000\000\000\042\000\000\000\000\000\
\043\000\000\000\043\000\042\000\000\000\000\000\000\000\000\000\
\043\000\000\000\000\000\000\000\042\000\000\000\042\000\042\000\
\042\000\043\000\000\000\043\000\043\000\043\000\070\000\000\000\
\071\000\072\000\073\000\074\000\000\000\075\000\000\000\076\000\
\000\000\077\000\078\000\079\000\080\000\081\000\082\000\083\000\
\071\000\072\000\073\000\074\000\000\000\075\000\000\000\076\000\
\000\000\077\000\078\000\079\000\080\000\081\000\082\000\083\000\
\071\000\072\000\073\000\074\000\000\000\075\000\000\000\076\000\
\000\000\077\000\000\000\079\000\080\000\081\000\082\000\083\000\
\071\000\072\000\073\000\074\000\000\000\075\000\000\000\076\000\
\000\000\000\000\000\000\079\000\080\000\081\000\082\000\083\000\
\071\000\072\000\073\000\074\000\000\000\075\000\000\000\000\000\
\000\000\000\000\000\000\000\000\080\000\081\000\082\000\083\000"

let yycheck = "\024\000\
\000\000\026\000\032\000\045\000\044\000\013\000\031\000\047\000\
\048\000\005\001\005\001\036\000\001\000\002\001\046\001\002\001\
\046\001\011\001\012\001\013\001\014\001\002\001\016\001\010\001\
\064\000\065\000\066\000\067\000\015\001\069\000\019\001\071\000\
\072\000\073\000\074\000\075\000\076\000\077\000\078\000\079\000\
\080\000\081\000\082\000\083\000\040\001\040\001\112\000\111\000\
\002\001\113\000\001\001\059\000\003\001\042\001\043\001\044\001\
\045\001\046\001\009\001\125\000\124\000\013\001\014\001\003\001\
\016\001\002\001\130\000\004\001\005\001\020\001\021\001\001\001\
\112\000\003\001\046\001\115\000\116\000\001\001\001\001\009\001\
\003\001\001\001\019\001\003\001\003\001\125\000\002\001\129\000\
\004\001\005\001\027\001\028\001\117\000\030\001\031\001\027\001\
\126\000\032\001\033\001\004\001\046\001\036\001\037\001\019\001\
\129\000\042\001\043\001\044\001\045\001\046\001\004\001\027\001\
\028\001\046\001\030\001\031\001\002\001\009\001\004\001\005\001\
\046\001\002\001\002\001\004\001\005\001\002\001\042\001\043\001\
\044\001\045\001\046\001\002\001\001\001\019\001\003\001\001\001\
\046\001\003\001\019\001\002\001\009\001\027\001\028\001\009\001\
\030\001\031\001\027\001\028\001\003\001\030\001\031\001\002\001\
\021\001\004\001\005\001\001\001\042\001\043\001\044\001\045\001\
\046\001\042\001\043\001\044\001\045\001\046\001\003\001\001\001\
\019\001\003\001\015\001\002\001\001\001\004\001\027\001\029\001\
\027\001\028\001\001\001\030\001\031\001\255\255\004\001\003\001\
\003\001\003\001\001\001\003\001\019\001\003\001\003\001\042\001\
\043\001\044\001\045\001\046\001\027\001\028\001\255\255\030\001\
\031\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\001\001\255\255\003\001\042\001\043\001\044\001\045\001\046\001\
\009\001\255\255\011\001\012\001\013\001\014\001\255\255\016\001\
\255\255\018\001\255\255\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\001\001\255\255\003\001\255\255\255\255\255\255\
\255\255\255\255\009\001\255\255\011\001\012\001\013\001\014\001\
\255\255\016\001\001\001\018\001\255\255\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\011\001\012\001\013\001\014\001\
\255\255\016\001\255\255\018\001\255\255\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\255\255\255\255\255\255\001\001\
\255\255\003\001\255\255\255\255\255\255\255\255\255\255\009\001\
\255\255\011\001\012\001\255\255\255\255\255\255\038\001\039\001\
\018\001\255\255\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\001\001\255\255\003\001\255\255\255\255\255\255\255\255\
\255\255\009\001\255\255\011\001\012\001\255\255\255\255\255\255\
\255\255\255\255\018\001\255\255\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\001\001\255\255\003\001\255\255\255\255\
\255\255\255\255\255\255\009\001\255\255\001\001\255\255\003\001\
\255\255\255\255\255\255\255\255\018\001\009\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\255\255\018\001\255\255\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\001\001\
\255\255\003\001\255\255\255\255\255\255\255\255\255\255\009\001\
\255\255\001\001\255\255\003\001\255\255\255\255\255\255\255\255\
\018\001\009\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\255\255\018\001\003\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\011\001\012\001\013\001\014\001\255\255\
\016\001\255\255\018\001\003\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\011\001\012\001\013\001\014\001\255\255\
\016\001\255\255\018\001\003\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\011\001\012\001\013\001\014\001\255\255\
\016\001\255\255\018\001\255\255\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\001\001\255\255\003\001\255\255\255\255\
\001\001\255\255\003\001\009\001\255\255\255\255\255\255\255\255\
\009\001\255\255\255\255\255\255\018\001\255\255\020\001\021\001\
\022\001\018\001\255\255\020\001\021\001\022\001\009\001\255\255\
\011\001\012\001\013\001\014\001\255\255\016\001\255\255\018\001\
\255\255\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\011\001\012\001\013\001\014\001\255\255\016\001\255\255\018\001\
\255\255\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\011\001\012\001\013\001\014\001\255\255\016\001\255\255\018\001\
\255\255\020\001\255\255\022\001\023\001\024\001\025\001\026\001\
\011\001\012\001\013\001\014\001\255\255\016\001\255\255\018\001\
\255\255\255\255\255\255\022\001\023\001\024\001\025\001\026\001\
\011\001\012\001\013\001\014\001\255\255\016\001\255\255\255\255\
\255\255\255\255\255\255\255\255\023\001\024\001\025\001\026\001"

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
  APOST\000\
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
  CLASS\000\
  METHOD\000\
  IVAR\000\
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
# 401 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "parser.mly"
                 ( [], [], [] )
# 407 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'classdecl) in
    Obj.repr(
# 42 "parser.mly"
                   ( (_2 :: fst _1), snd _1 )
# 415 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 43 "parser.mly"
               ( fst _1, (_2 :: snd _1) )
# 423 "parser.ml"
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
# 437 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
                  ( [] )
# 443 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 54 "parser.mly"
                  ( List.rev _1 )
# 450 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'type_label) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 57 "parser.mly"
                                    ( [(_1, _2)] )
# 458 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'type_label) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 58 "parser.mly"
                                   ( (_3, _4) :: _1 )
# 467 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
           ( Int )
# 473 "parser.ml"
               : 'type_label))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "parser.mly"
           ( Bool )
# 479 "parser.ml"
               : 'type_label))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "parser.mly"
           ( String )
# 485 "parser.ml"
               : 'type_label))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "parser.mly"
           ( Char )
# 491 "parser.ml"
               : 'type_label))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "parser.mly"
                     ( [] )
# 497 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 68 "parser.mly"
                     ( _2 :: _1 )
# 505 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'type_label) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 71 "parser.mly"
                        ( (_1, _2) )
# 513 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'action_list) in
    Obj.repr(
# 75 "parser.mly"
  ( { cname: _2;
      ivars : _4;
      actions: _8; })
# 525 "parser.ml"
               : 'classdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "parser.mly"
                ([])
# 531 "parser.ml"
               : 'action_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'action_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'actiondecl) in
    Obj.repr(
# 81 "parser.mly"
                           (_2::_1)
# 539 "parser.ml"
               : 'action_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 7 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 4 : 'type_label) in
    let _9 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 85 "parser.mly"
  ({
     action_name: _2;
     formals: _4;
     locals: List.rev _9;
     body: List.rev _10;
  })
# 555 "parser.ml"
               : 'actiondecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "parser.mly"
                   ( [] )
# 561 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 94 "parser.mly"
                   ( _2 :: _1 )
# 569 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 97 "parser.mly"
                ( Expr(_1) )
# 576 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 98 "parser.mly"
                      ( Return(_2) )
# 583 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 99 "parser.mly"
                            ( Block(List.rev _2) )
# 590 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 100 "parser.mly"
                                            ( If(_3, _5, Block([])) )
# 598 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 101 "parser.mly"
                                            ( If(_3, _5, _7) )
# 607 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 103 "parser.mly"
     ( For(_3, _5, _7, _9) )
# 617 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 104 "parser.mly"
                                  ( While(_3, _5) )
# 625 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 107 "parser.mly"
                  ( Noexpr )
# 631 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 108 "parser.mly"
                  ( _1 )
# 638 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 111 "parser.mly"
                     (0)
# 645 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 112 "parser.mly"
                     (0)
# 652 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 113 "parser.mly"
                     (0)
# 659 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 114 "parser.mly"
                     (0)
# 666 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 115 "parser.mly"
                     (0)
# 673 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 116 "parser.mly"
                     (0)
# 681 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 117 "parser.mly"
                     (0)
# 689 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 118 "parser.mly"
                     (0)
# 697 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "parser.mly"
                     (0)
# 705 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "parser.mly"
                     (0)
# 713 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "parser.mly"
                     (0)
# 721 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 122 "parser.mly"
                     (0)
# 729 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 123 "parser.mly"
                     (0)
# 737 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 124 "parser.mly"
                     (0)
# 745 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 125 "parser.mly"
                     (0)
# 753 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 126 "parser.mly"
                     (0)
# 761 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 127 "parser.mly"
                     (0)
# 769 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 128 "parser.mly"
                     (0)
# 777 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 129 "parser.mly"
             (0)
# 784 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 130 "parser.mly"
                     (0)
# 792 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 131 "parser.mly"
                     (0)
# 800 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 132 "parser.mly"
                            (0)
# 809 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 133 "parser.mly"
                                 (0)
# 817 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 134 "parser.mly"
                       (0)
# 824 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 137 "parser.mly"
                  ( [] )
# 830 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 138 "parser.mly"
                  ( List.rev _1 )
# 837 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 141 "parser.mly"
                            ( [_1] )
# 844 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 142 "parser.mly"
                           ( _3 :: _1 )
# 852 "parser.ml"
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
