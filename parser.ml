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
  | EQ
  | NOT
  | AND
  | OR
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | ENDWITH
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
  | CHARACTER
  | METHOD
  | TRAIT
  | NEW
  | LIT_NUM of (float)
  | LIT_BOOL of (bool)
  | LIT_STRING of (string)
  | LIT_CHAR of (char)
  | ID of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 56 "parser.ml"
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
  273 (* EQ *);
  274 (* NOT *);
  275 (* AND *);
  276 (* OR *);
  277 (* NEQ *);
  278 (* LT *);
  279 (* LEQ *);
  280 (* GT *);
  281 (* GEQ *);
  282 (* ENDWITH *);
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
  295 (* CHARACTER *);
  296 (* METHOD *);
  297 (* TRAIT *);
  298 (* NEW *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  299 (* LIT_NUM *);
  300 (* LIT_BOOL *);
  301 (* LIT_STRING *);
  302 (* LIT_CHAR *);
  303 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\005\000\005\000\008\000\
\008\000\006\000\006\000\006\000\006\000\006\000\009\000\009\000\
\010\000\010\000\010\000\010\000\003\000\012\000\012\000\013\000\
\007\000\007\000\014\000\014\000\014\000\014\000\014\000\014\000\
\014\000\014\000\015\000\015\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\016\000\016\000\017\000\
\017\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\010\000\000\000\001\000\002\000\
\004\000\001\000\001\000\001\000\001\000\002\000\000\000\002\000\
\003\000\004\000\005\000\006\000\009\000\000\000\002\000\010\000\
\000\000\002\000\002\000\001\000\005\000\003\000\005\000\007\000\
\009\000\005\000\000\000\001\000\001\000\001\000\001\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\002\000\003\000\
\003\000\005\000\004\000\006\000\003\000\000\000\001\000\001\000\
\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\066\000\000\000\000\000\000\000\001\000\003\000\
\004\000\000\000\000\000\000\000\000\000\010\000\011\000\012\000\
\013\000\000\000\000\000\000\000\000\000\000\000\014\000\000\000\
\008\000\000\000\000\000\000\000\000\000\015\000\000\000\009\000\
\000\000\025\000\000\000\016\000\000\000\000\000\000\000\000\000\
\021\000\000\000\023\000\000\000\025\000\005\000\000\000\000\000\
\000\000\000\000\000\000\037\000\038\000\039\000\040\000\000\000\
\028\000\000\000\026\000\000\000\017\000\000\000\000\000\000\000\
\000\000\055\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\027\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\018\000\000\000\000\000\000\000\061\000\030\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\044\000\045\000\046\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\019\000\
\000\000\000\000\000\000\000\000\000\000\059\000\000\000\000\000\
\000\000\020\000\000\000\029\000\000\000\000\000\034\000\000\000\
\000\000\000\000\000\000\000\000\000\000\060\000\000\000\032\000\
\000\000\025\000\000\000\000\000\033\000\024\000"

let yydgoto = "\002\000\
\003\000\004\000\008\000\009\000\019\000\035\000\038\000\021\000\
\033\000\057\000\058\000\037\000\043\000\059\000\098\000\101\000\
\102\000"

let yysindex = "\009\000\
\000\000\000\000\000\000\001\000\222\254\226\254\000\000\000\000\
\000\000\018\255\031\255\077\255\077\255\000\000\000\000\000\000\
\000\000\230\254\063\255\026\255\080\255\097\255\000\000\084\255\
\000\000\077\255\120\255\077\255\091\255\000\000\123\255\000\000\
\077\255\000\000\020\255\000\000\017\255\089\255\101\255\090\255\
\000\000\104\255\000\000\009\255\000\000\000\000\009\255\129\255\
\162\255\167\255\168\255\000\000\000\000\000\000\000\000\004\255\
\000\000\253\000\000\000\103\255\000\000\009\255\175\255\165\000\
\135\255\000\000\009\255\009\255\009\255\009\255\009\255\144\255\
\147\255\009\255\000\000\009\255\009\255\009\255\009\255\009\255\
\009\255\009\255\009\255\009\255\009\255\009\255\009\255\009\255\
\000\000\009\255\014\001\077\255\000\000\000\000\180\000\195\000\
\046\001\141\255\210\000\046\001\184\255\192\255\193\255\181\255\
\046\001\146\255\146\255\000\000\000\000\000\000\076\001\061\001\
\133\255\076\001\172\255\172\255\172\255\172\255\031\001\000\000\
\195\255\191\255\032\255\009\255\032\255\000\000\009\255\009\255\
\009\255\000\000\178\255\000\000\177\255\212\255\000\000\046\001\
\216\255\046\001\077\255\032\255\009\255\000\000\218\255\000\000\
\220\255\000\000\032\255\171\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\221\255\221\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\223\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\019\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\254\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\219\255\000\000\224\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\221\255\000\000\000\000\000\000\000\000\
\094\255\000\000\000\000\140\255\000\000\225\255\000\000\032\000\
\071\255\057\000\082\000\000\000\000\000\000\000\235\000\006\255\
\083\255\240\000\107\000\116\000\141\000\150\000\000\000\000\000\
\000\000\000\000\000\000\219\255\000\000\000\000\000\000\224\255\
\000\000\000\000\000\000\000\000\207\255\000\000\000\000\189\255\
\000\000\095\255\000\000\000\000\226\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\247\255\246\255\211\255\000\000\
\000\000\188\000\217\255\000\000\000\000\239\255\202\255\102\000\
\000\000"

let yytablesize = 613
let yytable = "\065\000\
\007\000\020\000\020\000\022\000\064\000\071\000\054\000\066\000\
\054\000\001\000\044\000\072\000\010\000\073\000\054\000\029\000\
\011\000\031\000\074\000\012\000\023\000\041\000\091\000\022\000\
\054\000\054\000\047\000\095\000\096\000\097\000\099\000\100\000\
\013\000\044\000\105\000\045\000\106\000\107\000\108\000\109\000\
\110\000\111\000\112\000\113\000\114\000\115\000\116\000\117\000\
\118\000\047\000\119\000\052\000\053\000\054\000\055\000\056\000\
\042\000\048\000\022\000\049\000\039\000\050\000\051\000\014\000\
\015\000\024\000\040\000\016\000\017\000\134\000\018\000\056\000\
\025\000\056\000\052\000\053\000\054\000\055\000\056\000\056\000\
\026\000\020\000\121\000\053\000\097\000\053\000\145\000\136\000\
\100\000\138\000\044\000\053\000\045\000\046\000\036\000\058\000\
\036\000\058\000\061\000\027\000\148\000\097\000\053\000\058\000\
\062\000\133\000\047\000\135\000\014\000\015\000\028\000\089\000\
\016\000\017\000\048\000\018\000\049\000\090\000\050\000\051\000\
\014\000\015\000\144\000\030\000\016\000\017\000\034\000\018\000\
\143\000\149\000\067\000\052\000\053\000\054\000\055\000\056\000\
\044\000\032\000\045\000\094\000\064\000\124\000\064\000\076\000\
\077\000\078\000\079\000\060\000\080\000\081\000\063\000\082\000\
\047\000\084\000\085\000\086\000\087\000\088\000\078\000\079\000\
\048\000\080\000\049\000\068\000\050\000\051\000\014\000\015\000\
\069\000\070\000\016\000\017\000\044\000\018\000\045\000\150\000\
\092\000\052\000\053\000\054\000\055\000\056\000\076\000\077\000\
\078\000\079\000\126\000\080\000\047\000\065\000\103\000\065\000\
\127\000\104\000\128\000\129\000\048\000\131\000\049\000\132\000\
\050\000\051\000\014\000\015\000\139\000\140\000\016\000\017\000\
\031\000\018\000\031\000\031\000\141\000\052\000\053\000\054\000\
\055\000\056\000\142\000\035\000\036\000\146\000\147\000\006\000\
\031\000\007\000\062\000\063\000\035\000\137\000\000\000\000\000\
\031\000\000\000\031\000\000\000\031\000\031\000\031\000\031\000\
\000\000\000\000\031\000\031\000\000\000\031\000\000\000\000\000\
\000\000\031\000\031\000\031\000\031\000\031\000\041\000\000\000\
\041\000\000\000\000\000\000\000\000\000\000\000\041\000\000\000\
\041\000\041\000\041\000\041\000\000\000\041\000\041\000\000\000\
\041\000\041\000\041\000\041\000\041\000\041\000\041\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\057\000\000\000\057\000\000\000\000\000\000\000\005\000\006\000\
\057\000\000\000\057\000\057\000\057\000\057\000\000\000\057\000\
\057\000\000\000\057\000\057\000\057\000\057\000\057\000\057\000\
\057\000\042\000\000\000\042\000\000\000\000\000\000\000\000\000\
\000\000\042\000\000\000\042\000\042\000\000\000\000\000\000\000\
\000\000\042\000\000\000\042\000\042\000\042\000\042\000\042\000\
\042\000\042\000\043\000\000\000\043\000\000\000\000\000\000\000\
\000\000\000\000\043\000\000\000\043\000\043\000\000\000\000\000\
\000\000\000\000\043\000\000\000\043\000\043\000\043\000\043\000\
\043\000\043\000\043\000\049\000\000\000\049\000\000\000\000\000\
\000\000\000\000\000\000\049\000\050\000\000\000\050\000\000\000\
\000\000\000\000\000\000\049\000\050\000\049\000\049\000\049\000\
\049\000\049\000\049\000\049\000\050\000\000\000\050\000\050\000\
\050\000\050\000\050\000\050\000\050\000\051\000\000\000\051\000\
\000\000\000\000\000\000\000\000\000\000\051\000\052\000\000\000\
\052\000\000\000\000\000\000\000\000\000\051\000\052\000\051\000\
\051\000\051\000\051\000\051\000\051\000\051\000\052\000\093\000\
\052\000\052\000\052\000\052\000\052\000\052\000\052\000\076\000\
\077\000\078\000\079\000\000\000\080\000\081\000\122\000\082\000\
\083\000\084\000\085\000\086\000\087\000\088\000\076\000\077\000\
\078\000\079\000\000\000\080\000\081\000\123\000\082\000\083\000\
\084\000\085\000\086\000\087\000\088\000\076\000\077\000\078\000\
\079\000\000\000\080\000\081\000\125\000\082\000\083\000\084\000\
\085\000\086\000\087\000\088\000\076\000\077\000\078\000\079\000\
\000\000\080\000\081\000\000\000\082\000\083\000\084\000\085\000\
\086\000\087\000\088\000\047\000\000\000\047\000\000\000\000\000\
\048\000\000\000\048\000\047\000\000\000\000\000\000\000\000\000\
\048\000\000\000\000\000\047\000\000\000\047\000\047\000\047\000\
\048\000\000\000\048\000\048\000\048\000\075\000\000\000\076\000\
\077\000\078\000\079\000\000\000\080\000\081\000\000\000\082\000\
\083\000\084\000\085\000\086\000\087\000\088\000\120\000\000\000\
\076\000\077\000\078\000\079\000\000\000\080\000\081\000\000\000\
\082\000\083\000\084\000\085\000\086\000\087\000\088\000\130\000\
\000\000\076\000\077\000\078\000\079\000\000\000\080\000\081\000\
\000\000\082\000\083\000\084\000\085\000\086\000\087\000\088\000\
\076\000\077\000\078\000\079\000\000\000\080\000\081\000\000\000\
\082\000\083\000\084\000\085\000\086\000\087\000\088\000\076\000\
\077\000\078\000\079\000\000\000\080\000\081\000\000\000\000\000\
\000\000\084\000\085\000\086\000\087\000\088\000\076\000\077\000\
\078\000\079\000\000\000\080\000\000\000\000\000\000\000\000\000\
\000\000\085\000\086\000\087\000\088\000"

let yycheck = "\045\000\
\000\000\012\000\013\000\013\000\044\000\002\001\001\001\047\000\
\003\001\001\000\002\001\008\001\047\001\010\001\009\001\026\000\
\047\001\028\000\015\001\002\001\047\001\005\001\062\000\005\001\
\019\001\020\001\018\001\067\000\068\000\069\000\070\000\071\000\
\002\001\002\001\074\000\004\001\076\000\077\000\078\000\079\000\
\080\000\081\000\082\000\083\000\084\000\085\000\086\000\087\000\
\088\000\018\001\090\000\043\001\044\001\045\001\046\001\047\001\
\040\001\026\001\040\001\028\001\041\001\030\001\031\001\032\001\
\033\001\003\001\047\001\036\001\037\001\124\000\039\001\001\001\
\047\001\003\001\043\001\044\001\045\001\046\001\047\001\009\001\
\001\001\092\000\092\000\001\001\124\000\003\001\141\000\127\000\
\128\000\129\000\002\001\009\001\004\001\005\001\001\001\001\001\
\003\001\003\001\009\001\003\001\146\000\141\000\020\001\009\001\
\015\001\123\000\018\001\125\000\032\001\033\001\027\001\009\001\
\036\001\037\001\026\001\039\001\028\001\015\001\030\001\031\001\
\032\001\033\001\140\000\004\001\036\001\037\001\004\001\039\001\
\139\000\147\000\002\001\043\001\044\001\045\001\046\001\047\001\
\002\001\047\001\004\001\005\001\001\001\001\001\003\001\011\001\
\012\001\013\001\014\001\047\001\016\001\017\001\047\001\019\001\
\018\001\021\001\022\001\023\001\024\001\025\001\013\001\014\001\
\026\001\016\001\028\001\002\001\030\001\031\001\032\001\033\001\
\002\001\002\001\036\001\037\001\002\001\039\001\004\001\005\001\
\002\001\043\001\044\001\045\001\046\001\047\001\011\001\012\001\
\013\001\014\001\003\001\016\001\018\001\001\001\047\001\003\001\
\001\001\047\001\002\001\015\001\026\001\003\001\028\001\009\001\
\030\001\031\001\032\001\033\001\027\001\029\001\036\001\037\001\
\002\001\039\001\004\001\005\001\001\001\043\001\044\001\045\001\
\046\001\047\001\003\001\001\001\033\000\004\001\003\001\003\001\
\018\001\003\001\003\001\003\001\003\001\128\000\255\255\255\255\
\026\001\255\255\028\001\255\255\030\001\031\001\032\001\033\001\
\255\255\255\255\036\001\037\001\255\255\039\001\255\255\255\255\
\255\255\043\001\044\001\045\001\046\001\047\001\001\001\255\255\
\003\001\255\255\255\255\255\255\255\255\255\255\009\001\255\255\
\011\001\012\001\013\001\014\001\255\255\016\001\017\001\255\255\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\001\001\255\255\003\001\255\255\255\255\255\255\038\001\039\001\
\009\001\255\255\011\001\012\001\013\001\014\001\255\255\016\001\
\017\001\255\255\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\001\001\255\255\003\001\255\255\255\255\255\255\255\255\
\255\255\009\001\255\255\011\001\012\001\255\255\255\255\255\255\
\255\255\017\001\255\255\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\001\001\255\255\003\001\255\255\255\255\255\255\
\255\255\255\255\009\001\255\255\011\001\012\001\255\255\255\255\
\255\255\255\255\017\001\255\255\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\001\001\255\255\003\001\255\255\255\255\
\255\255\255\255\255\255\009\001\001\001\255\255\003\001\255\255\
\255\255\255\255\255\255\017\001\009\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\017\001\255\255\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\001\001\255\255\003\001\
\255\255\255\255\255\255\255\255\255\255\009\001\001\001\255\255\
\003\001\255\255\255\255\255\255\255\255\017\001\009\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\017\001\003\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\011\001\
\012\001\013\001\014\001\255\255\016\001\017\001\003\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\011\001\012\001\
\013\001\014\001\255\255\016\001\017\001\003\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\011\001\012\001\013\001\
\014\001\255\255\016\001\017\001\003\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\011\001\012\001\013\001\014\001\
\255\255\016\001\017\001\255\255\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\001\001\255\255\003\001\255\255\255\255\
\001\001\255\255\003\001\009\001\255\255\255\255\255\255\255\255\
\009\001\255\255\255\255\017\001\255\255\019\001\020\001\021\001\
\017\001\255\255\019\001\020\001\021\001\009\001\255\255\011\001\
\012\001\013\001\014\001\255\255\016\001\017\001\255\255\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\009\001\255\255\
\011\001\012\001\013\001\014\001\255\255\016\001\017\001\255\255\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\009\001\
\255\255\011\001\012\001\013\001\014\001\255\255\016\001\017\001\
\255\255\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\011\001\012\001\013\001\014\001\255\255\016\001\017\001\255\255\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\011\001\
\012\001\013\001\014\001\255\255\016\001\017\001\255\255\255\255\
\255\255\021\001\022\001\023\001\024\001\025\001\011\001\012\001\
\013\001\014\001\255\255\016\001\255\255\255\255\255\255\255\255\
\255\255\022\001\023\001\024\001\025\001"

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
  EQ\000\
  NOT\000\
  AND\000\
  OR\000\
  NEQ\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  ENDWITH\000\
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
  CHARACTER\000\
  METHOD\000\
  TRAIT\000\
  NEW\000\
  EOF\000\
  "

let yynames_block = "\
  LIT_NUM\000\
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
# 39 "parser.mly"
            ( _1 )
# 426 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 42 "parser.mly"
                 ( [], [] )
# 432 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'cdecl) in
    Obj.repr(
# 43 "parser.mly"
               ( (_2 :: fst _1), snd _1 )
# 440 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 44 "parser.mly"
               ( fst _1, (_2 :: snd _1) )
# 448 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 6 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 3 : 'type_label) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 49 "parser.mly"
     ( { fname = _2;
	       fformals = _4;
         freturn = _7;
	       fbody = List.rev _9 } )
# 461 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "parser.mly"
                  ( [] )
# 467 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 56 "parser.mly"
                  ( List.rev _1 )
# 474 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'type_label) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 62 "parser.mly"
      ( [ { vtype = _1;
            vname = _2;
            vexpr = Noexpr } ] )
# 484 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'type_label) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 66 "parser.mly"
      ( { vtype = _3;
          vname = _4;
          vexpr = Noexpr } :: _1)
# 495 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
           ( Number )
# 501 "parser.ml"
               : 'type_label))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "parser.mly"
           ( Boolean )
# 507 "parser.ml"
               : 'type_label))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "parser.mly"
           ( String )
# 513 "parser.ml"
               : 'type_label))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser.mly"
           ( Char )
# 519 "parser.ml"
               : 'type_label))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 76 "parser.mly"
                   ( Object(_2) )
# 526 "parser.ml"
               : 'type_label))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "parser.mly"
                ( [] )
# 532 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 81 "parser.mly"
                     ( _2 :: _1)
# 540 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'type_label) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 86 "parser.mly"
      ( { vtype=_1;
          vname=_2;
          vexpr = Noexpr } )
# 550 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'type_label) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 91 "parser.mly"
      ( { vtype = _1;
          vname = _3;
          vexpr = Noexpr } )
# 560 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'type_label) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 97 "parser.mly"
      ( { vtype = _1;
          vname = _2;
          vexpr = _4 } )
# 571 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'type_label) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
      ( { vtype = _1;
          vname = _3;
          vexpr = _5 } )
# 582 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'action_list) in
    Obj.repr(
# 109 "parser.mly"
  ({  cname = _2;
      cformals = _4;
      cinstvars = _7;
      cactions = _8;
  })
# 596 "parser.ml"
               : 'cdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 117 "parser.mly"
                ([])
# 602 "parser.ml"
               : 'action_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'action_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'adecl) in
    Obj.repr(
# 118 "parser.mly"
                      (_2::_1)
# 610 "parser.ml"
               : 'action_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 6 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 3 : 'type_label) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 122 "parser.mly"
  ({
     aname = _2;
     aformals = _4;
     areturn = _7;
     abody = List.rev _9;
  })
# 625 "parser.ml"
               : 'adecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 131 "parser.mly"
                   ( [] )
# 631 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 132 "parser.mly"
                   ( _2 :: _1 )
# 639 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 136 "parser.mly"
                ( Expr(_1) )
# 646 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 137 "parser.mly"
          (VarDecl(_1))
# 653 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 138 "parser.mly"
                                      ( Return(_3) )
# 660 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 139 "parser.mly"
                            ( Block(List.rev _2) )
# 667 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 140 "parser.mly"
                                            ( If(_3, _5, Block([])) )
# 675 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 141 "parser.mly"
                                            ( If(_3, _5, _7) )
# 684 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 143 "parser.mly"
     ( For(_3, _5, _7, _9) )
# 694 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 144 "parser.mly"
                                  ( While(_3, _5) )
# 702 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 148 "parser.mly"
                  ( Noexpr )
# 708 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 149 "parser.mly"
                  ( _1 )
# 715 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 152 "parser.mly"
                     (LitNum(_1))
# 722 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 153 "parser.mly"
                     (LitBool(_1))
# 729 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 154 "parser.mly"
                     (LitString(_1))
# 736 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 155 "parser.mly"
                     (LitChar(_1))
# 743 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 156 "parser.mly"
                     (Id(_1))
# 750 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 157 "parser.mly"
                     (Binop(_1, Add, _3))
# 758 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 158 "parser.mly"
                     (Binop(_1, Sub, _3))
# 766 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 159 "parser.mly"
                     (Binop(_1, Mult, _3))
# 774 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 160 "parser.mly"
                     (Binop(_1, Div, _3))
# 782 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 161 "parser.mly"
                     (Binop(_1, Mod, _3))
# 790 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 162 "parser.mly"
                     (Binop(_1, Equal, _3))
# 798 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 163 "parser.mly"
                     (Binop(_1, Neq, _3))
# 806 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 164 "parser.mly"
                     (Binop(_1, Less, _3))
# 814 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 165 "parser.mly"
                     (Binop(_1, Leq, _3))
# 822 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 166 "parser.mly"
                     (Binop(_1, Greater, _3))
# 830 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 167 "parser.mly"
                     (Binop(_1, Geq, _3))
# 838 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 168 "parser.mly"
                     (Binop(_1, OR, _3))
# 846 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 169 "parser.mly"
                     (Binop(_1, AND, _3))
# 854 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 170 "parser.mly"
             (Unop(NOT, _2))
# 861 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 171 "parser.mly"
                     (Assign(_1, _3))
# 869 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 172 "parser.mly"
                     (Access(_1, _3))
# 877 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 173 "parser.mly"
                            (TraitAssign(_1, _3, _5))
# 886 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 174 "parser.mly"
                                 (FCall(_1, _3))
# 894 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 175 "parser.mly"
                                          (ACall(_1, _3, _5))
# 903 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 177 "parser.mly"
                       (_2)
# 910 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 181 "parser.mly"
                  ( [] )
# 916 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 182 "parser.mly"
                  ( List.rev _1 )
# 923 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 185 "parser.mly"
                            ( [_1] )
# 930 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 186 "parser.mly"
                           ( _3 :: _1 )
# 938 "parser.ml"
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
