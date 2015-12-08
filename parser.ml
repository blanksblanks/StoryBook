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
  | VOID
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
# 57 "parser.ml"
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
  288 (* VOID *);
  289 (* NUMBER *);
  290 (* BOOL *);
  291 (* TRUE *);
  292 (* FALSE *);
  293 (* STRING *);
  294 (* CHAR *);
  295 (* FUNCTION *);
  296 (* CHARACTER *);
  297 (* METHOD *);
  298 (* TRAIT *);
  299 (* NEW *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  300 (* LIT_NUM *);
  301 (* LIT_BOOL *);
  302 (* LIT_STRING *);
  303 (* LIT_CHAR *);
  304 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\005\000\005\000\008\000\
\008\000\006\000\006\000\006\000\006\000\006\000\006\000\009\000\
\009\000\010\000\010\000\010\000\010\000\003\000\012\000\012\000\
\013\000\007\000\007\000\014\000\014\000\014\000\014\000\014\000\
\014\000\014\000\014\000\015\000\015\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\016\000\016\000\
\017\000\017\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\010\000\000\000\001\000\002\000\
\004\000\001\000\001\000\001\000\001\000\001\000\002\000\000\000\
\002\000\003\000\004\000\005\000\006\000\009\000\000\000\002\000\
\010\000\000\000\002\000\002\000\001\000\005\000\003\000\005\000\
\007\000\009\000\005\000\000\000\001\000\001\000\001\000\001\000\
\001\000\001\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\002\000\
\003\000\003\000\005\000\004\000\006\000\003\000\000\000\001\000\
\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\067\000\000\000\000\000\000\000\001\000\003\000\
\004\000\000\000\000\000\000\000\000\000\010\000\011\000\012\000\
\013\000\014\000\000\000\000\000\000\000\000\000\000\000\015\000\
\000\000\008\000\000\000\000\000\000\000\000\000\016\000\000\000\
\009\000\000\000\026\000\000\000\017\000\000\000\000\000\000\000\
\000\000\022\000\000\000\024\000\000\000\026\000\005\000\000\000\
\000\000\000\000\000\000\000\000\038\000\039\000\040\000\041\000\
\000\000\029\000\000\000\027\000\000\000\018\000\000\000\000\000\
\000\000\000\000\056\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\028\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\019\000\000\000\000\000\000\000\062\000\031\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\045\000\046\000\047\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\020\000\000\000\000\000\000\000\000\000\000\000\060\000\000\000\
\000\000\000\000\021\000\000\000\030\000\000\000\000\000\035\000\
\000\000\000\000\000\000\000\000\000\000\000\000\061\000\000\000\
\033\000\000\000\026\000\000\000\000\000\034\000\025\000"

let yydgoto = "\002\000\
\003\000\004\000\008\000\009\000\020\000\036\000\039\000\022\000\
\034\000\058\000\059\000\038\000\044\000\060\000\099\000\102\000\
\103\000"

let yysindex = "\005\000\
\000\000\000\000\000\000\001\000\219\254\221\254\000\000\000\000\
\000\000\092\255\102\255\059\255\059\255\000\000\000\000\000\000\
\000\000\000\000\020\255\069\255\064\255\112\255\117\255\000\000\
\101\255\000\000\059\255\131\255\059\255\086\255\000\000\132\255\
\000\000\059\255\000\000\223\254\000\000\009\255\077\255\089\255\
\007\255\000\000\090\255\000\000\008\255\000\000\000\000\008\255\
\139\255\141\255\155\255\165\255\000\000\000\000\000\000\000\000\
\010\255\000\000\249\000\000\000\018\255\000\000\008\255\168\255\
\166\000\114\255\000\000\008\255\008\255\008\255\008\255\008\255\
\123\255\124\255\008\255\000\000\008\255\008\255\008\255\008\255\
\008\255\008\255\008\255\008\255\008\255\008\255\008\255\008\255\
\008\255\000\000\008\255\010\001\059\255\000\000\000\000\181\000\
\196\000\042\001\172\255\211\000\042\001\171\255\174\255\176\255\
\161\255\042\001\113\255\113\255\000\000\000\000\000\000\087\001\
\072\001\057\001\087\001\152\255\152\255\152\255\152\255\027\001\
\000\000\177\255\178\255\211\255\008\255\211\255\000\000\008\255\
\008\255\008\255\000\000\159\255\000\000\173\255\193\255\000\000\
\042\001\197\255\042\001\059\255\211\255\008\255\000\000\199\255\
\000\000\198\255\000\000\211\255\151\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\201\255\201\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\202\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\016\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\251\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\206\255\000\000\205\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\201\255\000\000\000\000\000\000\
\000\000\033\255\000\000\000\000\058\255\000\000\207\255\000\000\
\033\000\061\255\058\000\083\000\000\000\000\000\000\000\057\255\
\130\255\066\255\236\000\108\000\117\000\142\000\151\000\000\000\
\000\000\000\000\000\000\000\000\206\255\000\000\000\000\000\000\
\205\255\000\000\000\000\000\000\000\000\188\255\000\000\000\000\
\070\255\000\000\097\255\000\000\000\000\208\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\247\255\246\255\210\255\000\000\
\000\000\175\000\216\255\000\000\000\000\195\255\138\255\088\000\
\000\000"

let yytablesize = 624
let yytable = "\066\000\
\007\000\021\000\021\000\023\000\065\000\001\000\135\000\067\000\
\040\000\045\000\010\000\072\000\011\000\042\000\041\000\062\000\
\030\000\073\000\032\000\074\000\023\000\063\000\092\000\146\000\
\075\000\048\000\090\000\096\000\097\000\098\000\100\000\101\000\
\091\000\037\000\106\000\037\000\107\000\108\000\109\000\110\000\
\111\000\112\000\113\000\114\000\115\000\116\000\117\000\118\000\
\119\000\043\000\120\000\053\000\054\000\055\000\056\000\057\000\
\023\000\048\000\065\000\048\000\065\000\057\000\134\000\057\000\
\136\000\048\000\054\000\024\000\054\000\057\000\066\000\025\000\
\066\000\048\000\054\000\048\000\048\000\048\000\045\000\145\000\
\046\000\047\000\021\000\122\000\098\000\054\000\150\000\137\000\
\101\000\139\000\014\000\015\000\016\000\012\000\048\000\017\000\
\018\000\059\000\019\000\059\000\149\000\098\000\049\000\013\000\
\050\000\059\000\051\000\052\000\014\000\015\000\016\000\026\000\
\027\000\017\000\018\000\045\000\019\000\046\000\095\000\028\000\
\053\000\054\000\055\000\056\000\057\000\079\000\080\000\029\000\
\081\000\144\000\055\000\048\000\055\000\033\000\031\000\035\000\
\061\000\064\000\055\000\049\000\068\000\050\000\069\000\051\000\
\052\000\014\000\015\000\016\000\055\000\055\000\017\000\018\000\
\045\000\019\000\046\000\151\000\070\000\053\000\054\000\055\000\
\056\000\057\000\077\000\078\000\079\000\080\000\071\000\081\000\
\048\000\093\000\104\000\105\000\125\000\127\000\128\000\130\000\
\049\000\129\000\050\000\132\000\051\000\052\000\014\000\015\000\
\016\000\140\000\133\000\017\000\018\000\032\000\019\000\032\000\
\032\000\142\000\053\000\054\000\055\000\056\000\057\000\143\000\
\148\000\141\000\147\000\006\000\007\000\032\000\036\000\063\000\
\037\000\064\000\036\000\000\000\045\000\032\000\046\000\032\000\
\138\000\032\000\032\000\032\000\032\000\032\000\000\000\000\000\
\032\000\032\000\000\000\032\000\048\000\000\000\000\000\032\000\
\032\000\032\000\032\000\032\000\049\000\000\000\050\000\000\000\
\051\000\052\000\014\000\015\000\016\000\000\000\000\000\017\000\
\018\000\000\000\019\000\042\000\000\000\042\000\053\000\054\000\
\055\000\056\000\057\000\042\000\000\000\042\000\042\000\042\000\
\042\000\000\000\042\000\042\000\000\000\042\000\042\000\042\000\
\042\000\042\000\042\000\042\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\058\000\000\000\058\000\000\000\000\000\000\000\005\000\
\006\000\058\000\000\000\058\000\058\000\058\000\058\000\000\000\
\058\000\058\000\000\000\058\000\058\000\058\000\058\000\058\000\
\058\000\058\000\043\000\000\000\043\000\000\000\000\000\000\000\
\000\000\000\000\043\000\000\000\043\000\043\000\000\000\000\000\
\000\000\000\000\043\000\000\000\043\000\043\000\043\000\043\000\
\043\000\043\000\043\000\044\000\000\000\044\000\000\000\000\000\
\000\000\000\000\000\000\044\000\000\000\044\000\044\000\000\000\
\000\000\000\000\000\000\044\000\000\000\044\000\044\000\044\000\
\044\000\044\000\044\000\044\000\050\000\000\000\050\000\000\000\
\000\000\000\000\000\000\000\000\050\000\051\000\000\000\051\000\
\000\000\000\000\000\000\000\000\050\000\051\000\050\000\050\000\
\050\000\050\000\050\000\050\000\050\000\051\000\000\000\051\000\
\051\000\051\000\051\000\051\000\051\000\051\000\052\000\000\000\
\052\000\000\000\000\000\000\000\000\000\000\000\052\000\053\000\
\000\000\053\000\000\000\000\000\000\000\000\000\052\000\053\000\
\052\000\052\000\052\000\052\000\052\000\052\000\052\000\053\000\
\094\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
\077\000\078\000\079\000\080\000\000\000\081\000\082\000\123\000\
\083\000\084\000\085\000\086\000\087\000\088\000\089\000\077\000\
\078\000\079\000\080\000\000\000\081\000\082\000\124\000\083\000\
\084\000\085\000\086\000\087\000\088\000\089\000\077\000\078\000\
\079\000\080\000\000\000\081\000\082\000\126\000\083\000\084\000\
\085\000\086\000\087\000\088\000\089\000\077\000\078\000\079\000\
\080\000\000\000\081\000\082\000\000\000\083\000\084\000\085\000\
\086\000\087\000\088\000\089\000\049\000\000\000\049\000\000\000\
\000\000\000\000\000\000\000\000\049\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\049\000\000\000\049\000\049\000\
\049\000\076\000\000\000\077\000\078\000\079\000\080\000\000\000\
\081\000\082\000\000\000\083\000\084\000\085\000\086\000\087\000\
\088\000\089\000\121\000\000\000\077\000\078\000\079\000\080\000\
\000\000\081\000\082\000\000\000\083\000\084\000\085\000\086\000\
\087\000\088\000\089\000\131\000\000\000\077\000\078\000\079\000\
\080\000\000\000\081\000\082\000\000\000\083\000\084\000\085\000\
\086\000\087\000\088\000\089\000\077\000\078\000\079\000\080\000\
\000\000\081\000\082\000\000\000\083\000\084\000\085\000\086\000\
\087\000\088\000\089\000\077\000\078\000\079\000\080\000\000\000\
\081\000\082\000\000\000\083\000\000\000\085\000\086\000\087\000\
\088\000\089\000\077\000\078\000\079\000\080\000\000\000\081\000\
\082\000\000\000\000\000\000\000\085\000\086\000\087\000\088\000\
\089\000\077\000\078\000\079\000\080\000\000\000\081\000\000\000\
\000\000\000\000\000\000\000\000\086\000\087\000\088\000\089\000"

let yycheck = "\046\000\
\000\000\012\000\013\000\013\000\045\000\001\000\125\000\048\000\
\042\001\002\001\048\001\002\001\048\001\005\001\048\001\009\001\
\027\000\008\001\029\000\010\001\005\001\015\001\063\000\142\000\
\015\001\018\001\009\001\068\000\069\000\070\000\071\000\072\000\
\015\001\001\001\075\000\003\001\077\000\078\000\079\000\080\000\
\081\000\082\000\083\000\084\000\085\000\086\000\087\000\088\000\
\089\000\041\001\091\000\044\001\045\001\046\001\047\001\048\001\
\041\001\001\001\001\001\003\001\003\001\001\001\124\000\003\001\
\126\000\009\001\001\001\048\001\003\001\009\001\001\001\003\001\
\003\001\017\001\009\001\019\001\020\001\021\001\002\001\141\000\
\004\001\005\001\093\000\093\000\125\000\020\001\148\000\128\000\
\129\000\130\000\032\001\033\001\034\001\002\001\018\001\037\001\
\038\001\001\001\040\001\003\001\147\000\142\000\026\001\002\001\
\028\001\009\001\030\001\031\001\032\001\033\001\034\001\048\001\
\001\001\037\001\038\001\002\001\040\001\004\001\005\001\003\001\
\044\001\045\001\046\001\047\001\048\001\013\001\014\001\027\001\
\016\001\140\000\001\001\018\001\003\001\048\001\004\001\004\001\
\048\001\048\001\009\001\026\001\002\001\028\001\002\001\030\001\
\031\001\032\001\033\001\034\001\019\001\020\001\037\001\038\001\
\002\001\040\001\004\001\005\001\002\001\044\001\045\001\046\001\
\047\001\048\001\011\001\012\001\013\001\014\001\002\001\016\001\
\018\001\002\001\048\001\048\001\001\001\003\001\001\001\015\001\
\026\001\002\001\028\001\003\001\030\001\031\001\032\001\033\001\
\034\001\027\001\009\001\037\001\038\001\002\001\040\001\004\001\
\005\001\001\001\044\001\045\001\046\001\047\001\048\001\003\001\
\003\001\029\001\004\001\003\001\003\001\018\001\001\001\003\001\
\034\000\003\001\003\001\255\255\002\001\026\001\004\001\028\001\
\129\000\030\001\031\001\032\001\033\001\034\001\255\255\255\255\
\037\001\038\001\255\255\040\001\018\001\255\255\255\255\044\001\
\045\001\046\001\047\001\048\001\026\001\255\255\028\001\255\255\
\030\001\031\001\032\001\033\001\034\001\255\255\255\255\037\001\
\038\001\255\255\040\001\001\001\255\255\003\001\044\001\045\001\
\046\001\047\001\048\001\009\001\255\255\011\001\012\001\013\001\
\014\001\255\255\016\001\017\001\255\255\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\255\255\003\001\255\255\255\255\255\255\039\001\
\040\001\009\001\255\255\011\001\012\001\013\001\014\001\255\255\
\016\001\017\001\255\255\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\001\001\255\255\003\001\255\255\255\255\255\255\
\255\255\255\255\009\001\255\255\011\001\012\001\255\255\255\255\
\255\255\255\255\017\001\255\255\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\001\001\255\255\003\001\255\255\255\255\
\255\255\255\255\255\255\009\001\255\255\011\001\012\001\255\255\
\255\255\255\255\255\255\017\001\255\255\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\001\001\255\255\003\001\255\255\
\255\255\255\255\255\255\255\255\009\001\001\001\255\255\003\001\
\255\255\255\255\255\255\255\255\017\001\009\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\017\001\255\255\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\001\001\255\255\
\003\001\255\255\255\255\255\255\255\255\255\255\009\001\001\001\
\255\255\003\001\255\255\255\255\255\255\255\255\017\001\009\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\017\001\
\003\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\011\001\012\001\013\001\014\001\255\255\016\001\017\001\003\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\011\001\
\012\001\013\001\014\001\255\255\016\001\017\001\003\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\011\001\012\001\
\013\001\014\001\255\255\016\001\017\001\003\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\011\001\012\001\013\001\
\014\001\255\255\016\001\017\001\255\255\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\001\001\255\255\003\001\255\255\
\255\255\255\255\255\255\255\255\009\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\017\001\255\255\019\001\020\001\
\021\001\009\001\255\255\011\001\012\001\013\001\014\001\255\255\
\016\001\017\001\255\255\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\009\001\255\255\011\001\012\001\013\001\014\001\
\255\255\016\001\017\001\255\255\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\009\001\255\255\011\001\012\001\013\001\
\014\001\255\255\016\001\017\001\255\255\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\011\001\012\001\013\001\014\001\
\255\255\016\001\017\001\255\255\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\011\001\012\001\013\001\014\001\255\255\
\016\001\017\001\255\255\019\001\255\255\021\001\022\001\023\001\
\024\001\025\001\011\001\012\001\013\001\014\001\255\255\016\001\
\017\001\255\255\255\255\255\255\021\001\022\001\023\001\024\001\
\025\001\011\001\012\001\013\001\014\001\255\255\016\001\255\255\
\255\255\255\255\255\255\255\255\022\001\023\001\024\001\025\001"

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
  VOID\000\
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
# 431 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 42 "parser.mly"
                 ( [], [] )
# 437 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'cdecl) in
    Obj.repr(
# 43 "parser.mly"
               ( (_2 :: fst _1), snd _1 )
# 445 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 44 "parser.mly"
               ( fst _1, (_2 :: snd _1) )
# 453 "parser.ml"
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
# 466 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "parser.mly"
                  ( [] )
# 472 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 56 "parser.mly"
                  ( List.rev _1 )
# 479 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'type_label) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 62 "parser.mly"
      ( [ { vtype = _1;
            vname = _2;
            vexpr = Noexpr } ] )
# 489 "parser.ml"
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
# 500 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
           ( Void )
# 506 "parser.ml"
               : 'type_label))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "parser.mly"
           ( Number )
# 512 "parser.ml"
               : 'type_label))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "parser.mly"
           ( Boolean )
# 518 "parser.ml"
               : 'type_label))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser.mly"
           ( String )
# 524 "parser.ml"
               : 'type_label))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
           ( Char )
# 530 "parser.ml"
               : 'type_label))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 77 "parser.mly"
                   ( Object(_2) )
# 537 "parser.ml"
               : 'type_label))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "parser.mly"
                ( [] )
# 543 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 82 "parser.mly"
                     ( _2 :: _1)
# 551 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'type_label) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 87 "parser.mly"
      ( { vtype=_1;
          vname=_2;
          vexpr = Noexpr } )
# 561 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'type_label) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 92 "parser.mly"
      ( { vtype = _1;
          vname = _3;
          vexpr = Noexpr } )
# 571 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'type_label) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 98 "parser.mly"
      ( { vtype = _1;
          vname = _2;
          vexpr = _4 } )
# 582 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'type_label) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 103 "parser.mly"
      ( { vtype = _1;
          vname = _3;
          vexpr = _5 } )
# 593 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'action_list) in
    Obj.repr(
# 110 "parser.mly"
  ({  cname = _2;
      cformals = _4;
      cinstvars = _7;
      cactions = _8;
  })
# 607 "parser.ml"
               : 'cdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 118 "parser.mly"
                ([])
# 613 "parser.ml"
               : 'action_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'action_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'adecl) in
    Obj.repr(
# 119 "parser.mly"
                      (_2::_1)
# 621 "parser.ml"
               : 'action_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 6 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 3 : 'type_label) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 123 "parser.mly"
  ({
     aname = _2;
     aformals = _4;
     areturn = _7;
     abody = List.rev _9;
  })
# 636 "parser.ml"
               : 'adecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 132 "parser.mly"
                   ( [] )
# 642 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 133 "parser.mly"
                   ( _2 :: _1 )
# 650 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 137 "parser.mly"
                ( Expr(_1) )
# 657 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 138 "parser.mly"
          (VarDecl(_1))
# 664 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 139 "parser.mly"
                                      ( Return(_3) )
# 671 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 140 "parser.mly"
                            ( Block(List.rev _2) )
# 678 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 141 "parser.mly"
                                            ( If(_3, _5, Block([])) )
# 686 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 142 "parser.mly"
                                            ( If(_3, _5, _7) )
# 695 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 144 "parser.mly"
     ( For(_3, _5, _7, _9) )
# 705 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 145 "parser.mly"
                                  ( While(_3, _5) )
# 713 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 149 "parser.mly"
                  ( Noexpr )
# 719 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 150 "parser.mly"
                  ( _1 )
# 726 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 153 "parser.mly"
                     (LitNum(_1))
# 733 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 154 "parser.mly"
                     (LitBool(_1))
# 740 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 155 "parser.mly"
                     (LitString(_1))
# 747 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 156 "parser.mly"
                     (LitChar(_1))
# 754 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 157 "parser.mly"
                     (Id(_1))
# 761 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 158 "parser.mly"
                     (Binop(_1, Add, _3))
# 769 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 159 "parser.mly"
                     (Binop(_1, Sub, _3))
# 777 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 160 "parser.mly"
                     (Binop(_1, Mult, _3))
# 785 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 161 "parser.mly"
                     (Binop(_1, Div, _3))
# 793 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 162 "parser.mly"
                     (Binop(_1, Mod, _3))
# 801 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 163 "parser.mly"
                     (Binop(_1, Equal, _3))
# 809 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 164 "parser.mly"
                     (Binop(_1, Neq, _3))
# 817 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 165 "parser.mly"
                     (Binop(_1, Less, _3))
# 825 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 166 "parser.mly"
                     (Binop(_1, Leq, _3))
# 833 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 167 "parser.mly"
                     (Binop(_1, Greater, _3))
# 841 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 168 "parser.mly"
                     (Binop(_1, Geq, _3))
# 849 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 169 "parser.mly"
                     (Binop(_1, OR, _3))
# 857 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 170 "parser.mly"
                     (Binop(_1, AND, _3))
# 865 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 171 "parser.mly"
             (Unop(NOT, _2))
# 872 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 172 "parser.mly"
                     (Assign(_1, _3))
# 880 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 173 "parser.mly"
                     (Access(_1, _3))
# 888 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 174 "parser.mly"
                            (TraitAssign(_1, _3, _5))
# 897 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 175 "parser.mly"
                                 (FCall(_1, _3))
# 905 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 176 "parser.mly"
                                          (ACall(_1, _3, _5))
# 914 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 178 "parser.mly"
                       (_2)
# 921 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 182 "parser.mly"
                  ( [] )
# 927 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 183 "parser.mly"
                  ( List.rev _1 )
# 934 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 186 "parser.mly"
                            ( [_1] )
# 941 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 187 "parser.mly"
                           ( _3 :: _1 )
# 949 "parser.ml"
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
