type token =
  | LBRACE
  | RBRACE
  | SEMI
  | LPAREN
  | RPAREN
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | NOT
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | AND
  | OR
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | INT
  | BOOL
  | FLOAT
  | VOID
  | STRING
  | CHAR
  | STRUCT
  | LSQUARE
  | RSQUARE
  | PIPE
  | DOT
  | CHARLIT of (char)
  | INTLIT of (int)
  | BLIT of (bool)
  | ID of (string)
  | FLIT of (string)
  | STRLIT of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 4 "parser.mly"
open Ast

let fst (a,_,_) = a;;
let snd (_,b,_) = b;;
let trd (_,_,c) = c;;

# 56 "parser.ml"
let yytransl_const = [|
  257 (* LBRACE *);
  258 (* RBRACE *);
  259 (* SEMI *);
  260 (* LPAREN *);
  261 (* RPAREN *);
  262 (* COMMA *);
  263 (* PLUS *);
  264 (* MINUS *);
  265 (* TIMES *);
  266 (* DIVIDE *);
  267 (* ASSIGN *);
  268 (* NOT *);
  269 (* EQ *);
  270 (* NEQ *);
  271 (* LT *);
  272 (* LEQ *);
  273 (* GT *);
  274 (* GEQ *);
  275 (* AND *);
  276 (* OR *);
  277 (* RETURN *);
  278 (* IF *);
  279 (* ELSE *);
  280 (* FOR *);
  281 (* WHILE *);
  282 (* INT *);
  283 (* BOOL *);
  284 (* FLOAT *);
  285 (* VOID *);
  286 (* STRING *);
  287 (* CHAR *);
  288 (* STRUCT *);
  289 (* LSQUARE *);
  290 (* RSQUARE *);
  291 (* PIPE *);
  292 (* DOT *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  293 (* CHARLIT *);
  294 (* INTLIT *);
  295 (* BLIT *);
  296 (* ID *);
  297 (* FLIT *);
  298 (* STRLIT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\004\000\007\000\007\000\
\009\000\009\000\005\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\006\000\006\000\010\000\010\000\003\000\008\000\
\008\000\008\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\014\000\014\000\015\000\015\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\016\000\
\016\000\017\000\017\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\002\000\008\000\000\000\001\000\
\002\000\004\000\006\000\001\000\001\000\001\000\001\000\001\000\
\001\000\002\000\001\000\004\000\000\000\002\000\003\000\000\000\
\002\000\002\000\002\000\003\000\003\000\005\000\007\000\009\000\
\005\000\000\000\001\000\001\000\003\000\001\000\001\000\001\000\
\001\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\002\000\002\000\003\000\004\000\005\000\006\000\003\000\000\000\
\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\068\000\000\000\012\000\013\000\014\000\015\000\
\016\000\017\000\000\000\001\000\003\000\004\000\005\000\000\000\
\019\000\000\000\000\000\000\000\021\000\000\000\023\000\000\000\
\000\000\020\000\000\000\000\000\000\000\000\000\000\000\022\000\
\000\000\018\000\009\000\000\000\000\000\011\000\000\000\024\000\
\000\000\000\000\010\000\024\000\006\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\038\000\039\000\040\000\
\000\000\041\000\042\000\026\000\025\000\000\000\000\000\000\000\
\057\000\058\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\027\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\029\000\063\000\028\000\000\000\000\000\000\000\
\000\000\044\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\047\000\048\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\060\000\000\000\000\000\000\000\000\000\000\000\033\000\000\000\
\000\000\000\000\000\000\000\000\000\000\031\000\000\000\000\000\
\032\000"

let yydgoto = "\002\000\
\003\000\004\000\060\000\014\000\015\000\033\000\029\000\042\000\
\030\000\025\000\017\000\061\000\062\000\068\000\073\000\100\000\
\101\000"

let yysindex = "\023\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\036\255\000\000\000\000\000\000\000\000\229\254\
\000\000\054\255\041\255\008\255\000\000\048\255\000\000\192\255\
\166\000\000\000\059\255\231\254\096\255\104\255\109\255\000\000\
\233\254\000\000\000\000\113\255\192\255\000\000\115\255\000\000\
\024\255\065\255\000\000\000\000\000\000\202\255\202\255\202\255\
\202\255\126\255\136\255\137\255\202\255\000\000\000\000\000\000\
\018\255\000\000\000\000\000\000\000\000\197\000\107\255\231\000\
\000\000\000\000\021\001\118\255\202\255\202\255\202\255\021\001\
\014\255\202\255\202\255\087\255\103\255\000\000\202\255\202\255\
\202\255\202\255\202\255\202\255\202\255\202\255\202\255\202\255\
\202\255\202\255\000\000\000\000\000\000\247\000\151\255\007\001\
\202\255\000\000\021\001\150\255\152\255\021\001\125\255\155\255\
\050\255\050\255\000\000\000\000\069\000\069\000\006\000\006\000\
\006\000\006\000\048\001\035\001\191\255\202\255\191\255\021\001\
\000\000\202\255\156\255\202\255\140\255\215\000\000\000\021\001\
\202\255\021\001\191\255\202\255\021\001\000\000\163\255\191\255\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\028\255\000\000\000\000\000\000\000\000\000\000\164\255\
\000\000\000\000\000\000\000\000\000\000\167\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\157\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\242\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\060\255\000\000\000\000\157\255\000\000\015\255\
\000\000\177\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\066\255\000\000\178\255\117\255\000\000\000\000\
\031\000\054\000\000\000\000\000\005\000\164\000\077\000\100\000\
\123\000\146\000\013\255\078\255\000\000\000\000\000\000\017\255\
\000\000\000\000\000\000\000\000\149\255\000\000\000\000\111\255\
\000\000\121\255\000\000\180\255\159\255\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\000\000\003\000\000\000\000\000\010\000\000\000\149\000\
\000\000\000\000\000\000\195\255\212\255\186\255\000\000\000\000\
\000\000"

let yytablesize = 578
let yytable = "\095\000\
\012\000\064\000\065\000\066\000\067\000\019\000\013\000\019\000\
\072\000\019\000\023\000\024\000\020\000\016\000\035\000\055\000\
\039\000\055\000\055\000\097\000\036\000\074\000\037\000\001\000\
\094\000\067\000\096\000\032\000\075\000\099\000\102\000\055\000\
\055\000\028\000\105\000\106\000\107\000\108\000\109\000\110\000\
\111\000\112\000\113\000\114\000\115\000\116\000\041\000\055\000\
\098\000\036\000\076\000\037\000\120\000\077\000\021\000\125\000\
\019\000\127\000\081\000\082\000\018\000\135\000\035\000\043\000\
\035\000\044\000\045\000\018\000\046\000\134\000\066\000\066\000\
\047\000\126\000\137\000\018\000\048\000\128\000\022\000\130\000\
\056\000\026\000\056\000\056\000\133\000\049\000\050\000\067\000\
\051\000\052\000\005\000\006\000\007\000\008\000\009\000\010\000\
\027\000\056\000\034\000\053\000\036\000\054\000\055\000\056\000\
\057\000\058\000\059\000\044\000\091\000\037\000\046\000\038\000\
\056\000\040\000\047\000\067\000\067\000\023\000\048\000\059\000\
\093\000\059\000\059\000\061\000\103\000\061\000\061\000\049\000\
\050\000\069\000\051\000\052\000\005\000\006\000\007\000\008\000\
\009\000\010\000\027\000\070\000\071\000\053\000\104\000\054\000\
\055\000\056\000\057\000\058\000\059\000\030\000\030\000\059\000\
\030\000\118\000\121\000\061\000\030\000\122\000\123\000\034\000\
\030\000\062\000\131\000\062\000\062\000\124\000\129\000\136\000\
\007\000\030\000\030\000\008\000\030\000\030\000\030\000\030\000\
\030\000\030\000\030\000\030\000\030\000\064\000\065\000\030\000\
\034\000\030\000\030\000\030\000\030\000\030\000\030\000\044\000\
\063\000\062\000\046\000\000\000\000\000\000\000\047\000\000\000\
\000\000\000\000\048\000\000\000\000\000\046\000\000\000\000\000\
\000\000\047\000\000\000\049\000\050\000\048\000\051\000\052\000\
\000\000\005\000\006\000\007\000\008\000\009\000\010\000\027\000\
\000\000\053\000\000\000\054\000\055\000\056\000\057\000\058\000\
\059\000\000\000\000\000\000\000\053\000\000\000\054\000\055\000\
\056\000\057\000\058\000\059\000\043\000\000\000\043\000\043\000\
\043\000\043\000\043\000\043\000\000\000\000\000\043\000\043\000\
\043\000\043\000\043\000\043\000\043\000\043\000\000\000\049\000\
\000\000\049\000\049\000\000\000\079\000\080\000\081\000\082\000\
\000\000\049\000\049\000\000\000\043\000\000\000\000\000\049\000\
\049\000\000\000\005\000\006\000\007\000\008\000\009\000\010\000\
\011\000\045\000\000\000\045\000\045\000\045\000\045\000\049\000\
\000\000\000\000\000\000\045\000\045\000\045\000\045\000\045\000\
\045\000\045\000\045\000\000\000\000\000\000\000\000\000\000\000\
\046\000\000\000\046\000\046\000\046\000\046\000\000\000\000\000\
\000\000\045\000\046\000\046\000\046\000\046\000\046\000\046\000\
\046\000\046\000\000\000\079\000\080\000\081\000\082\000\051\000\
\000\000\051\000\051\000\085\000\086\000\087\000\088\000\000\000\
\046\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
\051\000\000\000\000\000\000\000\000\000\000\000\052\000\000\000\
\052\000\052\000\000\000\000\000\000\000\000\000\000\000\051\000\
\052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
\000\000\000\000\000\000\000\000\000\000\053\000\000\000\053\000\
\053\000\000\000\000\000\000\000\000\000\000\000\052\000\053\000\
\053\000\053\000\053\000\053\000\053\000\053\000\053\000\000\000\
\000\000\000\000\000\000\000\000\054\000\000\000\054\000\054\000\
\000\000\000\000\000\000\000\000\000\000\053\000\054\000\054\000\
\054\000\054\000\054\000\054\000\054\000\054\000\050\000\031\000\
\050\000\050\000\000\000\000\000\000\000\000\000\000\000\000\000\
\050\000\050\000\000\000\000\000\054\000\000\000\050\000\050\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\005\000\
\006\000\007\000\008\000\009\000\010\000\027\000\050\000\078\000\
\000\000\000\000\000\000\079\000\080\000\081\000\082\000\000\000\
\000\000\083\000\084\000\085\000\086\000\087\000\088\000\089\000\
\090\000\132\000\000\000\000\000\000\000\079\000\080\000\081\000\
\082\000\000\000\000\000\083\000\084\000\085\000\086\000\087\000\
\088\000\089\000\090\000\092\000\000\000\079\000\080\000\081\000\
\082\000\000\000\000\000\083\000\084\000\085\000\086\000\087\000\
\088\000\089\000\090\000\117\000\000\000\079\000\080\000\081\000\
\082\000\000\000\000\000\083\000\084\000\085\000\086\000\087\000\
\088\000\089\000\090\000\119\000\000\000\079\000\080\000\081\000\
\082\000\000\000\000\000\083\000\084\000\085\000\086\000\087\000\
\088\000\089\000\090\000\079\000\080\000\081\000\082\000\000\000\
\000\000\083\000\084\000\085\000\086\000\087\000\088\000\089\000\
\090\000\079\000\080\000\081\000\082\000\000\000\000\000\083\000\
\084\000\085\000\086\000\087\000\088\000\089\000\079\000\080\000\
\081\000\082\000\000\000\000\000\083\000\084\000\085\000\086\000\
\087\000\088\000"

let yycheck = "\070\000\
\000\000\046\000\047\000\048\000\049\000\033\001\004\000\033\001\
\053\000\033\001\003\001\004\001\040\001\004\000\040\001\003\001\
\040\001\005\001\006\001\006\001\006\001\004\001\006\001\001\000\
\069\000\070\000\071\000\025\000\011\001\074\000\075\000\019\001\
\020\001\024\000\079\000\080\000\081\000\082\000\083\000\084\000\
\085\000\086\000\087\000\088\000\089\000\090\000\037\000\035\001\
\035\001\035\001\033\001\035\001\097\000\036\001\001\001\117\000\
\033\001\119\000\009\001\010\001\033\001\132\000\003\001\040\001\
\005\001\001\001\002\001\040\001\004\001\131\000\005\001\006\001\
\008\001\118\000\136\000\040\001\012\001\122\000\038\001\124\000\
\003\001\034\001\005\001\006\001\129\000\021\001\022\001\132\000\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\020\001\040\001\035\001\005\001\037\001\038\001\039\001\
\040\001\041\001\042\001\001\001\002\001\006\001\004\001\003\001\
\035\001\001\001\008\001\005\001\006\001\003\001\012\001\003\001\
\003\001\005\001\006\001\003\001\038\001\005\001\006\001\021\001\
\022\001\004\001\024\001\025\001\026\001\027\001\028\001\029\001\
\030\001\031\001\032\001\004\001\004\001\035\001\040\001\037\001\
\038\001\039\001\040\001\041\001\042\001\001\001\002\001\035\001\
\004\001\003\001\005\001\035\001\008\001\006\001\034\001\003\001\
\012\001\003\001\023\001\005\001\006\001\011\001\011\001\005\001\
\005\001\021\001\022\001\005\001\024\001\025\001\026\001\027\001\
\028\001\029\001\030\001\031\001\032\001\005\001\005\001\035\001\
\005\001\037\001\038\001\039\001\040\001\041\001\042\001\001\001\
\044\000\035\001\004\001\255\255\255\255\255\255\008\001\255\255\
\255\255\255\255\012\001\255\255\255\255\004\001\255\255\255\255\
\255\255\008\001\255\255\021\001\022\001\012\001\024\001\025\001\
\255\255\026\001\027\001\028\001\029\001\030\001\031\001\032\001\
\255\255\035\001\255\255\037\001\038\001\039\001\040\001\041\001\
\042\001\255\255\255\255\255\255\035\001\255\255\037\001\038\001\
\039\001\040\001\041\001\042\001\003\001\255\255\005\001\006\001\
\007\001\008\001\009\001\010\001\255\255\255\255\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\255\255\003\001\
\255\255\005\001\006\001\255\255\007\001\008\001\009\001\010\001\
\255\255\013\001\014\001\255\255\035\001\255\255\255\255\019\001\
\020\001\255\255\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\003\001\255\255\005\001\006\001\007\001\008\001\035\001\
\255\255\255\255\255\255\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\255\255\255\255\255\255\255\255\255\255\
\003\001\255\255\005\001\006\001\007\001\008\001\255\255\255\255\
\255\255\035\001\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\255\255\007\001\008\001\009\001\010\001\003\001\
\255\255\005\001\006\001\015\001\016\001\017\001\018\001\255\255\
\035\001\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\255\255\255\255\255\255\255\255\255\255\003\001\255\255\
\005\001\006\001\255\255\255\255\255\255\255\255\255\255\035\001\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\255\255\255\255\255\255\255\255\255\255\003\001\255\255\005\001\
\006\001\255\255\255\255\255\255\255\255\255\255\035\001\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\255\255\
\255\255\255\255\255\255\255\255\003\001\255\255\005\001\006\001\
\255\255\255\255\255\255\255\255\255\255\035\001\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\003\001\002\001\
\005\001\006\001\255\255\255\255\255\255\255\255\255\255\255\255\
\013\001\014\001\255\255\255\255\035\001\255\255\019\001\020\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\026\001\
\027\001\028\001\029\001\030\001\031\001\032\001\035\001\003\001\
\255\255\255\255\255\255\007\001\008\001\009\001\010\001\255\255\
\255\255\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\003\001\255\255\255\255\255\255\007\001\008\001\009\001\
\010\001\255\255\255\255\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\005\001\255\255\007\001\008\001\009\001\
\010\001\255\255\255\255\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\005\001\255\255\007\001\008\001\009\001\
\010\001\255\255\255\255\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\005\001\255\255\007\001\008\001\009\001\
\010\001\255\255\255\255\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\007\001\008\001\009\001\010\001\255\255\
\255\255\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\007\001\008\001\009\001\010\001\255\255\255\255\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\007\001\008\001\
\009\001\010\001\255\255\255\255\013\001\014\001\015\001\016\001\
\017\001\018\001"

let yynames_const = "\
  LBRACE\000\
  RBRACE\000\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  COMMA\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  ASSIGN\000\
  NOT\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  AND\000\
  OR\000\
  RETURN\000\
  IF\000\
  ELSE\000\
  FOR\000\
  WHILE\000\
  INT\000\
  BOOL\000\
  FLOAT\000\
  VOID\000\
  STRING\000\
  CHAR\000\
  STRUCT\000\
  LSQUARE\000\
  RSQUARE\000\
  PIPE\000\
  DOT\000\
  EOF\000\
  "

let yynames_block = "\
  CHARLIT\000\
  INTLIT\000\
  BLIT\000\
  ID\000\
  FLIT\000\
  STRLIT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 42 "parser.mly"
            ( _1 )
# 405 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "parser.mly"
                 ( ([], [], []) )
# 411 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 46 "parser.mly"
                 ( ((_2 :: fst _1), snd _1, trd _1) )
# 419 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 47 "parser.mly"
                 ( (fst _1, (_2 :: snd _1), trd _1) )
# 427 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'sdecl) in
    Obj.repr(
# 48 "parser.mly"
                 ( (fst _1, snd _1, (_2 :: trd _1)) )
# 435 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : 'special_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 52 "parser.mly"
     ( { typ = _1;
     fname   = _2;
     formals = _4;
     locals  = List.rev (fst _7);
     body    = List.rev (snd _7) } )
# 449 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
                  ( [] )
# 455 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 60 "parser.mly"
                  ( List.rev _1 )
# 462 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'special_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 63 "parser.mly"
                                      ( [(_1, _2)]     )
# 470 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'special_type) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 64 "parser.mly"
                                      ( (_3, _4) :: _1 )
# 479 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    Obj.repr(
# 68 "parser.mly"
    ( { sname = _2;
  members = _4 } )
# 488 "parser.ml"
               : 'sdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
           ( Int    )
# 494 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "parser.mly"
           ( Bool   )
# 500 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "parser.mly"
           ( Float  )
# 506 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser.mly"
           ( Void   )
# 512 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
           ( String )
# 518 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 77 "parser.mly"
           ( Char   )
# 524 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 78 "parser.mly"
              ( Struct (_2) )
# 531 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 82 "parser.mly"
          ( _1 )
# 538 "parser.ml"
               : 'special_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'special_type) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 83 "parser.mly"
                                        ( Array(_1, _3) )
# 546 "parser.ml"
               : 'special_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
                     ( [] )
# 552 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 87 "parser.mly"
                     ( _2 :: _1 )
# 560 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'special_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 90 "parser.mly"
                         ( (_1, _2) )
# 568 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "parser.mly"
                    ( [], [], [] )
# 574 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 94 "parser.mly"
                    ( fst _1, (_2 :: (snd _1)), [] )
# 582 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 95 "parser.mly"
                    ( (_2 :: (fst _1)), snd _1, [] )
# 590 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 98 "parser.mly"
                                            ( Expr _1               )
# 597 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 99 "parser.mly"
                                            ( Return _2             )
# 604 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 100 "parser.mly"
                                            ( Block(List.rev (fst _2), List.rev (snd _2)) )
# 611 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 101 "parser.mly"
                                            ( If(_3, _5, Block([], [])) )
# 619 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 102 "parser.mly"
                                            ( If(_3, _5, _7)        )
# 628 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 104 "parser.mly"
                                            ( For(_3, _5, _7, _9)   )
# 638 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 105 "parser.mly"
                                            ( While(_3, _5)         )
# 646 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 108 "parser.mly"
                  ( Noexpr )
# 652 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "parser.mly"
                  ( _1 )
# 659 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 112 "parser.mly"
         ( _1 :: [] )
# 666 "parser.ml"
               : 'array_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'array_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 113 "parser.mly"
                          ( _3 :: _1 )
# 674 "parser.ml"
               : 'array_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 116 "parser.mly"
                     ( Charlit (_1)           )
# 681 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 117 "parser.mly"
                     ( Intlit(_1)             )
# 688 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 118 "parser.mly"
                     ( BoolLit(_1)            )
# 695 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 119 "parser.mly"
                     ( Fliteral(_1)           )
# 702 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 120 "parser.mly"
                     ( Strlit(_1)             )
# 709 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 121 "parser.mly"
                     ( Id(_1)                 )
# 716 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'array_list) in
    Obj.repr(
# 122 "parser.mly"
                         ( Arraylit(List.rev _2, List.length _2) )
# 723 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 123 "parser.mly"
                     ( Binop(_1, Add,   _3)   )
# 731 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 124 "parser.mly"
                     ( Binop(_1, Sub,   _3)   )
# 739 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 125 "parser.mly"
                     ( Binop(_1, Mult,  _3)   )
# 747 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 126 "parser.mly"
                     ( Binop(_1, Div,   _3)   )
# 755 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 127 "parser.mly"
                     ( Binop(_1, Equal, _3)   )
# 763 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 128 "parser.mly"
                     ( Binop(_1, Neq,   _3)   )
# 771 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 129 "parser.mly"
                     ( Binop(_1, Less,  _3)   )
# 779 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 130 "parser.mly"
                     ( Binop(_1, Leq,   _3)   )
# 787 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 131 "parser.mly"
                     ( Binop(_1, Greater, _3) )
# 795 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 132 "parser.mly"
                     ( Binop(_1, Geq,   _3)   )
# 803 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 133 "parser.mly"
                     ( Binop(_1, And,   _3)   )
# 811 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 134 "parser.mly"
                     ( Binop(_1, Or,    _3)   )
# 819 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 135 "parser.mly"
                         ( Unop(Neg, _2)      )
# 826 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 136 "parser.mly"
                     ( Unop(Not, _2)          )
# 833 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 137 "parser.mly"
                     ( Assign(_1, _3)         )
# 841 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 138 "parser.mly"
                              ( Call(_1, _3)  )
# 849 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 139 "parser.mly"
                              ( Sassign(_1, _3, _5) )
# 858 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 140 "parser.mly"
                                          ( Arrassign(_1, _3, _6) )
# 867 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 141 "parser.mly"
                       ( _2                   )
# 874 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 144 "parser.mly"
                  ( [] )
# 880 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 145 "parser.mly"
               ( List.rev _1 )
# 887 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 148 "parser.mly"
                         ( [_1]     )
# 894 "parser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 149 "parser.mly"
                         ( _3 :: _1 )
# 902 "parser.ml"
               : 'args_list))
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
