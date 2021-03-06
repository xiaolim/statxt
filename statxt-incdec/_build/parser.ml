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
  | DECREMENT
  | INCREMENT
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

# 58 "parser.ml"
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
  268 (* DECREMENT *);
  269 (* INCREMENT *);
  270 (* NOT *);
  271 (* EQ *);
  272 (* NEQ *);
  273 (* LT *);
  274 (* LEQ *);
  275 (* GT *);
  276 (* GEQ *);
  277 (* AND *);
  278 (* OR *);
  279 (* RETURN *);
  280 (* IF *);
  281 (* ELSE *);
  282 (* FOR *);
  283 (* WHILE *);
  284 (* INT *);
  285 (* BOOL *);
  286 (* FLOAT *);
  287 (* VOID *);
  288 (* STRING *);
  289 (* CHAR *);
  290 (* STRUCT *);
  291 (* LSQUARE *);
  292 (* RSQUARE *);
  293 (* PIPE *);
  294 (* DOT *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  295 (* CHARLIT *);
  296 (* INTLIT *);
  297 (* BLIT *);
  298 (* ID *);
  299 (* FLIT *);
  300 (* STRLIT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\004\000\007\000\007\000\
\009\000\009\000\005\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\006\000\006\000\010\000\010\000\003\000\008\000\
\008\000\008\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\014\000\014\000\015\000\015\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\016\000\016\000\017\000\017\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\002\000\008\000\000\000\001\000\
\002\000\004\000\006\000\001\000\001\000\001\000\001\000\001\000\
\001\000\002\000\001\000\004\000\000\000\002\000\003\000\000\000\
\002\000\002\000\002\000\003\000\003\000\005\000\007\000\009\000\
\005\000\000\000\001\000\001\000\003\000\001\000\001\000\001\000\
\001\000\001\000\001\000\002\000\002\000\004\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\002\000\002\000\003\000\004\000\003\000\
\003\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\070\000\000\000\012\000\013\000\014\000\015\000\
\016\000\017\000\000\000\001\000\003\000\004\000\005\000\000\000\
\019\000\000\000\000\000\000\000\021\000\000\000\023\000\000\000\
\000\000\020\000\000\000\000\000\000\000\000\000\000\000\022\000\
\000\000\018\000\009\000\000\000\000\000\011\000\000\000\024\000\
\000\000\000\000\010\000\024\000\006\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\038\000\039\000\040\000\
\000\000\041\000\042\000\026\000\025\000\000\000\000\000\000\000\
\060\000\061\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\027\000\000\000\000\000\000\000\000\000\
\000\000\045\000\044\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\029\000\065\000\028\000\000\000\
\000\000\000\000\000\000\047\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\064\000\000\000\000\000\
\000\000\000\000\063\000\000\000\046\000\000\000\000\000\033\000\
\000\000\000\000\000\000\031\000\000\000\000\000\032\000"

let yydgoto = "\002\000\
\003\000\004\000\060\000\014\000\015\000\033\000\029\000\042\000\
\030\000\025\000\017\000\061\000\062\000\068\000\073\000\102\000\
\103\000"

let yysindex = "\010\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\230\254\000\000\000\000\000\000\000\000\227\254\
\000\000\020\255\019\255\071\255\000\000\036\255\000\000\113\001\
\225\255\000\000\039\255\228\254\080\255\084\255\100\255\000\000\
\233\254\000\000\000\000\111\255\113\001\000\000\115\255\000\000\
\023\255\065\255\000\000\000\000\000\000\208\255\208\255\208\255\
\208\255\116\255\117\255\120\255\208\255\000\000\000\000\000\000\
\025\255\000\000\000\000\000\000\000\000\034\000\109\255\126\000\
\000\000\000\000\150\001\119\255\208\255\208\255\208\255\150\001\
\018\255\208\255\208\255\000\000\208\255\208\255\208\255\208\255\
\208\255\000\000\000\000\208\255\208\255\208\255\208\255\208\255\
\208\255\208\255\208\255\077\255\000\000\000\000\000\000\150\000\
\126\255\174\000\208\255\000\000\150\001\129\255\108\255\118\001\
\092\255\092\255\093\255\093\255\150\001\198\001\198\001\118\255\
\118\255\118\255\118\255\182\001\166\001\000\000\197\255\208\255\
\197\255\150\001\000\000\208\255\000\000\121\255\058\000\000\000\
\150\001\197\255\208\255\000\000\140\255\197\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\029\255\000\000\000\000\000\000\000\000\000\000\142\255\
\000\000\000\000\000\000\000\000\000\000\154\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\159\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\005\255\000\000\000\000\159\255\000\000\026\255\
\000\000\158\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\072\255\000\000\160\255\000\000\
\198\000\220\000\082\000\104\000\012\255\074\001\096\001\242\000\
\008\001\030\001\052\001\017\255\046\255\000\000\000\000\000\000\
\000\000\050\255\000\000\000\000\000\000\153\255\000\000\000\000\
\110\255\000\000\161\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\059\000\000\000\000\000\046\000\000\000\120\000\
\000\000\000\000\000\000\039\000\212\255\186\255\000\000\000\000\
\000\000"

let yytablesize = 748
let yytable = "\097\000\
\012\000\064\000\065\000\066\000\067\000\019\000\019\000\035\000\
\072\000\035\000\001\000\019\000\020\000\035\000\062\000\018\000\
\062\000\062\000\039\000\058\000\021\000\058\000\058\000\099\000\
\096\000\067\000\098\000\058\000\074\000\101\000\104\000\036\000\
\105\000\106\000\107\000\108\000\109\000\058\000\058\000\110\000\
\111\000\112\000\113\000\114\000\115\000\116\000\117\000\062\000\
\059\000\016\000\059\000\059\000\058\000\100\000\122\000\037\000\
\059\000\019\000\022\000\075\000\133\000\036\000\013\000\018\000\
\043\000\044\000\045\000\059\000\046\000\028\000\018\000\026\000\
\047\000\023\000\024\000\127\000\068\000\068\000\048\000\129\000\
\034\000\059\000\041\000\032\000\036\000\037\000\067\000\049\000\
\050\000\037\000\051\000\052\000\005\000\006\000\007\000\008\000\
\009\000\010\000\027\000\053\000\079\000\080\000\038\000\054\000\
\055\000\056\000\057\000\058\000\059\000\044\000\093\000\040\000\
\046\000\124\000\069\000\069\000\047\000\023\000\118\000\069\000\
\070\000\095\000\048\000\071\000\077\000\078\000\079\000\080\000\
\120\000\092\000\092\000\049\000\050\000\123\000\051\000\052\000\
\005\000\006\000\007\000\008\000\009\000\010\000\027\000\053\000\
\134\000\130\000\007\000\054\000\055\000\056\000\057\000\058\000\
\059\000\030\000\030\000\092\000\030\000\126\000\008\000\128\000\
\030\000\034\000\066\000\063\000\067\000\034\000\030\000\000\000\
\132\000\000\000\000\000\000\000\135\000\000\000\000\000\030\000\
\030\000\000\000\030\000\030\000\030\000\030\000\030\000\030\000\
\030\000\030\000\030\000\030\000\000\000\000\000\000\000\030\000\
\030\000\030\000\030\000\030\000\030\000\044\000\000\000\000\000\
\046\000\000\000\000\000\000\000\047\000\000\000\000\000\000\000\
\000\000\000\000\048\000\046\000\000\000\000\000\000\000\047\000\
\000\000\000\000\000\000\049\000\050\000\048\000\051\000\052\000\
\000\000\000\000\031\000\000\000\000\000\000\000\000\000\053\000\
\000\000\000\000\000\000\054\000\055\000\056\000\057\000\058\000\
\059\000\000\000\053\000\000\000\000\000\000\000\054\000\055\000\
\056\000\057\000\058\000\059\000\005\000\006\000\007\000\008\000\
\009\000\010\000\027\000\000\000\043\000\000\000\043\000\043\000\
\043\000\043\000\043\000\043\000\043\000\043\000\043\000\000\000\
\043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
\000\000\000\000\000\000\000\000\005\000\006\000\007\000\008\000\
\009\000\010\000\011\000\000\000\076\000\043\000\000\000\043\000\
\077\000\078\000\079\000\080\000\081\000\082\000\083\000\000\000\
\084\000\085\000\086\000\087\000\088\000\089\000\090\000\091\000\
\000\000\000\000\000\000\000\000\131\000\000\000\000\000\000\000\
\077\000\078\000\079\000\080\000\081\000\082\000\083\000\092\000\
\084\000\085\000\086\000\087\000\088\000\089\000\090\000\091\000\
\000\000\000\000\000\000\000\000\050\000\000\000\050\000\050\000\
\050\000\050\000\050\000\050\000\050\000\050\000\050\000\092\000\
\050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
\000\000\000\000\051\000\000\000\051\000\051\000\051\000\051\000\
\051\000\051\000\051\000\051\000\051\000\050\000\051\000\051\000\
\051\000\051\000\051\000\051\000\051\000\051\000\000\000\000\000\
\000\000\000\000\094\000\000\000\077\000\078\000\079\000\080\000\
\081\000\082\000\083\000\051\000\084\000\085\000\086\000\087\000\
\088\000\089\000\090\000\091\000\000\000\000\000\000\000\000\000\
\000\000\000\000\119\000\000\000\077\000\078\000\079\000\080\000\
\081\000\082\000\083\000\092\000\084\000\085\000\086\000\087\000\
\088\000\089\000\090\000\091\000\000\000\000\000\000\000\000\000\
\000\000\000\000\121\000\000\000\077\000\078\000\079\000\080\000\
\081\000\082\000\083\000\092\000\084\000\085\000\086\000\087\000\
\088\000\089\000\090\000\091\000\000\000\000\000\000\000\000\000\
\048\000\000\000\048\000\048\000\048\000\048\000\000\000\000\000\
\048\000\048\000\048\000\092\000\048\000\048\000\048\000\048\000\
\048\000\048\000\048\000\048\000\000\000\000\000\049\000\000\000\
\049\000\049\000\049\000\049\000\000\000\000\000\049\000\049\000\
\049\000\048\000\049\000\049\000\049\000\049\000\049\000\049\000\
\049\000\049\000\000\000\000\000\054\000\000\000\054\000\054\000\
\000\000\000\000\000\000\000\000\054\000\054\000\054\000\049\000\
\054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
\000\000\000\000\055\000\000\000\055\000\055\000\000\000\000\000\
\000\000\000\000\055\000\055\000\055\000\054\000\055\000\055\000\
\055\000\055\000\055\000\055\000\055\000\055\000\000\000\000\000\
\056\000\000\000\056\000\056\000\000\000\000\000\000\000\000\000\
\056\000\056\000\056\000\055\000\056\000\056\000\056\000\056\000\
\056\000\056\000\056\000\056\000\000\000\000\000\057\000\000\000\
\057\000\057\000\000\000\000\000\000\000\000\000\057\000\057\000\
\057\000\056\000\057\000\057\000\057\000\057\000\057\000\057\000\
\057\000\057\000\000\000\000\000\052\000\000\000\052\000\052\000\
\000\000\000\000\000\000\000\000\052\000\052\000\052\000\057\000\
\052\000\052\000\000\000\000\000\000\000\000\000\052\000\052\000\
\000\000\000\000\053\000\000\000\053\000\053\000\000\000\000\000\
\000\000\000\000\053\000\053\000\053\000\052\000\053\000\053\000\
\000\000\000\000\000\000\000\000\053\000\053\000\000\000\000\000\
\000\000\000\000\000\000\000\000\077\000\078\000\079\000\080\000\
\081\000\082\000\083\000\053\000\084\000\085\000\086\000\087\000\
\088\000\089\000\090\000\091\000\005\000\006\000\007\000\008\000\
\009\000\010\000\027\000\000\000\000\000\000\000\000\000\000\000\
\000\000\125\000\000\000\092\000\077\000\078\000\079\000\080\000\
\081\000\082\000\083\000\000\000\084\000\085\000\086\000\087\000\
\088\000\089\000\090\000\091\000\077\000\078\000\079\000\080\000\
\000\000\082\000\083\000\000\000\084\000\085\000\086\000\087\000\
\088\000\089\000\090\000\092\000\077\000\078\000\079\000\080\000\
\000\000\082\000\083\000\000\000\084\000\085\000\086\000\087\000\
\088\000\089\000\000\000\092\000\077\000\078\000\079\000\080\000\
\000\000\000\000\000\000\000\000\000\000\000\000\086\000\087\000\
\088\000\089\000\000\000\092\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\092\000"

let yycheck = "\070\000\
\000\000\046\000\047\000\048\000\049\000\035\001\035\001\003\001\
\053\000\005\001\001\000\035\001\042\001\042\001\003\001\042\001\
\005\001\006\001\042\001\003\001\001\001\005\001\006\001\006\001\
\069\000\070\000\071\000\011\001\004\001\074\000\075\000\006\001\
\077\000\078\000\079\000\080\000\081\000\021\001\022\001\084\000\
\085\000\086\000\087\000\088\000\089\000\090\000\091\000\036\001\
\003\001\004\000\005\001\006\001\036\001\036\001\099\000\006\001\
\011\001\035\001\040\001\035\001\131\000\036\001\004\000\035\001\
\042\001\001\001\002\001\022\001\004\001\024\000\042\001\036\001\
\008\001\003\001\004\001\120\000\005\001\006\001\014\001\124\000\
\042\001\036\001\037\000\025\000\005\001\036\001\131\000\023\001\
\024\001\006\001\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\035\001\009\001\010\001\003\001\039\001\
\040\001\041\001\042\001\043\001\044\001\001\001\002\001\001\001\
\004\001\006\001\005\001\006\001\008\001\003\001\042\001\004\001\
\004\001\003\001\014\001\004\001\007\001\008\001\009\001\010\001\
\003\001\038\001\038\001\023\001\024\001\005\001\026\001\027\001\
\028\001\029\001\030\001\031\001\032\001\033\001\034\001\035\001\
\005\001\025\001\005\001\039\001\040\001\041\001\042\001\043\001\
\044\001\001\001\002\001\038\001\004\001\119\000\005\001\121\000\
\008\001\003\001\005\001\044\000\005\001\005\001\014\001\255\255\
\130\000\255\255\255\255\255\255\134\000\255\255\255\255\023\001\
\024\001\255\255\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\035\001\255\255\255\255\255\255\039\001\
\040\001\041\001\042\001\043\001\044\001\001\001\255\255\255\255\
\004\001\255\255\255\255\255\255\008\001\255\255\255\255\255\255\
\255\255\255\255\014\001\004\001\255\255\255\255\255\255\008\001\
\255\255\255\255\255\255\023\001\024\001\014\001\026\001\027\001\
\255\255\255\255\002\001\255\255\255\255\255\255\255\255\035\001\
\255\255\255\255\255\255\039\001\040\001\041\001\042\001\043\001\
\044\001\255\255\035\001\255\255\255\255\255\255\039\001\040\001\
\041\001\042\001\043\001\044\001\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\255\255\003\001\255\255\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\012\001\013\001\255\255\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\255\255\255\255\255\255\255\255\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\255\255\003\001\036\001\255\255\038\001\
\007\001\008\001\009\001\010\001\011\001\012\001\013\001\255\255\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\255\255\255\255\255\255\255\255\003\001\255\255\255\255\255\255\
\007\001\008\001\009\001\010\001\011\001\012\001\013\001\038\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\255\255\255\255\255\255\255\255\003\001\255\255\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\012\001\013\001\038\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\255\255\255\255\003\001\255\255\005\001\006\001\007\001\008\001\
\009\001\010\001\011\001\012\001\013\001\036\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\255\255\255\255\
\255\255\255\255\005\001\255\255\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\036\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\255\255\255\255\255\255\255\255\
\255\255\255\255\005\001\255\255\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\038\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\255\255\255\255\255\255\255\255\
\255\255\255\255\005\001\255\255\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\038\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\255\255\255\255\255\255\255\255\
\003\001\255\255\005\001\006\001\007\001\008\001\255\255\255\255\
\011\001\012\001\013\001\038\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\255\255\255\255\003\001\255\255\
\005\001\006\001\007\001\008\001\255\255\255\255\011\001\012\001\
\013\001\036\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\255\255\255\255\003\001\255\255\005\001\006\001\
\255\255\255\255\255\255\255\255\011\001\012\001\013\001\036\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\255\255\255\255\003\001\255\255\005\001\006\001\255\255\255\255\
\255\255\255\255\011\001\012\001\013\001\036\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\255\255\255\255\
\003\001\255\255\005\001\006\001\255\255\255\255\255\255\255\255\
\011\001\012\001\013\001\036\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\255\255\255\255\003\001\255\255\
\005\001\006\001\255\255\255\255\255\255\255\255\011\001\012\001\
\013\001\036\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\255\255\255\255\003\001\255\255\005\001\006\001\
\255\255\255\255\255\255\255\255\011\001\012\001\013\001\036\001\
\015\001\016\001\255\255\255\255\255\255\255\255\021\001\022\001\
\255\255\255\255\003\001\255\255\005\001\006\001\255\255\255\255\
\255\255\255\255\011\001\012\001\013\001\036\001\015\001\016\001\
\255\255\255\255\255\255\255\255\021\001\022\001\255\255\255\255\
\255\255\255\255\255\255\255\255\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\036\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\255\255\255\255\255\255\255\255\255\255\
\255\255\036\001\255\255\038\001\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\255\255\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\007\001\008\001\009\001\010\001\
\255\255\012\001\013\001\255\255\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\038\001\007\001\008\001\009\001\010\001\
\255\255\012\001\013\001\255\255\015\001\016\001\017\001\018\001\
\019\001\020\001\255\255\038\001\007\001\008\001\009\001\010\001\
\255\255\255\255\255\255\255\255\255\255\255\255\017\001\018\001\
\019\001\020\001\255\255\038\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\038\001"

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
  DECREMENT\000\
  INCREMENT\000\
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
# 44 "parser.mly"
            ( _1 )
# 450 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "parser.mly"
                 ( ([], [], []) )
# 456 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 48 "parser.mly"
                 ( ((_2 :: fst _1), snd _1, trd _1) )
# 464 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 49 "parser.mly"
                 ( (fst _1, (_2 :: snd _1), trd _1) )
# 472 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'sdecl) in
    Obj.repr(
# 50 "parser.mly"
                 ( (fst _1, snd _1, (_2 :: trd _1)) )
# 480 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : 'special_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 54 "parser.mly"
     ( { typ = _1;
     fname   = _2;
     formals = _4;
     locals  = List.rev (fst _7);
     body    = List.rev (snd _7) } )
# 494 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
                  ( [] )
# 500 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 62 "parser.mly"
                  ( List.rev _1 )
# 507 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'special_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 65 "parser.mly"
                                      ( [(_1, _2)]     )
# 515 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'special_type) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 66 "parser.mly"
                                      ( (_3, _4) :: _1 )
# 524 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    Obj.repr(
# 70 "parser.mly"
    ( { sname = _2;
  members = _4 } )
# 533 "parser.ml"
               : 'sdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "parser.mly"
           ( Int    )
# 539 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser.mly"
           ( Bool   )
# 545 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
           ( Float  )
# 551 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 77 "parser.mly"
           ( Void   )
# 557 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "parser.mly"
           ( String )
# 563 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "parser.mly"
           ( Char   )
# 569 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 80 "parser.mly"
              ( Struct (_2) )
# 576 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 84 "parser.mly"
          ( _1 )
# 583 "parser.ml"
               : 'special_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'special_type) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 85 "parser.mly"
                                        ( Array(_1, _3) )
# 591 "parser.ml"
               : 'special_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "parser.mly"
                     ( [] )
# 597 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 89 "parser.mly"
                     ( _2 :: _1 )
# 605 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'special_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 92 "parser.mly"
                         ( (_1, _2) )
# 613 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "parser.mly"
                    ( [], [], [] )
# 619 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 96 "parser.mly"
                    ( fst _1, (_2 :: (snd _1)), [] )
# 627 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 97 "parser.mly"
                    ( (_2 :: (fst _1)), snd _1, [] )
# 635 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 100 "parser.mly"
                                            ( Expr _1               )
# 642 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 101 "parser.mly"
                                            ( Return _2             )
# 649 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 102 "parser.mly"
                                            ( Block(List.rev (fst _2), List.rev (snd _2)) )
# 656 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 103 "parser.mly"
                                            ( If(_3, _5, Block([], [])) )
# 664 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 104 "parser.mly"
                                            ( If(_3, _5, _7)        )
# 673 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 106 "parser.mly"
                                            ( For(_3, _5, _7, _9)   )
# 683 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 107 "parser.mly"
                                            ( While(_3, _5)         )
# 691 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 110 "parser.mly"
                  ( Noexpr )
# 697 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "parser.mly"
                  ( _1 )
# 704 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 114 "parser.mly"
         ( _1 :: [] )
# 711 "parser.ml"
               : 'array_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'array_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 115 "parser.mly"
                          ( _3 :: _1 )
# 719 "parser.ml"
               : 'array_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 118 "parser.mly"
                     ( Charlit (_1)           )
# 726 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 119 "parser.mly"
                     ( Intlit(_1)             )
# 733 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 120 "parser.mly"
                     ( BoolLit(_1)            )
# 740 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 121 "parser.mly"
                     ( Fliteral(_1)           )
# 747 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 122 "parser.mly"
                     ( Strlit(_1)             )
# 754 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 123 "parser.mly"
                     ( Id(_1)                 )
# 761 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 124 "parser.mly"
                       ( IncDec(_1, Inc)        )
# 768 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 125 "parser.mly"
                       ( IncDec(_1, Dec)        )
# 775 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 126 "parser.mly"
                            ( Arraccess(_1, _3) )
# 783 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'array_list) in
    Obj.repr(
# 127 "parser.mly"
                               ( Arraylit(List.rev _2, List.length _2) )
# 790 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 128 "parser.mly"
                     ( Binop(_1, Add,   _3)   )
# 798 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 129 "parser.mly"
                     ( Binop(_1, Sub,   _3)   )
# 806 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 130 "parser.mly"
                     ( Binop(_1, Mult,  _3)   )
# 814 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 131 "parser.mly"
                     ( Binop(_1, Div,   _3)   )
# 822 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 132 "parser.mly"
                     ( Binop(_1, Equal, _3)   )
# 830 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 133 "parser.mly"
                     ( Binop(_1, Neq,   _3)   )
# 838 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 134 "parser.mly"
                     ( Binop(_1, Less,  _3)   )
# 846 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 135 "parser.mly"
                     ( Binop(_1, Leq,   _3)   )
# 854 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 136 "parser.mly"
                     ( Binop(_1, Greater, _3) )
# 862 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 137 "parser.mly"
                     ( Binop(_1, Geq,   _3)   )
# 870 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 138 "parser.mly"
                     ( Binop(_1, And,   _3)   )
# 878 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 139 "parser.mly"
                     ( Binop(_1, Or,    _3)   )
# 886 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 140 "parser.mly"
                         ( Unop(Neg, _2)      )
# 893 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 141 "parser.mly"
                     ( Unop(Not, _2)          )
# 900 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 142 "parser.mly"
                       ( Assign(_1, _3)         )
# 908 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 143 "parser.mly"
                              ( Call(_1, _3)  )
# 916 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 144 "parser.mly"
                   ( Sretrieve (_1, _3)     )
# 924 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 146 "parser.mly"
                       ( _2                   )
# 931 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 149 "parser.mly"
                  ( [] )
# 937 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 150 "parser.mly"
               ( List.rev _1 )
# 944 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 153 "parser.mly"
                         ( [_1]     )
# 951 "parser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 154 "parser.mly"
                         ( _3 :: _1 )
# 959 "parser.ml"
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
