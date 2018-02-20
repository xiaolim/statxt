type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
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
  | CHARLIT of (char)
  | STRLIT of (string)
  | INTLIT of (int)
  | BLIT of (bool)
  | ID of (string)
  | FLIT of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 4 "parser.mly"
open Ast

let fst (a,_,_) = a;;
let snd (_,b,_) = b;;
let trd (_,_,c) = c;;

# 52 "parser.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
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
    0 (* EOF *);
    0|]

let yytransl_block = [|
  289 (* CHARLIT *);
  290 (* STRLIT *);
  291 (* INTLIT *);
  292 (* BLIT *);
  293 (* ID *);
  294 (* FLIT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\004\000\007\000\007\000\
\010\000\010\000\005\000\006\000\006\000\006\000\006\000\006\000\
\006\000\008\000\008\000\003\000\009\000\009\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\013\000\013\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\014\000\014\000\
\015\000\015\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\002\000\009\000\000\000\001\000\
\002\000\004\000\006\000\001\000\001\000\001\000\001\000\001\000\
\001\000\000\000\002\000\003\000\000\000\002\000\002\000\003\000\
\003\000\005\000\007\000\009\000\005\000\000\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\002\000\002\000\003\000\004\000\003\000\000\000\001\000\
\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\059\000\000\000\012\000\013\000\014\000\015\000\
\016\000\017\000\000\000\001\000\003\000\004\000\005\000\000\000\
\000\000\000\000\018\000\020\000\000\000\000\000\000\000\000\000\
\000\000\000\000\019\000\000\000\009\000\000\000\000\000\011\000\
\000\000\018\000\000\000\000\000\010\000\000\000\000\000\021\000\
\006\000\000\000\000\000\000\000\000\000\000\000\000\000\035\000\
\036\000\032\000\034\000\000\000\033\000\022\000\000\000\000\000\
\000\000\050\000\051\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\023\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\054\000\
\025\000\024\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\040\000\041\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\053\000\000\000\000\000\000\000\029\000\000\000\000\000\000\000\
\027\000\000\000\000\000\028\000"

let yydgoto = "\002\000\
\003\000\004\000\027\000\014\000\015\000\028\000\024\000\022\000\
\038\000\025\000\054\000\055\000\061\000\087\000\088\000"

let yysindex = "\011\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\237\254\000\000\000\000\000\000\000\000\022\255\
\040\255\020\255\000\000\000\000\116\255\065\255\023\255\039\255\
\051\255\060\255\000\000\032\255\000\000\067\255\116\255\000\000\
\071\255\000\000\044\255\116\255\000\000\041\255\096\255\000\000\
\000\000\096\255\096\255\096\255\082\255\083\255\085\255\000\000\
\000\000\000\000\000\000\005\255\000\000\000\000\218\255\165\000\
\078\255\000\000\000\000\215\000\087\255\096\255\096\255\096\255\
\096\255\096\255\000\000\096\255\096\255\096\255\096\255\096\255\
\096\255\096\255\096\255\096\255\096\255\096\255\096\255\000\000\
\000\000\000\000\183\000\088\255\201\000\215\000\094\255\095\255\
\215\000\038\255\038\255\000\000\000\000\254\000\254\000\213\255\
\213\255\213\255\213\255\242\000\229\000\159\255\096\255\159\255\
\000\000\096\255\084\255\238\255\000\000\215\000\159\255\096\255\
\000\000\102\255\159\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\103\255\000\000\000\000\000\000\
\106\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\115\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\109\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\197\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\053\255\000\000\000\000\109\255\000\000\
\118\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\007\255\000\000\119\255\
\049\255\002\000\033\000\000\000\000\000\133\000\137\000\053\000\
\073\000\093\000\113\000\157\000\008\255\000\000\000\000\000\000\
\000\000\000\000\152\255\000\000\000\000\014\255\000\000\039\255\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\114\000\000\000\000\000\037\000\000\000\090\000\
\085\000\000\000\160\255\217\255\195\255\000\000\000\000"

let yytablesize = 528
let yytable = "\056\000\
\012\000\084\000\058\000\059\000\060\000\107\000\065\000\109\000\
\049\000\057\000\049\000\001\000\057\000\049\000\113\000\066\000\
\058\000\017\000\116\000\058\000\020\000\021\000\083\000\060\000\
\085\000\086\000\089\000\049\000\090\000\091\000\092\000\093\000\
\094\000\095\000\096\000\097\000\098\000\099\000\100\000\101\000\
\016\000\030\000\039\000\019\000\040\000\041\000\070\000\071\000\
\042\000\052\000\114\000\052\000\043\000\031\000\052\000\031\000\
\031\000\023\000\018\000\029\000\032\000\044\000\045\000\108\000\
\046\000\047\000\110\000\035\000\033\000\026\000\034\000\020\000\
\060\000\048\000\049\000\050\000\051\000\052\000\053\000\039\000\
\037\000\040\000\081\000\062\000\063\000\042\000\064\000\082\000\
\103\000\043\000\005\000\006\000\007\000\008\000\009\000\010\000\
\105\000\039\000\044\000\045\000\106\000\046\000\047\000\042\000\
\115\000\007\000\111\000\043\000\008\000\030\000\048\000\049\000\
\050\000\051\000\052\000\053\000\021\000\013\000\021\000\021\000\
\055\000\056\000\021\000\036\000\057\000\000\000\021\000\000\000\
\048\000\049\000\050\000\051\000\052\000\053\000\000\000\021\000\
\021\000\000\000\021\000\021\000\000\000\005\000\006\000\007\000\
\008\000\009\000\010\000\021\000\021\000\021\000\021\000\021\000\
\021\000\026\000\000\000\026\000\026\000\000\000\000\000\026\000\
\039\000\000\000\040\000\026\000\000\000\000\000\042\000\000\000\
\000\000\000\000\043\000\000\000\026\000\026\000\000\000\026\000\
\026\000\000\000\000\000\044\000\045\000\000\000\046\000\047\000\
\026\000\026\000\026\000\026\000\026\000\026\000\000\000\048\000\
\049\000\050\000\051\000\052\000\053\000\037\000\000\000\037\000\
\000\000\000\000\037\000\037\000\037\000\037\000\037\000\000\000\
\000\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
\037\000\000\000\067\000\068\000\069\000\070\000\071\000\000\000\
\068\000\069\000\070\000\071\000\000\000\000\000\072\000\073\000\
\074\000\075\000\076\000\077\000\078\000\079\000\112\000\000\000\
\000\000\000\000\000\000\000\000\068\000\069\000\070\000\071\000\
\000\000\000\000\072\000\073\000\074\000\075\000\076\000\077\000\
\078\000\079\000\038\000\000\000\038\000\000\000\000\000\038\000\
\038\000\038\000\000\000\000\000\000\000\000\000\038\000\038\000\
\038\000\038\000\038\000\038\000\038\000\038\000\000\000\000\000\
\000\000\000\000\005\000\006\000\007\000\008\000\009\000\010\000\
\011\000\039\000\000\000\039\000\000\000\000\000\039\000\039\000\
\039\000\000\000\000\000\000\000\000\000\039\000\039\000\039\000\
\039\000\039\000\039\000\039\000\039\000\044\000\000\000\044\000\
\000\000\000\000\044\000\000\000\000\000\000\000\000\000\000\000\
\000\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
\044\000\045\000\000\000\045\000\000\000\000\000\045\000\000\000\
\000\000\000\000\000\000\000\000\000\000\045\000\045\000\045\000\
\045\000\045\000\045\000\045\000\045\000\046\000\000\000\046\000\
\000\000\000\000\046\000\000\000\000\000\000\000\000\000\000\000\
\000\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
\046\000\047\000\000\000\047\000\000\000\000\000\047\000\000\000\
\000\000\000\000\000\000\000\000\000\000\047\000\047\000\047\000\
\047\000\047\000\047\000\047\000\047\000\042\000\000\000\042\000\
\000\000\043\000\042\000\043\000\000\000\000\000\043\000\000\000\
\000\000\042\000\042\000\000\000\000\000\043\000\043\000\042\000\
\042\000\000\000\000\000\043\000\043\000\048\000\000\000\048\000\
\000\000\000\000\048\000\000\000\000\000\000\000\000\000\080\000\
\000\000\000\000\000\000\068\000\069\000\070\000\071\000\048\000\
\048\000\072\000\073\000\074\000\075\000\076\000\077\000\078\000\
\079\000\102\000\000\000\000\000\000\000\068\000\069\000\070\000\
\071\000\000\000\000\000\072\000\073\000\074\000\075\000\076\000\
\077\000\078\000\079\000\104\000\000\000\000\000\000\000\068\000\
\069\000\070\000\071\000\000\000\000\000\072\000\073\000\074\000\
\075\000\076\000\077\000\078\000\079\000\068\000\069\000\070\000\
\071\000\000\000\000\000\072\000\073\000\074\000\075\000\076\000\
\077\000\078\000\079\000\068\000\069\000\070\000\071\000\000\000\
\000\000\072\000\073\000\074\000\075\000\076\000\077\000\078\000\
\068\000\069\000\070\000\071\000\000\000\000\000\072\000\073\000\
\074\000\075\000\076\000\077\000\068\000\069\000\070\000\071\000\
\000\000\000\000\000\000\000\000\074\000\075\000\076\000\077\000"

let yycheck = "\039\000\
\000\000\063\000\042\000\043\000\044\000\102\000\002\001\104\000\
\001\001\003\001\003\001\001\000\006\001\006\001\111\000\011\001\
\003\001\037\001\115\000\006\001\001\001\002\001\062\000\063\000\
\064\000\065\000\066\000\020\001\068\000\069\000\070\000\071\000\
\072\000\073\000\074\000\075\000\076\000\077\000\078\000\079\000\
\004\000\003\001\002\001\004\001\004\001\005\001\009\001\010\001\
\008\001\001\001\112\000\003\001\012\001\001\001\006\001\003\001\
\006\001\021\000\037\001\037\001\001\001\021\001\022\001\103\000\
\024\001\025\001\106\000\031\000\037\001\005\001\004\001\001\001\
\112\000\033\001\034\001\035\001\036\001\037\001\038\001\002\001\
\037\001\004\001\005\001\002\001\002\001\008\001\002\001\001\001\
\001\001\012\001\026\001\027\001\028\001\029\001\030\001\031\001\
\003\001\002\001\021\001\022\001\006\001\024\001\025\001\008\001\
\003\001\003\001\023\001\012\001\003\001\001\001\033\001\034\001\
\035\001\036\001\037\001\038\001\002\001\004\000\004\001\005\001\
\003\001\003\001\008\001\034\000\040\000\255\255\012\001\255\255\
\033\001\034\001\035\001\036\001\037\001\038\001\255\255\021\001\
\022\001\255\255\024\001\025\001\255\255\026\001\027\001\028\001\
\029\001\030\001\031\001\033\001\034\001\035\001\036\001\037\001\
\038\001\002\001\255\255\004\001\005\001\255\255\255\255\008\001\
\002\001\255\255\004\001\012\001\255\255\255\255\008\001\255\255\
\255\255\255\255\012\001\255\255\021\001\022\001\255\255\024\001\
\025\001\255\255\255\255\021\001\022\001\255\255\024\001\025\001\
\033\001\034\001\035\001\036\001\037\001\038\001\255\255\033\001\
\034\001\035\001\036\001\037\001\038\001\001\001\255\255\003\001\
\255\255\255\255\006\001\007\001\008\001\009\001\010\001\255\255\
\255\255\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\255\255\001\001\007\001\008\001\009\001\010\001\255\255\
\007\001\008\001\009\001\010\001\255\255\255\255\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\001\001\255\255\
\255\255\255\255\255\255\255\255\007\001\008\001\009\001\010\001\
\255\255\255\255\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\001\001\255\255\003\001\255\255\255\255\006\001\
\007\001\008\001\255\255\255\255\255\255\255\255\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\255\255\255\255\
\255\255\255\255\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\001\001\255\255\003\001\255\255\255\255\006\001\007\001\
\008\001\255\255\255\255\255\255\255\255\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\001\001\255\255\003\001\
\255\255\255\255\006\001\255\255\255\255\255\255\255\255\255\255\
\255\255\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\001\001\255\255\003\001\255\255\255\255\006\001\255\255\
\255\255\255\255\255\255\255\255\255\255\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\001\001\255\255\003\001\
\255\255\255\255\006\001\255\255\255\255\255\255\255\255\255\255\
\255\255\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\001\001\255\255\003\001\255\255\255\255\006\001\255\255\
\255\255\255\255\255\255\255\255\255\255\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\001\001\255\255\003\001\
\255\255\001\001\006\001\003\001\255\255\255\255\006\001\255\255\
\255\255\013\001\014\001\255\255\255\255\013\001\014\001\019\001\
\020\001\255\255\255\255\019\001\020\001\001\001\255\255\003\001\
\255\255\255\255\006\001\255\255\255\255\255\255\255\255\003\001\
\255\255\255\255\255\255\007\001\008\001\009\001\010\001\019\001\
\020\001\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\003\001\255\255\255\255\255\255\007\001\008\001\009\001\
\010\001\255\255\255\255\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\003\001\255\255\255\255\255\255\007\001\
\008\001\009\001\010\001\255\255\255\255\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\007\001\008\001\009\001\
\010\001\255\255\255\255\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\007\001\008\001\009\001\010\001\255\255\
\255\255\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\007\001\008\001\009\001\010\001\255\255\255\255\013\001\014\001\
\015\001\016\001\017\001\018\001\007\001\008\001\009\001\010\001\
\255\255\255\255\255\255\255\255\015\001\016\001\017\001\018\001"

let yynames_const = "\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
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
  EOF\000\
  "

let yynames_block = "\
  CHARLIT\000\
  STRLIT\000\
  INTLIT\000\
  BLIT\000\
  ID\000\
  FLIT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 41 "parser.mly"
            ( _1 )
# 366 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "parser.mly"
                 ( ([], [], []) )
# 372 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 45 "parser.mly"
               ( ((_2 :: fst _1), snd _1, trd _1) )
# 380 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 46 "parser.mly"
               ( (fst _1, (_2 :: snd _1), trd _1) )
# 388 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'sdecl) in
    Obj.repr(
# 47 "parser.mly"
               ( (fst _1, snd _1, (_2 :: trd _1)))
# 396 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 51 "parser.mly"
     ( { typ = _1;
	 fname = _2;
	 formals = _4;
	 locals = List.rev _7;
	 body = List.rev _8 } )
# 411 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "parser.mly"
                  ( [] )
# 417 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 59 "parser.mly"
                  ( List.rev _1 )
# 424 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 62 "parser.mly"
                             ( [(_1,_2)]     )
# 432 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 63 "parser.mly"
                             ( (_3,_4) :: _1 )
# 441 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    Obj.repr(
# 67 "parser.mly"
    ( { sname = _2;
  members = _4 } )
# 450 "parser.ml"
               : 'sdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "parser.mly"
          ( Int   )
# 456 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
          ( Bool  )
# 462 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "parser.mly"
          ( Float )
# 468 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "parser.mly"
          ( Void  )
# 474 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser.mly"
           ( String )
# 480 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
         ( Char )
# 486 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "parser.mly"
                     ( [] )
# 492 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 80 "parser.mly"
                     ( _2 :: _1 )
# 500 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 83 "parser.mly"
               ( (_1, _2) )
# 508 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
                   ( [] )
# 514 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 87 "parser.mly"
                   ( _2 :: _1 )
# 522 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 90 "parser.mly"
                                            ( Expr _1               )
# 529 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 91 "parser.mly"
                                            ( Return _2             )
# 536 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 92 "parser.mly"
                                            ( Block(List.rev _2)    )
# 543 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 93 "parser.mly"
                                            ( If(_3, _5, Block([])) )
# 551 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 94 "parser.mly"
                                            ( If(_3, _5, _7)        )
# 560 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 96 "parser.mly"
                                            ( For(_3, _5, _7, _9)   )
# 570 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 97 "parser.mly"
                                            ( While(_3, _5)         )
# 578 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 100 "parser.mly"
                  ( Noexpr )
# 584 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "parser.mly"
                  ( _1 )
# 591 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 104 "parser.mly"
                     ( Intlit(_1)             )
# 598 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 105 "parser.mly"
                    ( Fliteral(_1)           )
# 605 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 106 "parser.mly"
                     ( BoolLit(_1)            )
# 612 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 107 "parser.mly"
                     ( Charlit (_1)           )
# 619 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 108 "parser.mly"
                     ( Strlit(_1)             )
# 626 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 109 "parser.mly"
                     ( Id(_1)                 )
# 633 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "parser.mly"
                     ( Binop(_1, Add,   _3)   )
# 641 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "parser.mly"
                     ( Binop(_1, Sub,   _3)   )
# 649 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 112 "parser.mly"
                     ( Binop(_1, Mult,  _3)   )
# 657 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 113 "parser.mly"
                     ( Binop(_1, Div,   _3)   )
# 665 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 114 "parser.mly"
                     ( Binop(_1, Equal, _3)   )
# 673 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 115 "parser.mly"
                     ( Binop(_1, Neq,   _3)   )
# 681 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 116 "parser.mly"
                     ( Binop(_1, Less,  _3)   )
# 689 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 117 "parser.mly"
                     ( Binop(_1, Leq,   _3)   )
# 697 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 118 "parser.mly"
                     ( Binop(_1, Greater, _3) )
# 705 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "parser.mly"
                     ( Binop(_1, Geq,   _3)   )
# 713 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "parser.mly"
                     ( Binop(_1, And,   _3)   )
# 721 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "parser.mly"
                     ( Binop(_1, Or,    _3)   )
# 729 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 122 "parser.mly"
                         ( Unop(Neg, _2)      )
# 736 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 123 "parser.mly"
                     ( Unop(Not, _2)          )
# 743 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 124 "parser.mly"
                     ( Assign(_1, _3)         )
# 751 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 125 "parser.mly"
                              ( Call(_1, _3)  )
# 759 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 126 "parser.mly"
                       ( _2                   )
# 766 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 129 "parser.mly"
                  ( [] )
# 772 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 130 "parser.mly"
               ( List.rev _1 )
# 779 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 133 "parser.mly"
                            ( [_1] )
# 786 "parser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 134 "parser.mly"
                         ( _3 :: _1 )
# 794 "parser.ml"
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
