type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LSQUARE
  | RSQUARE
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | NOT
  | DECREMENT
  | INCREMENT
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | TRUE
  | FALSE
  | AND
  | OR
  | DOT
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | INT
  | FLOAT
  | BOOL
  | VOID
  | LENGTH
  | CHAR
  | STRING
  | OF
  | STRUCT
  | LINDEX
  | RINDEX
  | NUM_LIT of (int)
  | FLOAT_LIT of (float)
  | STRING_LIT of (string)
  | CHAR_LITERAL of (char)
  | ID of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
