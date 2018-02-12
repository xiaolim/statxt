(* Ocamllex scanner for Statxt *)

{ open Parser }

let digits = ['0'='9']
let chars = [' '='!' '#'='[' ']'='~']
let strings = '"' ((chars)* as str) '"'
let floats = ((digits)+ '.' (digits)*) | ((digits)* '.' (digits)+)

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)

(* comments *)
| "/*"     { block_comment lexbuf }           (* Comments *)
| "//"     { line_comment lexbuf }

(*  *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LSQUARE }
| ']'      { RSQUARE }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '^'      { CONCAT }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '%'      { MOD }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "bool"   { BOOL }
| "void"   { VOID }
| "true"   { TRUE }
| "false"  { FALSE }
| "float"  { FLOAT }
| "char"   { CHAR }
| "[]"     { ARRAY }
| "struct" { STRUCT }

| (digits)+ as lxm { INT_LIT(int_of_string lxm) }
| floats as lxm { FLOAT_LIT(float_of_string lxm) }
| strings as { STR_LIT(str) }
|
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and block_comment = parse
  "*/" { token lexbuf }
| _    { block_comment lexbuf }

and line_comment = parse
  '\n' { token lexbuf }
| _    { line_comment lexbuf }
