(* Ocamllex scanner for Statxt *)

{ open Parser }

let digits = ['0'-'9']
let floats = ((digits)+ '.' (digits)*) | ((digits)* '.' (digits)+)
let chars = [' '-'!' '#'-'[' ']'-'~']
let strings = '"' ((chars)* as str) '"'

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)

(* comments *)
| "/*"     { block_comment lexbuf }           (* Comments *)
| "//"     { line_comment lexbuf }

(* Delimiters *)
| '('        { LPAREN }
| ')'        { RPAREN }
| '{'        { LBRACE }
| '}'        { RBRACE }
| '['        { LSQUARE }
| ']'        { RSQUARE }
| ';'        { SEMI }
| ','        { COMMA }

(* Operators *)
| '+'        { PLUS }
| '-'        { MINUS }
| '*'        { TIMES }
| '/'        { DIVIDE }
| '='        { ASSIGN }

(* Logical Operators *)
| "=="       { EQ }
| "!="       { NEQ }
| '<'        { LT }
| "<="       { LEQ }
| ">"        { GT }
| ">="       { GEQ }
| "&&"       { AND }
| "||"       { OR }
| "!"        { NOT }

(* Control *)
| "if"       { IF }
| "else"     { ELSE }
| "for"      { FOR }
| "while"    { WHILE }
| "return"   { RETURN }

(* Types *)
| "int"      { INT }
| "float"    { FLOAT }
| "bool"     { BOOL }
| "void"     { VOID }
| "true"     { TRUE }
| "false"    { FALSE }
| "char"     { CHAR }
| "string"   { STRING }
| "struct"   { STRUCT }

| (digits)+ as lxm { INT_LIT(int_of_string lxm) }
| floats as lxm { FLOAT_LIT(float_of_string lxm) }
| strings { STR_LIT(str) }
| ''' chars ''' as lxm { CHAR_LIT(String.get lxm 1) }

(* Identifiers *)
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }

(* end of file and invalid *)
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

(* comments *)
and block_comment = parse
  "*/" { token lexbuf }
| _    { block_comment lexbuf }

and line_comment = parse
  '\n' { token lexbuf }
| _    { line_comment lexbuf }
