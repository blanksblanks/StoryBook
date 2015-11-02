{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "~"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }                  (* Punctuation *)
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACK }
| ']'      { RBRACK }
| ';'      { SEMI }
| ','      { COMMA }
| '.'      { PERIOD }
(*| "'s"     { APOST } *)

(* Binary Operators *)
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '%'      { MOD }
| "<"      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "="      { EQ }
| "!="     { NEQ }
| "is"     { ASSIGN }

(* Logical Operators *)
| "not"    { NOT }
| "and"    { AND }
| "or"     { OR }

(* Control flow *)
| "if"     { IF }
| "else"   { ELSE }
(*| "elseif" { ELIF }*)
| "repeatfor" { FOR }
| "repeatwhile" { WHILE }
(*| "endwith" { ENDWITH }*)
| "returns" { RETURN }

(* Primitives--booleans, chars, strings, numbers *)
| "tof"     { BOOL }
| "true"    { TRUE }
| "false"   { FALSE}
| "number"  { NUMBER }
| "words"   { STRING }
| "letter"  { CHAR } (*
| "list"    { LIST }
| "null"    { NULL }
| "subtype" { SUBTYPE }
| "Plot"    { MAIN }*)
| "Chapter" { FUNCTION }
(*| "Character" { CLASS } *)
(*| "Action" { METHOD } *)
(*| "trait" { IVAR }*)
(*| "new" { NEW }*)
(*| "say" { SAY }*)
| eof { EOF }
| ['-']?['0'-'9']+ as lxm { LIT_INT(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "~" { token lexbuf }
| _    { comment lexbuf }
