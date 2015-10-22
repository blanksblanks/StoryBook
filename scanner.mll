{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "~"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACK }
| ']'      { RBRACK }
| ';'      { SEMI }
| ','      { COMMA }
| '.'      { PERIOD }
| "'s"     { APOST }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '%'      { MOD }
| "<"      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "is"     { ASSIGN }
| "="      { EQ }
| "not"    { NOT }
| "and"    { AND }
| "or"     { OR }
| "if"     { IF }
| "else"   { ELSE }
| "elseif" { ELIF }
| "repeatfor" { FOR }
| "repeatwhile" { WHILE }
| "endwith" { ENDWITH }
| "returns" { RETURN }
| "number"  { NUMBER }
| "tof"     { BOOL }
| "true"    { TRUE }
| "false"   { FALSE}
| "words"   { STRING }
| "letter"  { CHAR }
| "list"    { LIST }
| "null"    { NULL }
| "subtype" { SUBTYPE }
| "Plot"    { MAIN }
| "Chapter" { FUNCTION }
| "Character" { CLASS }
| "Action" { METHOD }
| "trait" { IVAR }
| "new" { NEW }
| "say" { SAY }
| ['-'']?['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "~" { token lexbuf }
| _    { comment lexbuf }
