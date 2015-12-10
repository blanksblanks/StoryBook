{ open Parser }

let whitespace = [' ' '\t' '\r' '\n']
let comment = "~~" [^ '\n']* "\n"
let digit = ['0'-'9']

rule token = parse
  (* Whitespace and Comments *)
  whitespace { token lexbuf }
  | comment  { token lexbuf }
  | '~'      { comment lexbuf }

  (* Punctuation *)
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
  | "endwith" { ENDWITH }
  | "returns" { RETURNS }

  (* Primitives--booleans, chars, strings, numbers *)
  | "tof"     { BOOL }
  | "true" as bool_val    { LIT_BOOL(bool_of_string bool_val)}
  | "false" as bool_val   { LIT_BOOL(bool_of_string bool_val)}
  | "number"  { NUMBER }
  | "words"   { STRING }
  | "letter"  { CHAR } (*
  | "list"    { LIST }
  | "null"    { NULL } *)
  | "nothing" { VOID } (*)
  | "subtype" { SUBTYPE }
  | "Plot"    { MAIN }*)
  | "Chapter" { FUNCTION }
  | "Character" { CHARACTER }
  | "Action" { METHOD }
  | "trait" { TRAIT }
  | "new" { NEW }
  | eof { EOF }
  | ['-']?(digit+'.'digit*)|['-']?(digit*'.'digit+)|['-']?(digit+) as lxm { LIT_NUM(float_of_string lxm) }
  (* String regex modified from:
   realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html *)
  | '"'( ('\\'('/'|'\\'| 'b' | 'f' | 'n' | 'r' | 't'))|([^'"']) )*'"' as lxm { LIT_STRING(lxm) }
  | ['\''] (_ as l) ['\''] {LIT_CHAR(l) }
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "~"  { token lexbuf }
| _    { comment lexbuf }
