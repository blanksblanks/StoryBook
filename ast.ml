type var_type =
  | Number
  | Boolean
  | String
  | Char
  | Character (* object *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq|
OR | AND | NOT

type expr = (* Expressions *)
  Lit_int of int (* 42 *)
| Lit_bool of bool
| Lit_string of string (* "foo_quoted" *)
| Lit_char of char (* 'c' *)
| Noexpr (* for (;;) *)
| Id of string (* foo_unquoted *)
| Assign of string * expr (* foo = 42 *)
| Access of string * string (* foo's name *)
| Trait_Assign of string * string * expr (* object name, instance var name, expr *)
| Binop of expr * op * expr (* a + b *)
| FCall of string * expr list (* foo(1, 25 *)
| ACall of string * string * expr list (* object id, action name, params *)
type stmt = (* Statements *)
Block of stmt list (* { ... } *)
| Expr of expr (* foo = bar + 3; *)
| Return of expr (* return 42; *)
| If of expr * stmt * stmt (* if (foo == 42) {} else {} *)
| For of expr * expr * expr * stmt (* for (i=0;i<10;i=i+1) { ... } *)
| While of expr * stmt (* while (i<10) { i = i + 1 } *)

type var_decl =
 Variable of var_type * string (* (type, name) tuple *)

type func_decl = {
fname : string; (* Name of the function *)
formals : var_decl list; (* Formal argument (type,name) tuples *)
locals : var_decl list; (* Local variables as (type, name) tuples *)
body : stmt list; 
}

type action = {
  action_name : string;
  formals: var_decl list;
  locals: var_decl list;
  body : stmt list;
}

type class_decl = {
 cname : string; (*name of the class *)
 ivars: var_decl list; (*instance vars as (type, name) tuples *)
 actions: action list; (*lists of actions (methods) *)
}

type program = class_decl list * func_decl list (* classes, funcs. no global vars *)
