type var_type =
  | Number
  | Boolean
  | String
  | Char
  | Object of string (* string is typename of object *)
 (* | Character*) (* object *)

type param_decl = 
  Parameter of var_type * string


type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq| Mod|
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
| Unop of op * expr
| FCall of string * expr list (* foo(1, 25 *)
| ACall of string * string * expr list (* object id, action name, params *)

type var_decl = 
  U_Var of var_type * string (* uninitialized *)
  |I_Var of var_type * string * expr (*initialized *)

type stmt = (* Statements *)
Block of stmt list (* { ... } *)
| Expr of expr (* foo = bar + 3; *)
| Return of expr (* return 42; *)
| If of expr * stmt * stmt (* if (foo == 42) {} else {} *)
| For of expr * expr * expr * stmt (* for (i=0;i<10;i=i+1) { ... } *)
| While of expr * stmt (* while (i<10) { i = i + 1 } *)
| Var_Decl of var_decl


type func_decl = {
fname : string; (* Name of the function *)
formals : param_decl list; (* Formal argument (type,name) tuples *)
(*locals : var_decl list; Local variables as (type, name) tuples *)
body : stmt list; 
}

type action = {
  action_name : string;
  formals: param_decl list;
 (* locals: var_decl list;*)
  body : stmt list;
}

type class_decl = {
 cname : string; (*name of the class *)
 traits: param_decl list; (*instance vars as (type, name) tuples *)
 actions: action list; (*lists of actions (methods) *)
}

type program = class_decl list * func_decl list (* classes, funcs. no global vars *)
