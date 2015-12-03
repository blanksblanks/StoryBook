(* Possible data types *)
type data_type =
  | Number
  | Boolean
  | String
  | Char
  | Object of string (* string is typename of object, not id *)

(* Operators *)
type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq| Mod|
OR | AND | NOT

(* Expressions *)
type expr =
  LitNum of float
| LitBool of bool
| LitString of string (* quoted string literal *)
| LitChar of char (* 'c' *)
| Noexpr (* for (;;) *)
| Id of string (* foo_unquoted *)
| Assign of string * expr (* x is 5 *)
| TraitAssign of string * string * expr (* SleepingBeauty's x is 5 *)
| Access of string * string (* Member value access: SleepingBeauty's x *)
| Binop of expr * op * expr (* a + b *)
| Unop of op * expr
| FCall of string * expr list (* chapter1() *)
| ACall of string * string * expr list (* SleepingBeauty, setX(5) *)

(* Variable Declarations *)
type var_decl = {
  vtype: data_type;
  vname : string;
  vexpr : expr;
}

(* Statements *)
type stmt =
  Block of stmt list
| Expr of expr
| VarDecl of var_decl
| Return of expr
| If of expr * stmt * stmt
| For of expr * expr * expr * stmt
| While of expr * stmt

(* Functions *)
type func_decl = {
  fname : string; (* name of the function *)
  fformals : var_decl list; (* formal params *)
  freturn : data_type; (* return type *)
  fbody : stmt list; (* statements, including local variable declarations *)
}

(* Actions *)
type act_decl = {
  aname : string; (* Name of the action *)
  aformals: var_decl list; (* formal params *)
  areturn : data_type; (* return type *)
  abody : stmt list; (* statements, including local variable declarations *)
}

(* Class Declarations *)
type cl_decl = {
  cname : string; (* name of the class *)
  cformals: var_decl list; (* formal params *)
  cinstvars : var_decl list; (*instance variables *)
  cactions: act_decl list; (*lists of actions (methods) *)
}

type program = cl_decl list * func_decl list (* classes, funcs. no global vars *)
