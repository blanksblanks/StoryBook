type var_type =
  | Number
  | Boolean
  | String
  | Char
  | Object of class_decl (* what the heck is this *)

type check_variable_decl =
  U_Var of var_type * string (* uninitialized *)
  |I_Var of var_type * string * check_expr

type expr_detail = (* Expressions *)
  Lit_int of int (* 42 *)
| Lit_bool of bool
| Lit_string of string (* "foo_quoted" *)
| Lit_char of char (* 'c' *)
| Noexpr (* for (;;) *)
| Id of check_variable_decl (* foo_unquoted *)
| Assign of check_variable_decl * expression (* foo = 42 *)
| Access of string * string (* foo's name *)
| Trait_Assign of string * string * expression (* object name, instance var name, expression *)
| Binop of expression * op * expression (* a + b *)
| Unop of op * expression
| FCall of string * expression list (* foo(1, 25 *)
| ACall of string * string * expression list (* object id, action name, params *)

type expression = expr_detail * Type.var_type (*type expression evalautes to *)

type stmt = (* Statements *)
Block of stmt list (* { ... } *)
| Expr of expression (* foo = bar + 3; *)
| Return of expression (* return 42; *)
| If of expression * stmt * stmt (* if (foo == 42) {} else {} *)
| For of expression * expression * expression * stmt (* for (i=0;i<10;i=i+1) { ... } *)
| While of expression * stmt (* while (i<10) { i = i + 1 } *)
| Var_Decl of var_decl

type func_decl = {
  fname : string; (* Name of the function *)
  check_formals : check_variable_decl list; (* Formal argument (type,name) tuples *)
  return_type: var_type;
  body : stmt list; 
}

type action = {
  action_name : string;
  check_formals: check_variable_decl list;
  return_type : var_type;
  body : stmt list;
}

 type class_decl = {
   cname : string; (*name of the class *)
   check_formals:  check_variable_decl list;
   check_instance_vars: check_variable_decl list; (*instance vars as (type, name) tuples *)
   actions: check_action list; (*lists of actions (methods) *)
}

