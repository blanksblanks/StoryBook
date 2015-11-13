open Ast

type var_type =
  | Number
  | Boolean
  | String
  | Char
  | Object of class_decl (* what the heck is this *)

and var_decl =
  | U_Var of var_type * string (* uninitialized *)
  | I_Var of var_type * string * expression

(* Example: "Number x is 5" would have var_decl_checked of:
  ((Number, x), Number). First number is x's type, second Number is
  5's type *)
and var_decl_checked =
  var_decl * var_type (* second var_type is type of expr for I_Var above *)

and expr_detail = (* Expressions *)
  Lit_int of int (* 42 *)
| Lit_bool of bool
| Lit_string of string (* "foo_quoted" *)
| Lit_char of char (* 'c' *)
| Noexpr (* for (;;) *)
| Id of var_decl_checked (* foo_unquoted *)
| Assign of var_decl_checked * expression (* foo = 42 *)
| Access of string * string (* foo's name *)
| Trait_Assign of string * string * expression (* object name, instance var name, expression *)
| Binop of expression * op * expression (* a + b *)
| Unop of op * expression
| FCall of string * expression list (* foo(1, 25 *)
| ACall of string * string * expression list (* object id, action name, params *)

and expression = expr_detail * var_type (*type expression evalautes to *)

and stmt = (* Statements *)
Block of stmt list (* { ... } *)
| Expr of expression (* foo = bar + 3; *)
| Return of expression (* return 42; *)
| If of expression * stmt * stmt (* if (foo == 42) {} else {} *)
| For of expression * expression * expression * stmt (* for (i=0;i<10;i=i+1) { ... } *)
| While of expression * stmt (* while (i<10) { i = i + 1 } *)
| Var_Decl of var_decl

and func_decl = {
  fname : string; (* Name of the function *)
  fformals : var_decl_checked list; (* Formal argument (type,name) tuples *)
  freturn: var_type;
  fbody : stmt list; 
}

and action_decl = {
  aname : string;
  aformals: var_decl_checked list;
  areturn : var_type;
  abody : stmt list;
}

and class_decl = {
   cname : string; (*name of the class *)
   cformals:  var_decl_checked list;
   cinstvars: var_decl_checked list; (*instance vars as (type, name) tuples *)
   cactions: action_decl list; (*lists of actions (methods) *)
}



