open Ast

type data_type =
  Number
  | Boolean
  | String
  | Char
  | Object of class_decl
and expr_detail =
    LitNum of float
  | LitBool of bool
  | LitString of string (* quoted string literal *)
  | LitChar of char (* 'c' *)
  | Noexpr (* for (;;) *)
  | Id of variable_decl
  | FCall of function_decl * expression list
  | Binop of expression * op * expression (* a + b *)
  | Unop of op * expression

and expression = expr_detail * data_type

and variable_decl = {
  vtype: data_type;
  vname : string;
  vexpr : expression;
}
(* Statements *)
and statement =
  Block of statement list
| Expression of expression
| VarDecl of variable_decl
| Return of expression
| If of expression * statement * statement
| For of expression * expression * expression * statement
| While of expression * statement


(* Functions *)
and function_decl = {
  fname : string; (* name of the function *)
  fformals : variable_decl list; (* formal params *)
  freturn : data_type; (* return type *)
  funcbody : statement list; (* statements, including local variable declarations *)
}

(* Actions *)
and action_decl = {
  aname : string; (* Name of the action *)
  aformals: variable_decl list; (* formal params *)
  areturn : data_type; (* return type *)
  abody : statement list; (* statements, including local variable declarations *)
}

(* Class Declarations *)
and class_decl = {
  cname : string; (* name of the class *)
  cformals: variable_decl list; (* formal params *)
  cinstvars : variable_decl list; (*instance variables *)
  cactions: action_decl list; (*lists of actions (methods) *)
}
and program = class_decl list * function_decl list
