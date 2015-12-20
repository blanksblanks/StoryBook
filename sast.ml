open Ast

type data_type =
    Void
  | Number
  | Boolean
  | String
  | Char
  | List of data_type
  | Object of class_decl

and expr_detail =
    LitNum of float
  | LitBool of bool
  | LitString of string (* quoted string literal *)
  | LitChar of char (* 'c' *)
  | Noexpr (* for (;;) *)
  | Id of variable_decl
  | Assign of string * expression (* x is 5 *)
  | TraitAssign of variable_decl * string * expression (* SleepingBeauty's x is 5 *)
  | ListAssign of string * expression * expression (* myList[2 + 3] = 5+ 7 *)
  | Instantiate of class_decl * expression list (* object type and constructor parameters *)
  (* | ListInstantiate of variable_decl * expression (* words list colors is new words list [size] *) *)
  | Access of variable_decl * variable_decl (* Member value access: SleepingBeauty's x *)
  | FCall of function_decl * expression list
  | ACall of variable_decl * action_decl * expression list
  | StrCat of expression * expression
  | MathBinop of expression * op * expression (* a + b *)
  | Unop of op * expression
  | ListAccess of variable_decl * expression
  (* | LitList of expression list *)

and expression = expr_detail * data_type

and variable_decl =
{
  vtype: data_type;
  mutable vname : string;
  mutable vexpr : expression;
  istrait: bool; 
}


(* Statements *)
and statement =
  Block of statement list
| Expression of expression
| VarDecl of variable_decl
| Return of expression
| If of expression * statement * statement
| For of statement * expression * expression * statement
| While of expression * statement


(* Functions *)
and function_decl = {
  fname : string; (* name of the function *)
  fformals : variable_decl list; (* formal params *)
  freturn : data_type; (* return type *)
  funcbody : statement list; (* statements, including local variable declarations *)
  isLib : bool;
}

(* Actions *)
and action_decl = {
  aname : string; (* Name of the action *)
  aclass : string;
  aformals: variable_decl list; (* formal params *)
  areturn : data_type; (* return type *)
  abody : statement list; (* statements, including local variable declarations *)
}

(* Class Declarations *)
and class_decl = {
  cname : string; (* name of the class *)
  cparent: string;
  cformals: variable_decl list; (* formal params *)
  cinstvars : variable_decl list; (*instance variables *)
  cactions: action_decl list; (*lists of actions (methods) *)
}
and program = class_decl list * function_decl list
