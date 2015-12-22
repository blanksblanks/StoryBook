open Ast

type data_type =
    Void
  | Number
  | Boolean
  | String
  | Char
  | Object of class_decl
  | NumberList
  | BooleanList
  | CharList
  | CharacterList (* list of pointers to point to Characters *)

and expr_detail =
    LitNum of float
  | LitBool of bool
  | LitString of string (* quoted string literal *)
  | LitChar of char (* 'c' *)
  | Noexpr 
  | Id of variable_decl
  | Assign of string * expression (* x is 5 *)
  | TraitAssign of expression * expression (* SleepingBeauty's x is 5 *)
  | Instantiate of class_decl * expression list (* object type and constructor parameters *)
  | ListInstantiate of data_type * expression   (* list type and size *)
  | ListAccess of variable_decl * expression    (* ages[5] *)
  | ListAssign of expression * expression       (* ages[5] is 10 *)
  | Access of variable_decl * variable_decl     (* Member value access: SleepingBeauty's. Or, my to access traits within a character*)
  | FCall of function_decl * expression list
  | ACall of variable_decl * action_decl * expression list
  | StrCat of expression * expression
  | MathBinop of expression * op * expression (* a + b *)
  | Unop of op * expression

(* Tuple of expression and the type it evaluates to *)
and expression = expr_detail * data_type

(* Variable declaration *)
(* All variable declarations have a type and a name 
   If variable is initialized upon instantiation, the variable declaration
   also has an expression attached to it *)
and variable_decl =
{
  vtype: data_type;
  mutable vname : string;
  mutable vexpr : expression; (* e.g.: 5+3 in : "number x is (5 + 3)." *)
  istrait: bool;   (* boolean denoting whether variable is character trait *)
  listsize: float; (* to prevent list variables to access beyond the lenght of the list*)
}


(* Statements *)
and statement =
  Block of statement list
| Expression of expression
| VarDecl of variable_decl
| Return of expression
  (* If statements: boolean expr, if statement, else statement *)
| If of expression * statement * statement
  (* For loops: variable decl, boolean stopping condition, increment expr, loop body *)
| For of statement * expression * expression * statement
  (* While loops: boolean expr, loop body *)
| While of expression * statement


(* Functions *)
and function_decl = {
  fname : string; (* name of the function *)
  fformals : variable_decl list; (* formal params *)
  freturn : data_type; (* return type *)
  funcbody : statement list; (* statements, including local variable declarations *)
  isLib : bool; (* boolean denoting whether function is library function *)
}

(* Actions *)
and action_decl = {
  aname : string; (* Name of the action *)
  aclass : string;
  aformals: variable_decl list; (* formal params *)
  areturn : data_type;    (* return type *)
  abody : statement list; (* statements, including local variable declarations *)
}

(* Class Declarations *)
and class_decl = {
  cname : string; (* name of the class *)
  cparent: string;
  cformals: variable_decl list;   (* formal params *)
  cinstvars : variable_decl list; (*instance variables *)
  cactions: action_decl list;     (*lists of actions (methods) *)
}

(* Program --class declarations and function declarations *)
and program = class_decl list * function_decl list
