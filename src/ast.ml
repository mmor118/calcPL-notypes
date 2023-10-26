type bop =
  | Add
  | Mult
  | LEQ
  
(** The type of the abstract syntax tree (AST). *)
type expr = 
| Int of int
| Bool of bool (* New data type for boolean values *)
| Binop of bop * expr * expr
| Var of string  (* Variable reference *)
| Let of string * expr * expr  (* Variable binding with 'let' *)
| If of expr * expr * expr  (* If-then-else *)
| Error of string  (* Error handling *)
