type id = string

type expr =
  | Var of id
  | Fun of id * expr
  | App of expr * expr

type typ =
  | TVar of int
  | TFun of typ * typ

type annotated_expr =
  | AVar of id * typ
  | AFun of id * annotated_expr * typ
  | AApp of annotated_expr * annotated_expr * typ

let rec expr_to_string expr =
  match expr with
  | Var id -> Printf.sprintf "%s" id
  | Fun(id, expr') -> Printf.sprintf "%s -> %s" id (expr_to_string expr')
  | App(func, arg) -> Printf.sprintf "%s %s" (expr_to_string func) (expr_to_string arg)
;;
let rec type_to_string typ =
  match typ with
  | TVar x -> Printf.sprintf "%d" x
  | TFun (TFun _ as f, x) -> Printf.sprintf "(%s) -> (%s)" (type_to_string f) (type_to_string x)
  | TFun (f, x) -> Printf.sprintf "(%s) .-> (%s)" (type_to_string f) (type_to_string x) 

