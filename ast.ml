type id = string

type expr =
  | Void | Int of int | Bool of bool
  | Var of id
  | Fun of id * expr
  | App of expr * expr
  | Let of id * expr * expr
  | If of expr * expr * expr
  | Pair of expr * expr
  | Left
  | Right

type typ =
  | TVoid | TInt | TBool
  | TVar of int
  | TFun of typ * typ
  | TPair of typ * typ

type annotated_expr = (* TODO: Make protectedso that AVar can only have type var etc.*)
  | AVoid
  | AInt of int * typ
  | ABool of bool * typ
  | AVar of id * typ
  | AFun of id * annotated_expr * typ
  | AApp of annotated_expr * annotated_expr * typ
  | ALet of id * annotated_expr * annotated_expr * typ
  | AIf of annotated_expr * annotated_expr * annotated_expr * typ
  | APair of annotated_expr * annotated_expr
  | ALeft of typ
  | ARight of typ

let rec expr_to_string expr =
  match expr with
  | Void -> Printf.sprintf "void(:3)"
  | Int nr -> Printf.sprintf "%d" nr
  | Bool b -> let text = if b then "true" else "false" in Printf.sprintf "%s" text
  | Var id -> Printf.sprintf "%s" id
  | Fun(id, expr') -> Printf.sprintf "%s -> %s" id (expr_to_string expr')
  | App(func, arg) -> Printf.sprintf "%s %s" (expr_to_string func) (expr_to_string arg)
  | Let(id, expr1, expr2) -> 
    Printf.sprintf "let %s := %s in %s" id (expr_to_string expr1) (expr_to_string expr2)
  | If(expr1, expr2, expr3) -> 
    Printf.sprintf "if %s then %s else %s" (expr_to_string expr1) (expr_to_string expr2) (expr_to_string expr3)
  | Pair(e1, e2) -> Printf.sprintf "(%s, %s)" (expr_to_string e1) (expr_to_string e2)
  | Left -> "left"
  | Right -> "right"
  ;;
let rec type_to_string typ =
  match typ with
  | TVoid -> Printf.sprintf "void"
  | TInt -> Printf.sprintf "int"
  | TBool -> Printf.sprintf "bool" 
  | TVar x -> Printf.sprintf "a%d" x
  | TFun (TFun _ as f, x) -> Printf.sprintf "(%s) -> %s" (type_to_string f) (type_to_string x)
  | TFun (f, x) -> Printf.sprintf "%s -> %s" (type_to_string f) (type_to_string x) 
  | TPair(a, b) -> Printf.sprintf "%s * %s" (type_to_string a) (type_to_string b)

