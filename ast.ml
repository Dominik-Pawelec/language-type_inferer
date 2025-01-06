type id = string

type expr =
  | Unit | Int of int | Bool of bool
  | Var of id
  | Fun of id * expr
  | App of expr * expr
  | Let of id * expr * expr
  | If of expr * expr * expr
  | Pair of expr * expr
  | Left
  | Right
  | Match of expr * (pattern * expr) list
  | DefineType of id * (id list) * (id * pattern_definition list)
  | CustomType of id * expr list
  (*typedef listab<a,b> = NIL of Unit | Cons1 of a * listab<a, b> | Cons2 of b * listab<a, b>*)

and pattern_definition =
  | PDefUnit
  | PDefBool
  | PDefInt
  | PDefVar of id
  | PDefProduct of pattern_definition * pattern_definition
  | PDefCustom of id * pattern_definition list

and pattern =
  | PUnit
  | PBool of bool
  | PInt of int
  | PWildcard
  | PVar of id
  | PPair of pattern * pattern
  | PCustom of id * pattern list

type typ =
  | TUnit | TInt | TBool
  | TVar of int
  | TFun of typ * typ
  | TPair of typ * typ
  | TCustom of id * typ list

type annotated_expr = (* TODO: Make protectedso that AVar can only have type var etc.*)
  | AUnit
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
  | AMatch of annotated_expr * (pattern * typ * annotated_expr) list * typ
  | DefineType of id * (id list) * (id * pattern_definition list)
  | CustomType of id * annotated_expr list * typ

(*let rec expr_to_string expr =
  match expr with
  | Unit -> Printf.sprintf "Unit"
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
  | Right -> "right"*)
  ;;
let rec type_to_string typ =
  match typ with
  | TUnit -> Printf.sprintf "unit"
  | TInt -> Printf.sprintf "int"
  | TBool -> Printf.sprintf "bool" 
  | TVar x -> Printf.sprintf "a%d" x
  | TFun (TFun _ as f, x) -> Printf.sprintf "(%s) -> %s" (type_to_string f) (parenthesis x)
  | TFun (f, x) -> Printf.sprintf "%s -> %s" (parenthesis f) (parenthesis x) 
  | TPair(a, b) -> Printf.sprintf "%s * %s" (parenthesis2 a) (type_to_string b)
  | TCustom(id, ls) -> 
    Printf.sprintf "%s <%s>" 
    id (List.fold_left (fun acc txt -> (type_to_string txt) ^", "^ acc) "" ls)

and parenthesis = function
  | TPair(a,b) -> "(" ^ (type_to_string (TPair(a,b))) ^ ")"
  | TFun(a,b) -> "(" ^ (type_to_string (TFun(a,b))) ^ ")"
  | x -> type_to_string x 

and parenthesis2 = function
  | TFun(a,b) -> "(" ^ (type_to_string (TFun(a,b))) ^ ")"
  | x -> type_to_string x 




