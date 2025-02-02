type id = string

type program =
  | Expr of expr 
  | Define of define
  | TypeDefine of id * id list * type_define list

and define = id * expr

and type_define = id * type_shape list

and type_shape = 
  | TDUnit | TDInt | TDBool
  | TDVar of id
  | TDProduct of type_shape * type_shape
  | TDADT of id * type_shape list

and expr =
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
  | Constructor of id * expr list

and pattern =
  | PUnit
  | PBool of bool
  | PInt of int
  | PWildcard
  | PVar of id
  | PPair of pattern * pattern
  | PConstructor of id * pattern list

and typ =
  | TUnit | TInt | TBool
  | TVar of int
  | TFun of typ * typ
  | TPair of typ * typ
  | TPolymorphic of typ
  | TADT of id * typ list

type annotated_expr =
  | AUnit
  | AInt of int * typ
  | ABool of bool * typ
  | AVar of id * typ
  | AFun of id * annotated_expr * typ
  | AApp of annotated_expr * annotated_expr * typ
  | ALet of id * annotated_expr * annotated_expr * typ * typ
  | AIf of annotated_expr * annotated_expr * annotated_expr * typ
  | APair of annotated_expr * annotated_expr
  | ALeft of typ
  | ARight of typ
  | AMatch of annotated_expr * (pattern * typ * annotated_expr) list * typ
  | AConstructor of id * (annotated_expr list) * type_shape list * (id * typ)list * typ


let rec type_to_string typ =
  match typ with
  | TUnit -> Printf.sprintf "unit"
  | TInt -> Printf.sprintf "int"
  | TBool -> Printf.sprintf "bool" 
  | TVar x -> Printf.sprintf "T%d" x
  | TFun (f, x) -> Printf.sprintf "%s -> %s" 
  (parenthesis_fun f) (parenthesis_pair x) 
  | TPair(a, b) -> Printf.sprintf "%s * %s" (parenthesis_fun a) (type_to_string b)
  | TPolymorphic t -> Printf.sprintf ".%s" (type_to_string t)
  | TADT (t, args) -> Printf.sprintf "%s %s" t 
  (print_type_args args)

and parenthesis_fun = function
  | TFun _ | TPair _ as typ -> "(" ^ (type_to_string typ) ^ ")"
  | x -> type_to_string x 

and parenthesis_pair = function
  | TPair(a,b) -> "(" ^ (type_to_string (TPair(a,b))) ^ ")" 
  | x -> type_to_string x 

and print_type_args = function
  | [] -> ""
  | x -> 
    let rec print_inner_type_args = function
        | [x] -> type_to_string x
        | x :: xs -> type_to_string x ^ ", " ^ (print_inner_type_args xs)
        | _ -> failwith "absurd"
    in "<" ^ (print_inner_type_args x) ^ ">"


