open Ast
open Infer_type

type subst = (int * typ) list


let rec substitute id typ new_type =
  match typ with
  | TUnit | TInt | TBool -> typ
  | TVar nr -> if nr = id then new_type else typ
  | TFun (input, output) -> 
    TFun (substitute id input new_type, substitute id output new_type)
  | TPair(a, b) -> TPair(substitute id a new_type, substitute id b new_type)
  | TPolymorphic t -> TPolymorphic (substitute id (copy_type t) new_type)
;;
let apply_substitution subst typ =
  List.fold_right (fun (id, t) acc -> substitute id acc t) subst typ
;;
let rec occurs id typ =
  match typ with
  | TUnit | TInt | TBool -> false
  | TVar y -> id = y
  | TFun (u, v) -> occurs id u || occurs id v
  | TPair(a, b) -> occurs id a || occurs id b
  | TPolymorphic t -> occurs id t


let rec unify_pair t1 t2 : subst =
  match t1, t2 with
  | (TInt as t), TVar a | (TBool as t), TVar a 
  | TVar a, (TInt as t) | TVar a, (TBool as t) 
  | TVar a, (TUnit as t) | (TUnit as t), TVar a  -> [(a, t)]
  | TVar a, TVar b -> if a = b then [] else [(a, TVar b)]
  | TFun(in1, out1), TFun(in2, out2) -> unify [(in1, in2);(out1, out2)]
  | TPolymorphic t, TVar x | TVar x, TPolymorphic t -> [(x, Infer_type.copy_type t)]
  | (t, TVar x) | (TVar x, t)-> 
    if occurs x t then failwith "circular definition"
    else [(x, t)]
  | TInt, TInt | TBool, TBool | TUnit , TUnit -> []
  | TPair(a1, b1), TPair(a2, b2) -> unify [(a1, a2);(b1, b2)]
  | TPolymorphic t1, t2 -> unify [(Infer_type.copy_type t1, t2)]
  | _, _ -> failwith "Type error: type mismatch"

and unify subst =   
  match subst with
  | [] -> []
  | (t1, t2)::xs ->
    let types = unify xs in
    let t2' = apply_substitution types t2 in
    let t1' = apply_substitution types t1 in
    let types' = unify_pair t1' t2' in
    types' @ types
;;


let infer expr def_env =
    let rec def_to_let env = 
      match env with
      | [] -> expr
      | (id, e)::xs -> Let(id, e, def_to_let xs)
    in
  Infer_type.type_name := 0;
  let annotated_expr = Infer_type.annotate (def_to_let def_env)
  in let constrains = Infer_type.collect_constrains [annotated_expr] []
  in let temp = unify constrains
  in apply_substitution temp (Infer_type.type_of annotated_expr)
