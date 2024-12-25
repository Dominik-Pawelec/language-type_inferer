open Ast

type subst = (int * typ) list

let rec substitute id typ new_type =
  match typ with
  | TUnit | TInt | TBool -> typ
  | TVar nr -> if nr = id then new_type else typ
  | TFun (input, output) -> 
    TFun (substitute id input new_type, substitute id output new_type)
  | TPair(a, b) -> TPair(substitute id a new_type, substitute id b new_type)
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

let rec unify_pair t1 t2 : subst =
  match t1, t2 with
  | (TInt as t), TVar a | (TBool as t), TVar a 
  | TVar a, (TInt as t) | TVar a, (TBool as t) 
  | TVar a, (TUnit as t) | (TUnit as t), TVar a  -> [(a, t)]
  | TVar a, TVar b -> if a = b then [] else [(a, TVar b)]
  | TFun(in1, out1), TFun(in2, out2) -> unify [(in1, in2);(out1, out2)]
  | (t, TVar x) | (TVar x, t)-> 
    if occurs x t then failwith "circular definition"
    else [(x, t)]
  | TInt, TInt | TBool, TBool | TUnit , TUnit -> []
  | TPair(a1, b1), TPair(a2, b2) -> unify [(a1, a2);(b1, b2)]
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
