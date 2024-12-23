open Ast

type subst = (int * typ) list

let rec substitute id typ new_type =
  match typ with
  | TInt | TBool -> typ
  | TVar nr -> if nr = id then new_type else typ
  | TFun (input, output) -> 
    TFun (substitute id input new_type, substitute id output new_type)
;;
let apply_substitution subst typ =
  List.fold_right (fun (id, t) acc -> substitute id acc t) subst typ
;;
let rec occurs id typ =
  match typ with
  | TInt | TBool -> false
  | TVar y -> id = y
  | TFun(u, v) -> occurs id u || occurs id v

let rec unify_pair t1 t2 : subst =
  match t1, t2 with
  | (TInt as t), TVar a | (TBool as t), TVar a 
  | TVar a, (TInt as t) | TVar a, (TBool as t) -> [(a, t)]
  | TVar a, TVar b -> if a = b then [] else [(a, TVar b)]
  | TFun(in1, out1), TFun(in2, out2) -> unify [(in1, in2);(out1, out2)]
  | (t, TVar x) | (TVar x, t)-> 
    if occurs x t then failwith "circular definition"
    else [(x, t)]
  | TInt, TInt | TBool, TBool -> []
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