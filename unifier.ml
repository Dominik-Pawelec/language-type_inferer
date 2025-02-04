open Ast
open Utils

type subst = (int * typ) list

let rec substitute id typ new_type =
  match typ with
  | TUnit | TInt | TBool -> typ
  | TVar nr              -> if nr = id then new_type else typ
  | TFun (input, output) -> TFun (substitute id input new_type, substitute id output new_type)
  | TPair(a, b)     -> TPair(substitute id a new_type, substitute id b new_type)
  | TPolymorphic t  -> instantiate (substitute id t new_type)
  | TADT(id', args) -> TADT(id', List.map (fun x -> substitute id x new_type) args)
;;
let apply_substitution subst typ =
    let result = List.fold_right (fun (id, t) acc -> substitute id acc t) subst typ in
  result
;;

let rec occurs id typ =
  match typ with
  | TUnit | TInt | TBool -> false
  | TVar y              -> id = y
  | TFun (u, v)        -> occurs id u || occurs id v
  | TPair(a, b)       -> occurs id a || occurs id b
  | TPolymorphic t   -> occurs id t
  | TADT (_, args)  -> List.fold_left (fun acc x -> (occurs id x) || acc) false args

let rec unify_pair t1 t2 : subst =
  match t1, t2 with
  | TVar a, TVar b -> if a = b then [] else [(a, TVar b)]
  | TFun(in1, out1), TFun(in2, out2) -> unify [(in1, in2);(out1, out2)]
  | TPolymorphic t, TVar x | TVar x, TPolymorphic t -> [(x, t)]
  | t, TVar x | TVar x, t -> 
    if not (occurs x t) then [(x, t)]
    else failwith
  (Printf.sprintf "Circular definition: %s appears in %s\n" (type_to_string (TVar x)) (type_to_string t))
  | TInt, TInt | TBool, TBool | TUnit , TUnit -> []
  | TPair(a1, b1), TPair(a2, b2)     -> unify [(a1, a2);(b1, b2)]
  | TPolymorphic t1, TPolymorphic t2 -> unify [(t1,t2); (TPolymorphic t1, t2); (t1, TPolymorphic t2)]
  | TPolymorphic t1, t2 | t2, TPolymorphic t1 -> unify [(instantiate t1, t2)]
  | TADT(id1, args1), TADT(id2, args2) 
    when id1 = id2 && List.length args1 = List.length args2 -> unify (List.combine args1 args2)
  | _, _ -> failwith ("Type error: type mismatch: " ^ (type_to_string t1) ^ "; " ^(type_to_string t2) )

and unify subst =   
  match subst with
  | [] -> []
  | (t1, t2)::xs ->
    let types  = unify xs in
    let t2'    = apply_substitution types t2 in
    let t1'    = apply_substitution types t1 in
    let types' = unify_pair t1' t2' in
    types' @ types
;;

