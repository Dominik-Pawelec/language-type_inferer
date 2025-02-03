open Ast

let type_name = ref 0
let new_type ()=
  let n = !type_name in
  type_name := n + 1; 
  TVar n

module M = Map.Make(String)
type env = typ M.t

let rec type_of = function
  | AUnit -> TUnit
  | AInt _ -> TInt
  | ABool _ -> TBool
  | AVar(_, t) -> t
  | AFun(_, _, t) -> t
  | AApp(_, _, t) -> t
  | ALet(_,_,_,_,t) -> t
  | AIf(_,_,_,t) -> t
  | APair(a,b) -> TPair(type_of a, type_of b)
  | ALeft t -> t
  | ARight t -> t
  | AMatch(_,_,t) -> t
  | AConstructor(_,_,_,_,t) -> t

let rec shape_to_type shape var_env = 
    match shape with
    | TDUnit -> TUnit
    | TDInt -> TInt
    | TDBool -> TBool
    | TDVar id -> 
        begin match List.find_opt (fun (a,_) -> a = id) var_env with
        | None -> failwith "this variable doesn't exist in the scope of type" 
        | Some (_,t) -> t
        end
    | TDProduct (a, b) -> TPair(shape_to_type a var_env, shape_to_type b var_env)
    | TDADT (id, shape_ls) -> TADT (id, List.map (fun x -> shape_to_type x var_env) shape_ls)

let instantiate t =
  let subst_table = Hashtbl.create 16 in
  let rec instantiate_aux t =
    match t with
    | TUnit | TInt | TBool -> t
    | TVar x -> (
        match Hashtbl.find_opt subst_table x with
        | Some new_t -> new_t
        | None ->
            let new_var = new_type () in
            Hashtbl.add subst_table x new_var;
            new_var)
    | TFun (input, output) -> TFun(instantiate_aux input, instantiate_aux output)
    | TPair (left, right) -> TPair(instantiate_aux left, instantiate_aux right)
    | TPolymorphic t' -> instantiate_aux t'
    | TADT(id, args_t) -> TADT(id, List.map (instantiate_aux) args_t )
  in
  instantiate_aux t 

