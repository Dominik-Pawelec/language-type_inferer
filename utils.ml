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

let rec type_of_pattern env type_env = function
  | PUnit -> (TUnit, env)
  | PBool _ -> (TBool, env)
  | PInt _ -> (TInt, env)
  | PWildcard -> (new_type (), env)
  | PVar id -> 
    let t = new_type () in
    (t, M.add id t env)
  | PPair (p1, p2) -> 
    let (t1, env') = type_of_pattern env type_env p1 in
    let (t2, env'') = type_of_pattern env' type_env p2 in
    (TPair(t1, t2), env'')
  | PConstructor(id,args) ->
    match M.find_opt id type_env with
    | None -> failwith "undefined type constructor."
    | Some (shapes, variables, type_name) ->
            print_string ((string_of_int (List.length args)) ^ "=?" ^ (string_of_int (List.length shapes)));
        let args_shape =
          List.combine args shapes (*FIX: for come reason sometimes args is 1 smaller than I anticipate*)
        in
        let specify_variables = retrieve_vars args_shape variables [] env type_env in
        let variable_env = List.map(fun x ->
        match List.find_opt (fun (y,_) -> y = x) specify_variables with
        | None -> (x, new_type ())
        | Some e -> e
        ) variables in
        let types_ls = List.fold_left (fun acc (x, t) -> M.add x t acc) env variable_env in
     M.iter (fun k v -> print_string (k ^ ":" ^(v |> type_to_string))) types_ls;
     (TADT(type_name, List.map (fun (_, typ) -> typ)variable_env), types_ls )
 
and retrieve_vars args_shape variables acc env type_env =
  match variables with
  | [] -> acc
  | x :: xs ->
    let matches = List.filter (fun (_, td) -> td = TDVar x) args_shape in
    match matches with
    | [] -> retrieve_vars args_shape xs acc env type_env
    | [(arg, _)] ->  
      begin match List.assoc_opt x acc with
      | None -> 
        let inferred_type, _ = type_of_pattern env type_env arg in
        retrieve_vars args_shape xs ((x, inferred_type) :: acc) env type_env
      | Some prev_type ->
        let inferred_type, _ = type_of_pattern env type_env arg in
        if prev_type <> inferred_type then failwith "Type mismatch in pattern matching"
        else retrieve_vars args_shape xs acc env type_env
      end
    | _ -> failwith "Unexpected multiple matches for a single type variable"

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
  let tt = instantiate_aux t in 
  (*print_string ("Instantiating type " ^ (type_to_string t) ^": "^(type_to_string tt) ^ "\n");*) tt

