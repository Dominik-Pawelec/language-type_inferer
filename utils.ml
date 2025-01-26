open Ast

let type_name = ref 0
let new_type ()=
  let n = !type_name in
  type_name := n + 1; 
  TVar n

module M = Map.Make(String)
type env = typ M.t

(*key - name of constructor, eg. Cons*)
(* In environment: (name of constructor, its "shape" , TCustom(name,<args>)) *)
(* Constructor(id, arguments) -> 
    it annotates everything in args,
    and then finds id in type_environment.
    if not found, error
    else, makes AConstructor(id, annotated_arguments, "shape", mini type environment: ("x", TVar 1)("y", TVar 2), TCustom(name,<TVar 1 TVar 2>))

  AConstructor (id, annot_args, shape, type_env, typ) -> 
      checks if sizes of annot and shape match
      if so, transform "shape" from Shape type to Typ type
      and then, collect constrain from each pair annot_arg, shape_elem
      (i dont believe constraining typ is needed)

    During unification, 2 custom types of args should have same name, and unify all their args
and that's it?
   *)

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

let rec type_of_pattern env = function
  | PUnit -> (TUnit, env)
  | PBool _ -> (TBool, env)
  | PInt _ -> (TInt, env)
  | PWildcard -> (new_type (), env)
  | PVar id -> 
    let t = new_type () in
    (t, M.add id t env)
  | PPair (p1, p2) -> 
    let (t1, env') = type_of_pattern env p1 in
    let (t2, env'') = type_of_pattern env' p2 in
    (TPair(t1, t2), env'')

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
  in
  let tt = instantiate_aux t in 
  (*print_string ("Instantiating type " ^ (type_to_string t) ^": "^(type_to_string tt) ^ "\n");*) tt
