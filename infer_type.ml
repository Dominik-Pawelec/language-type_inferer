open Ast
open Utils

let rec type_of_pattern env type_env = function
  | PUnit     -> (TUnit, env)
  | PBool _   -> (TBool, env)
  | PInt _    -> (TInt, env)
  | PWildcard -> (new_type (), env)
  | PVar id   -> 
    let t = new_type () in
    (t, M.add id t env)
  | PPair (p1, p2) -> 
    let (t1, env') = type_of_pattern env type_env p1 in
    let (t2, env'') = type_of_pattern env' type_env p2 in
    (TPair(t1, t2), env'')
 | PConstructor(id, args) ->
   (match M.find_opt id type_env with
     | None -> failwith ("Undefined type constructor: " ^ id)
     | Some (shapes, variables, type_name) ->
        if List.length args <> List.length shapes then failwith "Pattern's argument number mismatch."
        else
          let variable_env = List.map (fun v -> (v, new_type ())) variables in
          let env_map = List.fold_left (fun acc (v, t) -> M.add v t acc) env variable_env in
          let shape_types = List.map (fun s -> shape_to_type s variable_env) shapes in
          let updated_env, collected_constraints =
            List.fold_left2
              (fun (env_acc, constraints) pat shape_t ->
                let inferred_t, new_env = type_of_pattern env_acc type_env pat in
                ((M.merge (fun _ v1 v2 -> match v1, v2 with
                    | Some t, _ | _, Some t -> Some t
                    | _ -> None) 
                env_acc new_env),
                (inferred_t, shape_t) :: constraints))
              (env_map, []) args shape_types
          in

          let substitutions = Unifier.unify collected_constraints in
          let final_env = M.map (Unifier.apply_substitution substitutions) updated_env in
          let output_type = TADT(type_name, List.map snd variable_env) in
          let final_output_type = Unifier.apply_substitution substitutions output_type in
          (final_output_type, final_env)
      )



let rec collect_constrains aexpr_ls constrains_ls =
  match aexpr_ls with
  | []             -> constrains_ls
  | AUnit  ::rest -> collect_constrains rest constrains_ls
  | AInt   ::rest -> collect_constrains rest constrains_ls
  | ABool  ::rest -> collect_constrains rest constrains_ls
  | AVar _ ::rest -> collect_constrains rest constrains_ls
  | AFun (_, aexpr', _)::rest -> collect_constrains (aexpr' :: rest) constrains_ls
  | AApp (aexpr1, aexpr2, t)::rest -> 
    let (t1, t2) = (type_of aexpr1, type_of aexpr2) in
  collect_constrains (aexpr1 :: aexpr2 :: rest) ( (t1, TFun(t2, t))::constrains_ls)
   | ALet(id, value, expr, value_type, _)::rest -> 
    let constrains_value = collect_constrains [value] constrains_ls in
    let fresh_type = instantiate value_type in
    let constrains_expr = collect_constrains [expr] ((type_of (AVar (id, fresh_type)), fresh_type)::constrains_ls) in
    collect_constrains rest (constrains_value @ constrains_expr)
  | AIf(e, t, f, typ)::rest ->
    let e_typ = type_of e in
    let t_typ = type_of t and f_typ = type_of f in
    let e_constrains = collect_constrains [e] constrains_ls in
    let t_constrains = collect_constrains [t] constrains_ls in
    let f_constrains = collect_constrains [f] constrains_ls in
    collect_constrains rest ((e_typ, TBool)::(t_typ, typ)::(f_typ, typ):: t_constrains @ f_constrains @ e_constrains)
  | APair(a, b)::rest -> collect_constrains (a::b::rest) constrains_ls
  | AMatch(aexpr, cases, result_type)::rest ->
    let aexpr_constrains = collect_constrains [aexpr] constrains_ls in
    let case_expr_constrains = List.fold_left (fun constr (_,t,_) ->  
    (type_of aexpr, t) :: constr) aexpr_constrains cases in
    let output_constrains = List.fold_left (fun constr (_,_,aexpr') -> 
      (type_of aexpr', result_type)::constr) case_expr_constrains cases in
    let final_constrains = List.fold_left (fun constr (_,_,aexpr') -> 
      (collect_constrains [aexpr'] output_constrains) @constr ) [] cases in
    collect_constrains rest final_constrains
  | AConstructor(_, annot_args, shape_ls, var_ls, _)::rest ->
    let shape_type = List.map (fun shape -> shape_to_type shape var_ls) shape_ls in
    let constrains_ = List.map2 (fun arg shape_t -> (type_of arg, shape_t)) annot_args shape_type in
    let args_constrains = collect_constrains annot_args [] in
    collect_constrains rest (constrains_ls @ constrains_ @ args_constrains)
;;

let annotate expr type_env  =
  let (h_table : (id, typ) Hashtbl.t) = Hashtbl.create 16 in
  let rec annotate_rec expr env type_env =
  match expr with
  | Unit   -> AUnit
  | Int  _ -> AInt
  | Bool _ -> ABool
  | Var id -> 
    (match M.find_opt id env with
    | Some a -> AVar(id, a)
    | None ->
      (try let a = Hashtbl.find h_table id in AVar(id, a)
    with Not_found -> let a = new_type () in Hashtbl.add h_table id a; AVar (id,a)
      ) 
    )
  | Fun (id, expr') -> 
    let a = new_type () in
    let aexpr = annotate_rec expr' (M.add id a env) type_env in
    let t_fun = TFun(a, type_of aexpr) in
    AFun(id, aexpr, t_fun)
  | App (expr1, expr2) -> 
    AApp(annotate_rec expr1 env type_env, annotate_rec expr2 env type_env, (new_type ()))
  | Let(id, value, expr) ->
    let annotated_value = annotate_rec value env type_env in
    let value_type = type_of annotated_value in
    let constraints = collect_constrains [annotated_value] [] in
    let substitutions = Unifier.unify constraints in
    let resolved_value_type = Unifier.apply_substitution substitutions value_type in
    let updated_env = M.add id resolved_value_type env in
    let annotated_value = annotate_rec value updated_env type_env in
    let value_type = type_of annotated_value in
    let constraints = collect_constrains [annotated_value] [] in
    let substitutions = Unifier.unify constraints in
    let resolved_value_type = Unifier.apply_substitution substitutions value_type in
    let polymorphic_type = TPolymorphic resolved_value_type in
    let updated_env = M.add id polymorphic_type env in
    let annotated_expr = annotate_rec expr updated_env type_env in
    let expr_type = type_of annotated_expr in
    
   ALet(id, annotated_value, annotated_expr, resolved_value_type, expr_type)
| If(e, t, f) -> AIf(annotate_rec e env type_env, annotate_rec t env type_env, annotate_rec f env type_env, (new_type ()))
  | Pair(a, b) -> APair(annotate_rec a env type_env, annotate_rec b env type_env)
  | Match(expr, cases) ->
    let annotated_expr  = annotate_rec expr env type_env in
    let annotated_cases =
      List.map (fun (pattern, case) -> 
        let (pattern_type, new_env) = type_of_pattern env type_env pattern in
        (pattern, pattern_type, (annotate_rec case new_env type_env))) cases in
    let output_type = match (List.hd annotated_cases) with
      | (_,_,x) -> type_of x in
    AMatch(annotated_expr, annotated_cases, output_type)
  | Constructor(id, args_ls) ->
    match M.find_opt id type_env with
    | None -> failwith "undefined type constructor."
    | Some (shape_ls, variables, type_name) ->      
      let annot_args   = List.map (fun x -> annotate_rec x env type_env) args_ls in
      let variable_env = List.map (fun x -> (x, new_type ())) variables in
      AConstructor(id, annot_args, shape_ls, variable_env, 
      TADT(type_name, List.map (fun (_, typ) -> typ)variable_env))
      
  in annotate_rec expr (M.empty) type_env
;;

