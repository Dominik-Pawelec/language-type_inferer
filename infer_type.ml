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
  | ALet(_,_,_,t) -> t
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

let annotate expr =
  let (h_table : (id, typ) Hashtbl.t) = Hashtbl.create 16 in
  let rec annotate_rec expr env =
  match expr with
  | Unit -> AUnit
  | Int nr -> AInt(nr, TInt)
  | Bool b -> ABool(b,TBool)
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
    let aexpr = annotate_rec expr' (M.add id a env) in
    let t_fun = TFun(a, type_of aexpr) in
      AFun(id, aexpr, t_fun)
  | App (expr1, expr2) -> AApp(annotate_rec expr1 env, annotate_rec expr2 env, (new_type ()))
  | Let(id, value, expr) -> 
    let avalue = annotate_rec value env in
    let value_typ = type_of avalue in
    let aexpr = annotate_rec expr (M.add id value_typ env) in
    let a = type_of avalue in
    Hashtbl.add h_table id a;
    ALet(id, avalue, aexpr, type_of aexpr)
  | If(e, t, f) -> AIf(annotate_rec e env, annotate_rec t env, annotate_rec f env, (new_type ()))
  | Pair(a, b) -> APair(annotate_rec a env, annotate_rec b env)
  | Left -> 
    let new_var1, new_var2 = new_type (), new_type () in
    ALeft (TFun(TPair(new_var1, new_var2),new_var1))
  | Right -> 
    let new_var1, new_var2 = new_type (), new_type () in
    ARight (TFun(TPair(new_var1, new_var2),new_var2))
  | Match(expr, cases) ->
    let annotated_expr = annotate_rec expr env in
    let annotated_cases =
      List.map (fun (pattern, case) -> 
        let (pattern_type, new_env) = type_of_pattern env pattern in
        (pattern, pattern_type, (annotate_rec case new_env)))
        cases in
    let output_type = match (List.hd annotated_cases) with
        | (_,_,x) -> type_of x   in
    AMatch(annotated_expr, annotated_cases, output_type)
      
  in annotate_rec expr (M.empty) 
;;


let rec collect_constrains aexpr_ls constrains_ls =
  match aexpr_ls with
  | [] -> constrains_ls
  | AUnit :: rest -> collect_constrains rest constrains_ls
  | AInt _ :: rest -> collect_constrains rest constrains_ls
  | ABool _ :: rest -> collect_constrains rest constrains_ls
  | AVar (_,_)::rest -> collect_constrains rest constrains_ls
  | AFun (_, aexpr', _)::rest -> collect_constrains (aexpr' :: rest) constrains_ls
  | AApp (aexpr1, aexpr2, t)::rest -> 
    let (t1, t2) = (type_of aexpr1, type_of aexpr2) in
  collect_constrains (aexpr1 :: aexpr2 :: rest) ( (t1, TFun(t2, t))::constrains_ls)
  | ALet(id, value, expr, _)::rest -> 
    let var_id = type_of (annotate (Var id)) in
    let value_typ = type_of value in
    let constrains_value = collect_constrains [value] constrains_ls in
    let constrains_expr = collect_constrains [expr] ((var_id, value_typ)::constrains_ls) in
    collect_constrains rest (constrains_value @ constrains_expr @ constrains_ls)
  | AIf(e, t, f, typ)::rest ->
    let e_typ = type_of e in
    let t_typ = type_of t and f_typ = type_of f in
    let t_constrains = collect_constrains [t] constrains_ls in
    let f_constrains = collect_constrains [f] constrains_ls in
    collect_constrains rest ((e_typ, TBool)::(t_typ, typ)::(f_typ, typ):: t_constrains @ f_constrains)
  | APair(a, b)::rest -> collect_constrains (a::b::rest) constrains_ls
  | ALeft(TFun(TPair(x, _), out))::rest ->  
    collect_constrains rest ((x, out)::constrains_ls) 
  | ARight(TFun(TPair(_, y), out))::rest ->  
    collect_constrains rest ((y, out)::constrains_ls) 
  | AMatch(aexpr, cases, result_type) :: rest ->
    let aexpr_constrains = collect_constrains [aexpr] constrains_ls in
    let case_expr_constrains = 
      List.fold_left (fun constr (_,t,_) ->  (type_of aexpr, t) :: constr)
      aexpr_constrains cases in
    let output_constrains = 
      List.fold_left (fun constr (_,_,aexpr') -> (type_of aexpr', result_type):: constr )
      case_expr_constrains cases in
    let uwu = 
    List.fold_left (fun constr (_,_,aexpr') -> (collect_constrains [aexpr'] output_constrains) @constr )
      [] cases in
    collect_constrains rest (uwu @ output_constrains)
  | _ -> failwith "wrong type annotation"
;;

let infer expr def_env =
    let rec def_to_let env = 
      match env with
      | [] -> expr
      | (id, e)::xs -> Let(id, e, def_to_let xs)
    in
  type_name := 0;
  let annotated_expr = annotate (def_to_let def_env)
  in let constrains = collect_constrains [annotated_expr] []
  in let temp = Unifier.unify constrains
  in Unifier.apply_substitution temp (type_of annotated_expr)
