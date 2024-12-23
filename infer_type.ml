open Ast

let type_name = ref 0
let new_type ()=
  let n = !type_name in
  type_name := n + 1; 
  TVar n

module M = Map.Make(String)
type env = typ M.t


let type_of = function
  | AVoid -> TVoid
  | AInt _ -> TInt
  | ABool _ -> TBool
  | AVar(_, t) -> t
  | AFun(_, _, t) -> t
  | AApp(_, _, t) -> t
  | ALet(_,_,_,t) -> t
  | AIf(_,_,_,t) -> t

let annotate expr =
  let (h_table : (id, typ) Hashtbl.t) = Hashtbl.create 16 in
  let rec annotate_rec expr env =
  match expr with
  | Void -> AVoid
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
  in annotate_rec expr (M.empty) 
;;

let rec collect_constrains aexpr_ls constrains_ls =
  match aexpr_ls with
  | [] -> constrains_ls
  | AVoid :: rest -> collect_constrains rest constrains_ls
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
;;



let infer expr =
  type_name := 0;
  let annotated_expr = annotate expr
  in let constrains = collect_constrains [annotated_expr] []
  in let temp = Unifier.unify constrains
  in Unifier.apply_substitution temp (type_of annotated_expr)