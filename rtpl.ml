(*RTPL: Read; Type; Print; Loop*)

open Ast
open Utils

let debug_constraints constraints =
  (*List.iter (fun (t1, t2) -> 
    Printf.printf "Constraint: %s = %s\n" (type_to_string t1) (type_to_string t2)
  ) constraints; *)
  constraints
;;

let infer expr def_env type_env =
    let rec def_to_let env =
      match env with
      | [] -> expr
      | (id, e)::xs -> Let(id, e, def_to_let xs)
    in
  Utils.type_name := 0;
  let annotated_expr = Infer_type.annotate (def_to_let (List.rev def_env)) type_env
  in let constrains = debug_constraints (Infer_type.collect_constrains [annotated_expr] [])
  in let temp = Unifier.unify constrains
  in Unifier.apply_substitution temp (Utils.type_of annotated_expr)

let rec read () = 
  let line = read_line () in
  if String.ends_with ~suffix:";" line
    then String.sub line 0 (String.length line - 1)
    else line ^ " " ^ (read ())
let rec rtpl () def_env type_env =
  Printf.printf "> ";
  let input = read () in
  let lexbuf = Lexing.from_string input in
  begin match Parser.prog Lexer.token lexbuf with
  | Expr expr ->
    let typ = infer expr def_env type_env in
    Printf.printf ">> Type: %s\n" (type_to_string typ); rtpl () def_env type_env
  | Define(id, expr) -> 
    let typed_expr = infer expr def_env type_env in
    let new_def_env = (id, expr)::def_env in
    Printf.printf "Defined %s of Type: %s\n" (id) (type_to_string typed_expr);
    rtpl () new_def_env type_env
  | TypeDefine (id, args, constructors) ->
    let new_type_env = 
        List.fold_left (fun acc (name, shape) -> print_int (List.length shape) ;M.add name (shape, args, id) acc) type_env constructors 
    in 
    Printf.printf "Defined Type: %s <%s> \n" id (List.fold_right (fun x acc -> x ^ " " ^ acc) args "" );
    rtpl () def_env (new_type_env)
  end
