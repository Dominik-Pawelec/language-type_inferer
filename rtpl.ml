(*RTPL: Read; Type; Print; Loop*)

open Ast

let rec read () = 
  let line = read_line () in
  if String.ends_with ~suffix:";" line
    then String.sub line 0 (String.length line - 1)
    else line ^ " " ^ (read ())
let rec rtpl () def_env =
  Printf.printf "> ";
  let input = read () in
  let lexbuf = Lexing.from_string input in
  begin match Parser.prog Lexer.token lexbuf with
  | Expr expr ->
    let typ = Unifier.infer expr def_env in
    Printf.printf ">> Type: %s\n" (type_to_string typ); rtpl () def_env
  | Define(id, expr) -> 
    let typed_expr = Unifier.infer expr def_env in
    let new_def_env = (id, expr)::def_env in
    Printf.printf "Defined %s of Type: %s\n" (id) (type_to_string typed_expr);
    rtpl () new_def_env
  end
