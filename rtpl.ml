(*RTPL: Read; Type; Print; Loop*)

open Infer_type
open Ast

let rec read () = 
  let line = read_line () in
  if String.ends_with ~suffix:";" line
    then String.sub line 0 (String.length line - 1)
    else line ^ " " ^ (read ())
let rec rtpl () =
  Printf.printf "> ";
  let input = read () in
  let lexbuf = Lexing.from_string input in
  begin match Parser.prog Lexer.token lexbuf with
  | Expr expr ->
    let typ = infer expr in
    Printf.printf ">> Type: %s\n" (type_to_string typ)
  | Define(id, expr) -> Printf.printf "Defined %s of Type: %s\n" (id) (type_to_string (infer expr))
  end; rtpl()
