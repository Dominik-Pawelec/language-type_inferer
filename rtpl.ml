
(*RTPL: Read; Type; Print; Loop*)

open Infer_type
open Ast

 
let rec rtpl () =
  let input = read_line () in
  let lexbuf = Lexing.from_string input in
  let expr = Parser.prog Lexer.token lexbuf in
  let typ = infer expr in
  Printf.printf "\ttype of {%s}: %s\n" (expr_to_string expr) (type_to_string typ);
  rtpl ()
;;
rtpl ()
