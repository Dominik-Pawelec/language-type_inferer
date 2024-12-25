(*RTPL: Read; Type; Print; Loop*)

open Infer_type
open Ast

 
let rec rtpl () =
  Printf.printf "> ";
  let input = read_line () in
  let lexbuf = Lexing.from_string input in
  let expr = Parser.prog Lexer.token lexbuf in
  let typ = infer expr in
  Printf.printf ">> Type: %s\n" (type_to_string typ);
  rtpl ()