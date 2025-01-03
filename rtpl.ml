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
  Printf.printf "DEBUG: %s \n" input;
  let lexbuf = Lexing.from_string input in
  let expr = Parser.prog Lexer.token lexbuf in
  let typ = infer expr in
  Printf.printf ">> Type: %s\n" (type_to_string typ);
  rtpl ()