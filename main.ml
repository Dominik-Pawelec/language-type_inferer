open Infer_type
open Ast

let read_file file_name =
  try
    let ic = open_in file_name in
    try
      while true do
        let line = input_line ic in
        if line = "" || line = "\n" then () else 
        let lexbuf = Lexing.from_string line in
        let expr = Parser.prog Lexer.token lexbuf in
        let typ = infer expr in
        Printf.printf "> %s \n>> Type: %s\n" (line) (type_to_string typ);
      done
    with
    | End_of_file ->
      close_in ic;
  with
  | Sys_error err ->
    Printf.printf "Error: %s \n" err

let () =
  if Array.length Sys.argv <> 2 then
    Printf.printf "Usage: ./_build/default/main.exe <\"example.typed\" to type file line by line or -r for repl>\n "  
  else
    match Array.to_list Sys.argv with
    | [_; "-r"] -> Rtpl.rtpl ()
    | [_; file_name] -> read_file file_name
    | _ -> Printf.printf "Usage: ./_build/default/main.exe < example.typed to type file line by line or -r for repl>\n "  
