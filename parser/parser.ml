module type Values = sig
  type v
  val assign_parsed : string -> v
end

(*module SimpleTV = struct
  type v = string
  let assign_parsed str: v = str
end*)

module Sexpr (T : Values) = struct
  type elem = T.v
  type t =
    | ATOM of elem
    | LIST of t list
  type token =
    | ATOMtoken of string
    | LEFTPAR
    | RIGHTPAR
  let tokenize input = 
    let get_char input =
      if String.empty = input then None else Some (String.get input 0)
    in
    let rec tokenize_rec input =
      match get_char input with
      | None -> []
      | Some ')' -> RIGHTPAR :: (tokenize_rec (String.sub input 1 (String.length input - 1)))
      | Some '(' -> LEFTPAR :: (tokenize_rec (String.sub input 1 (String.length input - 1)))
      | Some x when x = ' ' || x = '\n' || x = '\t' -> (tokenize_rec (String.sub input 1 (String.length input - 1)))
      | Some _ -> 
        let parse_charlist ls = List.fold_left (fun acc x -> (String.make 1 x) ^ acc) "" ls
        in
        let rec generate_atom input characters =
          begin match get_char input with
          | None -> (parse_charlist characters, "")
          | Some x when x = ' ' || x = '\n' || x = '\t' || x = '(' || x = ')' -> (parse_charlist characters, input)
          | Some x -> generate_atom (String.sub input 1 (String.length input - 1)) (x :: characters)
          end
        in let (new_atom, input')  = generate_atom (input) []
        in ATOMtoken(new_atom) :: (tokenize_rec input')
      in tokenize_rec input

  let parse list_tokens = 
    let rec parse_sublist sublist_tokens depth =
      match sublist_tokens with
      | [] when depth <> 0 -> failwith "PARSER ERROR: Mismatch of right parenthesis"
      | ATOMtoken str :: xs -> ATOM (T.assign_parsed str) :: parse_sublist xs depth
      | LEFTPAR :: xs -> LIST (parse_sublist xs (depth + 1)) :: parse_sublist (get_list_after xs) depth
      | _ -> []
    and get_list_after xs =
      let rec get_list_after_rec xs depth =
        match xs with
        | LEFTPAR::xs -> get_list_after_rec xs (depth + 1)
        | RIGHTPAR::xs -> if depth = 0 then xs else get_list_after_rec xs (depth - 1)
        | ATOMtoken _ :: xs -> get_list_after_rec xs depth
        | _ -> failwith "PARSER ERROR: Mismatch of right parenthesis"
      in get_list_after_rec xs 0
    in parse_sublist list_tokens 0
end