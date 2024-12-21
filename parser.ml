module type TypeValues = sig
  type v
  type t 
  val assign_parsed : string -> v
  val infer_type : v -> t
end

module SimpleTV = struct
  type v = string
  type t = int
  let assign_parsed str = str
  let infer_type v = String.length v
end

module Sex(T : TypeValues) = struct
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
      | Some '(' -> RIGHTPAR :: (tokenize_rec (String.sub input 1 (String.length input - 1)))
      | Some ')' -> LEFTPAR :: (tokenize_rec (String.sub input 1 (String.length input - 1)))
      | Some x when x = ' ' || x = '\n' || x = '\t' -> (tokenize_rec (String.sub input 1 (String.length input - 1)))
      | Some x -> (* ATOM *)
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

  let rec parse token_list = 
      match token_list with
      | [] -> []
      | x :: xs -> 
        match x with
        | ATOMtoken str -> (ATOM(T.assign_parsed str)) :: parse xs
        | RIGHTPAR -> 
          (match List.find_index (fun x -> x = LEFTPAR) token_list with
          | None -> failwith "PARSING ERROR: parenthesis mismatch. Perhabs you're missing ')'?"
          | Some nr -> 
            let rec split_list (a,b) nr =
              if nr = 0 then (List.rev a, b)
              else 
              (match b with
              | [] -> failwith "PARSE INNER ERROR: Dominik messed something up"
              | x::xs -> split_list (x::a, xs) (nr - 1)
              )
            in let (a,b) = split_list ([],xs) nr
            in LIST(parse a) :: (parse b)
            )
        | LEFTPAR -> [] (*works incorrectly when only ) is placed as program, TODO: later*) 
end


module S = Sex(SimpleTV);;

S.tokenize "(A BCD)";;