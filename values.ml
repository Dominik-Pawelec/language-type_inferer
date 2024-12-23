module FunctionValues = struct
  type v =
  | Int of int
  | Bool of bool
  | String of string
  | Var of var
  | FunId of var
  and var = string

  let assign_parsed str fst_token_of_group = 
    match str with
    | "+" -> FunId "+" (*todo: make more operations and make them generic*)
    | "=" -> FunId "="
    | "def" -> FunId "def" (*def x (+ 2 2)*)
    | "if" -> FunId "if" (*if (= x 10) ("x = 10") ("x =/= 10")*)
    | "fun" -> FunId "fun" (*fun x (+ x 10)*) (*funkcja anonimowa*)
    | "let" -> FunId "let" (*let x 10 (= x 10)*)
    | "type" -> FunId "type" (*type true*)
    | "true" -> Bool true
    | "false" -> Bool false
    | _ -> if (String.get str 0) = '"' || (String.get str (String.length str - 1)) = '"' 
      then String (String.sub str 1 (String.length str - 2))
      else if String.for_all (fun x -> x >= '0' && x <= '9') str 
        || (String.get str 0 = '-' 
        && String.for_all (fun x -> x >= '0' && x <= '9') (String.sub str 1 (String.length str - 1)))
        then Int (int_of_string str)
        else if fst_token_of_group then FunId str else Var str
end