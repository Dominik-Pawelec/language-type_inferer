module type Values =
  sig
    type v
    val assign_parsed : string -> bool -> v
  end
module Sexpr :
  functor (T : Values) ->
    sig
      type elem = T.v
      type t = ATOM of elem | LIST of t list
      type token = ATOMtoken of string | LEFTPAR | RIGHTPAR
      val tokenize : string -> token list
      val parse : token list -> t list

    end