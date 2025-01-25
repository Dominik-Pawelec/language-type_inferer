{
    open Parser
}
let whitespace = [' ' '\t''\n']+
let digit = ['0'-'9']
let small_sign = ['a'-'z''_']
let big_sign = ['A'-'Z']
let sign = small_sign | big_sign | digit
let integer = '-'? digit+
let identifier = small_sign | sign+
let type_constructor = big_sign | sign+


rule token =
    parse
    | whitespace {token lexbuf}
    | "unit"{UNIT}
    | "(" {LPAREN}
    | ")" {RPAREN}
    | "fun" {FUN} 
    | "->" {ARROW}
    | "let" {LET}
    | "=" {EQUAL}
    | "in" {IN}
    | "if" {IF}
    | "then" {THEN}
    | "else" {ELSE}
    | "true" {TRUE}
    | "false" {FALSE}
    | "," {COMMA}
    | "left" {LEFT}
    | "right" {RIGHT}
    | "match" {MATCH}
    | "with" {WITH}
    | "case" {CASE}
    | "unit" {TUNIT}
    | "int" {TINT}
    | "bool" {TBOOL}
    | "*" {TPRODUCT}
    | integer {INT (int_of_string (Lexing.lexeme lexbuf))}
    | identifier {IDENT (Lexing.lexeme lexbuf)}
    | eof {EOF}
