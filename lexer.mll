{
    open Parser
}
let whitespace = [' ' '\t''\n']+
let digit = ['0'-'9']
let small_sign = ['a'-'z''_']
let big_sign = ['A'-'Z']
let sign =['a'-'z''A'-'Z''0'-'9''_'] 
let integer = '-'? digit+
let identifier = small_sign sign*
let big_identifier = big_sign sign*
let type_constructor = big_sign sign*



rule token =
    parse
    | whitespace {token lexbuf}
    | "(" {LPAREN}
    | ")" {RPAREN}
    | "fun" {FUN} 
    | "->" {ARROW}
    | "<" {LESS}
    | ">" {MORE}
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
    | "type" {TYPE}
    | "unit" {TUNIT}
    | "int" {TINT}
    | "bool" {TBOOL}
    | "|" {TPLUS}
    | "*" {TPRODUCT}
    | "." {DOT}
    | integer {INT (int_of_string (Lexing.lexeme lexbuf))}
    | big_identifier {BIGIDENT (Lexing.lexeme lexbuf)}
    | identifier {IDENT (Lexing.lexeme lexbuf)}
    | eof {EOF}
