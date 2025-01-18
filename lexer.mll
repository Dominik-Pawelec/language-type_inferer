{
    open Parser
}
let whitespace = [' ' '\t''\n']+
let digit = ['0'-'9']
let signs = [^' ''\t''\n''('')'',']
let integer = '-'? digit+
let identifier = signs+


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
    | "def" {DEF}
    | integer {INT (int_of_string (Lexing.lexeme lexbuf))}
    | identifier {IDENT (Lexing.lexeme lexbuf)}
    | eof {EOF}
