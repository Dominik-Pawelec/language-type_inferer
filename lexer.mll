{
    open Parser
}
let whitespace = [' ' '\t']+
let digit = ['0'-'9']
let signs = [^' ''\t''\n''('')'',']
let integer = '-'? digit+
let identifier = signs+


rule token =
    parse
    | '\n' {Lexing.new_line lexbuf; token lexbuf}
    | whitespace {token lexbuf}
    | "void"{VOID}
    | "(" {LPAREN}
    | ")" {RPAREN}
    | "fun" {FUN} 
    | "->" {ARROW}
    | "let" {LET}
    | ":=" {ASSIGN}
    | "in" {IN}
    | "if" {IF}
    | "then" {THEN}
    | "else" {ELSE}
    | "true" {TRUE}
    | "false" {FALSE}
    | "," {COMMA}
    | "left" {LEFT}
    | "right" {RIGHT}
    | integer {INT (int_of_string (Lexing.lexeme lexbuf))}
    | identifier {IDENT (Lexing.lexeme lexbuf)}
    | eof {EOF}
