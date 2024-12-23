{
    open Parser
}
let whitespace = [' ' '\t']+
let digits = ['0'-'9']
let signs = [^' ''\t''\n']
let integer = '-'? digit+
let identifier = signs+

rule token =
    parse
    | '\n' {Lexing.new_line lexbuf; token lexbuf}
    | whitespace {token lexbuf}
    | "(" {LPAREN}
    | ")" {RPAREN}
    | "fun" {FUN} 
    | "->" {ARROW}
    | "let" {LET}
    | ":=" {ASSIGN}
    | "in" {IN}
    | "true" {TRUE}
    | "false" {FALSE}
    | integer {INT (int_of_string (Lexing.lexeme lexbuf))}
    | identifier {IDENTIFIER (Lexing.lexeme lexbuf)}
    | eof {EOF}