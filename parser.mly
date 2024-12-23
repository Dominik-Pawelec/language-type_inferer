%{
    open Ast

    let rec create_function input_list e =
    match input_list with
    | [] -> e
    | x:: xs -> Fun(x, create_function xs e)
%}

%token <int> INT
%token <string> IDENTIFIER
%token LPAREN RPAREN
%token FUN ARROW
%token LET ASSIGN IN
%token TRUE FALSE
%token EOF

%start <Ast.expr> prog

%%

prog:
    | e = mixfix; EOF {e}
    ;

idents:
    | x = IDENT; xs = idents {x :: xs}
    | x = IDENTIFIER {[x]}
    ;
mixfix:
    | LET; x = IDENT; ASSIGN; e1 = mixfix; IN; e2 = mixfix {Let(x,e1,e2)}
    | FUN; xs = idents; ARROW; e = mixfix {create_function xs e}
    | x = base {x}
    ;

base:
    | LPAREN; e = mixfix; RPAREN {e}
    | nr = INT {Int nr}
    | x = TRUE {Bool true}
    | x = FALSE {Bool false}
    | x = IDENT {Var x}
    ;