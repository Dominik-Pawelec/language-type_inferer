%{
    open Ast

    let rec create_function input_list e =
    match input_list with
    | [] -> e
    | x:: xs -> Fun(x, create_function xs e)
%}

%token <int> INT
%token <string> IDENT
%token LPAREN RPAREN
%token FUN ARROW
%token LET ASSIGN IN
%token TRUE FALSE
%token EOF

%start <expr> prog

%%

prog:
    | e = mixfix EOF { e }
    ;

idents:
    | x = IDENT; xs = idents { x :: xs }
    | x = IDENT { [x] }
    ;

mixfix:
    | LET; x = IDENT; ASSIGN; e1 = mixfix; IN; e2 = mixfix { Let(x,e1,e2) }
    | FUN; xs = idents; ARROW; e = mixfix { create_function xs e }
    | x = expr { x }
    ;
expr:
    | e = app { e }
    ;
app:
    | e1 = app; e2 = base { App(e1, e2) }
    | e = base { e }
    ;
base:
    | nr = INT { Int nr }
    | TRUE { Bool true }
    | FALSE { Bool false }
    | x = IDENT { Var x } 
    | LPAREN; e = mixfix; RPAREN { e }
    ;
