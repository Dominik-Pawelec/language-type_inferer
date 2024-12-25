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
%token COMMA LEFT RIGHT
%token FUN ARROW
%token LET ASSIGN IN
%token IF THEN ELSE
%token TRUE FALSE
%token VOID
%token EOF

%start <expr> prog

%right COMMA

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
    | IF; e = expr; THEN; t = mixfix; ELSE; f = mixfix {If(e, t, f)}
    | x = expr { x }
    ;
expr:
    | e1 = expr; COMMA; e2 = expr {Pair(e1, e2)}
    | e = app { e }
    ;
app:
    | e1 = app; e2 = base { App(e1, e2) }
    | LEFT {Left}
    | RIGHT {Right}
    | e = base { e }
    ;
base:
    | VOID { Void }
    | nr = INT { Int nr }
    | TRUE { Bool true }
    | FALSE { Bool false }
    | x = IDENT { Var x } 
    | LPAREN; e = mixfix; RPAREN { e }
    ;
