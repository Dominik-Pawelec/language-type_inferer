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
%token MATCH WITH CASE
%token LET EQUAL IN
%token IF THEN ELSE
%token TRUE FALSE
%token VOID WILDCARD
%token EOF

%start <expr> prog

%left COMMA

%%

prog:
    | e = mixfix EOF { e }
    | EOF { Int 0 }
    ;

idents:
    | x = IDENT; xs = idents { x :: xs }
    | x = IDENT { [x] }
    ;

mixfix:
    | LET; x = IDENT; EQUAL; e1 = mixfix; IN; e2 = mixfix { Let(x,e1,e2) }
    | e1 = mixfix; COMMA; e2 = mixfix {Pair(e1, e2)}
    | FUN; xs = idents; ARROW; e = mixfix { create_function xs e }
    | IF; e = expr; THEN; t = mixfix; ELSE; f = mixfix {If(e, t, f)}
    | MATCH; e = expr; WITH; cases = patternmatch {Match(e, cases)}
    | x = expr { x }
    ;
patternmatch:
    | CASE; p = pattern; ARROW; e = expr {[(p, e)]}
    | p1 = patternmatch; p2 = patternmatch {p1 @ p2}
    ;
pattern:
    | WILDCARD {PWildcard}
    | VOID {PUnit}
    | nr = INT {PInt nr}
    | TRUE { PBool true }
    | FALSE { PBool false }
    | x = IDENT { PVar x }
    | e1 = pattern; COMMA; e2 = pattern {PPair(e1, e2)}
    | LPAREN; p = pattern; RPAREN {p}
    ; 
expr:
    | e = app { e }
    ;
app:
    | e1 = app; e2 = base { App(e1, e2) }
    | LEFT {Left}
    | RIGHT {Right}
    | e = base { e }
    ;
base:
    | VOID { Unit }
    | LPAREN; RPAREN { Unit }
    | nr = INT { Int nr }
    | TRUE { Bool true }
    | FALSE { Bool false }
    | x = IDENT { Var x } 
    | LPAREN; e = mixfix; RPAREN { e }
    ;
