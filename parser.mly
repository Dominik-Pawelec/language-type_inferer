%{
    open Ast

    let rec create_function input_list e =
    match input_list with
    | [] -> e
    | x:: xs -> Fun(x, create_function xs e)
%}

%token <int> INT
%token <string> IDENT
%token <string> BIGIDENT
%token LPAREN RPAREN
%token COMMA LEFT RIGHT
%token FUN ARROW
%token MATCH WITH CASE
%token LET EQUAL IN
%token IF THEN ELSE
%token TRUE FALSE
%token UNIT  WILDCARD LESS MORE
%token TINT TBOOL TUNIT TPRODUCT TPLUS TYPE
%token EOF 

%start <program> prog

%right COMMA TPRODUCT TPLUS


%%

prog:
    | e = mixfix; EOF { Expr (e) }
    | d = definition; EOF { d }
    | t = type_definition; EOF { t }
    | EOF { Expr Unit }
    ;
definition:
    | LET; x = IDENT; EQUAL; e = mixfix {Define(x, e)}
    ;
type_definition:
    | TYPE; x = IDENT; EQUAL; td = type_declaration {TypeDefine(x, [],td)}
    | TYPE; x = IDENT; LESS; ta = type_args;MORE; EQUAL; td = type_declaration {TypeDefine(x, ta, td)}
    ;
type_args:
    | x = IDENT; t = type_args {x::t}
    | x = IDENT {[x]}
    ;
type_declaration:
    | td1 = type_declaration; TPLUS; td2 = type_declaration {td1 @ td2}
    | constructor_name = BIGIDENT; t = type_annotation {[(constructor_name, t)]}
    ;
type_annotation:
    | t1 = base_type TPRODUCT t2 = type_annotation {TDProduct(t1, t2)}
    | t = base_type {t}
    ;
base_type:
    | TUNIT {TDUnit}
    | TINT {TDInt}
    | TBOOL {TDBool}
    | x = IDENT {TDVar x}
    | LPAREN t = type_annotation RPAREN {t}
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
    | UNIT {PUnit}
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
    | UNIT { Unit }
    | LPAREN; RPAREN { Unit }
    | nr = INT { Int nr }
    | TRUE { Bool true }
    | FALSE { Bool false }
    | x = IDENT { Var x } 
    | LPAREN; e = mixfix; RPAREN { e }
    ;
