
# Type Inferer

This project implements Type Inference algorithm for a simple functional language.

## Features of Language:
Basic types: unit, int, bool
   

    > ();
    >> Type: unit
    > 404;
    >> Type: int
    > false;
    >> Type: bool

Functions and pairs

    > fun x y -> x;
    >> Type: t0 -> t1 -> t0
    > fun x -> (x,x);
    >> Type: t0 -> (t0 * t0)
    > (((),false),20);
    >> Type: (unit * bool) * int

Pattern matching:

    > fun x -> match x with
    case (10,_) -> false
    case _ -> true;
    >> Type: (int * t1) -> bool

Let (allowing for polymorphic types) and if:

    > let id x = x;
    Defined id of Type: .t1 -> t1
    > (id 10, id true);
    >> Type: int * bool
    > fun x -> if x then 10 else 20;
    >> Type: bool -> int
Algebraic Data Types:

    > type list<a> = Nil | Cons a * list<a>;
    Defined Type: list <a > 
    > Cons(1 . Cons(2 . Cons(3 . Nil ())));
    >> Type: list <int>
    let fold_left = fun  f acc ls -> match ls with
	case Cons(x.xs) -> f (fold_left f acc xs) x
	case Nil() -> acc;
	Defined fold_left of Type: (t27 -> t28 -> t27) -> t27 -> list <t28> -> t27




## Usage
To run RIPL (Read Infer Print Loop) you need to first compile project using **dune**:

    dune build

Then run RIPL with command:

    _build/default/main.exe 

 

 

