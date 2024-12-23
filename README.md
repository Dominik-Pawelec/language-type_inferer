# Type Inferer


This project implements Hindley Milner Type Inference for a simple functional language.
This language contains λ-calculus with additional operations: let, if ; and primitive types: void, bool, int.

## RTPL
To run RTPL (Read Type Print Loop) you need to first compile project using **dune**:

    dune build

Then  you can run rtpl with command:

    _build/default/rtpl.exe
 

## Examples
Examples of expresions typed using rtpl.

    > a
    >> Type: a0
    > true
    >> Type: bool
    > fun x -> x
    >> Type: a0 -> a0
    > let id := fun x -> x in id 20
    >> Type: int
    > if true then fun x -> 2 else fun y -> 3
    >> Type: a1 -> int
    > let t := fun x y -> true in if t 2 void then false else true
    >> Type: bool
    > fun x y z -> x (y z z)
    >> Type: (a4 -> a3) -> (a2 -> a2 -> a4) -> a2 -> a3
    
    

y implementation of a simple S-Expression programing language with Hindley-Milner type system and type inference
