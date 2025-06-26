:- module(solver,[klamed/4,memReduce/2]).
:- use_module(abstract).
klamed(A,B,C,Res) :- tkaf(ttet(lamed),tkaf(A,tkaf(B,C))) = Res.


memReduce(T,R) :-
    splitFree(T,TF0,tkaf(F0,I0)),
    (klamed(Free,FromA,ToA,FromB,ToB,F0)).
    .

testSolve :- fail.



