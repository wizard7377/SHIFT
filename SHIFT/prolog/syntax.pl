
term(tet(yud(A))).
term(tet(lamed)).
term(kaf(A,B)) :- term(A), term(B).
term(dalet(A,B)) :- term(B).
term(ayin(From,To,Term)) :- term(To), term(Term).

makeList([A],C) :- writeTerm(A,C).
makeList([A | B],C) :- writeTerm(A,A1), makeList(B,B1), kaf(A1,B1) = C.
writeTerm(A,B) :-
    %(term(A) -> B = A) ;
    (var(A) -> A = B) ; 
    (A =.. [kaf | C1] -> makeList(C1,B)) ;
    (A =.. [C0 , C1 | C2] -> makeList([C0 , C1 | C2],B)) ;
    (lamed = A -> tet(lamed) = B) ;
    (B = tet(yud(A))).

readList(A,B) :-
    (A = kaf(C,D) -> readList(D,D1), B = [C | D1]) ;
    (B = [A]).

replace(From,To,Input,Output) :-
    ((Input = From -> Output = To) ;
    (Input = kaf(A,B) -> replace(From,To,A,A1), replace(From,To,B,B1), Output = kaf(A1,B1)) ;
    (Input = dalet(A,B) -> replace(From,To,A,A1), replace(From,To,B,B1), Output = dalet(A1,B1)) ;
    (Input = ayin(A,B,C) -> replace(From,To,A,A1), replace(From,To,B,B1), replace(From,To,C,C1), Output = ayin(A1,B1,C1)) ;
    (Input = tet(A) -> replace(From,To,A,B), Output = tet(B)) ;
    (Input = yud(A) -> replace(From,To,A,B), Output = tet(B)) ;
    (Input = Output)).

