:- module(abstract,[qof/2,fromQof/2,toQof/2,kafAsCons/2,klist/2,krep/4,freeUp/2]).
wft(A) :- !,((A = tkaf(B,C), !, wft(B), wft(C)) ; A = ttet(_) ; A = tdalet(B,C), wft(B), wft(C) ; var(A) ; A = kfail) . 

%!  qof(?Vars, ?Root) is det.
%
%   The functor for free functions

qof(A,B) :- forall(member(C,A),wft(C)), wft(B).
fromQof(qof([A|B],T),Out) :- fromQof(qof(B,T),Outs), Out = tdalet(Out,Outs).
toQof(tdalet(A,B),R) :- toQof(B,qof(C,D)), R = qof([B|C],D).  
%!  splitFree(+Term, -Qof) is det.
%!  splitFree(-Term, +Qof) is det.
%
%   Split a term into it's roots and frees

kafAsCons(tkaf(A,B),C) :- kafAsCons(B,R), C = [A|R].
kafAsCons(ttet(A),C) :- C = [A].
kafAsCons(A,C) :- C = [A].
klist([],B) :- ! , fail.
klist([A],B) :- kreadI(A,B).
klist([A|R],B) :-
    klist(R,C) ,
    kreadI(A,A1) , ! ,
    B = tkaf(A1,C).


%!  makeVar(+Input, -Output) is nondet.
%
%   Transform something into a variable

makeVar(A,B) :-
    (nonvar(A) -> B = C) ;
    B = A.
kreadI(kaf(A,B),C) :-
    kreadI(A,A0),
    kreadI(B,B0),
    !,
    C = tkaf(A0,B0).
kreadI(A,B) :-
    ((nonvar(A), A =.. C, [_,_|_] = C)
    -> (klist(C,L), B = L))
    ; (var(A) -> B = A)
    ; (B = ttet(A)).

kreadI(kfail,B) :- B = kfail.
:- (kreadI(A,B)) -> kread(A) = B.
kwriteI(ttet(A),B) :- B = A.
kwriteI(tkaf(A,B),C) :- kwriteI(A,A1), kafAsCons(B,R), C =.. [A1|R].
kwriteI(A,B) :- B = A.

%!  krep(+From, +To, +In, -Out) is det.
%
%   Replace From to To within In, returning Out
krep(From,To,In,Out) :- 
			((tkaf(In0,In1) = In) -> (krep(From,To,In0,Out0),krep(From,To,In1,Out1),Out = tkaf(Out0,Out1))) ;
			((tdalet(In0,In1) = In) -> (krep(From,To,In0,Out0),krep(From,To,In1,Out1),Out = tdalet(Out0,Out1))) ;
			((In = From) -> (Out = To)) ;
			(Out = In).

%!  freeUp(+FreeIn, +RootIn, -FreeOut, -RootOut) is nondet.
%
%   Make all vars free

freeUp(qof(FreeIn,RootIn),qof(FreeOut,RootOut)) :-
    (([FreeIn0|FreeIns] = FreeIn) -> (
					 (% Replace with var `C`
					 krep(FreeIn0,C,RootIn,RootOut0),
					 freeUp(qof(FreeIns,RootOut0),qof(FreesOut, RootOut1)),
					 FreeOut = [C|FreesOut],
					 RootOut = RootOut1)
				     )) ;
    (([] = FreeIn) -> (FreeOut = FreeIn, RootOut = RootIn)) ;
    (otherwise -> fail) .

