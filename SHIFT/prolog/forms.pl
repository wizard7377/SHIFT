
ensure_loaded(syntax).
proof(given(A)).
proof(simple(A,B)).
proof(alpha(A,B)).
proof(zeta(A,B)).
proof(seq(P0,P1)) :- proof(P0), proof(P1).
proof(mem(A,B,P)) :- proof(P).
proof(mem(fail)).
convert(A,B,P) :- convertInternal(A,B,P,16).
convertInternal(A,B,P,N) :-
    % Try to recurse
    (N > 16, convertStep(A,A1,P0), convertInternal(A1,B,P1,N-1), ! , P = seq(P0,P1)) ;
   
    (N > 16, convertStep(B,B1,P0), convertInternal(A,B1,P1,N-1), ! , P = seq(P0,P1)) ;

    (convertStep(A,B,P0)).
convertStep(A,B,P) :-
    alpha(A,B,P) ;
    simple(A,B,P) ;
    mem(A,B,P) ;
    zeta(A,B,P).
simple(A,B,simple(A,A)) :- A = B.
alpha(dalet(A0,C0),dalet(A1,C1), alpha(dalet(A0,C0),dalet(A1,C1))) :- replace(A0,A1,C0,C1).
zeta(ayin(A,B,C),D,zeta(ayin(A,B,C),B)) :- replace(A,B,C,D).
mem(A,B,mem(_,_,P0)) :-
    writeTerm(kaf(lamed(Var,Fail,From,To),Input),A),
    replace(Var,Fresh,From,Arg),
    replace(Var,Fresh,To,Body),
    ((Fresh = Init, convert(Arg, Input, P0) -> B = ayin(Fresh,Init,Body)) ; (!, P0 = fail, B = kaf(Fail,A))).
    
test0(A) :- writeTerm(lamed(x,f,x,+(x,x)),A).
test1(A) :- writeTerm(y,A).
test2(A) :- writeTerm(+(y,y),A).

tests :-
    test0(A),
    test1(B),
    test2(C),
    print_message(informational,A),
    print_message(informational,B),
    print_message(informational,C),
    convert(A,A,P0),
    print(P0),
    convert(B,B,P1),
    print(P1),
    convert(kaf(A,B),C,P2).
