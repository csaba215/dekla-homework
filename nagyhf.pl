%nagyhf
:- use_module(library(lists)).

solve(_,_Spec,SolMatrix,M,M,M,_K,O):- maplist(flatten,SolMatrix,O),!.
solve(Dist,Spec,SolMatrix,Row,Col,M,K,Out):-
Col < M ->  (   Col1 is Col+1,
    getsol(Dist,Spec,SolMatrix,Row,Col1,M,K,O),
	 set(SolMatrix,[O],Row,Col1,Out1),
    solve(Dist,Spec,Out1,Row,Col1,M,K,Out));
    (   Row1 is Row+1,
    getsol(Dist,Spec,SolMatrix,Row1,1,M,K,O),
	 set(SolMatrix,[O],Row1,1,Out1),
    solve(Dist,Spec,Out1,Row1,1,M,K,Out)).

sudoku(s(Dist,Spec),Out):-
    length(Spec,M),
    K is integer(sqrt(M)),
	fillbyspec(Spec,M,SolMatrix),
    solve(Dist,Spec,SolMatrix,1,0,M,K,Out).
    
%Possible solution list, used up solulions, output
%reduce([],_,[]):- !.
reduce([],[],_):- !, fail.
reduce(Sol,[],Sol):- !.
reduce(Sol, [ [H| T1]| T2], Out):- 
    T1 == [] -> 
  ( 
   findall(X, (member(X,Sol), X =\= H), Sol1),
   reduce(Sol1,T2,Out) 
  );
  reduce(Sol,T2,Out).

%getcol([],Col,Start,Length,Skip,Out):-!.
getcol(_,_,_,0,_,[]):-!.
getcol([_|T],Col,1,Length,1,Out):- 
    Length1 is Length-1,
    getcol(T,Col,1,Length1,0,Out),!.
getcol([H|T],Col,1,Length,Skip,[R|Out]):- 
    Length1 is Length-1,
    Skip1 is Skip-1,
    nth1(Col,H,R),
    getcol(T,Col,1,Length1,Skip1,Out).
getcol([_|T],Col,Start,Length,Skip,Out):-
    Start1 is Start-1,
    Skip1 is Skip-1,
    getcol(T,Col,Start1,Length,Skip1,Out).

%getcol([],Col,Start,Length,Out):-!.
getcol(_,_,_,0,[]):-!.
getcol([H|T],Col,1,Length,[R|Out]):- Length1 is Length-1,nth1(Col,H,R), getcol(T,Col,1,Length1,Out).
getcol([_|T],Col,Start,Length,Out):-
    Start1 is Start-1,
    getcol(T,Col,Start1,Length,Out).

%getcol([],Col,Skip,Out):-    
getcol([],_,_,[]):- !.
getcol([_|T],Col,1,Out):- getcol(T,Col,0,Out),!.
getcol([H|T],Col,Skip,[R|Out]):- nth1(Col,H,R),Skip1 is Skip-1, getcol(T,Col,Skip1,Out).

%getcell()

%getcell([],ColStart,ColLength,RowStart,RowLength,SkipRow,SkipCol,Out):- !.
getcell(_,_,0,_RowStart,_RowLength,_SkipRow,_SkipCol,[]):-!.
getcell(M,Col,ColLength,RowStart,RowLength, SkipRow,Col,Ret):- 
    getcol(M,Col,RowStart,RowLength,SkipRow,R),ColLength1 is ColLength-1,Col1 is Col+1,
    getcell(M,Col1,ColLength1,RowStart,RowLength,SkipRow,Col,Out),append(R,Out,Ret),!.
getcell(M,Col,ColLength,RowStart,RowLength,SkipRow,SkipCol,Ret):- 
    getcol(M,Col,RowStart,RowLength,R),ColLength1 is ColLength-1,Col1 is Col+1,
    getcell(M,Col1,ColLength1,RowStart,RowLength,SkipRow,SkipCol,Out),append(R,Out,Ret).

cella(S, SkipRow, SkipCol, K, C):- 
    I is div(SkipRow-1,K)*K+div(SkipCol-1,K)+1,
    RowStart is div(I-1,K)*K+1,
    ColStart is rem(I-1,K)*K+1,
    getcell(S,ColStart,K,RowStart,K,SkipRow,SkipCol,C).

getsol(Dist,Spec,SolMatrix,Row,Col,M,K,Out):-
    nth1(Row,SolMatrix,R1),nth1(Col,R1,PartialSol,R2),
    reduce(PartialSol,R2,PartialSol1),
    getcol(SolMatrix,Col,Row,C), reduce(PartialSol1,C,PartialSol2),
    cella(SolMatrix,Row,Col,K,Cell),reduce(PartialSol2,Cell,PartialSol3),
    reduce_dist(Dist,M,Spec,SolMatrix,PartialSol3,Row,Col,PartialSol4),
    member(Out,PartialSol4).


fillbyspec([H|T],M,[O|Out]):- fillbyspec_row(H,M,O), fillbyspec(T,M,Out).
fillbyspec([],_,[]):- !.


fillbyspec_row([H|T],M,[Out1|Out]):- contains_number(H,N), (   N == 0 -> numlist(1,M,Out1) ; numlist(N,N,Out1)), fillbyspec_row(T,M,Out).
fillbyspec_row([],_,[]):- !.

%
contains_number([],0):- !.
contains_number([H|T],N):- number(H) ->  N is H ; contains_number(T,N).


set(Table,Elem,Row,Col,Out):- 
nth1(Row,Table,Row1,Others),
nth1(Col,Row1,_,Others2),  
nth1(Col,Row2,Elem,Others2),  
nth1(Row,Out,Row2,Others).
%nth1(Row, Table, Row1),
%replace(Row1,Col,Elem,O),
%replace(Table,Row,O,Out).

replace([_|T], 1, X, [X|T]):- !.
replace([H|T], I, X, [H|R]):- I > -1, I2 is I-1, replace(T, I2, X, R), !.
replace(L, _, _, L).

%reduce_dist(Dist,M,PartialSol,Spec,Row,Col,Out):-
reduce_dist(Dist,M,SpecT,PartialSol,C,Row,Col,Out):-
%    nth1(Row,PartialSol,R),nth1(Col,R,C),
	nth1(Row,SpecT,RS),nth1(Col,RS,Spec),
    reduce_north(M,Dist,Row,Col,C,SpecT,PartialSol,O1),
	reduce_east(M,Dist,Row,Col,O1,SpecT,PartialSol,O2),
    reduce_south(M,Dist,Row,Col,O2,Spec,PartialSol,O3),
    reduce_west(M,Dist,Row,Col,O3,Spec,PartialSol,Out).
    
reduce_north(_M,_,1,_,C,_,_,C):- !.
reduce_north(_M,Dist,Row,Col,C,SpecT,PartialSol,O):-
Row1 is Row-1,
nth1(Row1,PartialSol,R),nth1(Col,R,North),nth1(Row1,SpecT,RS),nth1(Col,RS,NorthSpec),reduce_dist_s(Dist,C,NorthSpec,North,O).

reduce_east(M,_,_,M,C,_,_,C):- !.
reduce_east(_M,Dist,Row,Col,C,SpecT,PartialSol,O):-
Col1 is Col+1,
nth1(Row,PartialSol,R),nth1(Col1,R,East),nth1(Row,SpecT,RS),nth1(Col1,RS,EastSpec),reduce_dist_w(Dist,C,EastSpec,East,O).

reduce_south(M,_,M,_,C,_,_,C):- !.
reduce_south(_M,Dist,Row,Col,C,Spec,PartialSol,O):-
Row1 is Row+1,
nth1(Row1,PartialSol,R),
nth1(Col,R,South),
reduce_dist_s(Dist,C,Spec,South,O).

reduce_west(_,_,_,1,C,_,_,C):- !.
reduce_west(_M,Dist,Row,Col,C,Spec,PartialSol,O):-
Col1 is Col-1,
nth1(Row,PartialSol,R),nth1(Col1,R,West),reduce_dist_w(Dist,C,Spec,West,O).

reduce_dist_s(Dist,Cell,Spec,South,Out):-
member('s',Spec) -> reduce_dist_poz(Dist,Cell,South,Out);
reduce_dist_neg(Dist,Cell,South,Out).

reduce_dist_w(Dist,Cell,Spec,West,Out):-
member('w',Spec) -> reduce_dist_poz(Dist,Cell,West,Out);
reduce_dist_neg(Dist,Cell,West,Out).

reduce_dist_poz(Dist,C1,C2,Out):-
findall(O,(member(CC1,C1),member(CC2,C2),
              dist_poz_test(Dist,CC1,CC2,O),member(O,C1)),Out1),(   length(Out1) == 0 ->  fail;
    sort(Out1,Out)).

reduce_dist_neg(Dist,C1,C2,Out):-
findall(O,(member(CC1,C1),member(CC2,C2),
              dist_neg_test(Dist,CC1,CC2,O),member(O,C1)),Out1),(   length(Out1) == 0 ->  fail;sort(Out1,Out)).

dist_poz_test(Dist,C1,C2,Out):-
C3 is abs(C1-C2),C3 == Dist -> Out is C1; fail.

dist_neg_test(Dist,C1,C2,Out):-
C3 is abs(C1-C2),C3 == Dist ->  fail; Out is C1.

%swi prolog functions

numlist(L, U, Ns) :-
    L =< U,
    numlist_(L, U, Ns).

numlist_(U, U, List) :-
    !,
    List = [U].
numlist_(L, U, [L|Ns]) :-
    L2 is L+1,
    numlist_(L2, U, Ns).

flatten(List, FlatList) :-
    flatten(List, [], FlatList0),
    !,
    FlatList = FlatList0.

flatten(Var, Tl, [Var|Tl]) :-
    var(Var),
    !.
flatten([], Tl, Tl) :- !.
flatten([Hd|Tl], Tail, List) :-
    !,
    flatten(Hd, FlatHeadTail, List),
    flatten(Tl, Tail, FlatHeadTail).
flatten(NonList, Tl, [NonList|Tl]).

