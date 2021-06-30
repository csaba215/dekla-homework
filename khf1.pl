:- use_module(library(lists)).


%getcol([],Col,Start,Length,Out):-!.
getcol(_,_,_,0,[]):-!.
getcol([H|T],Col,1,Length,[R|Out]):- Length1 is Length-1,nth1(Col,H,R), getcol(T,Col,1,Length1,Out).
getcol([_|T],Col,Start,Length,Out):-
    Start1 is Start-1,
    getcol(T,Col,Start1,Length,Out).

%getcell([],ColStart,ColLength,RowStart,RowLength,Out):- !.
getcell(_,_,0,_RowStart,_RowLength,[]):-!.
getcell(M,Col,ColLength,RowStart,RowLength,Ret):- 
    getcol(M,Col,RowStart,RowLength,R),ColLength1 is ColLength-1,Col1 is Col+1,
    getcell(M,Col1,ColLength1,RowStart,RowLength,Out),append(R,Out,Ret).

% :- type field == int.
% :- type board  == list(list(field)).
% :- pred cella(board::in, int::in, list(field)::out).
% cella(S, I, C): C egy olyan lista, amely az S Sudoku-mátrix
%    I-edik cellájának elemeit oszlopfolytonosan tartalmazza.
cella(S, I, C):- 
    length(S,M),
    K is integer(sqrt(M)),
    RowStart is div(I-1,K)*K+1,
    ColStart is rem(I-1,K)*K+1,
    getcell(S,ColStart,K,RowStart,K,C).
