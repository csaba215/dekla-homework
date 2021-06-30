-module(khf1).
-author('csaba215@gmail.com').
-vsn('2020-10-02').
-export([cella/2]).
%-compile(export_all).

-type field() :: integer().
-type board() :: [[field()]].

-spec getcolumn(S::board(), RowStart::integer(), Length::integer(), Col::integer(), Out::[field()]) -> [field()].
getcolumn(_,1,0,_,Out) -> Out;
getcolumn([H|T],1,Length,Col,Out) -> getcolumn(T,1,Length-1,Col,[lists:nth(Col,H)|Out]);
getcolumn([_|T],RStart,Length,Col,Out) -> getcolumn(T,RStart-1,Length,Col,Out).

-spec getcolumns(S::board(), RowStart::integer(), RowLength::integer(), ColStart::integer(), ColLength::integer() , Out::[field()]) -> [field()].
getcolumns(_S, _RowStart, _RowLength, _ColStart, 0, Out) -> Out;
getcolumns(S, RowStart, RowLength, ColStart, ColLength, Out) -> Temp = getcolumn(S, RowStart, RowLength, ColStart, Out), getcolumns(S, RowStart, RowLength, ColStart+1, ColLength-1, Temp).

-spec khf1:cella(S::board(), I::integer()) -> C::[field()].
%% C egy olyan lista, amely az S Sudoku-mátrix I-edik
%% cellájának elemeit oszlopfolytonosan tartalmazza.
cella([H|T], I) -> K=trunc(math:sqrt(length(H))), lists:reverse(getcolumns([H|T], ((I-1) div K)*K+1, K, ((I-1) rem K)*K+1 ,K,[])).
