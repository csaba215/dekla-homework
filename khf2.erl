-module(khf2).
-author('csaba215@gmail.com').
-vsn('2020-10-12').
-export([n_away/2]).
%-compile(export_all).

-type sspec() :: {dist(), board()}.
-type dist()  :: integer().
-type field() :: [info()].
-type info()  :: s | w | integer().
-type board() :: [[field()]].
-type ssol()  :: [[integer()]].

-type ssolrow()  :: [integer()].
-type boardrow() :: [field()].

-spec khf2:n_away(SSpec::sspec(), SSol::ssol()) -> B::boolean().
%% B igaz, ha az SSol részlegesen kitöltött értékmátrix teljesíti az SSpec
%% Sudoku-feladványban szereplő távolságinfók által támasztott feltételeket.
n_away({N, Spec}, Sol) -> n_away_split(Spec,Sol,N,true).


-spec n_away_split(Spec::board(), Sol::ssol(), N::dist(), Sol::boolean()) -> B::boolean().
n_away_split(_,_,_,false) -> false;
n_away_split([SpecH|SpecT], [SolH1|[SolH2|SolT]], N, _) ->  Sol = n_away_test_row(SpecH, SolH1, SolH2, 0, N, true),  n_away_split(SpecT, [SolH2|SolT], N, Sol);
n_away_split([SpecH], [SolH1], N, _) ->  n_away_test_row(SpecH, SolH1, [], 0, N, true).
%n_away_split(_,_,_,_) -> true.

-spec n_away_test_row(SpecRow::boardrow(), SolRow1::ssolrow(), SolRow2::ssolrow(), W::integer(), N::dist(), Ret::boolean()) -> B::boolean().
n_away_test_row(_SpecRow, _SolRow1, _SolRow2, _W, _N, false) -> false;
n_away_test_row([SpecH|SpecT], [S1H|S1T], [S2H|S2T], W, N, _Ret) -> R = n_away_test(S2H,W,S1H,SpecH,N), n_away_test_row(SpecT, S1T, S2T, S1H,N,R);
n_away_test_row([SpecH|SpecT], [S1H|S1T], [], W, N, _Ret) -> R = n_away_test(0,W,S1H,SpecH,N), n_away_test_row(SpecT, S1T, [], S1H,N,R);
n_away_test_row(_,_,_,_,_,_) -> true.

%-spec n_away_test(SouthSol::integer(), WestSol::integer(), Sol::integer(), Spec::info(), N::dist()) -> B::boolean(). 
%n_away_test(_, _,0,_,_) -> true;
%n_away_test(_,_,_,[],_) -> true;
%n_away_test(SouthSol, _WestSol, Sol, [s|_T],N) when SouthSol =/= 0, abs(SouthSol-Sol) =/= N -> false;
%n_away_test(_SouthSol, WestSol, Sol, [w|_T],N) when WestSol =/= 0, abs(WestSol-Sol) =/= N -> false;
%n_away_test(SouthSol, WestSol, Sol, [_|T],N) -> n_away_test(SouthSol, WestSol, Sol, T, N).

-spec n_away_test(SouthSol::integer(), WestSol::integer(), Sol::integer(), Spec::info(), N::dist()) -> B::boolean().
n_away_test(SouthSol, WestSol, Sol, Spec,N) -> (Sol == 0) or (n_away_test_w(SouthSol, WestSol, Sol, Spec,N) and n_away_test_s(SouthSol, WestSol, Sol, Spec,N)).

% nincs benne s vagy w
-spec n_away_test_s(SouthSol::integer(), WestSol::integer(), Sol::integer(), Spec::info(), N::dist()) -> B::boolean().
n_away_test_s(SouthSol, _WestSol, Sol, Spec,N) -> Test = lists:member(s,Spec),
if
Test ->
   not ( (SouthSol =/= 0) and (abs(SouthSol-Sol) =/= N) );
true ->
   not ( (SouthSol =/= 0) and (abs(SouthSol-Sol) == N) )
end.

-spec n_away_test_w(SouthSol::integer(), WestSol::integer(), Sol::integer(), Spec::info(), N::dist()) -> B::boolean().
n_away_test_w(_SouthSol, WestSol, Sol, Spec,N) -> Test = lists:member(w,Spec),
if
Test ->
  not ( (WestSol =/= 0) and (abs(WestSol-Sol) =/= N) );
true ->
  not ( (WestSol =/= 0) and (abs(WestSol-Sol) == N) )
end.
