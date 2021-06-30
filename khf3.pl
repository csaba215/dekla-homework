reduce_n_dist(Dist,SpecM,SolM,Col,Row,Out):-
nth1(Row,Spec,SR),
nth1(Col,SR,Spec),
nth1(Row,SolM,SL),
nth1(Col,SL,Sol),
Col =/= 1 ->  (Row1 is Row-1,
    		nth1(Row1,SL,West),
    		reduce_n_dist_west(Dist,Spec,West,Sol,Out1) 
    	      );
    		Out1 is Sol,
Row =/= count(Dist) ->	
    
    ,reduce_n_dist_south(Dist,Spec,South,Out1,Out); Out is Out1.
    
%reduce_n_dist_south(Dist,Spec,South,Sol,Out):-
    
%reduce_n_dist_west(Dist,Spec,West,Sol,Out):-
    
    
%nagyhf
    
reduce(Sol,[],Sol):- !.
reduce(Sol, [ [H| T1]| T2], Out):- 
    T1 == [] -> (select(H,Sol,Sol1),reduce(Sol1,T2,Out));
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
