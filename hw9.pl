% import CLP(FD) definitions -- strongly encouraged
:- use_module(library(clpfd)).

zip([], [], []).
zip([X|L1], [Y|L2], [[X-Y]|L3]):-zip(L1, L2, L3).

sorted([]) :- true.
sorted([_]) :- true.
sorted([N,M|L]) :- N #=< M, sorted([M|L]).

symmetric(bin(L,_,R)) :- reflect(L,R).
symmetric(tip) :- true.

reflect(tip,tip) :- true.
reflect(bin(L1,C,R1), bin(R2,C,L2)) :- reflect(L1,L2), reflect(R1,R2).

insert_tree(X,bin(L,C,R),bin(LX,C,R)) :- C #> X, insert_tree(X,L,LX).
insert_tree(X,bin(L,C,R),bin(L,C,RX)) :- C #< X, insert_tree(X,R,RX).
insert_tree(X,bin(L,X,R),bin(L,X,R)) :- true.
insert_tree(X,tip,bin(tip,X,tip)).

% %%%%%%%%%%%%%%%%%%%%%%%%
% Extra Credit (10 points)
% %%%%%%%%%%%%%%%%%%%%%%%%
%
% route(+Source, +Destination, -Route)
%
% route/3 is a relation that holds between two locations and a route between them. A route
% is a list of triples of the form Src-Type-Dst, where connect(Src, Dst, Type) holds,
% where the destination of each step is the source of the next, and no location is visited
% more than once.
%
% ?- route(manhattan, bronx, R).
% R = [manhattan-bridge-bronx] ;
% R = [manhattan-bridge-brooklyn, brooklyn-road-queens, queens-bridge-bronx] ;
% R = [manhattan-ferry-staten_island, staten_island-bridge-brooklyn, brooklyn-road-queens, queens-bridge-bronx] ;
% R = [manhattan-bridge-queens, queens-bridge-bronx] ;
% false.

route(Src,Dest,Src-X-Dest) :- connect(Src,Dest,X).

% connect/3 is a relation between two locations and a connection type. It is symmetric,
% in that connect(S,D,T) holds if and only if connect(D,S,T) holds.

connect(Src, Dst, Type) :- connect_(Src, Dst, Type); connect_(Dst, Src, Type).

% connect_/3 is the underlying relation used by connect/3. Unlike connect/3, it is not
% symmetric.
%
% Feel free to add additional locations and connections!

connect_(manhattan, bronx, bridge).
connect_(manhattan, brooklyn, bridge).
connect_(manhattan, staten_island, ferry).
connect_(manhattan, queens, bridge).
connect_(staten_island, brooklyn, bridge).
connect_(brooklyn, queens, road).
connect_(queens, bronx, bridge).

% nonmember/2 is a relation that holds between an element and a list that does not contain
% the element.

nonmember(_, []).
nonmember(X, [Y|Ys]) :- dif(X,Y), nonmember(X,Ys).