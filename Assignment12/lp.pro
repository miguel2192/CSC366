% FILE: lp.pro
% TYPE: Prolog source
% LINE: some generally useful list processing predicates
% DATE: November, 8, 2017

% writelist
% ----------------------------------------------------------

writelist([]).
writelist([H|T]) :- write(H), nl, writelist(T).

%member
% ----------------------------------------------------------

member(X,[X|_]).
member(X,[_|Y]) :- member(X,Y).

%length
%-----------------------------------------------------------

size([],0).
size([_|T],L) :- size(T,K), L is (1 + K).

%item
%-----------------------------------------------------------

item(N,[H|_],H) :- N = 0.
item(N,[_|T],E) :- N > 0, K is N-1, item(K,T,E).

%append
%-----------------------------------------------------------

append([],L,L).
append([H|T1],L2,[H|T3]) :- append(T1,L2,T3).

append(L1,L2,L3,Result) :- 
	append(L1,L2,L12), append(L12,L3,Result).

append(L1,L2,L3,L4,Result) :-
	append(L1,L2,L3,L123),append(L123,L4,Result).

%last
%------------------------------------------------------------

last([H|[]],H).
last([_|T],Result) :- last(T,Result).

%remove
%------------------------------------------------------------

remove(_,[],[]).
remove(First,[First|Rest],Rest).
remove(Element,[First|Rest],[First|RestLessElement]) :-
remove(Element, Rest, RestLessElement).

%replace
%------------------------------------------------------------
replace(0,Object,[_|T],[Object|T]).
replace(ListPosition,Object,[H|T1],[H|T2]):-
	K is ListPosition -1,
	replace(K,Object,T1,T2).

%makelist
%------------------------------------------------------------

makelist(0,_,[]).
makelist(Length,Element,[Element|Rest]) :-
 K is Length -1,
makelist(K,Element,Rest).

%reverse
%-------------------------------------------------------------

reverse([],[]).
reverse([H|T],R) :-
	reverse(T,Rev),lastput(H,Rev,R).

% lastput
%-----------------------------------------------------------

lastput(E,[],[E]).
lastput(E,[H|T],[H|L]) :- lastput(E,T,L).

%pick
%------------------------------------------------------------

pick(L,Item) :-
  length(L,Length), 
  random(0,Length,RN),
  item(RN,L,Item).

%take
%------------------------------------------------------------

take(List,Element,Rest) :-
	pick(List,Element),
	remove(Element,List,Rest).

%iota
%------------------------------------------------------------

iota(0,[]).
iota(N,IotaN) :-
	K is N -1,
	iota(K,IotaK),
	lastput(N,IotaK,IotaN).

%sum
%-------------------------------------------------------------

sum([],0).
sum([Head|Tail],Sum) :-
	sum(Tail,SumOfTail),
	Sum is Head + SumOfTail.

%min
%-------------------------------------------------------------

%%%% min[NumericList,MinimumNumber] :: find the smallest number in the list
min([Min],Min).  % the min was found

   min([H,K|T],M) :- % H is less than or equal to K so use H
   H =< K,
   min([H|T],M).

min([H,K|T],M) :-    % H is greater than K so use K
   H > K,
   min([K|T],M).
   
    
    

%max
%-------------------------------------------------------------------

%%%% max[NumericList,MinimumNumber] :: find the largest number in the list

max([Max],Max).  % the max was found

   max([H,K|T],M) :- % Max is greater than or equal to K so use Max
   H >= K,
   max([H|T],M).

max([H,K|T],M) :-    % Max is less than K so use K
   H < K,
   max([K|T],M).


%sort_inc
%----------------------------------------------------------------------------
%%%% sort_inc[UnorderedNumericList,OrderedNumericList] :: order low to high

sort_inc(L,S) :-
	swap(L,LS), !, sort_inc(LS,S).
sort_inc(S,S).

swap([X,Y|T],[Y,X|T]) :- X>Y.
swap([Z|T],[Z|TT]) :- swap(T,TT).


%sort_dec
%------------------------------------------------------------------------------

%%%% sort_dec[UnorderedNumericList,OrderedNumericList] :: order high to low

sort_dec(L,S) :-
	swap2(L,LS), !, sort_dec(LS,S).
sort_dec(S,S).

swap2([X,Y|T],[Y,X|T]) :- X<Y.
swap2([Z|T],[Z|TT]) :- swap2(T,TT).


%alist
%--------------------------------------------------------------------------------

%%%% alist(FirstList, SecondList, AssociationList) :: create the association list
%%%% - create an association list (alist) from the two lists of equal length,
%%%% - the pairs of which are encapsulated into terms with the name 'pair'

%assoc
%--------------------------------------------------------------------------------

%%%% assoc(Alist, Key, Value) :: find the value in the second slot corresponding
%%%% - to the key in the first slot of some Alist pair

% flatten
%--------------------------------------------------------------------------------

flatten([],[]).
flatten([[]|T],L) :-
   flatten(T|L).
flatten([H|T],L) :-
   atom(H),
   flatten(T,Tflattened),
   append([H],Tflattened,L). 
flatten([H|T],L) :-
   flatten(H,FlatHead),
   flatten(T,FlatTail),
   append(FlatHead,FlatTail,L).


