% FILE: lp.pro
% TYPE: Prolog source
% LINE: some generally useful list processing predicates
% DATE: November, 8, 2017

% writelist - writes the list to the CLI
% ----------------------------------------------------------

writelist([]).
writelist([H|T]) :- write(H), nl, writelist(T).

%member - determines if X is the member of the list
% ----------------------------------------------------------

member(X,[X|_]).
member(X,[_|Y]) :- member(X,Y).

%length - determines the length of the list
%-----------------------------------------------------------

size([],0).
size([_|T],L) :- size(T,K), L is (1 + K).

%item - prints item located at a certain pos to CLI
%-----------------------------------------------------------

item(N,[H|_],H) :- N = 0.
item(N,[_|T],E) :- N > 0, K is N-1, item(K,T,E).

%append two list togethr
%-----------------------------------------------------------

append([],L,L).
append([H|T1],L2,[H|T3]) :- append(T1,L2,T3).

append(L1,L2,L3,Result) :- 
	append(L1,L2,L12), append(L12,L3,Result).

append(L1,L2,L3,L4,Result) :-
	append(L1,L2,L3,L123),append(L123,L4,Result).

%last -gets the last element in a list
%------------------------------------------------------------

last([H|[]],H).
last([_|T],Result) :- last(T,Result).

%remove - remove an element from a list
%------------------------------------------------------------

remove(_,[],[]).
remove(First,[First|Rest],Rest).
remove(Element,[First|Rest],[First|RestLessElement]) :-
remove(Element, Rest, RestLessElement).

%replace - replaces an element
%------------------------------------------------------------
replace(0,Object,[_|T],[Object|T]).
replace(ListPosition,Object,[H|T1],[H|T2]):-
	K is ListPosition -1,
	replace(K,Object,T1,T2).

%makelist - makes a list of given # items
%------------------------------------------------------------

makelist(0,_,[]).
makelist(Length,Element,[Element|Rest]) :-
 K is Length -1,
makelist(K,Element,Rest).

%reverse - reverses a list
%-------------------------------------------------------------

reverse([],[]).
reverse([H|T],R) :-
	reverse(T,Rev),lastput(H,Rev,R).

% lastput - puts an element at the end of a list
%-----------------------------------------------------------

lastput(E,[],[E]).
lastput(E,[H|T],[H|L]) :- lastput(E,T,L).

%pick - picks an element randomly from the list
%------------------------------------------------------------

pick(L,Item) :-
  length(L,Length), 
  random(0,Length,RN),
  item(RN,L,Item).

%take -takes a random element and binds it to var
%------------------------------------------------------------

take(List,Element,Rest) :-
	pick(List,Element),
	remove(Element,List,Rest).

%iota - counts till the given number
%------------------------------------------------------------

iota(0,[]).
iota(N,IotaN) :-
	K is N -1,
	iota(K,IotaK),
	lastput(N,IotaK,IotaN).

%sum -adds all the elements in a list
%-------------------------------------------------------------

sum([],0).
sum([Head|Tail],Sum) :-
	sum(Tail,SumOfTail),
	Sum is Head + SumOfTail.

%min -finds the minimum in a list
%-------------------------------------------------------------

%%%% min[NumericList,MinimumNumber] :: find the smallest number in the list

min([Min],Min).  % the min was found

   min([H,K|T],M) :- % H is less than or equal to K so use H
   H =< K,
   min([H|T],M).

min([H,K|T],M) :-    % H is greater than K so use K
   H > K,
   min([K|T],M).
   
    
    

%max - finds the maximun in a list
%-------------------------------------------------------------------

%%%% max[NumericList,MinimumNumber] :: find the largest number in the list

max([Max],Max).  % the max was found

   max([H,K|T],M) :- % Max is greater than or equal to K so use Max
   H >= K,
   max([H|T],M).

max([H,K|T],M) :-    % Max is less than K so use K
   H < K,
   max([K|T],M).


%sort_inc - sorts a list in ascending
%----------------------------------------------------------------------------
%%%% sort_inc[UnorderedNumericList,OrderedNumericList] :: order low to high

sort_inc([H|[]],[H]). %stopping condition

sort_inc([H|T],[Mi|Pr]):-
	min([H|T],Mi),
	remove(Mi,[H|T],R),
	sort_inc(R,Pr).


%sort_dec - sorts a list in descending
%------------------------------------------------------------------------------

%%%% sort_dec[UnorderedNumericList,OrderedNumericList] :: order high to low

sort_dec([H|[]],[H]).

sort_dec([H|T],[Mx|Pr]):-
	max([H|T],Mx),
	remove(Mx,[H|T],R),
	sort_dec(R,Pr).


%alist - takes two lists and displays the position pairs
%--------------------------------------------------------------------------------

%%%% alist(FirstList, SecondList, AssociationList) :: create the association list
%%%% - create an association list (alist) from the two lists of equal length,
%%%% - the pairs of which are encapsulated into terms with the name 'pair'

alist([], [], []).
alist([H1|K1], [H2|K2], AL) :-
	alist(K1, K2, List),
	AL = [pair(H1, H2)|List].

%assoc - relations
%--------------------------------------------------------------------------------

%%%% assoc(Alist, Key, Value) :: find the value in the second slot corresponding
%%%% - to the key in the first slot of some Alist pair

assoc([pair(K,R)|_], K, R).
assoc([_|T], K, R):-
	 assoc(T, K, R).
%--------------------------------------------------------------------------------
%given a two list in a list it merges

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


