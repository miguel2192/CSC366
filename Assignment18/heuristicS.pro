%Heuristic Crypto Problem Solver

%load some files
:- consult('gv2.pro').
:- consult('combosets.pro').

%RANDOM CRYPTO PROBLEM GENERATION
% Establish the problem parameters
establishCryptoProblemParameters :-
  declare(lo,0),
  declare(hi,9).
:- establishCryptoProblemParameters.

% Generate a random number within a range
generateRandomCryptoNumber(N) :-
  valueOf(lo,Lo),
  valueOf(hi,Hi),
  HiPlus1 is Hi + 1,
  random(Lo,HiPlus1,N).

% Generate one random crypto problem and add it to the KB
generateRandomCryptoProblem :-
  generateRandomCryptoNumber(N1),
  generateRandomCryptoNumber(N2),
  generateRandomCryptoNumber(N3),
  generateRandomCryptoNumber(N4),
  generateRandomCryptoNumber(N5),
  generateRandomCryptoNumber(G),
  addCryptoProblemToKnowledgeBase(N1,N2,N3,N4,N5,G).

addCryptoProblemToKnowledgeBase(N1,N2,N3,N4,N5,G) :-
  retract(problem(_,_)),
  assert(problem(numbers(N1,N2,N3,N4,N5),goal(G))).

addCryptoProblemToKnowledgeBase(N1,N2,N3,N4,N5,G) :-
  assert(problem(numbers(N1,N2,N3,N4,N5),goal(G))).

%---------------------------------------------------
%DISPLAY THE PROBLEM
%---------------------------------------------------
% Display crypto problem that is stored in the KB
displayProblem :-
  problem(numbers(N1,N2,N3,N4,N5),goal(G)),
  nl,
  write('Problem: '),
  write('Numbers = {'),
  write(N1), write(', '),
  write(N2), write(', '),
  write(N3), write(', '),
  write(N4), write(', '),
  write(N5), write('} Goal = '),
  write(G),
  nl.

% Establish/internalize a specific crypto problem

establishSpecificCryptoProblem(N1,N2,N3,N4,N5,G) :-
  addCryptoProblemToKnowledgeBase(N1,N2,N3,N4,N5,G).

getProblemFromKnowledgeBase(N1,N2,N3,N4,N5,G) :-
  problem(numbers(N1,N2,N3,N4,N5),goal(G)).

%------------------------------------------------------
%DISPLAY THE SOLUTION
%------------------------------------------------------
% Display the solution -- assuming that it has been solved
displaySolution :-
  %write('solution: '),
  solution(S),
  displayResult(S).
displaySolution.

displayResult(ex(A,O,B)) :-
  number(A),number(B),
  write('( '),write(A),write(' '),write(O),write(' '),write(B),write(' )').
displayResult(ex(A,O,B)) :-
  number(A), B = ex(A1,O1,B1),
  write('( '),write(A),write(' '),write(O),write(' '),
  displayResult(ex(A1,O1,B1)),write(' )').
displayResult(ex(A,O,B)) :-
  number(B), A = ex(A1,O1,B1),
  write('( '),displayResult(ex(A1,O1,B1)),write(' '),write(O),write(' '),
  write(B),write(' )').
displayResult(ex(A,O,B)) :-
  A = ex(A1,O1,B1), B = ex(A2,O2,B2),
  write('( '),displayResult(ex(A1,O1,B1)),write(' '),write(O),write(' '),
  displayResult(ex(A2,O2,B2)),write(' )').

%--------------------------------------------------
%Order two crypto problem solver
%--------------------------------------------------
crypto(N1,N2,Goal,ex(N1,+,N2)) :- Goal is (N1 + N2).
crypto(N1,N2,Goal,ex(N1,*,N2)) :- Goal is (N1 * N2).
crypto(N1,N2,Goal,ex(N1,-,N2)) :- Goal is (N1 - N2).
crypto(N1,N2,Goal,ex(N2,-,N1)) :- Goal is (N2 - N1). 
crypto(N1,N2,Goal,ex(N1,/,N2)) :- N2 > 0, Goal is (N1 / N2). 
crypto(N1,N2,Goal,ex(N2,/,N1)) :- N1 > 0, Goal is (N2 / N1).

crypto(N1,N2,N3,G,Expr) :-
combos(set(N1,N2,N3),combo(A,B),extras(C)),
crypto(A,B,SG,SGE),
crypto(C,SG,G,UGE),
substitute(SGE,SG,UGE,Expr).
%----------------------------------------------------
%KEY SUBSTITUTION CODE
substitute(New,Old,ex(Old,O,Z), ex(New,O,Z)).
substitute(New,Old,ex(X,O,Old), ex(X,O,New)).
substitute(New,Old,ex(X,O,Z), ex(Q,O,Z)):-
substitute(New,Old,X,Q).
substitute(New,Old,ex(X,O,Z), ex(X,O,Q)):- 
substitute(New,Old,Z,Q).

%-----------------------------------------------
% Solves The Problems Heuristically
%-----------------------------------------------

rule(1,situation1,action1).
rule(2,situation2,action2).
rule(3,situation3,action3).
rule(4,situation4,action4).
rule(5,situation5,action5).
rule(6,situation6,action6).
rule(7,situation7,action7).
rule(8,situation8,action8).

solveProblemHeuristically :-
 rule(Number,Situation,Action),
 write('considering rule '),write(Number),write(' ...'),nl,
 Situation,
 write('application of rule '),write(Number),write(' produces '),
 Action.
solveProblemHeuristically.

%----------------------------------------------------------------
%HEURISTIC ONE
%----------------------------------------------------------------
situation1 :-
	problem(Numbers,Goal),
	Goal = goal(0),
	Numbers = numbers(N1,N2,N3,N4,N5),
	member(0,[N1,N2,N3,N4,N5]).
action1 :-
	problem(Numbers,_),
	Numbers = numbers(N1,N2,N3,N4,N5),
	assert(solution(ex(N1,*,ex(N2,*,ex(N3,*,ex(N4,*,N5)))))).
%-----------------------------------------------------------------
%HEURISTIC TWO
%-----------------------------------------------------------------
situation2 :-
	problem(numbers(N1,N2,N3,N4,N5),goal(G)),
	member(G,[N1,N2,N3,N4,N5]),
	member(0,[N1,N2,N3,N4,N5]),
	not(G=0).
action2 :-
	problem(_,goal(G)),
	other_numbers(special(G),others(A,B,C,D)),
	assert(solution(ex(G,+,ex(A,*,ex(B,*,ex(C,*,D)))))).
%-----------------------------------------------------------------
%HEURISTIC THREE
%-----------------------------------------------------------------
situation3 :-
	problem(_,goal(0)),
	doubleton.
action3 :-
	doubleton(doubleton(A,B),rest(C,D,E)),
	assert(solution(ex(ex(A,-,B),*,ex(C,*,ex(D,*,E))))).
%-----------------------------------------------------------------
%HEURISTIC FOUR
%if all of the numbers are the same and the goal is also the same
%then add one of the numbers to the addition of the difference of
%two pairs
%-----------------------------------------------------------------
situation4 :-
        problem(numbers(N1,N2,N3,N4,N5),goal(G)),
        N1=N2,N2=N3,N3=N4,
        N5=G.
action4 :-
        problem(numbers(N1,N2,N3,N4,N5),_),
	assert(solution(ex(N5,+,ex(ex(N1,-,N2),+,ex(N3,-,N4))))).
%----------------------------------------------------------------
%HEURISTIC FIVE
%if all the numbers are the same and the goal is one then
%divide two of the numbers and add it to the difference and
%multiplication of the rest of the numbers
%----------------------------------------------------------------
situation5 :-
        problem(numbers(N1,N2,N3,N4,N5),goal(G)),
        N1=N2,N2=N3,N3=N4,N4=N5,
        1=G.
action5 :-
        problem(numbers(N1,N2,N3,N4,N5),_),
	assert(solution(ex(ex(N1,/,N2),+,ex(ex(N3,-,N4),*,N5)))).
%----------------------------------------------------------------
%HEURISTIC SIX
%If the goal is one and three of the numbers make zero and there 
%exists a pair then divide the pair and add the result of the rest
%of the numbers.
%---------------------------------------------------------------
situation6 :-
	problem(_,goal(G)),
        G=1,
	doubleton(_,rest(C,D,E)),
	crypto(C,D,E,0,_).
action6 :-
	doubleton(doubleton(A,B),rest(C,D,E)),
	crypto(C,D,E,0,ThreeMakeZero),
	assert(solution(ex(ex(A,/,B),+,ThreeMakeZero))).
%----------------------------------------------------------------
%HEURISTIC SEVEN
%If two of the numbers form a pair and the rest of the numbers
%make the goal then divide the pair and multiply it by the result
%of the other three numbers
%----------------------------------------------------------------
situation7 :-
	problem(_,goal(G)),
	doubleton(doubleton(A,B),rest(C,D,E)),
	can_make_goal_with_three_numbers(the_three(C,D,E),the_rest(A,B),the_goal(G)).
action7 :-
	problem(_,goal(G)),
	doubleton(doubleton(A,B),rest(C,D,E)),
	crypto(C,D,E,G,ThreeMakeGoal),
	assert(solution(ex(ex(A,/,B),*,ThreeMakeGoal))).
%----------------------------------------------------------------
%HEURISTIC EIGHT
%If the there exists a pair within the numbers and the goal then
%add the goal to the product of the difference of the pair
%and the addition of the rest
%----------------------------------------------------------------
situation8 :-
	problem(numbers(A,B,C,D,E),goal(G)),
	doubleton(doubleton(A,B),rest(C,D,E)),
	member(G,[C,D,E]).
action8 :-
	problem(_,goal(G)),
	doubleton(doubleton(A,B),rest(C,D,E)),
	make(C,D,E,G,X,Y),
	assert(solution(ex(G,+,ex(ex(A,-,B),*,ex(X,+,Y))))).
%----------------------------------------------------------------

%----------------------------------------------------------------
%Heuristic Support functions
%----------------------------------------------------------------
oneMore(A,B) :- A is B+1.

make(N1,N2,N3,G,X,Y) :-
	N1=G,X is N2,Y is N3.
make(N1,N2,N3,G,X,Y) :-
	N2=G,X is N1,Y is N3.
make(N1,N2,N3,G,X,Y) :-
	N3=G,X is N1,Y is N2.

member(X,[X|R],R).
member(X,[Y|R],Result) :-
 member(X,R,Subresult),
 Result = [Y|Subresult].

neighbor(A,B) :- A is B-1.
neighbor(A,B) :- A is B+1.

neighbor(G,[N|R],N,R) :-
 neighbor(N,G).
neighbor(G,[X|R],Neighbor,Rest) :-
 neighbor(G,R,Neighbor,More),
 Rest = [X|More].

neighbors([N1,N2,N3,N4],neighbors(N1,N2),others(N3,N4)) :- neighbor(N1,N2). 
neighbors([N1,N2,N3,N4],neighbors(N1,N3),others(N2,N4)) :- neighbor(N1,N3).
neighbors([N1,N2,N3,N4],neighbors(N1,N4),others(N2,N3)) :- neighbor(N2,N3).
neighbors([N1,N2,N3,N4],neighbors(N2,N3),others(N1,N4)) :- neighbor(N2,N3).
neighbors([N1,N2,N3,N4],neighbors(N2,N4),others(N1,N3)) :- neighbor(N2,N4).
neighbors([N1,N2,N3,N4],neighbors(N3,N4),others(N1,N2)) :- neighbor(N3,N4). 

doubleton :-
 problem(numbers(N1,N2,N3,N4,N5),_),
 combos(set(N1,N2,N3,N4,N5),combo(A,B),_),
 A=B.

doubleton(doubleton(A,B),rest(C,D,E)) :-
 problem(numbers(N1,N2,N3,N4,N5),_),
 combos(set(N1,N2,N3,N4,N5),combo(A,B),extras(C,D,E)),
 A=B.

can_make_goal_with_two_numbers(the_two(A,B),the_rest(C,D,E),the_goal(G)) :-
	problem(numbers(N1,N2,N3,N4,N5),goal(G)),
	combos(set(N1,N2,N3,N4,N5),combo(A,B),extras(C,D,E)),
	crypto(A,B,G,_).

can_make_goal_with_three_numbers(the_three(C,D,E),the_rest(A,B),the_goal(G)) :-
	problem(numbers(N1,N2,N3,N4,N5),_),
	combos(set(N1,N2,N3,N4,N5),combo(A,B),extras(C,D,E)),
	crypto(C,D,E,G,_).

other_numbers(special(N),others(N2,N3,N4,N5)) :-
 problem(numbers(N,N2,N3,N4,N5),goal(_)).
other_numbers(special(N),others(N1,N3,N4,N5)) :-
 problem(numbers(N1,N,N3,N4,N5),goal(_)).
other_numbers(special(N),others(N1,N2,N4,N5)) :-
 problem(numbers(N1,N2,N,N4,N5),goal(_)).
other_numbers(special(N),others(N1,N2,N3,N5)) :-
 problem(numbers(N1,N2,N3,N,N5),goal(_)).
other_numbers(special(N),others(N1,N2,N3,N4)) :-
 problem(numbers(N1,N2,N3,N4,N),goal(_)).

adjacent(A,B) :-
 crypto(A,1,B,ex(_,OP,_)),member(OP,[+,-]).

%--------------------------------------------------
%DEMO - GENERATE AND SOLVE A RANDOM CRYPTO PROBLEM.
 
solve :-
 retract(solution(_)),
 generateRandomCryptoProblem,
 displayProblem,
 solveProblemHeuristically,
 displaySolution. 

solve :-
 generateRandomCryptoProblem,
 displayProblem,
 solveProblemHeuristically,
 displaySolution.

demo(0).
demo(N) :-
 solve,nl,
 K is N-1,
 demo(K).

%-------------------------------------------------------
%ESTABLISH PARTICULAR CRYPTO PROBLEM SOLVER
 
establishCryptoProblem(numbers(N1,N2,N3,N4,N5),goal(G)):-
 addCryptoProblemToKnowledgeBase(N1,N2,N3,N4,N5,G).
%--------------------------------------------------------

%-------------------------------------------------------
%CRYPTO PROBLEM SOLVER

solve(numbers(N1,N2,N3,N4,N5),goal(G)) :-
 retract(solution(_)),
 establishCryptoProblem(numbers(N1,N2,N3,N4,N5),goal(G)),
 displayProblem,
 solveProblemHeuristically,
 displaySolution.
 
solve(numbers(N1,N2,N3,N4,N5),goal(G)) :-
 establishCryptoProblem(numbers(N1,N2,N3,N4,N5),goal(G)),
 displayProblem,
 solveProblemHeuristically,
 displaySolution.

%-------------------------------------------------------
%ITERATOR
 
repeat(0,_).
repeat(N,P) :-
 P,
 K is N-1,
 repeat(K,P).
%----------------------------------------------------------
