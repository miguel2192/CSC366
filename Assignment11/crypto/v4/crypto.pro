%*****************************************************************************
% FILE: crypto.pro
% TYPE: Prolog Source
% LINE: Crypto problem genereation and solution - exhaustive search
%*****************************************************************************

% Big picture, a crypto problem of the form

%   problem(numbers(N1,N2,N3,N4,N5)goal(G))

% is added to the KB, an exaustive problem solver will grab the
% problem from the KB and place a solution of the form

%   solution(Solution)

% into the KB. Auxilliary programs serve to nicely print problems
% and solutions. (The KR for a Solution is defined recursively in
% terms of expressions as)

%   ex(Operand, Operator, Operand)

% where and Operand can be a number or an expression and an
% Operator is one of the four basic arithmetic operators
% ------------------------------------------------------------------------------
% load some files
:- consult('gv1.pro').
:- consult('combosets.pro').

%-------------------------------------------------------------------------------
% V1 code
% - establish crypto cryptos problem parameters
% - internalize a crypto problem
% - generate/internalize a random crypto problem
% - display a crypto problem
% ------------------------------------------------------------------------------

%establish crypto cryptos problem parameters
establishCryptoProblem :-
  declare(lo,0),
  declare(hi,15).
  :- establishCryptoProblem.

%generates a random number within a range
generateRandomCryptoNumber(N) :-
  valueOf(lo,Lo),
  valueOf(hi,Hi),
  HiPlus1 is Hi + 1,
  random(Lo,HiPlus1,N).

% generates a random crypto problem & adds it to the KB
generateRandomCryptoProblem :-
  generateRandomCryptoNumber(N1),
  generateRandomCryptoNumber(N2),
  generateRandomCryptoNumber(N3),
  generateRandomCryptoNumber(N4),
  generateRandomCryptoNumber(N5),
  generateRandomCryptoNumber(G),
  addCryptoProblemToKnowledgeBase(N1,N2,N3,N4,N5,G).

% adds cryto problem to the Knowledge Base
addCryptoProblemToKnowledgeBase(N1,N2,N3,N4,N5,G) :-
  retract(problem(_,_)),
  assert(problem(numbers(N1,N2,N3,N4,N5),goal(G))).
addCryptoProblemToKnowledgeBase(N1,N2,N3,N4,N5,G) :-
  assert(problem(numbers(N1,N2,N3,N4,N5),goal(G))).

% displays the crypto problem that is stored in the KB
displayProblem :-
  problem(numbers(N1,N2,N3,N4,N5),goal(G)),
  write('Problem: '),
  write('Numbers = {'),
  write(N1),write(','),
  write(N2),write(','),
  write(N3),write(','),
  write(N4),write(','),
  write(N5),write('} Goal = '),
  write(G),
  nl.
%------------------------------------------------------------------------------
% v2 code and v3 code (exaustive solver)
% - exhaustively solve an order 2 crypto problem
% - exhaustively solve an order 3 crypto problem
% - exhaustively solve an order 4 crypto problem
% - exhaustively solve an order 5 crypto problem
% ------------------------------------------------------------------------------


% Order two crypto problem solver
crypto(N1,N2,Goal,ex(N1,+,N2)) :- Goal is (N1 + N2).
crypto(N1,N2,Goal,ex(N1,*,N2)) :- Goal is (N1 * N2).
crypto(N1,N2,Goal,ex(N1,-,N2)) :- Goal is (N1 - N2).
crypto(N1,N2,Goal,ex(N2,-,N1)) :- Goal is (N2 - N1).
crypto(N1,N2,Goal,ex(N1,/,N2)) :- N2 > 0, Goal is (N1 / N2).
crypto(N1,N2,Goal,ex(N2,/,N1)) :- N1 > 0, Goal is (N2 / N1).

% Order three crypto problem solver
crypto(N1,N2,N3,G,Expr) :-
  combos(set(N1,N2,N3), combo(A,B), extras(C)),
  crypto(A,B,SG,SGE),
  crypto(C,SG,G,UGE),
  substitute(SGE,SG,UGE,Expr).

% Order four crypto problem solver
crypto(N1,N2,N3,N4,G,Expr) :-
  combos(set(N1,N2,N3,N4),combo(A,B),extras(C,D)),
  crypto(A,B,SG,SGE),
  crypto(C,D,SG,G,UGE),
  substitute(SGE,SG,UGE,Expr).

% Order five crypto problem solver
crypto(N1,N2,N3,N4,N5,G,Expr) :-
  combos(set(N1,N2,N3,N4,N5),combo(A,B),extras(C,D,E)),
  crypto(A,B,SG,SGE),
  crypto(C,D,E,SG,G,UGE),
  substitute(SGE,SG,UGE,Expr).

% substitution code
substitute(New, Old, ex(Old, O, Z), ex(New, O, Z)).
substitute(New, Old, ex(X, O, Old), ex(X,O,New)).
substitute(New, Old, ex(X, O, Z), ex(Q,O,Z)) :-
  substitute(New, Old, X, Q).
substitute(New, Old, ex(X, O, Z), ex(X,O,Q)) :-
  substitute(New, Old, Z, Q).

%-------------------------------------------------------------------------------
% establish/internalize a specific crypto problem
% ------------------------------------------------------------------------------

establishSpecificCryptoProblem(N1,N2,N3,N4,N5,G) :-
  addCryptoProblemToKnowledgeBase(N1,N2,N3,N4,N5,G).

%-------------------------------------------------------------------------------
% solve an internalized problem (it may have been randomly 
% generated or specifically established) decompositionally,
% placing its solution in the KB.
% ------------------------------------------------------------------------------

solveProblemDecompositionally :-
  getProblemFromKnowledgeBase(N1,N2,N3,N4,N5,G),
  crypto(N1,N2,N3,N4,N5,G,Expression),
  addCryptoSolutionToKB(Expression).
solveProblemDecompositionally :-
  write('No solution to this one!'),nl.

getProblemFromKnowledgeBase(N1,N2,N3,N4,N5,G) :-
  problem(numbers(N1,N2,N3,N4,N5),goal(G)).

addCryptoSolutionToKB(Expression) :-
  retract(solution(_)),
  assert(solution(Expression)).
addCryptoSolutionToKB(Expression) :-
  assert(solution(Expression)).

%-------------------------------------------------------------------------------
% display the solution -- asuming that it has been solved
%-------------------------------------------------------------------------------

displaySolution :-
  write('Solution: '),
  solution(S),
  displayResult(S),
  nl.
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
  write('( '), displayResult(ex(A1,O1,B1)),write(' '),write(O),write(' '),
  displayResult(ex(A2,O2,B2)),write(' )').

%-------------------------------------------------------------------------------
% crypto problem solver -- solves a random problem
% ------------------------------------------------------------------------------

solve(random) :-
  generateRandomCryptoProblem,
  displayProblem,
  solveProblemDecompositionally,
  displaySolution.
%-------------------------------------------------------------------------------
% crypto problem solver -- solves a specific problem
% ------------------------------------------------------------------------------

solve(numbers(N1,N2,N3,N4,N5),goal(G)) :-
  establishSpecificCryptoProblem(N1,N2,N3,N4,N5,G),
  displayProblem,
  solveProblemDecompositionally,
  displaySolution.
%-------------------------------------------------------------------------------
% program to generate and solve random crypto problems.
% ------------------------------------------------------------------------------

demo(0).
demo(N) :-
  solve(random),
  K is N - 1,
  demo(K).
