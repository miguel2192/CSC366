%********************************************************
%FILE: crypto.pro
%LINE: This porgram generates N number of random crypto problems
%********************************************************

% cosults or loads Global Variable ADT Code
:-consult('gv1.pro').

%establishes the paramenters
establishCryptoProblemParameters :-
  declare(lo,0),
  declare(hi,15).
  :- establishCryptoProblemParameters.

%generates a random # within a range
generateRandomCryptoNumber(N) :-
  valueOf(lo,Lo),
  valueOf(hi,Hi),
  HiPlus1 is Hi + 1,
  random(Lo,HiPlus1,N).

%generates a random crypto problem & adds it to the KB
generateRandomCryptoProblem :-
  generateRandomCryptoNumber(N1),
  generateRandomCryptoNumber(N2),
  generateRandomCryptoNumber(N3),
  generateRandomCryptoNumber(N4),
  generateRandomCryptoNumber(N5),
  generateRandomCryptoNumber(G),
  addCryptoProblemToKnowledgeBase(N1,N2,N3,N4,N5,G).

%adds the crypto problem to the kwoledge base
addCryptoProblemToKnowledgeBase(N1,N2,N3,N4,N5,G) :-
  retract(problem(_,_)),
  assert(problem(numbers(N1,N2,N3,N4,N5),goal(G))).
addCryptoProblemToKnowledgeBase(N1,N2,N3,N4,N5,G) :-
  assert(problem(numbers(N1,N2,N3,N4,N5),goal(G))).

%gets the crypto problem from the KB & displays it
displayProblem :-
  problem(numbers(N1,N2,N3,N4,N5),goal(G)),
  write('Problem: '),
  write('Numbers = {'),
  write(N1),write(','),
  write(N2),write(','),
  write(N3),write(','),
  write(N4),write(','),
  write(N5),write('} Goal = '),
  write(G), nl.
 
%---------------------------------------------------
%generates N number of crypto random problems
%---------------------------------------------------
generateOne :-
  generateRandomCryptoProblem,
  displayProblem.

generate(1) :-
  generateOne.

%generates
generate(N) :-
  generateOne,
  NM1 is N - 1,
  generate(NM1).
