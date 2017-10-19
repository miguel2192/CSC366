%******************************************************
%%%%File: crypto.pro 
%program solves crypto problems of order 2,3,4,5
%******************************************************


%Permutations by 2
perm(s(A,B),p(A,B)).
perm(s(A,B),p(B,A)).

%Permutations by 3
perm(s(A,B,C),p(A,X,Y)) :- perm(s(B,C),p(X,Y)).
perm(s(A,B,C),p(B,X,Y)) :- perm(s(A,C),p(X,Y)).
perm(s(A,B,C),p(C,X,Y)) :- perm(s(A,B),p(X,Y)).

%Permutations by 4
perm(s(A,B,C,D),p(A,X,Y,Z)) :- perm(s(B,C,D),p(X,Y,Z)).
perm(s(A,B,C,D),p(B,X,Y,Z)) :- perm(s(A,C,D),p(X,Y,Z)).
perm(s(A,B,C,D),p(C,X,Y,Z)) :- perm(s(A,B,D),p(X,Y,Z)).
perm(s(A,B,C,D),p(D,X,Y,Z)) :- perm(s(A,B,C),p(X,Y,Z)).

%Permutations by 5
perm(s(A,B,C,D,E),p(A,X,Y,Z,W)) :- perm(s(B,C,D,E),p(X,Y,Z,W)).
perm(s(A,B,C,D,E),p(B,X,Y,Z,W)) :- perm(s(A,C,D,E),p(X,Y,Z,W)).
perm(s(A,B,C,D,E),p(C,X,Y,Z,W)) :- perm(s(B,A,D,E),p(X,Y,Z,W)).
perm(s(A,B,C,D,E),p(D,X,Y,Z,W)) :- perm(s(B,C,A,E),p(X,Y,Z,W)).
perm(s(A,B,C,D,E),p(E,X,Y,Z,W)) :- perm(s(B,C,D,A),p(X,Y,Z,W)).

%***********************************************
% Permutations Rules
%***********************************************

% Rules for permutations by 2's
permutations(A,B) :-
  perm(s(A,B),p(V,W)),
  write(V),write(W),write(' '),nl,
  fail.

%Rules for permutations by 3's
permutations(A,B,C) :-
  perm(s(A,B,C),p(V,W,X)),
  write(V),write(W),write(X),write(' '),nl,
  fail.

%Rules for permutations by 4's
permutations(A,B,C,D) :-
  perm(s(A,B,C,D),p(V,W,X,Y)),
  write(V),write(W),write(X),write(Y),write(' '),nl,
  fail.

%Rules for permutations by 5's
permutations(A,B,C,D,E) :-
  perm(s(A,B,C,D,E),p(V,W,X,Y,Z)),
  write(V),write(W),write(X),write(Y),write(Z),write(' '),nl,
  fail.

%**********************************************
% The combination facts
%**********************************************
%Combosets for 2's
combos(set(N1,N2,N3),combo(N1,N2),extras(N3)).
combos(set(N1,N2,N3),combo(N2,N3),extras(N1)).
combos(set(N1,N2,N3),combo(N1,N3),extras(N2)).

%Conbosets for 4's
combos(set(N1,N2,N3,N4),combo(N1,N2),extras(N3,N4)).
combos(set(N1,N2,N3,N4),combo(N1,N3),extras(N2,N4)).
combos(set(N1,N2,N3,N4),combo(N1,N4),extras(N2,N3)).
combos(set(N1,N2,N3,N4),combo(N2,N3),extras(N1,N4)).
combos(set(N1,N2,N3,N4),combo(N2,N4),extras(N1,N3)).
combos(set(N1,N2,N3,N4),combo(N3,N4),extras(N1,N2)).

%Combosets for 5's
combos(set(N1,N2,N3,N4,N5),combo(N1,N2),extras(N3,N4,N5)).
combos(set(N1,N2,N3,N4,N5),combo(N1,N3),extras(N2,N4,N5)).
combos(set(N1,N2,N3,N4,N5),combo(N1,N4),extras(N2,N3,N5)).
combos(set(N1,N2,N3,N4,N5),combo(N1,N5),extras(N2,N3,N4)).
combos(set(N1,N2,N3,N4,N5),combo(N2,N3),extras(N1,N4,N5)).
combos(set(N1,N2,N3,N4,N5),combo(N2,N4),extras(N1,N3,N5)).
combos(set(N1,N2,N3,N4,N5),combo(N2,N5),extras(N1,N3,N4)).
combos(set(N1,N2,N3,N4,N5),combo(N3,N4),extras(N1,N2,N5)).
combos(set(N1,N2,N3,N4,N5),combo(N3,N5),extras(N1,N2,N4)).
combos(set(N1,N2,N3,N4,N5),combo(N4,N5),extras(N1,N2,N3)).


% Order two crypto problem solver
crypto(N1,N2,Goal,ex(N1,+,N2)) :- Goal is ( N1 + N2 ).
crypto(N1,N2,Goal,ex(N1,*,N2)) :- Goal is ( N1 * N2 ).
crypto(N1,N2,Goal,ex(N1,-,N2)) :- Goal is ( N1 - N2 ).
crypto(N1,N2,Goal,ex(N2,-,N1)) :- Goal is ( N2 - N1 ).
crypto(N1,N2,Goal,ex(N1,/,N2)) :- N2 > 0, Goal is ( N1 / N2 ).
crypto(N1,N2,Goal,ex(N2,/,N1)) :- N1 > 0, Goal is ( N2 / N1 ).

% Order three crypto problem solver
crypto(N1,N2,N3,G,Expr) :- 
   combos(set(N1,N2,N3),combo(A,B),extras(C)),
   crypto(A,B,SG,SGE), 
   crypto(C,SG,G,UGE), 
   substitute(SGE,SG,UGE,Expr). 

% Order four crypto problem solver
crypto( N1, N2, N3, N4, G, Expr ) :- 
   combos( set(N1,N2,N3,N4), combo(A,B), extras(C,D) ),
   crypto( A, B, SG, SGE ), 
   crypto( C, D, SG, G, UGE ), 
   substitute( SGE, SG, UGE, Expr ).

% Order five crypto problem solver
crypto(N1, N2, N3, N4, N5, G, Expr ) :- 
	combos( set(N1,N2,N3,N4,N5), combo(A,B), extras(C,D,E) ),
	crypto( A, B, SG, SGE ), 
	crypto( C, D, G, AG, AUGE ), 
	crypto( E, AG, G, UGE), 
	substitute( SGE, SG, AUGE, SE), 
	substitute( SE, AG, UGE, Expr ). 

% Substitution
substitute( New, Old, ex( Old, O, Z), ex( New, O, Z ) ).
substitute( New, Old, ex( X, O, Old), ex( X, O, New ) ). 
substitute( New, Old, ex( X, O, Z), ex( Q, O, Z ) ) :- 
	substitute( New, Old, X, Q ). 
substitute( New, Old, ex( X, O, Z ), ex( X, O, Q ) ) :- 
	substitute( New, Old, Z, Q ).
