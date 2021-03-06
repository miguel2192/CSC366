%%%% File: zoo_world_1
%%%% Line: Animal zoo composed of lions and eagles world 1

%% Facts ...

% lion(X, weight(Y), location(Z)) :: X is the type of a lion with weight Y in pounds and location Z
lion(barbary, weight(660), location(africa)).
lion(masai, weight(600), location(africa)).
lion(congo, weight(600), location(africa)).

% eagle(X, weight(Y), location(Z)) :: X is the type of a eagle with weight Y in pounds and location Z
eagle(bald, weight(14), location(america)).
eagle(bateleur, weight(6), location(africa)).
eagle(harpy, weight(20), location(mexico)).
eagle(booted, weight(2), location(asia)).

%% Rules ...

% lions :: all those items listed are lions
lions :- lion(Type,_,_), write(Type),nl,fail.
lions.

%eagles :: all those items listed are eagles
eagles :- eagle(Type,_,_),write(Type),nl,fail.
eagles.

%animal :: all those items listed are shapes
animal :- lions,eagles.

% africa(Type) :: Type is a animal in africa
africa(Type) :- eagle(Type,_,location(africa)).
africa(Type) :- lion(Type,_,location(africa)).

% large(Type) :: Type is a large lion in feet
large(Type) :- height(Type,S),S >= 11.

% small(Type) :: Type is a small eagle in inches
small(Type) :- height(Type,S),S < 40.

% height(Type, H) :: H is the height of animal with type Type
height(Type,H) :- lion(Type, weight(Y), _),H is 1 * Y * Y.
height(Type,H) :- eagle(Type, weight(Y), _),H is Y * Y.
