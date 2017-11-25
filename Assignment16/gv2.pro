%%%% FILE: gv2.pro
%%%% TYPE: Prolog source
%%%% LINE: Augmented global variable ADT
%%%% DATE: 10/10/2017

%% Essential functionality

  declare(Var,Val) :-
    retract(binding(Var,_)),
    assert(binding(Var,Val)).
  declare(Var,Val) :-
    assert(binding(Var,Val)).

  bind(Variable,Value) :-
    retract(binding(Variable,_)),
    assert(binding(Variable,Value)).

  valueOf(Variable,Value) :-
    binding(Variable,Value).

  undeclare(Var) :-
    retract(binding(Var,_)).

%% Binding display functionality

  displayBindings :-
    binding(Variable,Value),
    write(Variable),write(' -> '),write(Value),nl,
    fail.
  displayBindings.

%% Arithmetic operator functionality

  inc(Variable) :-
    retract( binding(Variable, Value)),
    NewValue is Value + 1,
    assert(binding(Variable, NewValue)).

  dec(Variable) :-
    retract(binding(Variable, Value)),
    NewValue is Value - 1,
    assert(binding(Variable, NewValue)).

  add(VariableA, VariableB, VarSum) :-
    retract(binding(VariableA, A)),
    retract(binding(VariableB, B)),
    Vs is A + B,
    assert(binding(VariableA, A)),
    assert(binding(VariableB, B)),
    assert(binding(VarSum, Vs)).

  sub(VariableA, VariableB, VarDiff) :-
    retract(binding(VariableA, A)),
    retract(binding(VariableB, B)),
    Vd is A - B,
    assert(binding(VariableA, A)),
    assert(binding(VariableB, B)),
    assert(binding(VarDiff, Vd)).

  mul(VariableA, VariableB, VarProd) :-
    retract(binding(VariableA, A)),
    retract(binding(VariableB, B)),
    Vp is A * B,
    assert(binding(VariableA, A)),
    assert(binding(VariableB, B)),
    assert(binding(VarProd, Vp)).

  div(VariableA, VariableB, VarQuo) :-
    retract(binding(VariableA, A)),
    retract(binding(VariableB, B)),
    Vq is A / B,
    assert(binding(VariableA, A)),
    assert(binding(VariableB, B)),
    assert(binding(VarQuo, Vq)).

  pow(VariableA, VariableB, VarPow) :-
    retract(binding(VariableA, A)),
    retract(binding(VariableB, B)),
    Vp is A ^ B,
    assert(binding(VariableA, A)),
    assert(binding(VariableB, B)),
    assert(binding(VarPow, Vp)).