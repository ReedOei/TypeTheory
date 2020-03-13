rule(type, type, type).
rule(type, kind, kind).

subs(type, _, _, type).
subs(kind, _, _, kind).
subs(v(X), N, X, N).
subs(v(X), _N, Y, v(X)) :- dif(X, Y).
subs(lambda(X, T1, E), N, Y, lambda(X, NewT1, NewE)) :-
    subs(T1, N, Y, NewT1),
    subs(E, N, Y, NewE).
subs(pi_type(X, T1, T2), N, Y, pi_type(X, NewT1, NewT2)) :-
    subs(T1, N, Y, NewT1),
    subs(T2, N, Y, NewT2).

type_of(_Env, type, kind).

type_of(Env, v(X), T) :-
    member(X:T, Env),
    type_of(Env, T, _).

type_of(Env, lambda(X, T1, E), pi_type(X, T1, T2)) :-
    type_of(Env, T1, type),
    type_of([X:T1 | Env], E, T2).

type_of(Env, pi_type(_X, T1, T2), S3) :-
    type_of(Env, T1, S1),
    type_of(Env, T2, S2),
    rule(S1, S2, S3).

type_of(Env, app(F, E), Type) :-
    type_of(Env, F, pi_type(X, T1, T2)),
    type_of(Env, E, T1),
    subs(T2, E, X, Type).

