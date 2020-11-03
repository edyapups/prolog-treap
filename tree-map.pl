compLeft(A, B) :- A < B.
compRight(A, B) :- A =< B.

size(null, 0) :- !.
size(T, N) :- 
    T = node(_, _, _, _, L, R),
    size(L, N1),
    size(R, N2),
    N is 1 + N1 + N2.

merge(F, null, F) :- !.
merge(null, S, S) :- !.
merge(L, R, N) :- 
    L = node(_, LP, LK, LV, LL, LR),
    R = node(_, RP, RK, RV, RL, RR),
    (
        LP > RP,
        merge(LR, R, T),
        size(LL, S1),
        size(T, S2),
        S3 is 1 + S1 + S2,
        N = node(S3, LP, LK, LV, LL, T)    
    ;
    
        LP =< RP,
        merge(L, RL, T),
        size(RR, S1),
        size(T, S2),
        S3 is 1 + S1 + S2,
        N = node(S3, RP, RK, RV, T, RR)
    ).  

split(_, null, _, null, null) :- !.
split(OP, N, K, L, R) :- 
    N = node(_, NP, NK, NV, NL, NR),
    Do =.. [OP, K, NK],
    (
        call(Do),
        split(OP, NL, K, N1, N2),
        L = N1,
        size(N2, S1),
        size(NR, S2),
        S3 is 1 + S1 + S2,
        R = node(S3, NP, NK, NV, N2, NR)
    ;
        not(call(Do)),
        split(OP, NR, K, N1, N2),
        R = N2,
        size(NL, S1),
        size(N1, S2),
        S3 is 1 + S1 + S2,
        L = node(S3, NP, NK, NV, NL, N1)        
    ).

msplit(N, K, L, M, R) :-
    split(compRight, N, K, L, NR), split(compLeft, NR, K, M, R).

insert(N, NN, NO) :-
    NN = node(_, _, K, _, _, _),
    msplit(N, K, L, _, R),
    merge(L, NN, N1),
    merge(N1, R, NO).


map_put(N, K, V, NO) :-
    rand_int(2147483647, P),
    ND = node(1, P, K, V, null, null),
    insert(N, ND, NO).

map_get(N, K, V) :-
    N = node(_, _, NK, NV, L, R),
    (
        K < NK,
        map_get(L, K, V)
    ;
        NK = K,
        V = NV
    ;
        K > NK, 
        map_get(R, K, V)
    ).
map_remove(N, K, NO) :-
    msplit(N, K, L, _, R), merge(L, R, NO).

map_build([], null) :- !.
map_build([(KEY, VALUE) | T], NO) :-
        map_build(T, N1), map_put(N1, KEY, VALUE, NO).

get_size(null, 0) :- !.
get_size(node(S, _, _, _, _, _), S).
map_submapSize(null, _, _, 0) :- !.
map_submapSize(_, FK, TK, S) :- FK >= TK, S is 0, !.
map_submapSize(N, FK, TK, S) :-
    split(compRight, N, FK, FL, FR),
    split(compRight, FR, TK, SL, SR),
    get_size(SL, S).


example(N) :- 
    insert(null, node(1, 1, -3, yih, null, null), N1),
    map_get(N1, -3, O1),
    insert(N1, node(1, 2, -3, c, null, null), N2),
    map_get(N2, -3, N).

start :- 
    example(N).