grid_build_helper(_,[]).
grid_build_helper(N,[H|T]):-
length(H,N),
grid_build_helper(N,T).


grid_build(N,M):-
length(M,N),
grid_build_helper(N,M).

less_than(_,[]).

less_than(N,[H|T]):-
H=<N,
less_than(N,T).

random(F,N,X):-num_gen(F,N,R),R=[H|T],F1 is F+1,
(X=H;random(F1,N,X)).

grid_gen(N,M):-
grid_build(N,M),
grid_genH(N,M).

grid_genH(_,[]).
grid_genH(N,[[]|T]):-grid_genH(N,T).
grid_genH(N,[[H|T]|T1]):-random(1,N,H),grid_genH(N,[T|T1]).

num_gen(L,L,[L]).
num_gen(F,L,[F|T]):-
F<L,
F1 is F+1,
num_gen(F1,L,T).


check_num_grid(G):-
largest(G,M),
length(G,L),
M=<L,
num_gen(1,M,R),
include(R,G).

largest([],0).
largest([H|T],L):-max_list(H,X),largest(T,L),X=<L.
largest([H|T],L):-max_list(H,L),largest(T,X),X<L.


include([],_).
include([H|T],G):-
includeH(H,G),include(T,G).

includeH(X,[H|_]):-includehelper(X,H).
includeH(X,[H|T]):- \+includehelper(X,H),includeH(X,T).

includehelper(X,[X|_]).
includehelper(X,[H|T]):-X\=H,includehelper(X,T).


diff_pos([],[]).
diff_pos([H1|T1],[H2|T2]):-
H1\=H2,
diff_pos(T1,T2).


acceptable_permutation([H|T],R):-
permutation([H|T],R),
diff_pos([H|T],R).


trans([[H|T] |Tail], [[H|NT] |NTail]) :- 
	fC(Tail, NT, Rest), trans(Rest, NRest), fC(NTail, T, NRest).
trans([], []).
fC([[H|T] |Tail], [H|Col], [T|Rows]) :- fC(Tail, Col, Rows).
fC([], [], []).


acceptable_distribution(G):-
trans(G,S),
acceptable_distributionH(G,S).


acceptable_distributionH([H1|T],[H2|T2]):-
\+same_list(H1,H2),
\+same_list(T1,T2).

row_col_match(G):-
acceptable_distribution(G),
trans(G,S),
permutation(G,S).

row_col_matchH([],[]).
row_col_matchH([H1|T1],[H2|T2]):-
same_list(H1,H2),
row_col_matchH(T1,T2).

same_list([],[]).
same_list([H1|T1],[H2|T2]):-
H1==H2,
same_list(T1,T2).


diff_list(L,S):-
\+same_list(L,S).


distinct_rows([]).
distinct_rows([H|T]):-distinct_rowsH(H,T),distinct_rows(T).
distinct_rowsH(_,[]).
distinct_rowsH(H,[H1|T]):-H\=H1,distinct_rowsH(H,T).

distinct_columns(G):-trans(G,C),distinct_rows(C).

helsinki(N,G):-
grid_build(N,G),
grid_gen(N,G),
check_num_grid(G),
acceptable_distribution(G),
distinct_rows(G),
distinct_columns(G),
row_col_match(G).