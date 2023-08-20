/* cribbage.pl - contains the predicates hand_value/3 and select_hand/3 for the card game cribbage
 *
 * Author: Tristan Thomas (1269492)
 * Date: 19-08-2023
 */

% custom maplist
maplist(_, [], []).
maplist(P, [X|Xs], [Y|Ys]) :-
    call(P, X, Y),
    maplist(P, Xs, Ys).

% convert rank to numeric value for sorting

rank_to_int(card(ace, _), _, 1).

rank_to_int(card(jack, _), Type, Value) :-
    ( Type == nosum ->
        Value = 11 ;
        Value = 10 ).
rank_to_int(card(queen, _), Type, Value) :-
    ( Type == nosum ->
        Value = 12 ;
        Value = 10 ).
rank_to_int(card(king, _), Type, Value) :-
    ( Type == nosum ->
        Value = 13 ;
        Value = 10 ).

rank_to_int(card(Rank, _), _, Value) :-
    integer(Rank),
    Value is Rank.

% generate list of sortable cards by rank
generate_list(Cards, Type, Res) :-
    generate_list(Cards, Type, [], Res).

generate_list([], _, Acc, Acc).

generate_list([C|Cs], Type, Acc, Res) :-
    rank_to_int(C, Type, Value),
    append([Value], Acc, NewAcc),
    generate_list(Cs, Type, NewAcc, Res).

% Counts the frequency of Elt and returns the rest of list
count_frequency(Elt, List, Rest, Freq) :-
    count_frequency(Elt, List, 0, Rest, Freq).

count_frequency(_, [], Acc, [], Acc).

count_frequency(Elt, [Elt|Rest], Acc, Excess, Freq) :-
    Acc1 is Acc + 1,
    count_frequency(Elt, Rest, Acc1, Excess, Freq).

count_frequency(_, [X|Rest], Acc, [X|Rest], Acc).

% generates frequencies of values in list
list_to_freq([], []).
list_to_freq([X|Xs], [X-Freq|Pairs]) :-
    count_frequency(X, [X|Xs], NewXs, Freq),
    list_to_freq(NewXs, Pairs).

% calculates total points from pairs
choose_2(1, 0).
choose_2(N, Res) :-
    Res is N * (N - 1) // 2.

calculate_pairs(List, Points) :-
    pairs_values(List, Values),
    maplist(choose_2, Values, Res),
    sum_list(Res, Sum),
    Points is Sum * 2.

% calculate total points from runs
consecutive_run([], [], []).
consecutive_run([X-Fx], [X-Fx], []).
consecutive_run([X-Fx, Y-Fy | Rest], [X-Fx | Run], Remaining) :-
    X + 1 =:= Y,
    consecutive_run([Y-Fy | Rest], Run, Remaining).
consecutive_run([X-Fx, Y-Fy | Rest], [X-Fx], [Y-Fy | Rest]) :-
    X + 1 =\= Y.

generate_runs([], []).
generate_runs([X-Fx,Y-Fy], []).
generate_runs([X-Fx,Y-Fy|Rest], Runs) :-
    % check for potential runs
    X + 1 =:= Y,
    consecutive_run([X-Fx,Y-Fy|Rest], Run, Excess),
    length(Run, Len),
    Len >= 3,
    generate_runs(Excess, NextRuns),
    Runs = [Run-Len|NextRuns].
% skip through to next value if above clause failed
generate_runs([_ | Rest], Runs) :-
    generate_runs(Rest, Runs).
    
% multiply elements of list
prod_list(L, Prod) :- prod_list(L, 1, Prod).

prod_list([], A, Prod) :-
    Prod = A.
prod_list([N|Ns], A, Prod) :-
    A1 is A * N,
    prod_list(Ns, A1, Prod).

points_list(Run-Len, Points) :-
    pairs_values(Run, Values),
    prod_list(Values, ScaleFactor),
    Points is ScaleFactor * Len.

calculate_runs(List, Points) :-
    generate_runs(List, Runs),
    maplist(points_list, Runs, PointsPerRun),
    sum_list(PointsPerRun, Points).

% hand value predicate
hand_value(Hand, Startcard, Value) :-
    % pairs and runs scoring
    All = [Startcard|Hand],
    generate_list(All, nosum, List),
    msort(List, Sorted),
    list_to_freq(Sorted, Freqs),
    calculate_pairs(Freqs, PairsPoints),
    calculate_runs(Freqs, RunsPoints),
    Value is PairsPoints + RunsPoints.


