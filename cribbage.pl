/* cribbage.pl - contains the predicates hand_value/3 and select_hand/3 for the card game cribbage
 *
 * Author: Tristan Thomas (1269492)
 * Date: 19-08-2023
 */

% terms
rank(ace).
rank(2).
rank(3).
rank(4).
rank(5).
rank(6).
rank(7).
rank(8).
rank(9).
rank(10).
rank(jack).
rank(queen).
rank(king).
dummy(1).

suit(diamonds).
suit(clubs).
suit(hearts).
suit(spades).

% convert rank to numeric value for sorting

card_to_rank(card(ace, _), _, 1).

card_to_rank(card(jack, _), Type, Value) :-
    ( Type == nosum ->
        Value = 11 ;
        Value = 10 ).
card_to_rank(card(queen, _), Type, Value) :-
    ( Type == nosum ->
        Value = 12 ;
        Value = 10 ).
card_to_rank(card(king, _), Type, Value) :-
    ( Type == nosum ->
        Value = 13 ;
        Value = 10 ).

card_to_rank(card(Rank, _), _, Value) :-
    integer(Rank),
    Value is Rank.

card_to_suit(card(_, Suit), _, Value) :-
    Value = Suit.

% generate list of sortable cards by rank
generate_list(Cards, Type, Conversion, Res) :-
    generate_list(Cards, Type, Conversion, [], Res).

generate_list([], _, _, Acc, Acc).

generate_list([C|Cs], Type, Conversion, Acc, Res) :-
    call(Conversion, C, Type, Value),
    append([Value], Acc, NewAcc),
    generate_list(Cs, Type, Conversion, NewAcc, Res).

% Counts the frequency of Elt and returns the rest of list
count_frequency(Elt, List, Rest, Freq) :-
    count_frequency(Elt, List, 0, Rest, Freq).

count_frequency(_, [], Acc, [], Acc).

count_frequency(Elt, [Elt|Rest], Acc, Excess, Freq) :-
    Acc1 is Acc + 1,
    count_frequency(Elt, Rest, Acc1, Excess, Freq), !.

count_frequency(Elt, [X|Rest], Acc, [X|Rest], Acc) :-
    Elt \= X.

% generates frequencies of values in list
list_to_freq([], []).
list_to_freq([X|Xs], [X-Freq|Pairs]) :-
    count_frequency(X, [X|Xs], NewXs, Freq),
    list_to_freq(NewXs, Pairs).

% calculates total points from pairs
choose_2(1, 0).
choose_2(N, Res) :-
    N > 1,
    Res is N * (N - 1) // 2.

calculate_pairs(List, Points) :-
    pairs_values(List, Values),
    map_list(choose_2, Values, Res),
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

generate_runs([], []) :- !.
generate_runs([_,_], []) :- !.
generate_runs([X-Fx,Y-Fy|Rest], Runs) :-
    % check for potential runs
    X + 1 =:= Y,
    consecutive_run([X-Fx,Y-Fy|Rest], Run, Excess),
    length(Run, Len),
    Len >= 3,
    !,
    generate_runs(Excess, NextRuns),
    Runs = [Run-Len|NextRuns].
% skip through to next value if above clause failed
generate_runs([_|Rest], Runs) :-
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
    map_list(points_list, Runs, PointsPerRun),
    sum_list(PointsPerRun, Points).

% calculate total points for 15s

% Calculate the score for a list of cards
calculate_score([], 0).
calculate_score([Card | Rest], Score) :-
    calculate_score(Rest, RestScore),
    Score is RestScore + Card.

% Check if a combination of cards adds up to 15
adds_up_to_15(Cards) :-
    calculate_score(Cards, TotalScore),
    TotalScore =:= 15.

% Find all distinct combinations of cards that add up to 15
is_15(Number, Res) :-
    (Number =:= 15 -> Res = 1 ; Res = 0).

combination(_, 0, []).
combination([X|Xs], K, [X|Combination]) :-
    K > 0,
    K1 is K - 1,
    combination(Xs, K1, Combination).
combination([_|Xs], K, Combination) :-
    K > 0,
    combination(Xs, K, Combination).

combinations(_, [], []).
combinations(List, [N|Ns], Combs) :-
    findall(Comb, combination(List, N, Comb), NewCombs),
    combinations(List, Ns, RestCombs),
    append(NewCombs, RestCombs, Combs).

calculate_15s(List, SumPoints) :-
    numlist(2, 5, Nums),
    combinations(List, Nums, Res),
    map_list(sum_list, Res, Sums),
    map_list(is_15, Sums, Is15s),
    sum_list(Is15s, Successes),
    SumPoints is Successes * 2.


% calculate flush points
calculate_flush(Suits, StartSuit, Points) :-
    list_to_freq(Suits, Freqs),
    length(Freqs, 1),
    Freqs = [Suit-_|_],
    ( Suit = StartSuit -> 
        Points = 5 ;
        Points = 4 ), !.

calculate_flush(_, _, 0).


% calculate nob point
calculate_nob(Hand, card(_, Suit), Points) :-
    member(card(jack, Suit), Hand),
    Points = 1,
    !.
calculate_nob(_, _, 0).

% custom maplist
map_list(_, [], []).
map_list(P, [X|Xs], [Y|Ys]) :-
    call(P, X, Y),
    map_list(P, Xs, Ys).

% hand value predicate
hand_value(Hand, Startcard, Value) :-
    % pairs and runs scoring
    All = [Startcard|Hand],
    generate_list(All, nosum, card_to_rank, List1),
    msort(List1, Sorted1),
    list_to_freq(Sorted1, Freqs1),
    calculate_pairs(Freqs1, PairsPoints),
    calculate_runs(Freqs1, RunPoints),
    % dummy(1),
    % 15s scoring
    generate_list(All, sum, card_to_rank, List2),
    calculate_15s(List2, SumPoints),
    % % flush scoring
    generate_list(Hand, _, card_to_suit, List3),
    card_to_suit(Startcard, _, StartSuit),
    calculate_flush(List3, StartSuit, FlushPoints),
    % nob scoring
    calculate_nob(Hand, Startcard, NobPoint),
    Value is PairsPoints + RunPoints + SumPoints + FlushPoints + NobPoint.
    % Value is PairsPoints.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% select hand predicate
select_hand(Cards, Hand, Cribcards) :-
    findall(Comb, combination(Cards, 4, Comb), HandsOf4),
    map_list(expected_value(Cards), HandsOf4, ExpectedValues),
    max_list(ExpectedValues, Max),
    Max = Hand-_,
    get_crib(Hand, Cards, Cribcards).


expected_value(Hand, HandOf4, Result) :-
    findall(card(Rank, Suit), (rank(Rank), suit(Suit), \+ member(card(Rank, Suit), Hand)), StartCards),
    map_list(hand_value(HandOf4), StartCards, HandValues),
    avg_list(HandValues, Average),
    Result = HandOf4-Average.


avg_list(List, Avg) :-
    sum_list(List, Sum),
    length(List, Length),
    Length > 0,
    Avg is Sum / Length.

max(H1-X, H2-Y, H1-X) :- X >= Y.
max(H1-X, H2-Y, H2-Y) :- X < Y.

max_list([H-X], H-X).
max_list([H-X|Xs], Max) :-
    max_list(Xs, MaxRestH),
    max(H-X, MaxRestH, Max).

get_crib([], Cards, Cards).
get_crib([Card|Rest], Cards, Crib) :-
    select(Card, Cards, UpdatedCards),
    get_crib(Rest, UpdatedCards, Crib).