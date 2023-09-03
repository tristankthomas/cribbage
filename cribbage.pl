%% cribbage.pl - contains the predicates hand_value/3 and select_hand/3 for the 
%  card game cribbage
%
% Author: Tristan Thomas (1269492)
% Date: 19-08-2023
%


%% hand_value(+Hand, +Startcard, -Value)
%
%  computes the value of a cribbage hand
hand_value(Hand, Startcard, Value) :-
    AllCards = [Startcard|Hand],
    % pairs and runs scoring
    generate_list(AllCards, nosum, card_to_rank, ListNoSum),
    msort(ListNoSum, Sorted),
    list_to_freq(Sorted, Freqs),
    calculate_pairs(Freqs, PairsPoints),
    calculate_runs(Freqs, RunPoints),
    % 15s scoring
    generate_list(AllCards, sum, card_to_rank, ListSum),
    calculate_15s(ListSum, SumPoints),
    % flush scoring
    generate_list(Hand, _, card_to_suit, ListSuit),
    card_to_suit(Startcard, _, StartSuit),
    calculate_flush(ListSuit, StartSuit, FlushPoints),
    % nob scoring
    calculate_nob(Hand, Startcard, NobPoint),
    Value is PairsPoints + RunPoints + SumPoints + FlushPoints + NobPoint.


%% card_to_rank(+Card, +Type, -Value)
% 
%  finds the numeric rank of a given card which is used for sorting
card_to_rank(card(ace, _), _, 1).

card_to_rank(card(jack, _), Type, Value) :-
    (   Type == nosum ->
        Value = 11 
    ;   Value = 10 
    ).

card_to_rank(card(queen, _), Type, Value) :-
    (   Type == nosum ->
        Value = 12 
    ;   Value = 10 
    ).

card_to_rank(card(king, _), Type, Value) :-
    ( Type == nosum ->
        Value = 13
    ;   Value = 10 
    ).

card_to_rank(card(Rank, _), _, Value) :-
    integer(Rank),
    Value is Rank.


%% card_to_suit(+Card, +Type, -Value)
% 
%  returns the suit of a card (for generate_list compatibility)
card_to_suit(card(_, Suit), _, Value) :-
    Value = Suit.

%% generate_list(+Cards, +Type, +Conversion, -Res)
%
%  produces a list of sortable values based on either rank or suit depending 
%  on Conversion
generate_list(Cards, Type, Conversion, Res) :-
    generate_list(Cards, Type, Conversion, [], Res).

% generate_list(+Cards, +Type, +Conversion, +Acc, -Res)
generate_list([], _, _, Acc, Acc).

generate_list([C|Cs], Type, Conversion, Acc, Res) :-
    % converts card to required value
    call(Conversion, C, Type, Value),
    append([Value], Acc, NewAcc),
    generate_list(Cs, Type, Conversion, NewAcc, Res).


%% list_to_freq(+List, -Freqs)
%
%  given a sorted list returns the frequency of each element in key value pairs
list_to_freq([], []).

list_to_freq([X|Xs], [X-Freq|Pairs]) :-
    count_frequency(X, [X|Xs], NewXs, Freq),
    list_to_freq(NewXs, Pairs).


%% count_frequency(+Elt, +List, -Rest, -Freq)
%
%  given a sorted list, counts the frequency of the first element of List 
%  and outputs the list without this first element once done
count_frequency(Elt, List, Rest, Freq) :-
    count_frequency(Elt, List, 0, Rest, Freq).

% count_frequency(+Elt, +List, +Acc, -Rest, -Freq)
count_frequency(_, [], Acc, [], Acc).

count_frequency(Elt, [Elt|Rest], Acc, Excess, Freq) :-
    Acc1 is Acc + 1,
    count_frequency(Elt, Rest, Acc1, Excess, Freq).

count_frequency(Elt, [X|Rest], Acc, [X|Rest], Acc) :-
    Elt \= X.


/* ---------------------------------- Pairs --------------------------------- */

%% calculate_pairs(+List, -Points)
%
%  computes the points from card pairs given a frequency list
calculate_pairs(List, Points) :-
    pairs_values(List, Values),
    % finds possible duplicates using frequencies
    maplist(choose_2, Values, Res),
    sum_list(Res, Sum),
    Points is Sum * 2.

%% choose_2(+N, -Res)
%
%  calculates the value of N choose 2
choose_2(1, 0).

choose_2(N, Res) :-
    N > 1,
    Res is N * (N - 1) // 2.


/* ---------------------------------- Runs ---------------------------------- */

%% calculate_runs(+List, -Points)
%
%  computes points from runs given a freuqncy list
calculate_runs(List, Points) :-
    generate_runs(List, Runs),
    maplist(points_list, Runs, PointsPerRun),
    sum_list(PointsPerRun, Points).


%% generate_runs(+List, -Runs)
%
%  finds all consecutive runs given a sorted list
generate_runs([], []).
generate_runs([_], []).

generate_runs([X-Fx,Y-Fy|Rest], Runs) :-
    % find consecutive run
    consecutive_run([X-Fx,Y-Fy|Rest], Run, Excess),
    length(Run, Len),
    % add run to list if big enough
    (   Len >= 3 ->
        generate_runs(Excess, NextRuns),
        Runs = [Run-Len|NextRuns] 
    ;   generate_runs(Excess, Runs) ).


%% consecutive_run(+List, -Run, -Excess)
%
%  finds a single consecutive run (from start) and outputs the list 
%  without this run
consecutive_run([], [], []).
consecutive_run([X-Fx], [X-Fx], []).

consecutive_run([X-Fx, Y-Fy | Rest], [X-Fx | Run], Remaining) :-
    X + 1 =:= Y,
    consecutive_run([Y-Fy | Rest], Run, Remaining).

consecutive_run([X-Fx, Y-Fy | Rest], [X-Fx], [Y-Fy | Rest]) :-
    X + 1 =\= Y.


%% points_list(+Runs, -Points)
%
%  calculates the points for runs
points_list(Run-Len, Points) :-
    pairs_values(Run, Values),
    prod_list(Values, ScaleFactor),
    Points is ScaleFactor * Len.

  
%% prod_list(+List, -Prod)
%
%  multiplies elements of a list
prod_list(List, Prod) :- 
    prod_list(List, 1, Prod).

% prod_list(+List, +Acc, -Prod)
prod_list([], Acc, Prod) :-
    Prod = Acc.

prod_list([X|Xs], Acc, Prod) :-
    Acc1 is Acc * X,
    prod_list(Xs, Acc1, Prod).


/* ----------------------------------- 15s ---------------------------------- */

%% calculate_15s(+List, -Points)
%
%  computes points from sums of 15
calculate_15s(List, Points) :-
    numlist(2, 5, Nums),
    % finds all possible combinations of the 5 cards
    combinations(List, Nums, Res),
    maplist(sum_list, Res, Sums),
    maplist(is_15, Sums, Is15s),
    sum_list(Is15s, Successes),
    Points is Successes * 2.


%% combinations(+List, +Nums, -Combs)
%
%  finds all combinations of cards so can check if sum to 15
combinations(_, [], []).

combinations(List, [N|Ns], Combs) :-
    % finds all combinations of the 5 cards into N groups
    findall(Comb, combination(List, N, Comb), NewCombs),
    combinations(List, Ns, RestCombs),
    append(NewCombs, RestCombs, Combs).

%% combination(+List, +K, -Comb) is nondet
%
%  finds a combination of K elements non-deterministically
combination(_, 0, []).

combination([X|Xs], K, [X|Combination]) :-
    K > 0,
    K1 is K - 1,
    combination(Xs, K1, Combination).

% leaves choicepoints to find all combinations
combination([_|Xs], K, Combination) :-
    K > 0,
    combination(Xs, K, Combination).


%% is_15(+N, -Res)
%
%  assigns a score to a card combination if it sums to 15
is_15(N, Res) :-
    (   N =:= 15 -> 
        Res = 1 
    ;   Res = 0
    ).


/* ---------------------------------- Flush --------------------------------- */

%% calculate_flush(+Suits, +StartSuit, -Points)
%
%  comptutes the points from a flush given list of suits
calculate_flush(Suits, StartSuit, Points) :-
    list_to_freq(Suits, Freqs),
    ( length(Freqs, 1) ->
        Freqs = [Suit-_|_],
        ( Suit = StartSuit -> 
            Points = 5 
        ;   Points = 4 
        )
    ;
        Points = 0
    ).


/* ----------------------------------- Nob ---------------------------------- */

%% calculate_nob(+Hand, +StartCard, -Points)
%
%  finds if the hand contains a jack with the suit the same 
%  as the start card
calculate_nob(Hand, card(_, Suit), Points) :-
    ( member(card(jack, Suit), Hand) ->
        Points = 1 
    ;   Points = 0 
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% select_hand(+Cards, -Hand, -Cribcards)
% 
%  finds the best 4 card hand possible, given either a 5 or 6 card initial hand
select_hand(Cards, Hand, Cribcards) :-
    findall(Comb, combination(Cards, 4, Comb), HandsOf4),
    maplist(expected_value(Cards), HandsOf4, ExpectedValues),
    max_list(ExpectedValues, Max),
    Max = Hand-_,
    get_crib(Hand, Cards, Cribcards).


%% expected_value(+Cards, +HandOf4, -Result)
%
%  calculates the expected hand value of a given 4 card hand
expected_value(Cards, HandOf4, Result) :-
    % finds all possible start cards
    findall(card(Rank, Suit), (rank(Rank), suit(Suit), 
        \+ member(card(Rank, Suit), Cards)), StartCards),
    % finds the hand value for each hand with each start card
    maplist(hand_value(HandOf4), StartCards, HandValues),
    avg_list(HandValues, Average),
    Result = HandOf4-Average.


%% avg_list(+List, -Avg)
%
%  finds the average of all elements in a list
avg_list(List, Avg) :-
    sum_list(List, Sum),
    length(List, Length),
    Length > 0,
    Avg is Sum / Length.


%% max_list(+List, -Max)
%
%  finds the maximum value in a list of key value pairs (finds based on value)
max_list([H-X], H-X).

max_list([H-X|Xs], Max) :-
    max_list(Xs, MaxRest),
    max(H-X, MaxRest, Max).

%% max(+Elt1, +Elt2, -Max)
%
%  finds the max of two key value pairs based on value
max(H1-X, _-Y, H1-X) :- 
    X >= Y.
max(_-X, H2-Y, H2-Y) :- 
    X < Y.


%% get_crib(+Hand, +Cards, -Cribcards)
%
%  finds the cards from Cards that are not in Hand
get_crib([], Cards, Cards).

get_crib([Card|Rest], Cards, Crib) :-
    select(Card, Cards, UpdatedCards),
    get_crib(Rest, UpdatedCards, Crib).


%% rank(?Rank)
%
%  states the possible ranks as facts
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

%% suit(?Suit)
%
%  states the possible suits as facts
suit(diamonds).
suit(clubs).
suit(hearts).
suit(spades).
