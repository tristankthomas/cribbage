/* cribbage.pl - contains the predicates hand_value/3 and select_hand/3 for the card game cribbage
 *
 * Author: Tristan Thomas (1269492)
 * Date: 19-08-2023
 */

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

% hand value predicate
hand_value(Hand, Startcard, Value) :-
    % pairs and runs scoring
    All = [Startcard|Hand],
    generate_list(All, nosum, List),
    msort(List, Sorted).