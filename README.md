# Cribbage Solver
This project was created as part of a declarative programming subject where a solver was implemented to find the best possible 4 cards out of a hand of 5 or 6 that the player should keep in the card game Cribbage. The total points of a 4 card hand can be calculated from 15s, Pairs, Runs, Flushes and "One for his nob". The rules for this game can be found [here](https://bicyclecards.com/how-to-play/cribbage). The approach taken was to iterate over all 5 or 6 cards and find the highest expected 4 card hand total.

## Usage
The program can be ran using SWI-Prolog and calling the `hand_value/3` and `select_hand/3` predicates in the following way:
```
hand_value([card(6, hearts), card(5, spades), card(jack, clubs), card(jack, diamonds)], card(7, diamonds), Value).
Value = 10.
```

And
```
?- select_hand([card(ace, clubs), card(jack, spades), card(6, diamonds), card(ace, clubs), card(2, clubs), card(ace, spades)], Hand, Crib).
Hand = [card(ace, clubs), card(jack, spades), card(ace, clubs), card(ace, spades)], Crib = [card(6, diamonds), card(2, clubs)].
```
Where both predicates are non-deterministic and only return one solution.
