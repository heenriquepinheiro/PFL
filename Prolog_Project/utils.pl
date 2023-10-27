:- use_module(library(between)).


get_choice(Min, Max, Context, Choice):-
    format('~a between ~d and ~d: ', [Context, Min, Max]),
    repeat,
    read_number(Choice),
    between(Min, Max, Choice), !.

get_move(Board, CI-RI, CF-RF):-
    length(Board, Size),
    get_choice(1, Size, 'Column of the piece to move', CI),
    get_choice(1, Size, 'Row of the piece to move', RI),
    get_choice(1, Size, 'Column of the destination', CF),
    get_choice(1, Size, 'Row of the destination', RF).


in_bounds(Board, Col-Row):-
    length(Board, Size),
    between(1, Size, Col),
    between(1, Size, Row).