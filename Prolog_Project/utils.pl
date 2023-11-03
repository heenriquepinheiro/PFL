:- use_module(library(between)).


get_choice(Min, Max, Context, Choice):-
    format('~a between ~d and ~d: ', [Context, Min, Max]),
    repeat,
    read_number(Choice),
    between(Min, Max, Choice), !.

read_number(X):-
    read_number_aux(X,0).
read_number_aux(X,Acc):- 
    get_code(C),
    between(48, 57, C), !,
    Acc1 is 10*Acc + (C - 48),
    read_number_aux(X,Acc1).
read_number_aux(X,X).

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

abs(X,X) :- X >= 0, !.
abs(X,Y) :- Y is -X.

player_change(player1, player2).
player_change(player2, player1).

put_piece(Board, C-R, Piece, NewBoard):-
    Ridx is R-1,
    Cidx is C -1,
    nth0(Ridx, Board, Line),
    replace(Cidx, Piece, Line, NewLine),
    replace(Ridx, NewLine, Board, NewBoard).


replace(Idx, Piece, Target, NewTarget):-
    nth0(Idx, Target, _, MidTarget),
    nth0(Idx, NewTarget, Piece, MidTarget).