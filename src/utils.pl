:- use_module(library(between)).

% get_choice(+Min, +Max, +Context, -Choice)
get_choice(Min, Max, Context, Choice):-
    format('~a between ~d and ~d: ', [Context, Min, Max]),
    repeat,
    read_number(Choice),
    between(Min, Max, Choice), !.


% get_line(-Result, +Acc)
get_line(Result, Acc):-
    get_char(Char),
    Char \= '\n',
    append(Acc, [Char], NewAcc),
    get_line(Result, NewAcc).
get_line(Result, Acc):- atom_chars(Result, Acc).


% read_number(-Number)
read_number(X):-
    read_number_aux(X,0).
read_number_aux(X,Acc):- 
    get_code(C),
    between(48, 57, C), !,
    Acc1 is 10*Acc + (C - 48),
    read_number_aux(X,Acc1).
read_number_aux(X,X).


% get_move(+Board, -CI-RI, -CF-RF)
% Obtains the current and future coordinates of the piece you want to play with
get_move(Board, CI-RI, CF-RF):-
    length(Board, Size),
    get_choice(1, Size, 'Column of the piece to move', CI),
    get_choice(1, Size, 'Row of the piece to move', RI),
    get_choice(1, Size, 'Column of the destination', CF),
    get_choice(1, Size, 'Row of the destination', RF).


% in_bounds(+Board, +Col-Row)
% Checks that the given coordinates are inside the board
in_bounds(Board, Col-Row):-
    length(Board, Size),
    between(1, Size, Col),
    between(1, Size, Row).


% abs(+Number,-Absolute)
abs(X,X) :- X >= 0, !.
abs(X,Y) :- Y is -X.


player_change(player1, player2).
player_change(player2, player1).


% put_board(+Board, +Coordinate, +Piece, -NewBoard)
% Change de position of a piece.
put_piece(Board, C-R, Piece, NewBoard):-
    Ridx is R - 1,
    Cidx is C - 1,
    nth0(Ridx, Board, Line),
    replace(Cidx, Piece, Line, NewLine),
    replace(Ridx, NewLine, Board, NewBoard).


% replace(+Idx, +Piece, +Targer, -NewTarget)
% replace an element from a list by piece
replace(Idx, Piece, Target, NewTarget):-
    nth0(Idx, Target, _, MidTarget),
    nth0(Idx, NewTarget, Piece, MidTarget).


% empty_list(+List, -Answer)
% Verifies if a list is an empty list.
empty_list([], true).
empty_list([_|_], false).


% random_item(+List, -Item)
% Return a random item from a given list.
random_item(List, Item):-
    length(List, Len),
    random(0, Len, Idx),
    nth0(Idx, List, Item).


% swap_minimax(+MiniMaxMode, -NewMode)
% Swaps minimax mode.
swap_minimax(min, max).
swap_minimax(max, min).


% minmax_op(+Mode, +Values, -Result)
minmax_op(min, [Value|_], Result):- Result is -Value.
minmax_op(max, Values, Value):- last(Values, Value).


clear_console:- write('\33\[2J').

clear_data :-
   retractall(player_color(_,_)),
    retractall(difficulty_of(_, _)),
    retractall(name_of(_, _)).
