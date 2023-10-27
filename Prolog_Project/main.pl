:- use_module(library(lists)).
:- use_module(library(random)).
:- consult(settings).



game_cycle(GameState):-
    display_game(GameState).


display_game([Board,_,_,_]) :-
    clear_console,
    length(Board, Size),
    display_header(1, Size),
    display_bar(Size),
    display_rows(Board, 1, Size).


play:- settings(GameState), game_cycle(GameState).


choose_move(Board, Player, CI-RI-CF-RF):-
    get_move(Board, CI-RI, CF-RF),
    validate_move(Board, Player, CI-RI-CF-RF), !.



validate_move(Board, Player, CI-RI-CF-RF):-
    