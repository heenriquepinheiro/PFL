:- use_module(library(lists)).
:- use_module(library(random)).
:- consult(settings).
:- consult(utils).



game_cycle(GameState):-
    display_game(GameState),
    user_turn(GameState),
    choose_move(GameState, Move),
    new_move(GameState, Move, NewGameState), !,
    game_cycle(NewGameState).


user_turn([_, Player, _, _]):-
    name_of(Player, Name),
    format('Player ~a, is your turn!\n', [Name]), !.

display_game([Board,_,_,_]) :-
    clear_console,
    length(Board, Size),
    display_header(1, Size),
    display_bar(Size),
    display_rows(Board, 1, Size).


play:- settings(GameState), !, game_cycle(GameState).


choose_move([Board, Player, _, _], CI-RI-CF-RF):-
    get_move(Board, CI-RI, CF-RF),
    validate_move(Board, Player, CI-RI-CF-RF), !.



validate_move(Board, Player, CI-RI-CF-RF):-
    