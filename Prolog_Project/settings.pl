:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(system), [now/1]).
:- consult(utils).


:- dynamic player_color/2.
:- dynamic difficulty_of/2.
:- dynamic name_of/2.


% game header
apart:-
    write('=================\n'),
    write('      APART      \n'),
    write('=================\n').


% initial menu
initial_menu:- 
    write('Choose an option:\n'),
    write('1. Human vs. Human\n'),
    write('2. Human vs. Machine\n'),
    write('3. Machine vs. Machine\n').


% Main menu options, each one representing a game mode.
option(1):- 
    write('Human vs. Human\n'),
    set_name(player1),
    set_name(player2).
option(2):- 
    write('Human vs. Machine\n'),
    set_name(player1),
    asserta((name_of(player2, 'Machine'))), !,
    set_difficulty(player2).
option(3):- 
    write('Machine vs. Machine\n'),
    asserta((name_of(player1, 'Machine1'))),
    asserta((name_of(player2, 'Machine2'))), !,
    set_difficulty(player1),
    set_difficulty(player2).


% Get and set name of player.
set_name(Player):-
    format('Hello ~a, what is your name? ', [Player]),
    get_line(Name, []),
    asserta((name_of(Player, Name))).


get_line(Result, Acc):-
    get_char(Char),
    Char \= '\n',
    append(Acc, [Char], NewAcc),
    get_line(Result, NewAcc).
get_line(Result, Acc):- atom_chars(Result, Acc).


% Choose and set difficulty.
set_difficulty(Machine):- 
    format('Choose difficulty of ~a:\n', [Machine]),
    write('1. Random\n'),
    write('2. Greedy\n'),
    get_choice(1, 2, 'Difficulty', Difficulty),!,
    asserta((difficulty_of(Machine, Difficulty))).

% Choose board size
choose_board(Size):-
    write('Board size: 6, 8 or 10? '),
    repeat,
    read_number(Size),
    member(Size, [6, 8, 10]), !.

% 
choose_player(Player):-
    name_of(player1, Name1),
    name_of(player2, Name2),
    format('Who starts playing?\n1 - ~a with White\n2 - ~a with White\n', [Name1, Name2]),
    get_choice(1, 2, 'Select', Index),
    nth1(Index, [player1, player2], Player),
    assert_colors(Index).

assert_colors(Index):-
    Index =:= 1, asserta(player_color(player1,white)), asserta(player_color(player2,black)).

assert_colors(Index):-
    Index =:= 2, asserta(player_color(player2,white)), asserta(player_color(player1,black)).


% Game mode choice
set_mode :-
    initial_menu,
    get_choice(1, 3, 'Mode', Option), !,
    option(Option).


% initialize GameState with board
settings([Board, Player, []]):-
    apart,
    set_mode,
    choose_player(Player),
    choose_board(Size),
    board(Size, Board).

board(10,[
        [empty,     black,      black,     black,     black,     black,     black,     black,     black,     empty],
        [empty,     black,      black,     black,     black,     black,     black,     black,     black,     empty],
        [empty,     black,      black,     black,     black,     black,     black,     black,     black,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty],
        [empty,     white,      white,     white,     white,     white,     white,     white,     white,     empty],
        [empty,     white,      white,     white,     white,     white,     white,     white,     white,     empty],
        [empty,     white,      white,     white,     white,     white,     white,     white,     white,     empty]
        
]).

board(8,[
        [empty,     black,      black,     black,     black,     black,     black,     empty],
        [empty,     black,      black,     black,     black,     black,     black,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty,     empty],
        [empty,     white,      white,     white,     white,     white,     white,     empty],
        [empty,     white,      white,     white,     white,     white,     white,     empty]
        
]).

board(6,[
        [empty,     empty,      empty,     black,     empty,     black],
        [empty,     empty,      empty,     empty,     empty,     empty],
        [empty,     black,      black,     empty,     empty,     empty],
        [empty,     empty,      white,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty],
        [empty,     white,      empty,     white,     white,     empty]
]).

symbol(empty, ' '):-!.
symbol(black, 'B'):-!.
symbol(white, 'W'):-!.

position(Board, Col-Row, Piece):-
    nth1(Row, Board, Line),
    nth1(Col, Line, Piece).

    
get_symbol(Board, Row, Col, Symbol):-
    position(Board, Col-Row, Piece),
    symbol(Piece, Symbol).


display_header(Final, Final):-
    format('~d\n  ', [Final]), !.
display_header(1, Final):-
    format('\n    ~d   ', [1]),
    Next is 2,
    display_header(Next, Final).
display_header(Num, Final):-
    format('~d   ', [Num]),
    Next is Num + 1,
    display_header(Next, Final).


display_bar(0):-
    write('|\n'), !.
display_bar(Size):-
    write('|---'),
    Next is Size - 1,
    display_bar(Next).


display_pieces(_, _, Col, Size):- 
    Col > Size, write('\n  '), !.
display_pieces(Board, Line, Col, Size):-
    get_symbol(Board, Line, Col, Symbol),
    format(' ~a |', [Symbol]),
    NextCol is Col + 1,
    display_pieces(Board, Line, NextCol, Size).


display_rows(_, Line, Size):- 
    Line > Size, nl, !.

display_rows(Board, Line, Size):-
    (Line < 10 -> format('~d |', [Line]) ; format('~d|', [Line])),
    display_pieces(Board, Line, 1, Size),
    display_bar(Size),
    NextLine is Line + 1,
    display_rows(Board, NextLine, Size).

clear_console:- write('\33\[2J').





