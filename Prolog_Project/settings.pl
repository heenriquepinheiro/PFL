:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(system), [now/1]).

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

% main menu options
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


% get and set name of player
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


% choose and set difficulty
set_difficulty(Machine):- 
    format('Choose difficulty of ~a:\n', [Machine]),
    write('1. Normal\n'),
    write('2. Hard\n'),
    get_option(1, 2, 'Difficulty', Difficulty),!,
    asserta((difficulty_of(Machine, Difficulty))).


get_option(Low, High, Message, Option):-
    format('~a between ~d and ~d: ', [Message, Low, High]),
    repeat,
    read_number(Option),
    between(Low, High, Option), !.


read_number(X):-
    read_number_aux(X,0).
read_number_aux(X,Acc):- 
    get_code(C),
    between(48, 57, C), !,
    Acc1 is 10*Acc + (C - 48),
    read_number_aux(X,Acc1).
read_number_aux(X,X).


choose_board(Size):-
    write('Board size: 8? '),
    repeat,
    read_number(Size),
    member(Size, [8]), !.


choose_player(Player):-
    name_of(player1, Name1),
    name_of(player2, Name2),
    format('Who starts playing?\n1 - ~a with White\n2 - ~a with White\n', [Name1, Name2]),
    get_option(1, 2, 'Select', Index),
    nth1(Index, [player1, player2], Player).


% Game mode choice
set_mode :-
    initial_menu,
    get_option(1, 3, 'Mode', Option), !,
    option(Option).


% initialize GameState with board
settings([Board, Player, [], 0]):-
    apart,
    set_mode,
    choose_player(Player),
    choose_board(Size),
    board(Size, Board).


play:- settings(GameState), game_cycle(GameState).

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

symbol(empty, ' '):-!.
symbol(black, 'B'):-!.
symbol(white, 'W'):-!.

position(Board, Col-Row, Piece):-
    nth1(Row, Board, Line),
    nth1(Col, Line, Piece).

    

game_cycle(GameState):-
    display_game(GameState).

get_symbol(Board, Row, Col, Symbol):-
    position(Board, Col-Row, Piece),
    symbol(Piece, Symbol).


display_game([Board,_,_,_]) :-
    clear_console,
    length(Board, Size),
    display_header(1, Size),
    display_bar(Size),
    display_rows(Board, 1, Size).


display_header(Final, Final):-
    format('~d\n ', [Final]), !.
display_header(1, Final):-
    format('\n   ~d   ', [1]),
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


display_rows(_, LineNum, Size):-
    LineNum > Size, nl, !.
display_rows(Board, LineNum, Size):-
    format('~d|', [LineNum]),
    display_pieces(Size),
    write(' '),
    display_bar(Size),
    Next is LineNum + 1,
    display_rows(Board, Next, Size).


display_pieces(_, _, Col, Size):- 
    Col > Size, nl, !.
display_pieces(Board, Row, Col, Size):-
    get_symbol(Board, Row, Col, Symbol),
    format(' ~a |', [Symbol]),
    Next is Col - 1,
    display_pieces(Board, Row, Next, Size).

clear_console:- write('\33\[2J').





