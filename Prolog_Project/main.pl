:- use_module(library(lists)).
:- use_module(library(random)).
:- consult(settings).
:- consult(utils).



game_cycle(GameState):-
    display_game(GameState),
    user_turn(GameState), !,
    choose_move(GameState, Move),
    new_move(GameState, Move, NewGameState), !,
    /*
    jump_mode(NewGameState, Move), !,*/
    write('ta2\n'),
    change_players(GameState, NewGameState),
    game_cycle(NewGameState).

jump_mode(GameState, Move):-
    CI-RI-CF-RF = Move,
    abs(CF - CI, Res1), Res1 >= 2, abs(RF - RI, Res2), Res2 >= 2,
    validate_jump(Gamestate, Move).

jump_mode(GameState, Move):-
    CI-RI-CF-RF = Move,
    abs(CF - CI, Res1), Res1 < 2, abs(RF - RI, Res2), Res2 < 2,
    validate_step(GameState, Move).


user_turn([_, Player, _]):-
    name_of(Player, Name),
    format('Player ~a, is your turn!\n', [Name]), !.


display_game([Board,_,_]) :-
    clear_console,
    length(Board, Size),
    display_header(1, Size),
    display_bar(Size),
    display_rows(Board, 1, Size).


play:- settings(GameState), !, game_cycle(GameState).


choose_move([Board, Player, _], CI-RI-CF-RF):-
    get_move(Board, CI-RI, CF-RF),
    validate_move(Board, Player, CI-RI-CF-RF), !.

% Direction: 1 - Horizontal; 2 - Vertical; 3 - Diagonal (\); 4 - Diagonal (//).

validate_move(Board, Player, CI-RI-CF-RF):-
    position(Board, CI-RI, Piece),
    player_color(Player, Piece), !,
    CF>=1, RF>=1, CF=<8, RF=<8,
    obstructed(Board, CI-RI-CF-RF),
    /*
    get_line_length(RI,CI,Len),
    */
    get_direction(CI-RI-CF-RF, Direction, JumpSize),
    check_jump_size(CI-RI, Board, Player, Direction, RealJumpSize), !,
    
    write('final check\n'),

    format('Wanted: ~d | Real : ~d \n', [JumpSize, RealJumpSize]),

    JumpSize =:= RealJumpSize,
    write('move validated\n').


get_direction(CI-RI-CF-RF, Direction, JumpSize):-
    (RI == RF, Direction is 1, Diff is CF - CI, abs(Diff, A), JumpSize is A);
    (CI == CF, Direction is 2, Diff is RF - RI, abs(Diff, B), JumpSize is B);
    ((CF - CI) =:= (RF - RI), Diff is CF - CI, Direction is 3, abs(Diff, C), JumpSize is C);
    ((CF - CI) =:= -(RF - RI), Diff is CF - CI, Direction is 4, abs(Diff, D), JumpSize is D).

check_jump_size(CI-RI, Board, Player, Direction, RealJumpSize):-
    (Direction == 1, Acc is 1- CI,  check_jump_size_aux(CI-RI, Board, Player, Direction, 0, RealJumpSize, Acc));
    (Direction == 2, Acc is 1- RI,  check_jump_size_aux(CI-RI, Board, Player, Direction, 0, RealJumpSize, Acc));
    (Direction == 3, check_jump_size_aux(CI-RI, Board, Player, Direction, 0, RealJumpSize, -7));
    (Direction == 4, check_jump_size_aux(CI-RI, Board, Player, Direction, 0, RealJumpSize, -7)).

check_jump_size_aux(CI-RI, Board, Player, Direction, CurrJS, RealJumpSize, Acc):-
    length(Board, Size),
    ((Direction =:= 1, Col is CI + Acc, Col > Size);
    (Direction =:= 2, Row is RI + Acc, Row > Size);
    (Direction =:= 3, Col is CI + Acc, Row is RI + Acc, (Col > Size; Row > Size));
    (Direction =:= 4, Col is CI + Acc, Row is RI - Acc, (Col > Size; Row < 1))), !,
    RealJumpSize is CurrJS.


check_jump_size_aux(CI-RI, Board, Player, Direction, CurrJS, RealJumpSize, Acc):-
    length(Board, Size),
    Direction == 1, NewCol is Acc + CI, NewCol =< Size,
    player_color(Player, PlayerPiece),
    position(Board, NewCol-RI, NewPiece),
    (player_color(Player,NewPiece), NewRealJumpSize is CurrJS + 1; \+player_color(Player,NewPiece), NewRealJumpSize is CurrJS),
    NewAcc is Acc+1,
    check_jump_size_aux(CI-RI, Board, Player, Direction, NewRealJumpSize, RealJumpSize, NewAcc).

check_jump_size_aux(CI-RI, Board, Player, Direction, CurrJS, RealJumpSize, Acc):-
    length(Board,Size),
    Direction == 2, NewRow is Acc + RI, NewRow =< Size,
    player_color(Player, PlayerPiece),
    position(Board, CI-NewRow, NewPiece),
    (player_color(Player,NewPiece), NewRealJumpSize is CurrJS + 1; \+player_color(Player,NewPiece), NewRealJumpSize is CurrJS),
    NewAcc is Acc+1,
    check_jump_size_aux(CI-RI, Board, Player, Direction, NewRealJumpSize, RealJumpSize, NewAcc).

check_jump_size_aux(CI-RI, Board, Player, Direction, CurrJS, RealJumpSize, Acc):-
    length(Board,Size),
    Direction == 3,
    NewCol is CI + Acc, NewCol =< Size,
    NewRow is RI + Acc, NewRow =< Size,
    (((NewCol<1;NewRow<1), NewAcc is Acc + 1, check_jump_size_aux(CI-RI, Board, Player, Direction, CurrJS, RealJumpSize, NewAcc));(NewCol>=1, NewRow>=1,
    player_color(Player, PlayerPiece),
    position(Board, NewCol-NewRow, NewPiece),
    (player_color(Player,NewPiece), NewRealJumpSize is CurrJS + 1; \+player_color(Player,NewPiece), NewRealJumpSize is CurrJS),
    NewAcc is Acc+1,
    check_jump_size_aux(CI-RI, Board, Player, Direction, NewRealJumpSize, RealJumpSize, NewAcc))).

check_jump_size_aux(CI-RI, Board, Player, Direction, CurrJS, RealJumpSize, Acc):-
    length(Board,Size),
    Direction == 4,
    NewCol is CI + Acc, NewCol =< Size,
    NewRow is RI - Acc, NewRow >= 1,
    (((NewCol<1;NewRow>Size), NewAcc is Acc + 1, NewRealJumpSize is CurrJS, check_jump_size_aux(CI-RI, Board, Player, Direction, NewRealJumpSize, RealJumpSize, NewAcc));(NewCol>=1, NewRow=<Size,
    player_color(Player, PlayerPiece),
    position(Board, NewCol-NewRow, NewPiece),
    (player_color(Player,NewPiece), NewRealJumpSize is CurrJS + 1; \+player_color(Player,NewPiece), NewRealJumpSize is CurrJS),
    NewAcc is Acc+1,
    check_jump_size_aux(CI-RI, Board, Player, Direction, NewRealJumpSize, RealJumpSize, NewAcc))).


obstructed(Board, CI-RI-CF-RF):-
    position(Board, CI-RI, CurrPiece),
    position(Board, CF-RF, NextPlace),
    (NextPlace \= CurrPiece).

    
change_players([A,Player,_],NewGameState):-
    write('ta1\n'),
    player_change(Player, NewPlayer),
    format('Res ~a\n', [NewPlayer]),
    NewGameState = [A,NewPlayer, []].




new_move(GameState, Move, NewGameState):-
    [Board, Player, AlreadyJumped] = GameState,
    CI-RI-CF-RF = Move,
    position(Board, CI-RI, Piece),
    put_piece(Board, CI-RI, empty, CleanedBoard),
    put_piece(CleanedBoard, CF-RF, Piece, NewBoard),
    % append(CF-RF,NewAlreadyJumped)
    NewGameState = [NewBoard, Player, AlreadyJumped].



isValid(X, Y):- X >= 1 , X =< 8, Y >= 1 , Y =< 8.

get_line_length(Board, X-Y, L):-
    Y > 0,
    Y1 is Y-1,
    Y2 is Y+1,
    get_line_length_aux(Board, X-Y, Y1, L1), 
    get_line_length_aux(Board, X-Y, Y2, L2),
    L is (L1 + L2 + 1).
get_line_length_aux(Board, X-Y, YN, 0) :- \+ isValid(X, YN).
get_line_length_aux(Board, X-Y, YN, L) :-
    isValid(X, YN), 
    position(Board, X-Y, Piece),
    position(Board, X-YN, Npiece),
    ((Piece == Npiece, L is 1) ; (Piece \= Npiece, L is 0)).