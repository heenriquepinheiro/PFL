:- use_module(library(lists)).
:- use_module(library(random)).
:- consult(settings).
:- consult(utils).


game_cycle(GameState):-
    game_over(GameState, Winner), 
    Winner \= none, !,
    display_game(GameState),
    show_winner(Winner).

game_cycle(GameState):-
    display_game(GameState),
    user_turn(GameState),
    choose_move(GameState, Move), !,
    new_move(GameState, Move, NewGameState), !,
    jump_mode(NewGameState, JumpState), !,
    change_players(JumpState, FinalGameState),
    game_cycle(FinalGameState).

jump_mode([Board, Player, AlreadyJumped], JumpState):-
    empty_list(AlreadyJumped, true),
    JumpState = [Board, Player, AlreadyJumped].

jump_mode([Board, Player, AlreadyJumped], JumpState):-
    empty_list(AlreadyJumped, false), [LastMove|T] = AlreadyJumped,
    \+jump_possible([Board, Player, AlreadyJumped], LastMove),
    JumpState = [Board, Player, AlreadyJumped].

jump_mode([Board, Player, AlreadyJumped], JumpState):-
    empty_list(AlreadyJumped, false), [LastMove|T] = AlreadyJumped,
    jump_possible([Board, Player, AlreadyJumped], LastMove),
    ask_to_jump(Choice),
    ((Choice == 110, JumpState = [Board, Player, AlreadyJumped]);
    (Choice == 121,
    game_cycle([Board, Player, AlreadyJumped])
    )).

% y - 121, n - 110
ask_to_jump(Choice):-
    write('Do you want to continue jumping (y/n): '),
    repeat,
    get_code(Choice),
    member(Choice, [121, 110]), !.

    
jump_possible([Board, Player, AlreadyJumped], CI-RI):-
    valid_moves_piece([Board, Player, AlreadyJumped], CI-RI, ListOfMoves).

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


choose_move([Board, Player, AlreadyJumped], CI-RI-CF-RF):-
    get_move(Board, ColI-RowI, ColF-RowF),
    ((validate_move_normal([Board, Player, AlreadyJumped], ColI-RowI-ColF-RowF), CI is ColI, RI is RowI, CF is ColF, RF is RowF);
    (\+validate_move_normal([Board, Player, AlreadyJumped], ColI-RowI-ColF-RowF),
    write('The selected move is not valid, please try again!\n'),
    choose_move([Board, Player, AlreadyJumped], CI-RI-CF-RF))).

valid_moves(GameState, Player, ListOfMoves):-
    [Board,Player,[]] = GameState,
    findall(CI-RI-CF-RF, validate_move_normal([Board,Player,AlreadyJumped],CI-RI-CF-RF),ListOfMoves).

valid_moves(GameState, _, ListOfMoves):-
    findall(CI-RI-CF-RF, validate_move_normal(GameState,CI-RI-CF-RF),ListOfMoves),
    \+length(ListOfMoves, 0), !.

valid_moves_piece(GameState, CI-RI, ListOfMoves):-
    findall(CI-RI-CF-RF, validate_move_normal(GameState,CI-RI-CF-RF),ListOfMoves),
    \+length(ListOfMoves, 0), !.



% Direction: 1 - Horizontal; 2 - Vertical; 3 - Diagonal (\); 4 - Diagonal (//).

validate_move_normal([Board, Player, AlreadyJumped], CI-RI-CF-RF):-
    length(Board, Size),
    position(Board, CI-RI, Piece),
    player_color(Player, Piece), 
    in_bounds(Board,CF-RF),
    obstructed(Board, CI-RI-CF-RF),
    get_direction(CI-RI-CF-RF, Direction, JumpSize),
    check_jump_size_normal(CI-RI, Board, Player, Direction, RealJumpSize), 
    JumpSize =:= RealJumpSize,
    (empty_list(AlreadyJumped,true); (empty_list(AlreadyJumped,false), RealJumpSize > 1, selected_previous_piece(AlreadyJumped, CI-RI))),
    already_jumped(CF-RF, AlreadyJumped).

selected_previous_piece(AlreadyJumped, CI-RI):-
    [LastMove|_] = AlreadyJumped,
    CLast-RLast = LastMove,
    CI == CLast, RI == RLast.

check_jump_size_normal(CI-RI, Board, Player, Direction, RealJumpSize):-
    Direction == 1,
    ColBack is CI - 1, ColFront is CI + 1,
    player_color(Player, PlayerColor),
    ((position(Board, ColBack-RI, PlayerColor), position(Board, ColFront-RI, PlayerColor), RealJumpSize is 3);
    (\+position(Board, ColBack-RI, PlayerColor), position(Board, ColFront-RI, PlayerColor), RealJumpSize is 2);
    (position(Board, ColBack-RI, PlayerColor), \+position(Board, ColFront-RI, PlayerColor), RealJumpSize is 2);
    (\+position(Board, ColBack-RI, PlayerColor), \+position(Board, ColFront-RI, PlayerColor), RealJumpSize is 1)).

check_jump_size_normal(CI-RI, Board, Player, Direction, RealJumpSize):-
    Direction == 2,
    RowBack is RI - 1, RowFront is RI + 1,
    player_color(Player, PlayerColor),
    ((position(Board, CI-RowBack, PlayerColor), position(Board, CI-RowFront, PlayerColor), RealJumpSize is 3);
    (\+position(Board, CI-RowBack, PlayerColor), position(Board, CI-RowFront, PlayerColor), RealJumpSize is 2);
    (position(Board, CI-RowBack, PlayerColor), \+position(Board, CI-RowFront, PlayerColor), RealJumpSize is 2);
    (\+position(Board, CI-RowBack, PlayerColor), \+position(Board, CI-RowFront, PlayerColor), RealJumpSize is 1)).

check_jump_size_normal(CI-RI, Board, Player, Direction, RealJumpSize):-
    Direction == 3,
    ColBack is CI - 1, ColFront is CI + 1,
    RowBack is RI - 1, RowFront is RI + 1,
    player_color(Player, PlayerColor),
    ((position(Board, ColBack-RowBack, PlayerColor), position(Board, ColFront-RowFront, PlayerColor), RealJumpSize is 3);
    (\+position(Board, ColBack-RowBack, PlayerColor), position(Board, ColFront-RowFront, PlayerColor), RealJumpSize is 2);
    (position(Board, ColBack-RowBack, PlayerColor), \+position(Board, ColFront-RowFront, PlayerColor), RealJumpSize is 2);
    (\+position(Board, ColBack-RowBack, PlayerColor), \+position(Board, ColFront-RowFront, PlayerColor), RealJumpSize is 1)).

check_jump_size_normal(CI-RI, Board, Player, Direction, RealJumpSize):-
    Direction == 4,
    ColBack is CI - 1, ColFront is CI + 1,
    RowBack is RI - 1, RowFront is RI + 1,
    player_color(Player, PlayerColor),
    ((position(Board, ColBack-RowFront, PlayerColor), position(Board, ColFront-RowBack, PlayerColor), RealJumpSize is 3);
    (\+position(Board, ColBack-RowFront, PlayerColor), position(Board, ColFront-RowBack, PlayerColor), RealJumpSize is 2);
    (position(Board, ColBack-RowFront, PlayerColor), \+position(Board, ColFront-RowBack, PlayerColor), RealJumpSize is 2);
    (\+position(Board, ColBack-RowFront, PlayerColor), \+position(Board, ColFront-RowBack, PlayerColor), RealJumpSize is 1)).

get_direction(CI-RI-CF-RF, Direction, JumpSize):-
    (RI == RF, Direction is 1, Diff is CF - CI, abs(Diff, A), JumpSize is A);
    (CI == CF, Direction is 2, Diff is RF - RI, abs(Diff, B), JumpSize is B);
    ((CF - CI) =:= (RF - RI), Diff is CF - CI, Direction is 3, abs(Diff, C), JumpSize is C);
    ((CF - CI) =:= -(RF - RI), Diff is CF - CI, Direction is 4, abs(Diff, D), JumpSize is D).

already_jumped(CF-RF, []).

already_jumped(CF-RF, AlreadyJumped):-
    [H|T] = AlreadyJumped,
    ColJump-RowJump = H,
    (ColJump \= CF; RowJump \= RF), !,
    already_jumped(CF-RF, T).

% ------------------------Possible Hard Mode------------------------------
/*
validate_move_hard(Board, Player, AlreadyJumped, CI-RI-CF-RF):-
    position(Board, CI-RI, Piece),
    player_color(Player, Piece), 
    CF>=1, RF>=1, CF=<8, RF=<8,
    obstructed(Board, CI-RI-CF-RF),
    get_direction(CI-RI-CF-RF, Direction, JumpSize),
    check_jump_size_hard(CI-RI, Board, Player, Direction, RealJumpSize), !,

    JumpSize =:= RealJumpSize.

check_jump_size_hard(CI-RI, Board, Player, Direction, RealJumpSize):-
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
*/
% ---------------------------------------------------------------------------

obstructed(Board, CI-RI-CF-RF):-
    position(Board, CI-RI, CurrPiece),
    position(Board, CF-RF, NextPlace),
    (NextPlace \= CurrPiece).

    
change_players([Board, Player, _], [Board, NewPlayer, []]) :-
    player_change(Player, NewPlayer).


new_move(GameState, Move, NewGameState):-
    [Board, Player, AlreadyJumped] = GameState,
    CI-RI-CF-RF = Move,
    position(Board, CI-RI, Piece),
    put_piece(Board, CI-RI, empty, CleanedBoard),
    put_piece(CleanedBoard, CF-RF, Piece, NewBoard),
    Diff1 is CF - CI, abs(Diff1, Res1), Diff2 is RF - RI, abs(Diff2, Res2),
    (((Res1 >= 2; Res2 >=2),
    ((empty_list(AlreadyJumped, true), append([CI-RI],AlreadyJumped, L), append([CF-RF], L, NewAlreadyJumped));
    (empty_list(AlreadyJumped, false), append([CF-RF],AlreadyJumped, NewAlreadyJumped)))); (Res1 < 2, Res2 < 2)),
    NewGameState = [NewBoard, Player, NewAlreadyJumped].


isJumped(C-R, List) :-
    member(C-R, List).

/*
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
*/


% ------------------------ Check Winner ------------------------------
game_over(GameState, Winner):-
    [Board, Player, AlreadyJumped] = GameState,
    (is_winner(Board, player1) -> Winner = player1 ; is_winner(Board, player2) -> Winner = player2 ; Winner = none).

show_winner(Winner):-
    name_of(Winner, Name),
    write('\n------------------------\n'),
    format('Winner is ~a!\n', [Name]),
    write('------------------------\n').

is_winner(Board, Player):-
    player_color(Player, Piece),
    check(Board, Piece).

check(Board, Piece):-
    find_player_pieces(Board, Piece, Pieces),
    see_all(Pieces, Board, Piece).

find_player_pieces(Board, Piece, Pieces):-
    findall(Col-Row, position(Board, Col-Row, Piece), Pieces).

see_all([], _, _).
see_all([H|Res], Board, Piece):-
    C-R = H,
    adjacent_positions(C-R, AdjacentPositions),
    remove_coordinates_outside_range(AdjacentPositions, Filtered, Board),
    check_all_adjacent(Filtered, Board, Piece, 0, Acc),
    Acc < 1,
    see_all(Res, Board, Piece).
    
adjacent_positions(C-R, AdjacentPositions) :-
    C1 is C - 1, R1 is R - 1,
    C2 is C, R2 is R - 1,
    C3 is C + 1, R3 is R - 1,
    C4 is C - 1, R4 is R,
    C5 is C + 1, R5 is R,
    C6 is C - 1, R6 is R + 1,
    C7 is C, R7 is R + 1,
    C8 is C + 1, R8 is R + 1,
    AdjacentPositions = [C1-R1, C2-R2, C3-R3, C4-R4, C5-R5, C6-R6, C7-R7, C8-R8].

check_all_adjacent([], _, _, Acc, Acc).
check_all_adjacent([C-R | Rest], Board, Piece, Acc, TotalCount) :-
    (position(Board, C-R, AdjacentPiece), Piece \= AdjacentPiece -> NewAcc is Acc ; NewAcc is Acc + 1),
    check_all_adjacent(Rest, Board, Piece, NewAcc, TotalCount).

remove_coordinates_outside_range([], [], Board).
remove_coordinates_outside_range([C-R | Rest], Filtered, Board) :-
    (in_bounds(Board, C-R)) ->
        Filtered = [C-R | NewRest], remove_coordinates_outside_range(Rest, NewRest, Board) ; remove_coordinates_outside_range(Rest, Filtered, Board).
