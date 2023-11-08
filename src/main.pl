:- use_module(library(lists)).
:- use_module(library(random)).
:- consult(settings).
:- consult(utils).



% ------------------------ Game basics ------------------------------

% play/0
% Starts the game
play:- settings(GameState), !, game_cycle(GameState), !, clear_data.


% game_cycle(+GameState)
% Main Loop 
game_cycle(GameState):-
    game_over(GameState, Winner), 
    Winner \= none, !,
    display_game(GameState),
    show_winner(Winner), !.
game_cycle(GameState):-
    display_game(GameState),
    user_turn(GameState),
    choose_move(GameState, Move), !,
    move(GameState, Move, NewGameState), !,
    jump_mode(NewGameState, JumpState), !,
    change_players(JumpState, FinalGameState),
    game_cycle(FinalGameState).


% display_game(+GameState)
% Prints the board
display_game([Board,_,_]) :-
    % clear_console,
    length(Board, Size),
    display_header(1, Size),
    display_bar(Size),
    display_rows(Board, 1, Size).
    

% choose_move(+GameState,-Move)
% Choose move a human player
choose_move([Board, Player, AlreadyJumped], CI-RI-CF-RF):-
    \+difficulty_of(Player, _),
    get_move(Board, ColI-RowI, ColF-RowF),
    ((validate_move([Board, Player, AlreadyJumped], ColI-RowI-ColF-RowF), CI is ColI, RI is RowI, CF is ColF, RF is RowF);
    (\+validate_move([Board, Player, AlreadyJumped], ColI-RowI-ColF-RowF),
    write('The selected move is not valid, please try again!\n'),
    choose_move([Board, Player, AlreadyJumped], CI-RI-CF-RF))).
    
choose_move([Board, Player, AlreadyJumped], Move):-
    difficulty_of(Player, Level),                  
    choose_move([Board,Player,AlreadyJumped], Player, Level, Move), !.


% user_turn(+GameState)
% Prints the player name
user_turn([_, Player, _]):-
    name_of(Player, Name),
    format('Player ~a, is your turn!\n', [Name]), !.

    

% ------------------------ Move Logic and Validation ------------------------------

% jump_mode(+GameState, -JumpState)
% Checks if the player can jump again. If it is able to jump, the player can choose if it wants to do so or not.
% For random bot, it randomly chooses if it wants to jump or not. For greedy bot, it chooses the best option.
jump_mode([Board, Player, AlreadyJumped], JumpState):-
    empty_list(AlreadyJumped, true),
    JumpState = [Board, Player, AlreadyJumped].

jump_mode([Board, Player, AlreadyJumped], JumpState):-
    empty_list(AlreadyJumped, false), [LastMove|_] = AlreadyJumped,
    \+jump_possible([Board, Player, AlreadyJumped], LastMove),
    JumpState = [Board, Player, AlreadyJumped].

jump_mode([Board, Player, AlreadyJumped], JumpState):-
    \+difficulty_of(Player, _),
    empty_list(AlreadyJumped, false), [LastMove|_] = AlreadyJumped,
    jump_possible([Board, Player, AlreadyJumped], LastMove),
    ask_to_jump(Choice),
    ((Choice == 110, JumpState = [Board, Player, AlreadyJumped]);
    (Choice == 121,
    game_cycle([Board, Player, AlreadyJumped])
    )).

jump_mode([Board, Player, AlreadyJumped], JumpState):-
    difficulty_of(Player, 1),
    empty_list(AlreadyJumped, false), [LastMove|_] = AlreadyJumped,
    jump_possible([Board, Player, AlreadyJumped], LastMove),
    Choices = [121, 110],
    random_item(Choices, Choice),
    ((Choice == 110, JumpState = [Board, Player, AlreadyJumped]);
    (Choice == 121,
    game_cycle([Board, Player, AlreadyJumped])
    )).

jump_mode([Board, Player, AlreadyJumped], JumpState):-
    difficulty_of(Player, 2),
    empty_list(AlreadyJumped, false), [LastMove|_] = AlreadyJumped,
    jump_possible([Board, Player, AlreadyJumped], LastMove),
    greedy_choice([Board, Player, AlreadyJumped], LastMove, Choice),
    ((Choice == 110, JumpState = [Board, Player, AlreadyJumped]);
    (Choice == 121,
    game_cycle([Board, Player, AlreadyJumped])
    )).

% greedy_choice(+GameState, +LastMove, -Choice)
% Chooses the best option for the greedy bot.
greedy_choice(GameState, LastMove, Choice):-
    [_, Player, _] = GameState,
    valid_moves_piece(GameState, LastMove, Moves),
    value(GameState, Player, ValueNow),
    findall(Value-Move, (member(Move, Moves), 
        move(GameState, Move, NewGameState), 
        value(NewGameState,Player, ValueNext),
        Value is ValueNext - ValueNow), Pairs),
    sort(Pairs, SortedPairs),
    [Diff-_|_] = SortedPairs,
    (Diff < 0 -> Choice is 121; Choice is 110).
    

% ask_to_jump(-Choice)
% Asks the player if it wants to jump again. y - 121, n - 110.
ask_to_jump(Choice):-
    write('Do you want to continue jumping (y/n): '),
    repeat,
    get_code(Choice),
    member(Choice, [121, 110]), !.

% jump_possible(+GameState, +LastMove)
% Checks if the player can jump again.
jump_possible([Board, Player, AlreadyJumped], CI-RI):-
    valid_moves_piece([Board, Player, AlreadyJumped], CI-RI, _).

% valid_moves(+GameState, +Player, -ListOfMoves)
% List all possible moves for a player.
valid_moves(GameState, Player, ListOfMoves):-
    [Board,_,AlreadyJumped] = GameState,
    findall(CI-RI-CF-RF, validate_move([Board, Player, AlreadyJumped],CI-RI-CF-RF),ListOfMoves), !.

% valid_moves_piece(+GameState, +CI-RI, -ListOfMoves)
% List all possible moves for a piece.
valid_moves_piece(GameState, CI-RI, ListOfMoves):-
    findall(CI-RI-CF-RF, validate_move(GameState,CI-RI-CF-RF),ListOfMoves),
    \+length(ListOfMoves, 0), !.



% validate_move(+GameState, +CI-RI-CF-RF)
% Checks if a move is valid.
validate_move([Board, Player, AlreadyJumped], CI-RI-CF-RF):-
    position(Board, CI-RI, Piece),
    player_color(Player, Piece), 
    in_bounds(Board,CF-RF),
    obstructed(Board, CI-RI-CF-RF),
    get_direction(CI-RI-CF-RF, Direction, JumpSize),
    check_jump_size(CI-RI, Board, Player, Direction, RealJumpSize), 
    JumpSize =:= RealJumpSize,
    (empty_list(AlreadyJumped,true); (empty_list(AlreadyJumped,false), RealJumpSize > 1, selected_previous_piece(AlreadyJumped, CI-RI))),
    \+already_jumped(CF-RF, AlreadyJumped).

% selected_previous_piece(+AlreadyJumped, +CI-RI)
% Checks if the player selected the piece that jumped before.
selected_previous_piece(AlreadyJumped, CI-RI):-
    [LastMove|_] = AlreadyJumped,
    CLast-RLast = LastMove,
    CI == CLast, RI == RLast.

% check_jump_size(+CI-RI, +Board, +Player, +Direction, -RealJumpSize)
% Checks if the jump size is valid.
check_jump_size(CI-RI, Board, Player, 1, RealJumpSize):-
    ColBack is CI - 1, ColFront is CI + 1,
    player_color(Player, PlayerColor),
    ((position(Board, ColBack-RI, PlayerColor), position(Board, ColFront-RI, PlayerColor), RealJumpSize is 3);
    (\+position(Board, ColBack-RI, PlayerColor), position(Board, ColFront-RI, PlayerColor), RealJumpSize is 2);
    (position(Board, ColBack-RI, PlayerColor), \+position(Board, ColFront-RI, PlayerColor), RealJumpSize is 2);
    (\+position(Board, ColBack-RI, PlayerColor), \+position(Board, ColFront-RI, PlayerColor), RealJumpSize is 1)).

check_jump_size(CI-RI, Board, Player, 2, RealJumpSize):-
    RowBack is RI - 1, RowFront is RI + 1,
    player_color(Player, PlayerColor),
    ((position(Board, CI-RowBack, PlayerColor), position(Board, CI-RowFront, PlayerColor), RealJumpSize is 3);
    (\+position(Board, CI-RowBack, PlayerColor), position(Board, CI-RowFront, PlayerColor), RealJumpSize is 2);
    (position(Board, CI-RowBack, PlayerColor), \+position(Board, CI-RowFront, PlayerColor), RealJumpSize is 2);
    (\+position(Board, CI-RowBack, PlayerColor), \+position(Board, CI-RowFront, PlayerColor), RealJumpSize is 1)).

check_jump_size(CI-RI, Board, Player, 3, RealJumpSize):-
    ColBack is CI - 1, ColFront is CI + 1,
    RowBack is RI - 1, RowFront is RI + 1,
    player_color(Player, PlayerColor),
    ((position(Board, ColBack-RowBack, PlayerColor), position(Board, ColFront-RowFront, PlayerColor), RealJumpSize is 3);
    (\+position(Board, ColBack-RowBack, PlayerColor), position(Board, ColFront-RowFront, PlayerColor), RealJumpSize is 2);
    (position(Board, ColBack-RowBack, PlayerColor), \+position(Board, ColFront-RowFront, PlayerColor), RealJumpSize is 2);
    (\+position(Board, ColBack-RowBack, PlayerColor), \+position(Board, ColFront-RowFront, PlayerColor), RealJumpSize is 1)).

check_jump_size(CI-RI, Board, Player, 4, RealJumpSize):-
    ColBack is CI - 1, ColFront is CI + 1,
    RowBack is RI - 1, RowFront is RI + 1,
    player_color(Player, PlayerColor),
    ((position(Board, ColBack-RowFront, PlayerColor), position(Board, ColFront-RowBack, PlayerColor), RealJumpSize is 3);
    (\+position(Board, ColBack-RowFront, PlayerColor), position(Board, ColFront-RowBack, PlayerColor), RealJumpSize is 2);
    (position(Board, ColBack-RowFront, PlayerColor), \+position(Board, ColFront-RowBack, PlayerColor), RealJumpSize is 2);
    (\+position(Board, ColBack-RowFront, PlayerColor), \+position(Board, ColFront-RowBack, PlayerColor), RealJumpSize is 1)).

% get_direction(+CI-RI-CF-RF, -Direction, -JumpSize)
% Gets the direction of the move and the jump size asked by the player.
% Direction: 1 - Horizontal; 2 - Vertical; 3 - Diagonal (\); 4 - Diagonal (//).
get_direction(CI-RI-CF-RF, Direction, JumpSize):-
    (RI == RF, Direction is 1, Diff is CF - CI, abs(Diff, A), JumpSize is A);
    (CI == CF, Direction is 2, Diff is RF - RI, abs(Diff, B), JumpSize is B);
    ((CF - CI) =:= (RF - RI), Diff is CF - CI, Direction is 3, abs(Diff, C), JumpSize is C);
    ((CF - CI) =:= -(RF - RI), Diff is CF - CI, Direction is 4, abs(Diff, D), JumpSize is D).


% obstructed(+Board, +Coordinates)
% Checks if there is already a player piece in the position you want to move to.
obstructed(Board, CI-RI-CF-RF):-
    position(Board, CI-RI, CurrPiece),
    position(Board, CF-RF, NextPlace),
    (NextPlace \= CurrPiece).


% change_players(+GameState, -NewGameState)
% Change the player in the new state
change_players([Board, Player, _], [Board, NewPlayer, []]) :-
    player_change(Player, NewPlayer).


% move(+GameState, +Move, -NewGameState)
% Moves a piece
move(GameState, Move, NewGameState):-
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

% already_jumped(+Coordinates, +List)
% Checks if the player already jumped to a given position.
already_jumped(C-R, List) :-
    member(C-R, List).




% ------------------------ Check Winner ------------------------------

% game_over(+GameState, +Winner)
% Checks if the game is over
game_over(GameState, Winner):-
    [Board, _, _] = GameState,
    (is_winner(Board, player1) -> Winner = player1 ; is_winner(Board, player2) -> Winner = player2 ; Winner = none).


% show_winner(+Winner)
% Print the winner.
show_winner(Winner):-
    name_of(Winner, Name),
    write('\n------------------------\n'),
    format('Winner is ~a!\n', [Name]),
    write('------------------------\n').


% is_winner(+Board, +Player)
% Check if Player wins.
is_winner(Board, Player):-
    player_color(Player, Piece),
    check(Board, Piece).


% check(+Board, +Piece)
% check winning condition for a given type of pieces (Piece).
check(Board, Piece):-
    find_player_pieces(Board, Piece, Pieces),
    see_all(Pieces, Board, Piece).


% find_player_pieces(+Board, +Piece, -Pieces)
% List all pieces of a player that are stil on the board.
find_player_pieces(Board, Piece, Pieces):-
    findall(Col-Row, position(Board, Col-Row, Piece), Pieces).


% see_all(+Pieces, +Board, +Pi)
% Check for all pieces of a type (Piece) if all of them have adjacents.
see_all([], _, _).
see_all([H|Res], Board, Piece):-
    C-R = H,
    adjacent_positions(C-R, AdjacentPositions),
    remove_coordinates_outside_range(AdjacentPositions, Filtered, Board),
    check_all_adjacent(Filtered, Board, Piece, 0, Acc),
    Acc < 1,
    see_all(Res, Board, Piece).


% adjacent_positions(+Coordinate, -AdjacentPositions)
% List all possible adjacent positions of a given coordinate.
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


% check_all_adjacent(+Pieces, +Board, +Piece, +Acc, -TotalCount)
% Checks for adjacencies and return the number of pieces that have them.
check_all_adjacent([], _, _, Acc, Acc).
check_all_adjacent([C-R | Rest], Board, Piece, Acc, TotalCount) :-
    (position(Board, C-R, AdjacentPiece), Piece \= AdjacentPiece -> NewAcc is Acc ; NewAcc is Acc + 1),
    check_all_adjacent(Rest, Board, Piece, NewAcc, TotalCount).


% remove_coordinates_outside_range(+Positions, -Filtered, +Board)
% Remove impossible/out of board positions.
remove_coordinates_outside_range([], [], _).
remove_coordinates_outside_range([C-R | Rest], Filtered, Board) :-
    (in_bounds(Board, C-R)) ->
        Filtered = [C-R | NewRest], remove_coordinates_outside_range(Rest, NewRest, Board) ; remove_coordinates_outside_range(Rest, Filtered, Board).


% ----------------------- Bot Functions ------------------------------

% value(+GameState, +Player, -Value)
% Gets the value  for minimax algorithm
value([Board,_,_], Player, Value):-
    player_color(Player, Piece),
    find_player_pieces(Board, Piece, Pieces),
    count_adjacents(Pieces, Board, Piece, 0, Value).


% count_adjacents(+Pieces, +Board, +Piece, +Acc, -Value)
% Sum all the adjacencies of a part type to calculate the value of the minmax algorithm.
count_adjacents([], _, _, Res, Res).
count_adjacents([H|T], Board, Piece, Acc, Res):-
    C-R = H,
    adjacent_positions(C-R, AdjacentPositions),
    remove_coordinates_outside_range(AdjacentPositions, Filtered, Board),
    check_all_adjacent(Filtered, Board, Piece, 0, Acc1),
    NewAcc is Acc + Acc1,
    count_adjacents(T, Board, Piece, NewAcc, Res).

% choose_move(+GameState, +Player, +Level, -Move)
% Choose move for random bot
choose_move(GameState, Player, 1, Move):- 
    valid_moves(GameState, Player, ListOfMoves),
    random_item(ListOfMoves, Move).

% choose_move(+GameState, +Player, +Level, -Move)
% Choose move for greedy bot
choose_move(GameState, Player, 2, CI-RI-CF-RF):-
	valid_moves(GameState, Player, Moves),
    player_change(Player, NewPlayer),
	findall(Value-Move, (member(Move, Moves), 
        move(GameState, Move, NewGameState), 
        value(NewGameState,Player, Value1),
        minimax(NewGameState, NewPlayer, min, 1, Value2),
        Value is Value1 + Value2), Pairs),
    sort(Pairs, SortedPairs),
    [Min-_|_] = SortedPairs,
    findall(ValidMoves, member(Min-ValidMoves, SortedPairs), MinMoves),
    random_member(CI-RI-CF-RF, MinMoves).


% minimax(+GameState, +Player, +Type, +Depth, -Value)
% Minimax algorithm
minimax(_, _, _, 2, 0):- !.
minimax(GameState, Player, MinMax, Depth, Value):-
	player_change(Player, NewPlayer),
	swap_minimax(MinMax, MaxMin),
    NextDepth is Depth + 1,
    [Board,_,_] = GameState,
	valid_moves([Board,_,[]], Player, ListOfMoves),
	setof(Val, (member(Coordinate, ListOfMoves), 
        move(GameState, Coordinate, NewGameState), 
        value(NewGameState,Player,Value1),
        minimax(NewGameState, NewPlayer, MaxMin, NextDepth, Value2), 
        Val is Value1 + Value2), Values),
    minmax_op(MinMax, Values, Value).
