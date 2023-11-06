# PFL Project 1
## Game and Group
The game developed was Apart, by group Apart_6.
Group:
- Francisco Silva Cunha Campos (up202108735)
- José Henrique Nogueira Campos de Morais Pinheiro (up20218879)

## Installation and Execution
To install this version of Apart, you first need to download and unzip the files in PFL_TP1_T08_Apart_6.ZIP.
Then go to the _src_ directory and consult the **_main.pl_** file using the SICStus terminal. Finally, start the game by calling the predicate **_play/0_**.

## Description of the game
Apart is a board game for tow players originally played on a 8x8 board with white and black pieces.
There are two types of pieces, white and black, and the player who gets white is the first to play.
The pieces are grouped as follows:

<img src="https://cdn.shopify.com/s/files/1/0578/3502/8664/files/Apart1_300x300.png?v=1682247247" >

<br>
The aim is to move the pieces so that none of their pieces are vertically, horizontally, or diagonally adjacent to each other.

The players play alternately in sequences of moves, which can be of two types:
- single step to adjacent positions
- jumps to more distant squares

The jumps are limited by the length of the line, which is defined by the number of pieces belonging to that line. Furthermore, in a sequence of jumps, the piece to be played cannot land on the same square more than once.

The game rules were consulted [here](https://kanare-abstract.com/en/pages/apart).

## Game Logic
### Internal Game State Representation
The _GameState_ variable is divided in 3 elements: <br>
**Board:** It's a matrix with the dimensions chosen by the player who set up the game at the start. The initial board is completely designed by us, with 3 types of filling that will be replaced by their respective symbols when printed: 
- _empty_ = ' '
- _white_ = 'W'
- _black_ = 'B'

As the game develops, the programme updates the board.
**Player:** Identifies the player who is playing that turn.
**AlreadyJumped:** It's a list, cleared each time a player changes, with the coordinates of the squares already trodden on by the player in that turn, since any move towards them cannot be made.

### Game State Visualization
Before they start playing, one of the players will have to make the necessary settings for the following parameters:
- Game Mode: Human vs Human, Human vs Bot or Bot vs Bot
- Players' names
- Level of difficulty of bot: greaedy or random
- Which of the two players plays with the white pieces
- Board's size
<br><br>

### Move Validation and Execution
After the settings, the game enters a place in the **_game_cyle/1_** predicate. Here, **_choose_move/2_** is called, where **_get_move/3_** will ask for the coordinates of the piece the player wants to play as well as the coordinates of where they want to move it to. Each choice is passed to **_validate_move_normal/2_** where the following conditions are checked:
- coordinates within the board
- existence of a piece of the same type in the final position
- validate that the move is possible given the size of the line.

After this, **_move/3_** is in charge of drawing the new state. In **_jump_mode/2_** it distinguishes whether the move is a single step or a jump, and if it is the latter it validates it and enters the possibility of jumping as many times as the player wants. At the end of the move, call **_game_cycle/1_** again with NewGameState, check that there is a winner with **_game_over/2_** and print the board using **_display_game/2_**.

### List of Valid Moves
When a bot is playing, it needs to know all the possible moves that can be made given the state the game is in. Using **_valid_moves/3_** you get a list of all the moves that are valid for that play.

### End of Game
The game ends when all the pieces of one of the players are not adjacent to each other.
By calling **_game_over/2_**, this condition is checked using the predicate **_is_winner_** which checks, for both players, that each of their pieces on the current board is free of adjacencies. The player who fulfils this condition is declared the winner of the game.

### Game State Evaluation
The game can be evaluated using the **_value/3_** predicate, used when we are at the greedy level.
This returns a value that reflects the sum of the number of adjacencies each piece has with its equals. To do this, it uses **_find_player_pices/3_**, which returns a list with the coordinates of all the pieces on the player's board, and **_count_adjacents_**, which performs the calculations. In Apart, the winner is the one who scores first 0 points.

### Computer Plays
There are two types of level implemented in computer plays, random and greedy respectively.
<br>
**Level 1:** the bot randomly chooses the combination (Start_column-Start_row , End_column-End_row). If this move represents a jump, it will also randomly decide whether to continue the move.
<br>
**Level 2:** the bot proceeds with a greedy approach (minimax), choosing the next move, trying to minimise your score and maximise your opponent's. The score refers to the sum of the adjacencies of the same player's pieces.

## Conclusion
During the course of the project, the only question that emerged was in relation to the pieces that had to be taken into account when calculating the line size for the jump, leading us to two interpretations:
- taking into account only adjacent pieces
- taking into account all the pieces on the line from one end of the board to the other

The two dynamics were developed and experimented with and, in the end, it was decided to leave only the first way of playing.

In addition to this, another question arose in the finalisation phase, relating to the nomenclature of the requested predicates. Initially, the development of the game was not based on them, in view of their change. However, later on, it became a little difficult to completely fulfil the requested names. However, changes were made so that we could fulfil the request as much as possible.

Aside from this, it can be considered that the implementation of the Apart game with its basic rules, as well as the three modes requested for this project, was a success.
