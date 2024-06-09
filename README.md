# NPRG005-credit-program

## How to build?
- Clone this repository
- Open terminal, cd to the repository
- Run `ghc Dames.hs`

## How to play?
- Run `Dames.exe`
- Enter the number of players:
    - `1` for singleplayer
    - `2` for multiplayer
- If you are playing singleplayer, enter the AI difficulty:
    - `1` for easy mode
    - `2` for normal mode
    - `3` for hard mode
- You will be prompted to enter your first move. Enter moves in the following format:
    - `B6 A5`, `B6 D4 B2`, `A8 D5 B3 C1` etc.
    - moves longer than one (i.e.2 coordinates) must only contain jumps

## What are the rules?
- there are 2 types of pieces - stones and dames
- all pieces can only move diagonally
- stones may only move forward
- dames may move in any (diagonal) direction
- unless skipping, stones may only move by 1
- dames may make moves of any length
- stones may skip pieces of the other color if they are in a line adjacent to the skipping stone's position
- dames may skip piecces of the other colors as long as the move is diagonal.
- every move must end on an empty field
- multiple skips may be performed after one another if possible
- IF A PLAYER CAN SKIP, THEY MUST DO SO
    - if they can skip in multiple ways, they may choose which one
- the game ends by a player winning if:
    - the other player has no more pieces left
    - all the pieces of the other player are blocked and can not move