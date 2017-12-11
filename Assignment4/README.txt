Checker game modeling
---------------------

1. Data type for checker board
    - 8x8 board
    - 2 players
    - Each player has 12 discs(colored black and red).
        Here on we'll call red player and black player
    - Initially discs have positions.
    - When single piece reaches furthest row, it's
        crowned and becomes king.

2. Rules for checker board
    - Each player moves 1 disc at a time
    - Discs can only move diagonally in one distance
    - Discs can not move to the position if it's occupied.
        - If occupied position is same color cannot move.
        - If occupied position is opposite, then opposite piece at the position can be captured
        - Piece can capture opposite piece by moving over. (next position should be free)
    - Pieces can move only in forward direction.
    - Player loses if he doesn't have any moves to make. It includes the rule "if player runs out of pieces, player loses".

