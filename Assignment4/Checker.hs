{-
   Tulga Ariuntuya, Naga Charan.

   Checker board modeling.
-}
module Checker where

import qualified Data.List as List

data Board = Board { board::[[Cell]] }

-- | Piece on the board. It can either be Red or Black.
data Color 
    = Red 
    | Black deriving (Eq)

data Piece
    = Single Color
    | Double Color deriving (Eq)

-- | Cell information on the board. Cell can be empty or have one of the pieces.
data Cell 
    = Empty 
    | C Piece deriving (Eq)

-- | Player. The piece player is playing with.
-- Red player can move piece only downwards
-- Black player can move piece only upwards
data Player = Player Color

-- | Players initialized.
red_player   = Player Red
black_player = Player Black

-- | Description of moves.
data Move 
    -- | Move can include number of positions.
    -- first position is always initial position.
    = Step Pos [Pos]

-- | The 'initialBoard' returns initial setting of the Checker board.
initialBoard :: Board
initialBoard = Board { board = [ 
                        [ e, r, e, r, e, r, e, r],
                        [ r, e, r, e, r, e, r, e],
                        [ e, r, e, r, e, r, e, r],
                        [ e, e, e, e, e, e, e, e],
                        [ e, e, e, e, e, e, e, e],
                        [ b, e, b, e, b, e, b, e],
                        [ e, b, e, b, e, b, e, b],
                        [ b, e, b, e, b, e, b, e]
                   ]}

e = Empty
r = C (Single Red)
b = C (Single Black)

instance Show Board where
    show = showBoard

instance Show Move where
    show = showMove

instance Show Player where
    show (Player Red)   = "Red"
    show (Player Black) = "Black"

showMove :: Move -> String
showMove (Step p0 ps) = List.intercalate "->" (map show (p0:ps))

showBoard :: Board -> String
showBoard (Board { board=b }) = let boardDisp = concat $ map (unlines . showRow) b
                                    boardBott = takeWhile (/='\n') boardDisp
                                 in boardDisp++boardBott

showRow :: [Cell] -> [String]
showRow []     = "+":repeat "|"
showRow (c:cs) = joinRows (showOnBoard c) (showRow cs)
    where joinRows :: [String] -> [String] -> [String]
          joinRows []       _        = []
          joinRows (r0:rs0) (r1:rs1) = (r0++r1):(joinRows rs0 rs1)

showOnBoard :: Cell -> [String]
showOnBoard Empty     = ["+---", 
                         "|   ",
                         "|   ",
                         "|   "]
showOnBoard (C (Single Red))   = ["+---", 
                                  "| X ",
                                  "|X X",
                                  "| X "]
showOnBoard (C (Single Black)) = ["+---", 
                                  "| O ",
                                  "|O O",
                                  "| O "]
showOnBoard (C (Double Red))   = ["+---", 
                                  "| X ",
                                  "|XXX",
                                  "| X "]
showOnBoard (C (Double Black)) = ["+---", 
                                  "| O ",
                                  "|OOO",
                                  "| O "]

-- | Position on the board.
type Pos = (Int, Int)

