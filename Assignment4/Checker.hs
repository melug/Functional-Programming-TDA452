{-
   Tulga Ariuntuya, Naga Charan.

   Checker board modeling.
-}
module Main (main) where

import qualified Data.List as List

data Board = Board { board::[[Cell]] }

-- | Piece on the board. It can either be Red or Black.
data Piece 
    = Red 
    | Black deriving (Eq)

-- | Cell information on the board. Cell can be empty or have one of the pieces.
data Cell 
    = Empty 
    | C Piece deriving (Eq)

-- | Player. The piece player is playing with.
-- Red player can move piece only downwards
-- Black player can move piece only upwards
data Player = Player Piece

-- | Players initialized.
red_player   = Player Red
black_player = Player Black

-- | Description of moves.
data Move 
    = NoMove 
    -- | Single diagonal move
    | Step Pos Pos 
    -- | Capturing move.
    | Jump Pos [Pos]

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
    where e = Empty
          r = C Red
          b = C Black

-- | The example board
example0 :: Board
example0 = Board { board = [ 
                        [ e, e, e, e, e, e, e, e],
                        [ e, e, e, e, e, e, e, e],
                        [ e, e, r, e, r, e, e, e],
                        [ e, e, e, e, e, e, e, e],
                        [ e, e, e, e, r, e, r, e],
                        [ e, e, e, r, e, e, e, e],
                        [ r, e, r, e, e, e, r, e],
                        [ e, b, e, e, e, e, e, b]
                   ]}
    where e = Empty
          r = C Red
          b = C Black

-- | The example board
example1 :: Board
example1 = Board { board = [ 
                        [ e, e, e, e, e, e, e, e],
                        [ e, e, e, e, e, e, e, e],
                        [ e, e, r, e, r, e, e, e],
                        [ e, e, e, e, e, e, e, e],
                        [ e, e, b, e, r, e, r, e],
                        [ e, b, e, r, e, e, e, e],
                        [ r, e, r, e, e, e, r, e],
                        [ e, b, e, e, e, e, e, b]
                   ]}
    where e = Empty
          r = C Red
          b = C Black

instance Show Board where
    show = showBoard

instance Show Move where
    show = showMove

showMove :: Move -> String
showMove NoMove       = "X"
showMove (Step p0 p1) = show p0 ++ "->" ++ show p1
showMove (Jump p0 ps) = List.intercalate "->" (map show (p0:ps))

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
showOnBoard (C Red)   = ["+---", 
                         "| X ",
                         "|XXX",
                         "| X "]
showOnBoard (C Black) = ["+---", 
                         "| O ",
                         "|O O",
                         "| O "]

-- | Position on the board.
type Pos = (Int, Int)

-- | The 'cells' function takes board and returns list of cell information.
cells :: Board -> [(Pos, Cell)]
cells (Board b) = concat $ map (\(r, row) -> map (\(c, val) -> ((r, c), val)) (zip [0..] row)) (zip [0..] b)

-- | The 'pieces' function takes piece and board. Returns positions of pieces with same color.
pieces :: Piece -> Board -> [Pos]
pieces p b = [ pos | (pos, cell) <- cells b, cell==(C p) ]

-- | Returns cell information at given position.
cellAt :: Pos -> Board -> Cell
cellAt (r, c) (Board b) = head $ drop c $ head $ drop r b

-- | Check if given position is empty
isEmptyAt :: Pos -> Board -> Bool
isEmptyAt p b = cellAt p b == Empty

-- | Check if given position is out of bounds of the board.
isValidPos :: Pos -> Bool
isValidPos (r, c) = 0 <= r && r <= 7 && 0 <= c && c <= 7

-- | Return opposite piece
oppositePiece :: Piece -> Piece
oppositePiece Red   = Black
oppositePiece Black = Red

-- | The 'pieceMoves' function takes position returns next possible positions
pieceMoves :: Player -> Board -> Pos -> [Move]
pieceMoves (Player pi) b (r, c) 
-- | Check if player has his piece on given position
  | cellAt (r, c) b /= (C pi) = error "Not player's piece"
  | pi == Black               = steps blackSteps ++ jumps
  | pi == Red                 = steps redSteps ++ jumps
  where blackSteps = [(-1, -1), (-1,  1)]
        redSteps   = [( 1,  1), ( 1, -1)]
        steps dirs = map (Step (r, c)) $ filter (\p -> isValidPos p && isEmptyAt p b) (applyVec (r, c) dirs)
        jumps      = map (\(p0, p1) -> Jump (r, c) [p1]) $ filter validCapture captures
        captureDir = [(-1, -1), (-1, 1), (1, 1), (1, -1)]
        captures   = zip dirOneStep dirTwoStep
        dirOneStep = applyVec (r, c) captureDir
        dirTwoStep = applyVec (r, c) $ map (\(x, y) -> (2*x, 2*y)) captureDir
        validCapture = (\(p0, p1) -> isValidPos p1 && isEmptyAt p1 b && cellAt p0 b==(C (oppositePiece pi))) 

-- | From given position and direction vector, return next positions.
-- Example: (3, 3) -> [(-1, -1), (-1, +1)] -> [(2, 2), (2, 4)]
applyVec :: Pos -> [(Int, Int)] -> [Pos]
applyVec (r,c) []           = []
applyVec (r,c) ((dr,dc):ds) = (dr+r,dc+c):applyVec (r,c) ds

-- | Updates board, at given position change value to cell
updateBoard :: Board -> [(Pos, Cell)] -> Board
updateBoard b us = foldl updateCell b us

-- | Update cell
updateCell :: Board -> (Pos, Cell) -> Board
updateCell (Board b) ((r, c), cell) = Board { board=topRows ++ [updatedRow] ++ bottomRows }
    where topRows    = take r b
          updatedRow = let row   = head $ drop r b
                           left  = take c row
                           right = drop (c+1) row
                        in left++[cell]++right
          bottomRows = drop (r+1) b

-- | The 'playerMoves' function returns possible moves for given player.
-- Return type is (Pos, Pos), first element is initial position
-- last element is target position.
playerMoves :: Player -> Board -> [Move]
playerMoves pl@(Player pi) b = concat $ map (pieceMoves pl b) (pieces pi b)

main :: IO ()
main = undefined
