{-
   Tulga Ariuntuya, Naga Charan.

   Checker board modeling.
-}
module Game where

import Parsers (pmove)
import Parsing (parse)
import Checker

import qualified Data.List as List

-- | The example board
example0 :: Board
example0 = Board { board = [ 
                        [ e, e, e, e, e, e, e, e],
                        [ e, e, e, e, e, e, e, e],
                        [ e, e, r, e, r, e, e, e],
                        [ e, e, e, b, e, e, e, e],
                        [ e, e, e, e, r, e, r, e],
                        [ e, e, e, r, e, e, e, e],
                        [ r, e, r, e, e, e, r, e],
                        [ e, b, e, e, e, e, e, b]
                   ]}

-- | The example board
example1 :: Board
example1 = Board { board = [ 
                        [ e, e, e, e, e, e, e, e],
                        [ e, e, e, e, e, e, e, e],
                        [ e, e, r, e, r, e, e, e],
                        [ e, e, e, e, e, e, e, e],
                        [ e, e, b, e, r, e, r, e],
                        [ e, e, e, r, e, e, e, e],
                        [ r, e, r, e, e, e, r, e],
                        [ e, e, e, e, e, e, e, b]
                   ]}

-- | The 'cells' function takes board and returns list of cell information.
cells :: Board -> [(Pos, Cell)]
cells (Board b) = concat $ map (\(r, row) -> map (\(c, val) -> ((r, c), val)) (zip [0..] row)) (zip [0..] b)

-- | The 'pieces' function takes color and board. Returns positions of pieces with same color.
pieces :: Color -> Board -> [Pos]
pieces c b = [ pos | (pos, cell) <- cells b, cell==(C (Single c)) || cell==(C (Double c)) ]

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
oppositeColor :: Color -> Color
oppositeColor Red   = Black
oppositeColor Black = Red

-- | The 'pieceMoves' function takes position returns next possible positions
pieceMoves :: Player -> Board -> Pos -> [Move]
pieceMoves pl@(Player pi) b p = steps ++ captures
  where steps       = filter (isValidStep b pl) allSteps 
        allSteps    = [ (Step p [p1]) | p1 <- (applyVec p (dirVec pi)) ]
        captures    = filter (isValidCapture b pl) allCaptures
        allCaptures = [ (Step p [p2]) | (p1, p2) <- (zip oneStep twoStep) ]
        oneStep     = applyVec p allDir
        twoStep     = applyVec p $ map (\(x, y) -> (2*x, 2*y)) allDir

-- | All 4 direction vectors
allDir :: [(Int, Int)]
allDir = redDirs ++ blackDirs

-- | Direction vector
dirVec :: Piece -> [(Int, Int)]
dirVec (Single Red)   = redDirs
dirVec (Single Black) = blackDirs
dirVec (Double _)     = allDir

nextSteps :: Piece -> Pos -> [Pos]
nextSteps pi@(Single _) p = applyVec p (dirVec pi)
nextSteps pi@(Double _) p = applyVec p (dirVec pi)

-- | Generic integer generator. Can accept negative.
-- Example:
-- range 3 -1 returns [3, 2, 1, 0, -1]
range :: Int -> Int -> [Int]
range a b 
  | a > b     = a:(range (a-1) b)
  | a < b     = a:(range (a+1) b)
  | otherwise = a:[]

-- | Take two points and return all points between
-- these two (inclusive).
positionsBetween :: Pos -> Pos -> [Pos]
positionsBetween (r0, c0) (r1, c1) = zip (range r0 r1) (range c0 c1)

isValidCapture :: Board -> Player -> Move -> Bool
isValidCapture b p@(Player pi) (Step p0 (p1:ps)) 
  | ps == []  = isValidPos p1 && 
                isEmptyAt p1 b && 
                isDiagonal p0 p1 && 
                numOfReds==1 && 
                numOfBlacks==1 && 
                cellAt p0 b==(C pi)
  | otherwise = isValidCapture b p (Step p0 [p1]) && isValidCapture b' p (Step p1 ps)
    where numOfReds   = length $ filter (\p -> cellAt p b==(C Red)) (positionsBetween p0 p1)
          numOfBlacks = length $ filter (\p -> cellAt p b==(C Black)) (positionsBetween p0 p1)
          b'          = makeCapture b p (Step p0 [p1])

isDiagonal :: Pos -> Pos -> Bool
isDiagonal (r0, c0) (r1, c1) = abs (r0-r1) == abs (c0-c1)
          
isValidStep :: Board -> Player -> Move -> Bool
isValidStep b (Player pi) (Step p0 (p1:[])) = isValidPos p1 && 
                                              isEmptyAt p1 b && 
                                              cellAt p0 b==(C pi) &&
                                              List.elem p1 (applyVec p0 (dirVec pi))
isValidStep b (Player pi) (Step p0 (p1:ps)) = False

-- | Vector for capture directions
captureDir = [(-1, -1), (-1, 1), (1, 1), (1, -1)]
-- | Vector for steps of black pieces
blackDirs = [(-1, -1), (-1,  1)]
-- | Vector for steps of red pieces
redDirs   = [( 1,  1), ( 1, -1)]

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
playerMoves :: Board -> Player -> [Move]
playerMoves b pl@(Player pi) = concat $ map (pieceMoves pl b) (pieces pi b)

-- | Check if game is finished for given player
isGameFinished :: Player -> Board -> Bool
isGameFinished p b = 0 == (length $ playerMoves b p)

-- | Start game loop, asks for moves from players alternating
loopGame :: Board -> (Player, Player) -> IO ()
loopGame board (p0, p1) = do
    putStrLn $ show board
    putStr $ "Turn of " ++ show p0 ++ ": "
    move <- getMove board p0
    let newBoard = makeMove board p0 move
    case isGameFinished p1 newBoard of
      True  -> putStrLn $ show p0 ++ " has won the game!"
      False -> loopGame newBoard (p1, p0)

-- | Ask for move until user gives valid move.
-- Valid input would be number a and b, then if move includes
-- number of moves it needs to be separated by dash.
-- Example:
--      5 5 - 6 6       .- Simple diagonal step
--      0 1 - 2 3 - 4 5 .- Captures
getMove :: Board -> Player -> IO Move
getMove b pl@(Player pi) = do
    l <- getLine
    case parse pmove l of
      Just (m, "") -> if isValidMove b pl m then return m 
                                            else do putStrLn "Not valid move" 
                                                    getMove b pl
      _            -> do putStrLn "Couldn't parse move"
                         getMove b pl

-- | Check if move is valid
isValidMove :: Board -> Player -> Move -> Bool
isValidMove b p m = isValidStep b p m || isValidCapture b p m

makeMove :: Board -> Player -> Move -> Board
makeMove b p m
  | isValidStep    b p m = makeStep b p m
  | isValidCapture b p m = makeCapture b p m

makeStep :: Board -> Player -> Move -> Board
makeStep b p m@(Step p0 (p1:[])) = updateBoard b [(p0, Empty), (p1, cellAt p0 b)]

makeCapture :: Board -> Player -> Move -> Board
makeCapture b p m@(Step p0 (p1:ps))
  | ps == []  = updateBoard b' [(p1, (cellAt p0 b))]
  | otherwise = makeCapture (updateBoard b' [(p1, cellAt p0 b)]) p (Step p1 ps)
    where b' = updateBoard b (map (\pos -> (pos, Empty)) (positionsBetween p0 p1))

main :: IO ()
main = do
    putStrLn " Welcome to Checker game. \n\
             \ Top left cell of the board is (0,0) and bottom right is (7,7). \n\
             \ You need to enter moves in following format: \n\
             \ row column - row column \n\
             \ Or if you want to make several moves: \n\
             \ row column - row column - row column ...\n"
    loopGame example1 (black_player, red_player)
