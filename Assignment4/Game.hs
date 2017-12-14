{-
   Tulga Ariuntuya, Naga Charan.

   Checker board modeling.
-}
module Game where

import System.Random

import Parsers (pmove)
import Parsing (parse)
import Checker

import qualified Data.List as List

-- | The 'cells' function takes board and returns list of cell information.
cells :: Board -> [(Pos, Cell)]
cells (Board b) = concat $ map (\(r, row) -> map (\(c, val) -> ((r, c), val)) (zip [0..] row)) (zip [0..] b)

-- | The 'pieces' function takes color and board. Returns positions of pieces with same color.
pieces :: Color -> Board -> [Pos]
pieces c b = [ pos | (pos, cell) <- cells b, cell==(C (Single c)) || cell==(C (Double c)) ]

-- | Returns cell information at given position.
cellAt :: Pos -> Board -> Cell
cellAt (r, c) (Board b) = head $ drop c $ head $ drop r b

cellColor :: Pos -> Board -> Maybe Color
cellColor p b = case cellAt p b of
                  (C (Single cc)) -> Just cc
                  (C (Double cc)) -> Just cc
                  _               -> Nothing

cellPiece :: Pos -> Board -> Piece
cellPiece p b = let (C pi) = cellAt p b
                 in pi

-- | Check if given position is empty
isEmptyAt :: Pos -> Board -> Bool
isEmptyAt p b = cellAt p b == Empty

-- | Check if given position is out of bounds of the board.
isValidPos :: Pos -> Bool
isValidPos (r, c) = 0 <= r && r <= 7 && 0 <= c && c <= 7

-- | Return opposite color
oppositeColor :: Color -> Color
oppositeColor Red   = Black
oppositeColor Black = Red

-- | The 'pieceMoves' function takes position returns two
-- list of moves. First list of moves is steps, second list
-- of moves is captures. 
pieceMoves :: Player -> Board -> Pos -> ([Move], [Move])
pieceMoves pl@(Player pi) b p = (steps, captures)
    where steps       = filter (isValidStep b pl) $ allMoves p
          captures    = filter (isValidCapture b pl) $ allMoves p

-- | All 4 direction vectors
allDir :: [(Int, Int)]
allDir = dirs Red ++ dirs Black

-- | Generate all possible moves for given position
allMoves :: Pos -> [Move]
allMoves p = [ (Step p [p1]) | p1 <- nextPositions ]
    where nextPositions = (concat $ map (positionsBetween p) $ map (\(x, y) -> (7*x, 7*y)) allDir) List.\\ [p]

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

-- | Check if given positions lays on diagonal
isDiagonal :: Pos -> Pos -> Bool
isDiagonal (r0, c0) (r1, c1) = abs (r0-r1) == abs (c0-c1)
          
-- | Vector for steps of black pieces
dirs :: Color -> [(Int, Int)]
dirs Red   = [( 1,  1), ( 1, -1)]
dirs Black = [(-1, -1), (-1,  1)]

-- | From given two position make
-- direction vector
dirFromPos :: Pos -> Pos -> (Int, Int)
dirFromPos (r0, c0) (r1, c1) = (makeVec r0 r1, makeVec c0 c1)

makeVec :: Int -> Int -> Int
makeVec a b 
  | a > b     = -1 
  | a < b     = 1 
  | otherwise = 0

-- | Basic validity of move, irrespective to
-- step and capture. Which includes diagonal 
-- test, if player owns the piece, destination
-- position stays on the board.
basicValidMove :: Board -> Player -> Move -> Bool
basicValidMove b p@(Player pc) (Step p0 (p1:ps)) = validPiece && validPos
    where validPiece  = cell' /= Empty && cellColor' == Just pc
          cell'       = cellAt p0 b
          cellColor'  = cellColor p0 b
          validPos    = isValidPos p1 && isEmptyAt p1 b && isDiagonal p0 p1

-- | Filter out valid step moves.
isValidStep :: Board -> Player -> Move -> Bool
isValidStep b p@(Player pc) s@(Step p0 (p1:ps))
-- | No multiple moves allowed
  | ps /= []  = False
  | otherwise = basicValidMove b p s && validPath
  where validPath   = isEmptyPath && isDistanceCorrect && isDirCorrect
        isEmptyPath = all (\p -> cellAt p b==Empty) path
        isDistanceCorrect = case cellPiece p0 b of
                              (Single _) -> length path == 1
                              (Double _) -> True
        path        = (positionsBetween p0 p1 List.\\ [p0])
        isDirCorrect = case cellPiece p0 b of
                         (Single _) -> List.elem (dirFromPos p0 p1) (dirs pc)
                         -- | King can move everywhere
                         (Double _) -> True

isValidCapture :: Board -> Player -> Move -> Bool
isValidCapture b p@(Player pc) s@(Step p0 (p1:ps)) 
  | ps == []  = basicValidMove b p s && validPath
-- | Check series of captures
  | otherwise = isValidCapture b p (Step p0 [p1]) && isValidCapture b' p (Step p1 ps)
    where numOfReds   = length $ filter (\p -> cellColor p b==Just Red) (positionsBetween p0 p1)
          numOfBlacks = length $ filter (\p -> cellColor p b==Just Black) (positionsBetween p0 p1)
          b'          = makeCapture b p (Step p0 [p1])
          validPath   = isPathOk && isDistanceCorrect
          isPathOk    = numOfReds == 1 && numOfBlacks == 1
          isDistanceCorrect = case cellPiece p0 b of
                                (Single _) -> length path == 2
                                (Double _) -> True
          path        = (positionsBetween p0 p1 List.\\ [p0])

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
-- If player has "capture" moves, player must do capture.
playerMoves :: Board -> Player -> [Move]
playerMoves b pl@(Player pi) = let (steps', captures') = (unzip $ map (pieceMoves pl b) (pieces pi b))
                                   steps = concat steps'
                                   captures = concat captures'
                                in if length captures/=0 then captures else steps

-- | Check if game is finished for given player
isGameFinished :: Player -> Board -> Bool
isGameFinished p b = 0 == (length $ playerMoves b p)

-- | Start game loop, asks for moves from players alternating
loopGame :: Board -> (Player, Player) -> Bool -> IO ()
loopGame board (p0, p1) robot = do
    putStrLn $ decorate (show board)
    putStrLn $ "Turn of " ++ show p0 ++ ": "
    move <- if (robot && p0==red_player) then robotMove board p0
                                         else getMove board p0 
    let newBoard = upgradePiece (makeMove board p0 move) (dest move)
    case isGameFinished p1 newBoard of
      True  -> putStrLn $ show p0 ++ " has won the game!"
      False -> loopGame newBoard (p1, p0) robot

-- | Random player
robotMove :: Board -> Player -> IO Move
robotMove b p = do
    let moves = playerMoves b p
    r <- getStdRandom (randomR (0, (length moves)-1))
    return (moves !! r)

-- | Returns destination of step
dest :: Move -> Pos
dest (Step s0 ss) = dest' ss
    where dest' (s:[]) = s
          dest' (s:ss) = dest' ss

-- | Crown piece if the piece reaches
-- other end.
upgradePiece :: Board -> Pos -> Board
upgradePiece b p@(r, c) = case cellAt p b of 
                            (C (Single Black)) -> if r==0 then updateCell b (p, (C (Double Black)))
                                                          else b
                            (C (Single Red))   -> if r==7 then updateCell b (p, (C (Double Red)))
                                                          else b
                            _                  -> b

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
                                            else do putStrLn $ "Not valid move, valid moves are: " ++ validMoves
                                                    getMove b pl
      _            -> do putStrLn "Couldn't parse move"
                         getMove b pl
    where validMoves = List.intercalate ", " $ map show (playerMoves b pl)

-- | Check if move is valid
isValidMove :: Board -> Player -> Move -> Bool
isValidMove b p (Step _ []) = False
isValidMove b p m           = isValid (playerMoves b p) m && (isValidCapture b p m || isValidStep b p m)
-- | Check if given move is in list of moves
    where isValid :: [Move] -> Move -> Bool
          isValid [] m1                                         = False
          isValid ((Step s0 (s1:[])):ss) m1@(Step t0 (t1:_))
            | s0==t0 && s1==t1 = True
            | otherwise        = isValid ss m1


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

startGame :: Bool -> IO ()
startGame robot = do
    putStrLn " Welcome to Checker game. \n\
             \ Top left cell of the board is (0,0) and bottom right is (7,7). \n\
             \ You need to enter moves in following format: \n\
             \ row column - row column \n\
             \ Or if you want to make several moves: \n\
             \ row column - row column - row column ...\n"
    loopGame initialBoard (black_player, red_player) robot

main :: IO ()
main = startGame True
