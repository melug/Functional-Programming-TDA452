module Main (
main
) where

import Test.QuickCheck
import Data.List (nub, transpose, sort, union, intersect, (\\))
import Data.Maybe (fromJust)
import Debug.Trace (trace)

data Sudoku = Sudoku { rows::[[Maybe Int]] } deriving (Eq)
-- Part B
type Pos = (Int, Int)

instance Show Sudoku where
    show = unlines . map (concat . (map toStr)) . rows
        where toStr Nothing  = "."
              toStr (Just n) = show n

example :: Sudoku
example = Sudoku
    [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
    , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
    , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
    , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
    , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
    , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
    , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
    , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
    , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
    ]

-- * A1
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku { rows=replicate 9 (replicate 9 Nothing) }

-- * A2
-- check if numbers given in sudoku is between 1 and 9
-- and size is 9x9
isSudoku :: Sudoku -> Bool
isSudoku s = sizeOk && numbersOk
    where sizeOk = length (rows s)==9 && all (\row -> length row==9) (rows s)
          numbersOk = all numberOk $ concat (rows s)
          numberOk Nothing  = True
          numberOk (Just n) = 1<=n && n<=9

-- * A3
-- check is sudoku is filled i.e there shouldn't 
-- be cell with value Nothing
isFilled :: Sudoku -> Bool
isFilled = not . any (==Nothing) . concat . rows

-- * B1
printSudoku :: Sudoku -> IO ()
printSudoku = putStrLn . show

-- * B2
readSudoku :: FilePath -> IO Sudoku
readSudoku fp = do
    sudokuAsStr <- readFile fp
    let sudoku = toSudoku sudokuAsStr
    if isSudoku sudoku then return sudoku
                       else error "Program error: Not a sudoku"
    where toSudoku s = Sudoku { rows=map (map toMaybeInt) (lines s) }
          toMaybeInt '.' = Nothing
          toMaybeInt c   = (Just (read [c] :: Int))

-- * C1
cell :: Gen (Maybe Int)
cell = frequency [(9, (return Nothing)), (1, fmap Just $ elements [1..9])]

-- * C2
instance Arbitrary Sudoku where
    arbitrary = do 
        rows <- vectorOf 9 (vectorOf 9 cell) 
        return (Sudoku rows)

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku s = isSudoku s

-- * D1
type Block = [Maybe Int]
isOkayBlock :: Block -> Bool
isOkayBlock ns = length (nub numbers) == length numbers
    where numbers = filter (/=Nothing) ns

-- * D2
blocks :: Sudoku -> [Block]
blocks s@(Sudoku rows) = rows ++ (transpose rows) ++ [blockAt (3*r) (3*c) s | r<-[0..2], c<-[0..2]]

blockAt :: Int -> Int -> Sudoku -> Block
blockAt row col (Sudoku rows) = concat [ take 3 (drop col r)  | r <- (take 3 (drop row rows)) ]

-- * D3
isOkay :: Sudoku -> Bool
isOkay s = all isOkayBlock (blocks s)

-- * E1
-- index each row to (row index, row), then expand each
-- tuple to (row index, (column index, value)) then
-- filter by value==Nothing.
-- at last transform into (row index, column index)
blanks :: Sudoku -> [Pos]
blanks Sudoku { rows=rows }  = concat $ map blankRows (zip [0..] rows)
    where blankRows (r, row) = map dropVal $ filter nothingCell (zip3 (repeat r) [0..] row)
          nothingCell (r, c, val) = val==Nothing
          dropVal (r, c, v) = (r, c)

-- return value of cell at given positon
cellAt :: Sudoku -> Pos -> Maybe Int
cellAt Sudoku { rows=rows } (r, c) = head $ drop c $ head $ drop r rows

prop_blank :: Sudoku -> Bool
prop_blank s = all isCellBlank (blanks s)
    where isCellBlank p = Nothing==cellAt s p

-- * E2
-- position where it supposed to be replaced
-- could be greater than length of array.
(!!=) :: [a] -> (Int,a) -> [a]
(a:arr) !!= (0,elem) = elem:arr
(a:arr) !!= (i,elem) = a:(arr !!= (i-1, elem))
_ !!= _              = error "index is greater than size"

-- check property on non-empty int arrays.
-- replace element at random position with 99 and
-- check whether the element at i-th is 99.
prop_replace :: [Int] -> Int -> Bool
prop_replace [] index   = True
prop_replace nums index = 99 == (nums !!= (i, 99)) !! i
    -- make some normalization for index since
    -- index could be greater than size or negative.
    where i = mod (abs index) (length nums)

-- * E3
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update (Sudoku {rows=oldRows}) (r, c) val = Sudoku {rows=newRows}
    where newRows = oldRows !!= (r, newRow)
          newRow  = (oldRows !! r) !!= (c, val)

-- takes sudoku and position, then new value.
-- checks if new value is set.
prop_update :: Sudoku -> (Int, Int) -> Maybe Int -> Bool
prop_update s (r, c) val = let r'=(mod (abs r) 9)
                               c'=(mod (abs c) 9)
                               newSudoku=update s (r', c') val
                            in cellAt newSudoku (r', c')==val

-- * E4
-- any cell belongs to exactly 3 blocks
-- (1 horizontal, 1 vertical, 1 box block).
-- start with possible candidates as [1..9]
-- then remove numbers that already exists
-- in those 3 blocks. `naiveCandidates` does
-- so. It's improved in candidates function.
naiveCandidates :: Sudoku -> Pos -> [Int]
naiveCandidates s@(Sudoku {rows=rows}) p@(r, c) = case cellAt s p of
                                               Nothing  -> candidates'
                                               (Just v) -> [v]
   where candidates' = map fromJust (foldl (-=) (map Just [1..9]) [horizontalBlock, verticalBlock, boxBlock])
         horizontalBlock = rows !! r
         verticalBlock = (transpose rows) !! c
         boxBlock = blockAt (3*(div r 3)) (3*(div c 3)) s

-- try to prune search space using clues
-- from cells which are in same block.
candidates :: Sudoku -> Pos -> [Int]
candidates sud pos = foldl1 intersect $ map (candidatesFromBlock sud) [horizontalPos pos, verticalPos pos, boxPos pos]

candidatesFromBlock :: Sudoku -> [Pos] -> [Int]
candidatesFromBlock sud poss = let (certainNumbers, possibleNumbers) = collectNumbers sud poss 
                                   neighborChoices = certainNumbers `union` possibleNumbers 
                                in case length neighborChoices==9 of 
                                     True  -> [1..9] \\ certainNumbers
                                     False -> [1..9] \\ neighborChoices

-- return two list of numbers
-- first list contains numbers extracted from given positions
-- second list contains possible numbers on those positions
collectNumbers :: Sudoku -> [Pos] -> ([Int], [Int])
collectNumbers sud@(Sudoku {rows=rows}) poss = (certainNumbers, possibleNumbers)
    where certainNumbers    = map fromJust $ justNumbers
          possibleNumbers   = nub $ concat $ map (naiveCandidates sud) nothingNumbersPos
          justNumbers       = filter (/=Nothing) $ cellValues
          nothingNumbersPos = filter (\p -> cellAt sud p==Nothing) poss
          cellValues        = map (cellAt sud) poss

    
horizontalPos :: Pos -> [Pos]
horizontalPos pos@(row, col) = zip (repeat row) [0..8] \\ [pos]

verticalPos :: Pos -> [Pos]
verticalPos pos@(row, col) = zip [0..8] (repeat col) \\ [pos]

boxPos :: Pos -> [Pos]
boxPos pos@(row, col) = let brow = (div row 3)*3
                            bcol = (div col 3)*3
                         in [ (brow+r, bcol+c) | r<-[0..2], c<-[0..2] ] \\ [pos]

-- takes two set of numbers and return first
-- set by removing elements that exists in 
-- 2nd set.
(-=) :: [Maybe Int] -> [Maybe Int] -> [Maybe Int]
(-=) []     bs = []
(-=) (a:as) bs 
  | elem a bs  = as -= bs
  | otherwise  = a:(as -= bs)

-- union of two sets
(+=) :: (Eq a) => [a] -> [a] -> [a]
(+=) as bs = nub (as++bs)

-- * F1
solve :: Sudoku -> Maybe Sudoku
solve s
  | isSudoku s && isOkay s = solve' s (blanks s) (length (blanks s))
  | otherwise              = Nothing

solve' :: Sudoku -> [Pos] -> Int -> Maybe Sudoku
solve' sud blankPositions blanksLen
  | blanksLen==0          = Just sud
  | otherwise             = notNothing searchSpace
-- start with cells which has least number of candidates
-- if any blank cell has 0 number of candidates, sudoku cannot be solved.
  where blanksOrdered 
          | isDead    = []
          | otherwise = snd $ unzip orderedPos
        orderedPos  = sort [ (length (candidates sud pos), pos) | pos<-blankPositions, length (candidates sud pos)==1 ]
        isDead      = fst (head orderedPos)==0
        searchSpace = [ solve' (update sud bPos (Just c)) bRest (blanksLen-1) | (bPos, bRest)<-dropOne blanksOrdered, c<-candidates sud bPos ]

-- return first element that is not Nothing
notNothing :: (Eq a) => [Maybe a] -> Maybe a
notNothing [] = Nothing
notNothing (x:xs)
  | x==Nothing = notNothing xs
  | otherwise  = x

-- creates new lists by dropping each element once.
-- used for enumerating possible candidates at given cell
-- in sudoku.
-- i.e [1,2,3] -> [(1, [2, 3]), (2, [1, 3]), (3, [1, 2])]
dropOne :: [a] -> [(a, [a])]
dropOne []     = []
dropOne (x:xs) = (x, xs):map fixRest (dropOne xs)
    where fixRest (d, ds) = (d, x:ds)

-- F2 *
readAndSolve :: FilePath -> IO ()
readAndSolve f = do
    s <- readSudoku f
    case solve s of
      Nothing    -> putStrLn "(no solution)"
      (Just sol) -> putStrLn $ show sol

-- F3 *
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf (Sudoku {rows=rowsSol}) (Sudoku {rows=rowsSud}) = all covers $ zip (concat rowsSol) (concat rowsSud)
    where covers ((Just c0), Nothing)   = True
          covers ((Just c0), (Just c1)) = c0==c1
          covers _                      = False

-- F4 *
prop_SolveSound :: Sudoku -> Property
prop_SolveSound sud = let solution = solve sud
                       in (solution /= Nothing) ==> isSolutionOf (fromJust solution) sud

main :: IO ()
main = readAndSolve "sudokus/easy01.sud"
