module Sudoku (
Sudoku
) where

import Test.QuickCheck
import Data.List (nub, transpose)

data Sudoku = Sudoku { rows::[[Maybe Int]] }
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
-- in those 3 blocks.
candidates :: Sudoku -> Pos -> [Int]
candidates s@(Sudoku {rows=rows}) (r, c) = map unwrapMaybe (foldl (-=) (map Just [1..9]) [horizontalBlock, verticalBlock, boxBlock])
    where horizontalBlock = rows !! r
          verticalBlock = (transpose rows) !! c
          boxBlock = blockAt (div r 3) (div c 3) s
          unwrapMaybe (Just v) = v

-- takes two set of numbers and return first
-- set by removing elements that exists in 
-- 2nd set.
(-=) :: [Maybe Int] -> [Maybe Int] -> [Maybe Int]
(-=) []     bs = []
(-=) (a:as) bs 
  | elem a bs  = as-=bs
  | otherwise  = a:(as-=bs)

-- print sudoku files
-- > printSudokuFiles $ sudokuFiles "sudokus/easy" 50
-- > printSudokuFiles $ sudokuFiles "sudokus/hard" 95
printSudokuFiles :: [FilePath] -> IO ()
printSudokuFiles []  = do return ()
printSudokuFiles (f:fs) = do
    sudoku <- readSudoku f
    printSudoku sudoku
    printSudokuFiles fs

sudokuFiles :: String -> Int -> [FilePath]
sudokuFiles prefix n = map (\n -> prefix++n++".sud") $ map padInt [1..n]

padInt :: Int -> String
padInt n 
  | n<10      = "0" ++ show n
  | otherwise = show n
