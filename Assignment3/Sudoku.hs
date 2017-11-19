module Sudoku (
Sudoku
) where

import Test.QuickCheck
import Data.List (nub, transpose)

data Sudoku = Sudoku { rows::[[Maybe Int]] }

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
