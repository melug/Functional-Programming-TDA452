module Sudoku (
    Sudoku
    ) where

data Sudoku = Sudoku { rows::[[Maybe Int]] } deriving (Show)

example0 =
    Sudoku
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

example1 =
    Sudoku
        [ [Just 3, Just 6, Just 2,Just 2,Just 7, Just 1, Just 2, Just 2,Just 2]
        , [Just 2,Just 5, Just 2,Just 2,Just 2,Just 2,Just 1, Just 8, Just 2]
        , [Just 2,Just 2,Just 9, Just 2, Just 2,Just 4, Just 7, Just 2,Just 2]
        , [Just 2,Just 2,Just 2,Just 2,Just 1, Just 3, Just 2,Just 2, Just 8]
        , [Just 4, Just 2,Just 2,Just 5, Just 2,Just 2, Just 2,Just 2,Just 9]
        , [Just 2, Just 7, Just 2,Just 4, Just 6, Just 2,Just 2,Just 2,Just 2]
        , [Just 2,Just 2,Just 5, Just 3, Just 2,Just 8, Just 9, Just 2,Just 2]
        , [Just 2,Just 8, Just 3, Just 2,Just 2,Just 2,Just 2,Just 6, Just 2]
        , [Just 2,Just 2,Just 7, Just 6, Just 9, Just 2,Just 2,Just 4, Just 3]
    ]

allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku { rows=replicate 9 (replicate 9 Nothing) }

-- check if numbers given in sudoku is between 1 and 9
-- and size is 9x9
isSudoku :: Sudoku -> Bool
isSudoku s = sizeOk && numbersOk
    where sizeOk = length (rows s)==9 && all (\row -> length row==9) (rows s)
          numbersOk = all numberOk $ concat (rows s)
          numberOk Nothing  = True
          numberOk (Just n) = 1<=n && n<=9

-- check is sudoku is filled i.e there shouldn't 
-- be cell with value Nothing
isFilled :: Sudoku -> Bool
isFilled s = not $ any (==Nothing) $ concat (rows s)

printSudoku :: Sudoku -> IO ()
printSudoku s = do
    putStrLn $ unlines $ map (concat . (map toStr)) (rows s)
    where toStr Nothing  = "."
          toStr (Just n) = show n

readSudoku :: FilePath -> IO Sudoku
readSudoku fp = do
    sudokuAsStr <- readFile fp
    let sudoku = toSudoku sudokuAsStr
    if isSudoku sudoku then return sudoku
                       else error "Program error: Not a sudoku"
    where toSudoku s = Sudoku { rows=map (map toMaybeInt) (lines s) }
          toMaybeInt '.' = Nothing
          toMaybeInt c   = (Just (read [c] :: Int))

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
