module Main (main) where

data Board = Board { board::[[Cell]] }
data Cell = Empty | Red | Black

emptyBoard :: Board
emptyBoard = Board { 
    board = [
        [ Red  , Red  , Red  ], 
        [ Empty, Empty, Empty], 
        [ Black, Black, Black], 
    ]
}

instance Show Board where
    show = showBoard

instance Show Cell where
    show = showCell

showBoard :: Board -> String
showBoard (Board { board=b }) = unlines $ map (concat . map show) b

showCell :: Cell -> String
showCell Empty = "E"
showCell Red   = "R"
showCell Black = "B"
