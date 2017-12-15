module Test where

import qualified Data.List as List
import Test.QuickCheck

import Checker
import Game

cell :: Gen Cell
cell = frequency [(40, return Empty), (12, return (C (Single Black))), (12, return (C (Single Red)))]

instance Arbitrary Board where
    arbitrary = fmap Board (fmap concat $ vectorOf 4 row2)

-- | Alternate empty and random cell
row :: Int -> Bool -> Gen [Cell]
row 0 _     = return []
row r False = do
    rest <- row (r-1) True
    return (Empty:rest)
row r True  = do
    c <- cell
    rest <- row (r-1) False
    return (c:rest)

-- | 
row2 :: Gen [[Cell]]
row2 = do
    r0 <- row 8 False
    r1 <- row 8 True
    return [r0, r1]

-- | Check if game is finished if all of the pieces are black
prop_gamefinished :: Board -> Bool
prop_gamefinished b = isGameFinished black_player b'
    where b' = updateBoard b $ map (\p -> (p, Empty)) (pieces Black b)

prop_steps_and_captures :: Board -> Property
prop_steps_and_captures b = let (steps', captures') = (unzip $ map (pieceMoves black_player b) (pieces Black b))
                                steps = concat steps' 
                                captures = concat captures' 
                             in (length steps /= 0) ==> (length (List.intersect steps captures)==0)
