module Parsers where

import Checker 
import Parsing

ppos :: Parser Pos
ppos = do
    d0 <- oneOrMore digit 
    char ' ' 
    d1 <- oneOrMore digit
    return ((read d0), (read d1))

psep :: Parser Char
psep = do
    char ' '
    char '-'
    char ' '

pmove :: Parser Move
pmove = do
    moves <- chain ppos psep
    return (Step (head moves) (tail moves))
