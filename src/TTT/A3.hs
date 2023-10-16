module TTT.A3 where

import Data.List (transpose)
import TTT.A1
import TTT.A2

-- Q#01

showInts :: [Int] -> [String]
showInts = map (\x -> show x)

_HEADER_ = ' ' : formatLine (showInts _RANGE_)

-- Q#02
showSquares :: [Square] -> [String]
showSquares = map (\x -> showSquare x)

-- Q#03
formatRows :: [Row] -> [String]
formatRows  = map (formatLine . showSquares) 

-- Q#04
isColEmpty :: Row -> Int -> Bool
isColEmpty [] _ = False
isColEmpty r i
     | i `elem` _RANGE_ = r !! i == E
     | otherwise = False

-- Q#05
dropFirstCol :: Board -> Board
dropFirstCol [] = []
dropFirstCol (r : rs)= tail r : dropFirstCol rs

dropLastCol :: Board -> Board
dropLastCol [] = []
dropLastCol (r : rs) = init r : dropFirstCol rs
-- Q#06

getDiag1 :: Board -> [Square]
getDiag1 [] = []
getDiag1 (r : rs) = head r : (getDiag1 . dropFirstCol) rs 

getDiag2 :: Board -> [Square]
getDiag2  [] = []
getDiag2 (r : rs) = last r : (getDiag2 . dropLastCol) rs 

getAllLines :: Board -> [[Square]]
getAllLines b = b ++ transpose b ++ [getDiag1 b, getDiag2 b]

-- Q#07
putSquare :: Player -> Board -> Move -> Board
putSquare = undefined

-- Q#08

prependRowIndices = undefined

isWinningLine_ = undefined

-- Q#10

isValidMove = undefined