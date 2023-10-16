module TTT.A2 where

import Data.List (intercalate)
import TTT.A1
import Control.Monad (forM)

-- Q#01

promptPlayer :: Player -> String
promptPlayer E = ""
promptPlayer p = concat ["Player ", show p,  "'s turn: enter a row and column position (ex. A1)"]

-- Q#02

_RANGE_ = [0..(_SIZE_-1)]

-- Q#03

isDigit :: Char -> Bool
isDigit c = c `elem` ['0' .. '9']

readDigit :: Char -> Int
readDigit c 
     | isDigit c = read [c]
     | otherwise = -1

-- Q#04

_EMPTY_ROW_ = replicate  _SIZE_ E

_EMPTY_BOARD_ = replicate _SIZE_ _EMPTY_ROW_

-- Q#05

_TIED_BOARD_ :: Board
_TIED_BOARD_ = [
    [X, O, O]
  , [O, X, X]
  , [O, X, O]
  ]

isTied :: Board -> Bool
isTied b = E `notElem` concat b


-- Q#06
indexRowStrings :: [String] -> [(Char, String)]
indexRowStrings = zip ['A'..'Z'] 

-- Q#07
formatLine :: [String] -> String
formatLine i = concat [_SEP_, intercalate _SEP_ i, _SEP_ ]

-- Q#08
isMoveInBounds :: Move -> Bool
isMoveInBounds m = all (== True)[fst m `elem` _RANGE_, snd m `elem` _RANGE_]

-- Q#09
stringToMove :: String -> Move
stringToMove s
   | length s == 2 = (convertRowIndex x, readDigit y)
   | otherwise = _INVALID_MOVE_
   where [x, y] = take 2 s

   

-- Q#10
replaceSquareInRow :: Player -> Int -> Row -> Row
replaceSquareInRow _ _ [] = []
replaceSquareInRow p i r
       | i `elem` _RANGE_ = lhs ++ [p] ++  rhs
       | otherwise = r
       where (lhs, _ : rhs) = splitAt i r

rsX = replaceSquareInRow X
rsO = replaceSquareInRow O
