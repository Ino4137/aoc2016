module Ex.E2 where

import Inputs

import Data.Sequence
import Data.Foldable
import Data.Char

type Keypad = [[Int]]

data Instr = U | L | R | D deriving Show

readI 'U' = U
readI 'L' = L
readI 'R' = R
readI 'D' = D

move U n = if n < 4 then n else n - 3
move L n = if n `rem` 3 == 1 then n else n - 1
move R n = if n `rem` 3 == 0 then n else n + 1
move D n = if n > 6 then n else n + 3

-- have to provide a starting key
execInstr :: Int -> [Instr] -> Int
execInstr = foldl (flip move)

execMoves _ [] s = toList s
execMoves i (x : xs) s = 
  let n = execInstr i x 
  in execMoves n xs (s |> n)

solve21 = showMove $ execMoves 5 myMoves Empty
  where
    myMoves = (map.map) readI . lines $ inp2
    showMove = foldr ((++).show) ""

-- part 2

-- 1 <-> 3 is special
move2 U n = if n `elem` upBound then n else if repr n == 3 then '1' else unRepr (repr n - 4)
move2 L n = if n `elem` leBound then n else unRepr (repr n - 1)
move2 R n = if n `elem` riBound then n else unRepr (repr n + 1)
move2 D n = if n `elem` doBound then n else if repr n == 1 then '3' else unRepr (repr n + 4)

upBound = "12549"
riBound = "149CD"
doBound = "5ADC9"
leBound = "125AD"
repr :: Char -> Int
repr n@(isNumber -> True) = subtract 48 . ord $ n
repr 'D' = 15
repr n = subtract 55 . ord $ n
unRepr n@((<10) -> True) = chr . (+48) $ n
unRepr 15 = 'D'
unRepr n = chr . (+55) $ n  

execInstr2 :: Char -> [Instr] -> Char
execInstr2 = foldl (flip move2)

execMoves2 _ [] s = toList s
execMoves2 i (x : xs) s = 
  let n = execInstr2 i x 
  in execMoves2 n xs (s |> n)

solve22 = execMoves2 '5' myMoves Empty
  where
    myMoves = (map.map) readI . lines $ inp2