module Ex.E3 where

import Inputs

import Data.List
import Data.List.Split

validTriang (sort -> [x,y,z]) = z < x + y

solve31 = length . filter validTriang $ myInp
  where
    myInp :: [[Int]]
    myInp = map (map read . words) . lines $ inp3

solve32 = foldr ((+) . length . filter validTriang) 0 $ myInp
  where
    myInp :: [[[Int]]]
    myInp = map (chunksOf 3) . transpose 
            . map (map read . words) 
            . lines $ inp3