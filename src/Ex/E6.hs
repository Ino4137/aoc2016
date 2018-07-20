module Ex.E6 where

import Inputs

import Data.List
import Data.Ord

solve61 = map mostFreq . transpose . lines $ inp6
  where
    mostFreq = (\(Down (_,x)) -> x) . head . sort 
      . map (Down . ((,) <$> length <*> head)) 
      . group . sort 

solve62 = map mostFreq . transpose . lines $ inp6
  where
    mostFreq = snd . head . sort 
      . map ((,) <$> length <*> head)
      . group . sort 