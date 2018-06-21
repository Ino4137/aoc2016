module Ex.E4 (solve41, solve42) where

import Inputs

import qualified Data.Map as M
import Data.Monoid
import Data.List.Split
import Data.Char
import Control.Lens
import Control.Monad.Trans.State
import Data.Foldable
import Data.List
import Data.Ord

data Line = Line { 
    _chars :: [String], 
    _number :: Sum Int, 
    _chksum :: String
  } deriving Show

makeLenses ''Line

parseLine :: String -> Line
parseLine li = 
  let br = splitOn "-" li
      numChk = last br
      chars = init br
      number = Sum . read $ takeWhile isNumber numChk
      chksum = init . last . splitOn "[" $ numChk
  in Line chars number chksum

validLine li = (toBest5 . concat $ _chars li) == _chksum li
  where
    toBest5 chrs = take 5 . map fst . sortBy (comparing (Down . snd)) 
      . M.toList 
      $ foldl' (\acc x -> M.insertWith (const (+1)) x 1 acc) M.empty chrs

solve41 = foldMap _number . filter validLine . map parseLine $ myInp
  where
    myInp = lines inp4

decipher :: Line -> String
decipher li@Line{..} = (++ show (getSum _number)) . (++" ") . unwords . (map.map) deci $ _chars
    where
      deci = under enum ((+97) . (`mod` 26) . (+ (getSum _number `rem` 26)) . (subtract 97))

-- Just have to find the right place yourself, the exercise wasn't clear what the name was
solve42 = mapM_ putStrLn . map decipher . filter validLine . map parseLine $ myInp
  where
    myInp = lines inp4
    