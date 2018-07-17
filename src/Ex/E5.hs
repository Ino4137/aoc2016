module Ex.E5 (solve51, solve52) where 

import Inputs

import Data.Hash.MD5
import Data.Function
import qualified Data.IntMap as M
import Data.Char

isValid = (==) "00000" . take 5 

getNext n inp@((md5s . Str . flip (++) (show n)) -> hsd@(isValid -> True)) prev =
  let res = prev ++ [head . drop 5 $ hsd] in
  if (==8) . length $ res
    then res
    else getNext (n+1) inp res
getNext n inp prev = getNext (n+1) inp prev

solve51 = getNext 0 "cxdnnyjw" ""

getNext2 n inp@((md5s . Str . flip (++) (show n)) -> hsd@(isValid -> True)) prev =
  let (pos':val:[]) = take 2 . drop 5 $ hsd
      pos = if isNumber pos' then digitToInt pos' else 42 -- it just works
      res = if pos >= 0 && pos < 8
        then M.insertWith (flip const) pos val prev
        else prev
  in
  if (==8) . M.size $ res
    then map ((M.!) res) [0..7]
    else getNext2 (n+1) inp res
getNext2 n inp prev = getNext2 (n+1) inp prev

solve52 = getNext2 0 "cxdnnyjw" M.empty
