module Ex.E7 (solve71, solve72) where

import Inputs
import qualified Data.DList as D
import qualified Data.Set as S
import Data.List (partition)
import Data.Bifunctor

isAbba (x:y:((==y) -> True):(\x' ->
  (x'==x) && (x'/=y) -> True):xs) = True
isAbba (x:xs) = isAbba xs 
isAbba x = False

checkRegion :: Region -> Maybe Bool
checkRegion (Closed x) = if isAbba x then Nothing else Just False
checkRegion (Free x) = Just $ isAbba x

data Region = Free {unReg :: String} | Closed {unReg :: String} deriving Show
type IPv7 = [Region]

suppsTLS :: IPv7 -> Bool
suppsTLS (fmap or . sequence . map checkRegion -> Just True) = True
suppsTLS x = False

-- would give incorrect results if the string was invalid
-- it assumes that a Free block will be the last one
parseLine :: D.DList Char -> IPv7 -> String -> IPv7
parseLine backref res (x:xs) = case x of
  '[' -> parseLine D.empty (res ++ [Free (D.toList backref)]) xs
  ']' -> parseLine D.empty (res ++ [Closed (D.toList backref)]) xs
  x   -> parseLine (backref `D.snoc` x) res xs 
parseLine backref res x = res ++ [Free (D.toList backref)]

solve71 = sum . map (fromEnum . suppsTLS . parseLine D.empty []) . lines $ inp7

-- part 2

listABA :: String -> S.Set String
listABA (x:y:(\x' -> x'==x && x'/=y -> True):xs) = S.insert [x,y,x] $ listABA (y:x:xs)
listABA (x:y:x':xs) = listABA (y:x':xs) 
listABA x = S.empty

isClosed (Closed _) = True
isClosed x = False

inverse (a:b:_) = [b,a,b] 

suppsSSL :: IPv7 -> Bool
suppsSSL = not . uncurry S.disjoint . bimap (S.map inverse . foo) foo . partition isClosed 
  where
    foo = S.unions . map (listABA . unReg)

solve72 = sum . map (fromEnum . suppsSSL . parseLine D.empty []) . lines $ inp7