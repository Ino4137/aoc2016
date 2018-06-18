module Ex.E1 (solve11, solve12) where

import Data.List.Split (splitOn)
import Control.Monad.Trans.State
import qualified Data.Set as ST
import Control.Monad
import Control.Lens
import Data.List (find)
import Data.Function (on)
import Data.Maybe (isJust)

import Inputs (inp1)
import Types.Point
import Types.Dir

data Move = R Int | L Int deriving Show
undir (R i) = i
undir (L i) = i 

parseMove = \case
  ' ':'L':(read -> n) -> L n
  ' ':'R':(read -> n) -> R n

turn (L _) N = W
turn (L _) n = pred n
turn (R _) W = N
turn (R _) n = succ n

walk p N = x +~ (undir p)
walk p E = y +~ (undir p)
walk p S = x -~ (undir p)
walk p W = y -~ (undir p)

-- state: (Point, Dir)
makeMove move = do
  _2 %= turn move
  (pt, dir) <- get
  put $ (walk move dir pt, dir)

makeMove2 move = do
  _2 %= turn move
  (pt, dir, mp, res) <- get
  let newP@(Point nx ny) = walk move dir pt
      (Point ox oy) = pt
      -- construct the path
      foo = do
        x  <- [min nx ox..max nx ox]
        y  <- [min ny oy..max ny oy]
        [(x,y)]
      -- tail/init, then the first/last element was the last/first in the previous iteration
      path = ST.fromList $ (if dir == S || dir == W then tail else init) foo

  case ST.toList $ path `ST.intersection` mp of 
    [] -> do
        put $ (newP, dir, ST.union path mp, res)
    (el:xs) -> if isJust res then do
        put $ (newP, dir, ST.union path mp, res)
      else do
        put $ (newP, dir, ST.union path mp, Just el)
  
solve11 = uncurry ((+) `on` abs) . toTup . fst $ execState (mapM makeMove moves) (Point 0 0, N)
  where
    moves = map parseMove . splitOn "," $ inp1
    toTup (Point x y) = (x,y)
    test1 = " R188"

solve12 = fmap (uncurry ((+) `on` abs)) . (\(_,_,_,x) -> x) $ execState (mapM makeMove2 moves) (Point 0 0, N, ST.empty, Nothing)
  where
    moves = map parseMove . splitOn "," $ inp1
    toTup (Point x y) = (x,y)
