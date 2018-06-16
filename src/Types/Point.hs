module Types.Point where

import Control.Lens

data Point = Point { _x :: Int, _y :: Int } deriving (Eq, Show, Ord) 
makeLenses ''Point