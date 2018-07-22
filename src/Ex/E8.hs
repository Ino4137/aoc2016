module Ex.E8 (solve81, solve82) where

import Inputs
import Control.Lens (imap)
import Data.Kind (Type)
import Data.List (transpose, foldl')
import Data.List.Split (splitOn)

data Pixel = Lit | Off deriving Eq
type Board = [[Pixel]]

-- transposed screen rotates collumns while normal, rows
data ScreenState = Normal | Transposed deriving (Show, Eq)

-- screen settings
g_height = 6
g_width = 50

-- singleton boilerplate
data SingSS :: ScreenState -> Type where
  SNormal :: SingSS 'Normal
  STransposed :: SingSS 'Transposed

class SingSSI s where
  singSS :: SingSS s
instance SingSSI 'Normal where
  singSS = SNormal
instance SingSSI 'Transposed where
  singSS = STransposed

-- actual screen
data Screen :: ScreenState -> Type where
  MkScreen :: {board :: Board} -> Screen s
data Screen1 :: Type where
  MkScreen1 :: SingSSI s => SingSS s -> Screen s -> Screen1

instance SingSSI s => Show (Screen s) where
  show = unlines . (map.map) pixelize . board . normalized
    where
      pixelize Lit = '#'
      pixelize Off = '.'
instance Show Screen1 where
  show (MkScreen1 si scr) = show $ restoreSingSS si scr

restoreSingSS :: SingSS s -> Screen s -> Screen s
restoreSingSS _ = id 

-- TODO think about how to unify normalized with transposed
normalized :: forall s. SingSSI s => Screen s -> Screen 'Normal
normalized = case singSS @s of
  SNormal -> id 
  STransposed -> lift transpose

transposed :: forall s. SingSSI s => Screen s -> Screen 'Transposed
transposed = case singSS @s of
  SNormal -> lift transpose
  STransposed -> id

lift :: (Board -> Board) -> Screen a -> Screen b
lift f = MkScreen . f . board

emptyScreen :: Screen 'Normal
emptyScreen = MkScreen . replicate g_height . replicate g_width $ Off

fromScreen :: SingSSI a => Screen a -> Screen1
fromScreen = fromScreen_ singSS
  where
    fromScreen_ :: SingSSI a => SingSS a -> Screen a -> Screen1
    fromScreen_ = MkScreen1

emptyScreen1 :: Screen1
emptyScreen1 = fromScreen emptyScreen

countPixels (MkScreen1 _ scr) = sum . map (length . filter (==Lit)) . board $ scr

-- spooky existential quantification singleton bullshittery
data Transformation1 :: TTransformation -> Type where
  Trans1 :: Int -> Int -> Transformation1 s
data Transformation :: Type where 
  Trans :: (Executable s, SingTI s, (SingSSI (Execute s))) => SingT s -> Transformation1 s -> Transformation

-- parsing :^)
parseLine :: String -> Transformation
parseLine line = case words line of 
  ("rect":(splitOn "x" -> ((read -> x):(read -> y):[])):[])        -> Trans SRect $ Trans1 x y 
  (ro:"row":(splitOn "=" -> (y:(read -> n):[])):by:(read -> x):[]) -> Trans SRow $ Trans1 n x
  (ro:col:(splitOn "=" -> (y:(read -> n):[])):by:(read -> x):[])   -> Trans SColumn $ Trans1 n x

data TTransformation = Row | Rect | Column
data SingT :: TTransformation -> Type where
  SRow :: SingT 'Row
  SRect :: SingT 'Rect
  SColumn :: SingT 'Column
class SingTI s where
  singT :: SingT s
instance SingTI 'Row where
  singT = SRow
instance SingTI 'Column where
  singT = SColumn
instance SingTI 'Rect where
  singT = SRect

type family Execute (s :: TTransformation) = (ss :: ScreenState) where
  Execute 'Column = 'Transposed
  Execute 'Row    = 'Normal
  Execute 'Rect   = 'Normal

execute' :: Screen1 -> Transformation -> Screen1
execute' (MkScreen1 siscr scr) (Trans si (Trans1 n x)) = fromScreen $ execute_ si siscr n x scr
  where
    execute_ :: forall s a. (Executable s, SingTI s, SingSSI a) => SingT s -> SingSS a -> Int -> Int -> Screen a -> Screen (Execute s)
    execute_ _ _ = execute @s @a 

class Executable s where
  execute :: SingSSI a => Int -> Int -> Screen a -> Screen (Execute s)
instance Executable 'Row where
  execute n x = lift foo . normalized
    where
      foo = imap (\i1 -> if i1 == n
          then take g_width . drop (g_width-x `rem` g_width) . cycle
          else id
        )  
instance Executable 'Column where
  execute n x = lift foo . transposed
    where
      foo = imap (\i1 -> if i1 == n
          then take g_height . drop (g_height-x `rem` g_height) . cycle
          else id
        ) 
instance Executable 'Rect where
  execute x y = lift foo . normalized
    where
      foo = imap (\i1 -> if i1 < y 
          then imap (\i2 -> 
            if i2 < x then
              const Lit
            else id )
          else id
        )

solve81 = countPixels . foldl' execute' emptyScreen1 . map parseLine . lines $ inp8

solve82 = foldl' execute' emptyScreen1 . map parseLine . lines $ inp8