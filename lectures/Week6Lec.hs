------------------------------------------------------------------------
-- FUNCTORS
-- Structure-preserving transformations
------------------------------------------------------------------------

import Data.List (intercalate)

-- Type signatures
-- Laws

data Nullable a = Null | HasValue a



-- DEFINE newtype Compose

-- DEFINE Functor (Compose g f)



-- DEFINE newtype Matrix
newtype Matrix a = MkMatrix { getMatrix :: [[a]] }
-- UNCOMMENT Show Matrix magic

-- DEFINE genMatrix
genMatrix :: Int -> Int -> Matrix (Int, Int)
genMatrix m n = MkMatrix [ (i,) <$> [1..n] | i <- [1..m] ]

-- DEFINE Functor Matrix
instance Functor Matrix where
  fmap f (MkMatrix rcs) = MkMatrix (fmap (fmap f) rcs)

-- DEFINE maybeEq
maybeEq :: Eq a => (a, a) -> Nullable (a, a)
maybeEq (i, j)
  | i == j = HasValue (i, j)
  | otherwise = Null


-- EXAMPLES
rectangle :: Matrix (Int, Int)
rectangle = genMatrix 10 5

diagonalish :: Matrix (Nullable (Int, Int))
diagonalish = maybeEq <$> rectangle

































------------------------------------------------------------------------
-- Magic box; don't open

instance Show a => Show (Nullable a) where
  show Null = "∅"
  show (HasValue x) = show x

instance Show a => Show (Matrix a) where
  show (MkMatrix []) = ""
  show (MkMatrix rcs@(r:_)) =
    let rw = length r in
    let strs = (show <$>) <$> rcs in
    let w = maximum (maximum . fmap length <$> strs) in
    let center str =
          let m = w - length str in
          let wl = m `div` 2 in
          let wr = wl + m `mod` 2 in
          concat
            [ replicate wl ' '
            , str
            , replicate wr ' '
            ] in
    let lines = (intercalate " │ " . (center <$>)) <$> strs in
    let hborders = replicate rw (replicate w '─') in
    unlines
      [ "┌" ++ intercalate "─┬─" hborders ++ "┐"
      , intercalate
      ("\n├" ++ intercalate "─┼─" hborders ++ "┤\n")
      $ fmap (("│" ++) . (++ "│"))
      $ lines
      , "└" ++ intercalate "─┴─" hborders ++ "┘"
      ]
