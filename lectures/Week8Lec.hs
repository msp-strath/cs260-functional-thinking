module Week08 where


data NonEmptyList e
  = JustTheElement e
  | Cons e (NonEmptyList e)
  deriving (Show)

mapNonEmptyList :: (a -> b) -> NonEmptyList a -> NonEmptyList b
mapNonEmptyList f (JustTheElement x) = JustTheElement (f x)
mapNonEmptyList f (Cons x xs)        = Cons (f x) (mapNonEmptyList f xs)

instance Functor NonEmptyList where
  fmap = mapNonEmptyList

myList :: NonEmptyList String
myList = JustTheElement "Clemens"

-- try running `mapNonEmptyList length myList`
