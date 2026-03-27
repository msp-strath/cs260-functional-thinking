module Week10 where
-- you need to have quickcheck installed!
import Test.QuickCheck

instance Show (a->b) where
  show f = "foo"

-------------- quickcheck --------------
mymap :: (a -> b) -> [a] -> [b]
mymap f xs = foldr (\head tail -> (f head):tail) [] xs

prop_map f xs = map f xs == mymap f xs

prop_rev xs = xs == reverse (reverse xs)

----------- helper functions -----------
myZipWith
  :: (a -> b -> c)
  -> [a]
  -> [b]
  -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
-- demonstrate usage of helper functions
myZipWith f (a:as) (b:bs) =
  (helper f a b) : myZipWith f as bs
    where
      helper f a b = f a b


---------------- monads ----------------
data Weapon
  = Axe
  | Sword
  | Longbow
  | Guillaume

data MyNonEmptyList a
  = Singleton a
  | Cons a (MyNonEmptyList a)

myAppend
  :: MyNonEmptyList a
  -> MyNonEmptyList a
  -> MyNonEmptyList a
myAppend (Singleton a) bs = Cons a bs
myAppend (Cons a as)   bs = Cons a (myAppend as bs)

data MyMaybe a = MyNothing | MyJust a

-- how do we string together multiple
-- computations that can fail
-- a -> MyMaybe b
-- b -> MyMaybe c

bindNonEmptyList
  :: MyNonEmptyList a
  -> (a -> MyNonEmptyList b)
  -> MyNonEmptyList b
bindNonEmptyList (Singleton a) f = f a
bindNonEmptyList (Cons a as) f =
  (f a) `myAppend` (bindNonEmptyList as f)


-- data type for propositional formulas
-- parametrised over the type of variables
-- e.g., a or b, not (a and b), etc...
data Formula a
  = Var a | FTrue | FFalse
  | And (Formula a) (Formula a)
  | Or  (Formula a) (Formula a)
  | Not (Formula a)

instance Show a => Show (Formula a) where
  show (Var a)       = show a
  show FTrue         = "true"
  show FFalse        = "false"
  show (And phi psi) = "( " ++ show phi ++ " /\\ " ++ show psi ++ " )"
  show (Or  phi psi) = "( " ++ show phi ++ " \\/ " ++ show psi ++ " )"
  show (Not phi)     = "~( " ++ show phi ++ " )"

-- what does the monadic `bind` for formulas do?
-- ... it substitutes variables for another formulas
bindF
 :: Formula a         -- the formula `phi` we are substituting in
 -> (a -> Formula b)  -- how to substitute each variable `a` in `phi`
                      -- with a Formula b
 -> Formula b         -- the resulting formula
bindF (Var a) m       = m a
bindF FTrue   _       = FTrue
bindF FFalse  _       = FFalse
bindF (And phi psi) m = And (bindF phi m) (bindF psi m)
bindF (Or  phi psi) m = Or  (bindF phi m) (bindF psi m)
bindF (Not phi)     m = Not (bindF phi m)

formula1    = And (Or (Var "x") (Var "y")) FTrue
-- substitute each variable with its negation
notFormula1 = bindF formula1 (\x -> Not (Var x))
