{- TYPES TYPES TYPES! -}

---------------------------------------------

data What = MyAxe | MyBow | MySword
 deriving Show

foo :: What -> Bool
foo MyAxe = True
foo _     = False



instance Eq What where
  MyAxe == MyAxe = True
  MyBow == MyBow = True
  MySword == MySword = True
  _ == _         = False



data Suit = Diamonds | Clubs | Hearts | Spades
 deriving (Eq, Ord, Show,Enum)



data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
     deriving (Eq,Show,Ord,Enum)



data Card = MkCard Rank Suit
   deriving (Eq,Show,Ord)

data Nullable a = Null | HaveValue a
 deriving Show


maybefailure
  :: Nullable a
  -> (        a -> Nullable b)
  ->               Nullable b
maybefailure Null f          = Null
maybefailure (HaveValue x) f = f x

data MyList a
  = Empty
  | Cons a (MyList a)
       --  ^^^^^^^^^^


testList = Cons "Clemens" (Cons "Bob" (Cons "Fred" (Cons "Neil" (Cons "Conor" Empty))))

myLenth :: MyList a -> Int
myLenth Empty = 0
myLenth (Cons x xs) = 1 + myLenth xs


transform
  :: (a -> Nullable b)
  -> MyList a
  -> Nullable (MyList b)
transform f Empty = HaveValue Empty
transform f (Cons x xs) =
  maybefailure (f x) $ \ y ->
  maybefailure (transform f xs) $ \ ys ->
  HaveValue (Cons y ys)


data BTree a
  = Leaf a
  | Node (BTree a) (BTree a)
   --    ^^^^^^^^^  ^^^^^^^
  deriving (Show, Eq, Ord)

bTree :: BTree Int
bTree
  = Node
      (Node (Leaf 0) (Leaf 1))
      (Node
        (Node (Leaf 2) (Leaf 3))
        (Leaf 4)
      )

size :: BTree a -> Int
size (Leaf x) = 1
size (Node l r) = size l + size r




data AltTree a b
  = AltLeaf
  | AltNode (AltTree b a) a (AltTree b a)
  deriving (Show)

altTree :: AltTree Int String
altTree =
  AltNode
    (AltNode AltLeaf "Clemens" AltLeaf)
    0
    (AltNode AltLeaf "Alasdair"
      (AltNode AltLeaf 1 AltLeaf))
















{-
getSuit :: Card -> Suit
getSuit (MkCard rank suit) = suit


makeDeck = [MkCard y x | x<- [Diamonds .. Spades], y <- [Two .. Ace]]

data Nullable a = Null | Value a
  deriving (Eq, Ord, Show)

functionThatIsNotInnuendo :: [a] -> Nullable a
functionThatIsNotInnuendo [] = Null
functionThatIsNotInnuendo (x:xs) = Value x

handle :: (a -> b) -> Nullable a -> b -> b
handle f Null def = def
handle f (Value x) def = f x

data MyList a = Cons a (MyList a) | Empty
  deriving(Show,Eq)


myLength :: MyList a -> Int
myLength Empty = 0
myLength (Cons a xs) = 1 + (myLength xs )

testList = Cons "Clemens" (Cons "Bob" (Cons "Fred" (Cons "Neil" (Cons "Conor" Empty))))

data BTree a = Leaf | Node (BTree a) a (BTree a)
  deriving (Show, Eq)


testTree :: BTree Int
testTree = Node (Node (Node Leaf
                            1
                            Leaf)
                       3 (Node Leaf
                               4
                               Leaf))
                5
                (Node (Node Leaf 6 Leaf) 7 Leaf)

treeDepth :: BTree a -> Int
treeDepth Leaf = 1
treeDepth (Node l x r) = 1 + max (treeDepth l) (treeDepth r)

insert :: (Ord a) => BTree a -> a -> BTree a
insert Leaf x = Node Leaf x Leaf
insert (Node l y r) x | (x <= y) = Node (insert l x) y r
                      | (x > y)  = Node l y (insert r x)



--myval = Cons 1 (Cons 6 Empty)



{-}
test2 :: BTree Int
test2 = Node Leaf 5 Leaf-}



-}
