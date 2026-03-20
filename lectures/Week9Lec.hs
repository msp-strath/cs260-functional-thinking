import Test.QuickCheck


increment = (\x -> x + 1)

-- turn into lambda function: a * b + 42 * c
-- \a b c -> a * b + 42 * c
-- notice no `,` between the arguments!!!


calculate :: Int -> Int -> Int -> Int
calculate a b c = a * b + 42 * c



data Characters = Ranger | Melee | Wizard

data WeaponOfChoice
  = Staff
  | Sword
  | Longbow
  | Wand

spawn :: String -> (Characters, WeaponOfChoice)

spawn "Alasdair" = (Melee, Staff)
spawn "Georgi"   = (Ranger, Sword)
spawn "Parry Hotter" = (Wizard, Wand)
spawn "Something Else" = (Melee, Sword)
spawn _   = error "not implemnted"


damage1 :: String -> Int
damage1 name = helper (spawn name)
  where
    helper :: (Characters, WeaponOfChoice) -> Int
    helper (Melee, Staff) = 10
    helper (Ranger, Sword) = 10
    helper (Wizard, Wand) = 90



damage2 :: String -> Int
damage2 name = case (spawn name) of
  (Melee, Staff) -> 10
  (Ranger, Sword) -> 10
  (Wizard, Wand)  -> 90
--  _              -> error "not implemented"



damage3 :: String -> Int
damage3 name = let (char, weapon) = (spawn name)
               in case weapon of
                    Staff -> 10



applyClemens :: (String -> String) -> String

applyClemens f = f "Clemens"

myMap :: (a -> b) -> [a] -> [b]
myMap func [] = []
myMap func (x : xs) = func x : myMap func xs

otherMap f xs = myFoldr (\head tail -> (f head):tail) [] xs


otherFilter pred xs = myFoldr (\head tail -> if pred head then head:tail else tail) [] xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter pred [] = []
myFilter pred (x:xs) | pred x = x : myFilter pred xs
                     | otherwise = myFilter pred xs

-- function [] = base
-- function (x:xs) = step and function xs



--- foldr :: (a -> b -> b ) -> b -> [ a ] -> b


myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr step base [] = base
myFoldr step base (x:xs) = x `step` (myFoldr step base xs)
