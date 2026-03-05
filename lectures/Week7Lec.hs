module Week07 where


foldrEval :: (Foldable c, Show a) => c a -> String
foldrEval xs = foldr
  (\x acc -> "(" ++ show x ++ " `f` " ++ acc ++ ")")
  "<base>"
  xs

foldlEval :: (Foldable c, Show a) => c a -> String
foldlEval xs = foldl
  (\acc x -> "(" ++ acc ++ " `f` " ++ show x ++ ")")
  "<base>"
  xs

genericSize :: Foldable c => c a -> Int
genericSize = foldr (\_ acc -> acc + 1)  0

reverseFromFoldr :: [a] -> [a]
reverseFromFoldr xs = foldr (\x accf -> accf . (x:)) id xs $ []

type Name = String
type ClassCode = String
type Faculty = String
type Department = String

staff :: [(Name, Int, [ClassCode])]
staff = [("Alasdair", 121,["CS106", "CS107","CS260"]),
         ("Georgi", 211, ["CS260","CS121"]),
         ("Joe Bloggs",457,["CS501","CS123","CS312"]),
         ("Sarah Smith",876,["MM101","MM409"]),
         ("Mark Markson",404,[]),
         ("Ada Lovelace",209,["MM312","MM401"]),
         ("Joe Johnson",999,["EC209"]),
         ("Clemens Kupke",412,["CS411"])]

classes :: [(Department, [ClassCode])]
classes = [("Computer and Information Sciences",["CS106","CS107","CS103", "CS121", "CS260","CS825"]),
           ("Mathematics and Statistics",["MM101","MM203","MM409","MM312","MM401"]),
           ("Economics",["EC209","EC104"])]

faculties :: [(Faculty, [Department])]
faculties = [("Science", ["Computer and Information Sciences","Mathematics and Statistics"]),
             ("Business", ["Economics", "Finance"])]


getClasses :: Name -> Maybe [ClassCode]
getClasses = undefined

getDepartment :: [ClassCode] -> Maybe Department
getDepartment = undefined

getFaculty :: Department -> Maybe [Faculty]
getFaculty = undefined

data Error
  = ClassNotFound Name
  | DepartmentNotFound [ClassCode]
  | FacultyNotFound Department
  deriving (Show)

type DBResult a = Either Error a

getClasses' :: Name -> DBResult [ClassCode]
getClasses' x = foldr
  (\(s, _, cls) acc -> if x == s then Right cls else acc)
  (Left $ ClassNotFound x)
  staff

getDepartment' :: [ClassCode] -> DBResult Department
getDepartment' kls = foldr
  (\(d, cls) acc -> if any (`elem` cls) kls then Right d else acc)
  (Left . DepartmentNotFound $ kls)
  classes

getFaculty' :: Department -> DBResult Faculty
getFaculty' d = foldr
  (\(f, ds) acc -> if d `elem` ds then Right f else acc)
  (Left $ FacultyNotFound d)
  faculties

returnDBResult :: a -> DBResult a
returnDBResult = Right

bindDBResult :: DBResult a -> (a -> DBResult b) -> DBResult b
bindDBResult (Left err) _ = Left err
bindDBResult (Right a) f  = f a

find' :: Name -> DBResult Department
find' name =
  returnDBResult name
  `bindDBResult`
  getClasses'
  `bindDBResult`
  getDepartment'
  `bindDBResult`
  getFaculty'
