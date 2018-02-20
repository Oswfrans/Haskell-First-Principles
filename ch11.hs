--ch11
data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultR'Us | TakeYourChancesUnited 
               deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
              | Plane Airline
              deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car Manufacturer _) = Manufacturer
--getManu (Plane _) = ERROR 
--getManu _ = ERROR

--Cardinality
--1. 1
--2. 3
--3. 65536
--4. bleh
--5. 2^x

--1 Example
--2 not in scope
--3  data Example Int  = MakeExample Int deriving Show  END  Int -> Example a

--sum types
--1 2
--2 258

--Jammin Excercises
--?????????????????????????

--normal form
--1  all iterations of data Garden = String Gardenia | etc.

--binary tree recursive
data BinaryTree a = Leaf| Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)
-- acceptance test for mapTree
mapOkay = if mapTree (+1) testTree' == mapExpected then print "yup okay!" else error "test failed!"


--convert to lists
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right)= [a] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right)= preorder left ++ [a] ++ preorder right  --[preorder left, a, preorder right]

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right)= preorder left ++ preorder right ++ [a] --[preorder left, preorder right, a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
    if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
    if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
    if postorder testTree == [1, 3, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "postorder failed check"

main :: IO ()
main = do
    testPreorder
    testInorder
    testPostorder

--fold tree
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f z Leaf = z
foldTree f z (Node left a right) =  foldTree f (f a (foldTree f z left)) right --(foldTree f (f a) left) (foldTree f (f a) right)  --Node (mapTree f left) (f a) (mapTree f right)

--1 a
--2 c
--3 b
--4 c

--ciphers

module Cipher where
import Data.Char

--between 97 and 122
-- 97 + mod x 97 

--
import Data.List (intersperse)
import Data.Char

preventDrift :: Int -> Int
preventDrift x | x > 122 = (mod x 123) + 97
               | x < 97 =  123 - (mod (97-x) 26)
               | otherwise = x

shifter :: [Char] -> [Int]
shifter x  = cycle (map ((subtract 97).ord.toLower) x)

shiftingSand inputAdapt= (ord (fst inputAdapt)) - snd inputAdapt

--this does not deal well with spaces
vigenere input key = map (chr.preventDrift.shiftingSand) $ zip input (shifter key)
--vigenere input key = concat $ (intersperse " ") ( ( map (map (chr.preventDrift.shiftingSand))) (zip input (shifter key ) ) )
--unCaesar input shift = concat $ (intersperse " ") ((map (map (chr.preventDrift.(subtract shift).ord))) (words input))

--as patterns
module AsPatterns where

import Data.Char (toUpper)

isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf s t = go s t
  where
    go [] _ = True
    go _ [] = False
    go (a:as) (b:bs) = (a == b && go as bs) || go s bs

capitalizeWords :: String -> [(String, String)]
capitalizeWords = go . words
  where
    go [] = []
    go (w@(c:cs):ws) = (w, toUpper c : cs) : go ws

--phone excercises
--see seperate file

--huntons razor
data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit n) = n
eval (Add a b) = eval a + eval b

printExpr :: Expr -> String
printExpr (Lit n) = show n
printExpr (Add a b) = printExpr a ++ " + " ++ printExpr b
