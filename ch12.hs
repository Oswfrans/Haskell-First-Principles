--ch12
--1 * -> *
--2 ( ; *->*)

--string processing
--1
notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x = Just x

import Data.List

replaceThe :: String -> String
replaceThe x = replacer input where
  input = words x
  f :: [Char] -> [Char] --String -> [String]
  f "the" = "a"
  f x = x
  replacer :: [[Char]] -> [Char]
  replacer [] = ""
  replacer (z:zs) = f (z) ++" "++ replacer (zs)

isVowel :: Char -> Bool
isVowel = flip elem "aeiouy"

vowel :: String -> Bool
vowel = any isVowel

--count the before vowels
vowels = words "a e i o u y"

countTheBeforeVowel :: String -> [Int]
countTheBeforeVowel x = replacer input where
  input = words x
  
  f :: [Char] -> [Int] --String -> [String]
  f "the" = [1 :: Int]
  f x  | [head x] `elem` vowels = [2 :: Int]
       | not $ [head x] `elem` vowels = [0 :: Int]

  replacer :: [[Char]] -> [Int]
  replacer [] = [0 :: Int]
  replacer (z:zs) = f (z) ++ replacer (zs)

--countvowels
vowels = words "a e i o u y"
countVowels :: String -> Int
countVowels x = length [z | z <- x, [z] `elem` vowels]

--Validate
--vowels = words "a e i o u y"
vowels3 = words "a e i o u" ++ [" "]
countCons :: String -> Int
countCons x = length [z | z <- x, not ([z] `elem` vowels3) ]

newtype Word' = Word' String deriving (Eq, Show)
vowels2 = "aeiou"
mkWord :: String -> Maybe Word'
mkWord x | countVowels x > countCons x = Nothing
mkWord x | countVowels x  <= countCons x = Just (Word' x)

--Natural
data Nat =  Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger _ = 1+ integerToNat 

--integerToNat :: Integer -> Maybe Nat
integerToNat :: Integer -> Maybe Nat
integerToNat x | x <0 = Nothing
               | x==0 = Just Zero
               | x>0 = Just (Succ Zero)

--Maybe Library
--1
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isJust Nothing = True
isJust _ = False

--2
--mayybee :: b -> (a -> b) -> Maybe a -> b

--3
fromMaybe :: a -> Maybe a -> a
fromMaybe z Nothing = z
fromMaybe _ (Just x) = x

--4
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe [x:xs] = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

--5
catMaybes :: [Maybe a] -> [a]
catMaybes = concat $ map maybeToList
--catMaybes ls = [ x | Just x <- ls ]

--6
--flipMaybe :: [Maybe a] -> Maybe [a]

--Unfolds;
myIterate :: (a -> a) -> a -> [a]
myIterate function start = [start] ++ myIterate function (function start)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b =
  case f b of
    Nothing      -> []
    Just (a, b') -> a : myUnfoldr f b'

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\a -> Just (a, f a))

--building trees
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f a  = case f a of 
              Nothing -> Leaf
              Just (l, m ,r ) -> Node (unfold f l) m (unfold f r)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n
  | n < 0     = Leaf
  | otherwise = unfold f 0
    where
      f k
        | k == n    = Nothing
        | otherwise = Just (k+1, k, k+1)
