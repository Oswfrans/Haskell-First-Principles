module "8" where
  summer :: (Eq a, Num a) => a -> a
  summer 0=0
  summer x = x + summer(x-1)

module "9" where
  multi :: (Integral a) => a -> a -> a
  multi 1 y = y
  multi x 1 = x
  multi x y = x + multi x (y-1)
  
  --multi x y = go x y 0
  --  where go xz yz 
  
module "McCarthy" where
  mc91 n | n>100= n-10
         | n<100 = 91
         
module WordNumber where
import Data.List (intersperse)
--digitToWord :: Int -> String
--digitToWord n = undefined
converter :: Integer -> String
converter 1 = "one"
converter 2 = "two"
converter 3 = "three"
converter 4 = "four"
converter 5 = "five"
converter 6 = "six"
converter 7 = "seven"
converter 8 = "eight"
converter 9 = "nine"
converter 0 = "zero"

--import Data.Char
toDigits :: Integer -> [Integer]
toDigits n = map (\x -> toInteger (digitToInt x)) (show n)

--combiner :: [String] -> String
--combiner = 

wordNumber :: Integer -> String
wordNumber= concat.(intersperse "-").(map converter).toDigits 
