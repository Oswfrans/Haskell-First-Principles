eftInt :: Int -> Int -> [Int]
eftInt start stop = go start stop [] where
    go start stop res | start==stop = reverse res
                      | start /= stop = go (start+1) stop (((:[]) start) ++ res)

import Data.List (intersperse)
myWords :: [String] -> String
myWords = concat.(intersperse " ")

--- 2.

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen
-- putStrLn sentences -- should print
-- Tyger Tyger, burning bright
-- In the forests of the night
-- What immortal hand or eye
-- Could frame thy fearful symmetry?
-- Implement this
myLines :: String -> [String]
myLines "" = [""]
myLines x = (:[]) (takeWhile (/='\n') x) ++ myLines (drop 1 (dropWhile (/='\n') x))
-- What we want 'myLines sentences' to equal
shouldEqual =
[ "Tyger Tyger, burning bright"
, "In the forests of the night"
, "What immortal hand or eye"
, "Could frame thy fearful symmetry?"
]
-- The main function here is a small test
-- to ensure you've written your function
-- correctly.
main :: IO ()
main =
print $ "Are they equal? "
++ show (myLines sentences == shouldEqual)

let mySqr = [x^2 | x <- [1..5]]
let myCube = [y^3 | y <- [1..5]]
let sqrAndCube z k = [(x, y) | x <- z, y <- k ]
let sqrAndCubeLimit z k = [(x, y) | x <- z, y <- k , x<50, y <50]

length $ sqrAndCubeLimit mySqr myCube

blow it up
1. boom
2. run
3. boom
4. run
5. boom
6. run
7. boom
8. run
9. run
10. boom

1. 1
2. 2
3. 3
4. 3
5. 3
6. 2
7. 2

1. bottom
2. value
3. bottom
4. get a list of true and falses if something is a vowel
5. check
6. 
import Data.Bool

map (\x -> if x == 3 then (-x) else (x)) [1..10]

9.11
1. filter (\x -> (rem x 3)==0) [1..30]
2. check
3. 
myfilter :: String -> [String]
myfilter = (filter (/="an")).(filter (/="a")).words
myfilter2 :: String -> [String]
myfilter2 input =[x | x <- words input, x/="a", x/="an"]

zipping
 --meh

9.12

1. check
2.
import Data.Char
getUpper input = [x | x <- input, isUpper x]

3.
capFirst :: String -> String
capFirst input = toUpper head input ++ tail input

4.
capAll :: String -> String
capAll input = concat $ [x | x <- toUpper input]

5.
see 3 and drop everything after the ++

6. 
capComposed = toUpper.head

--Caesar Cipher 
module Cipher where
import Data.Char

--between 97 and 122
-- 97 + mod x 97 

preventDrift :: Int -> Int
-- dealing with spaces
preventDrift x | x==32 = 32
               | x/=32 = 97 + mod (mod x 97) 26

--preventDrift x =  97 + mod (mod x 97) 26
shifterPlus shift x | x==32 = 32
                    | x/=32 = x+ shift

shifterNeg shift x | x==32 = 32
                   | x/=32 = x - shift

--
import Data.List (intersperse)
import Data.Char

preventDrift :: Int -> Int
preventDrift x | x > 122 = (mod x 123) + 97
               | x < 97 =  123 - (mod (97-x) 26)
               | otherwise = x

--caesar :: String -> String
caesar input shift = concat $ (intersperse " ") ((map (map (chr.preventDrift.(+ shift).ord))) (words input))

--concat.(intersperse " ") 
--concat $ 
--unCaesar :: String -> String
unCaesar input shift = concat $ (intersperse " ") ((map (map (chr.preventDrift.(subtract shift).ord))) (words input))
