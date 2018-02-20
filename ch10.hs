-ch10

--1 b, c
--2  etc. (2*(1*1))
--3 c
--4 a
--5

--1. 

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate x = filter isUTCTime x where 
    isUTCTime (UTCTime i) = True
    isUTCTime _ = False

--4.
sumDb :: [DatabaseItem] -> Integer

sumDb x = foldr func 0 x where
    func (DbNumber j) = (+j)
    func _ = (+0)

--scan excercises
--1
fibs = 1 : scanl (+) 1 fibs
fibsN = fibs !! 20

--2.
fibs = 1 : scanl (+) 1 fibs
fibsN = [x | x <- fibs, x<-100]
--let sqrAndCubeLimit z k = [(x, y) | x <- z, y <- k , x<50, y <50]

--10.10
--1.
--a.
stops = "pbtdkg"
vowels = "aeiou"

--generator :: [Char] -> [Char] -> [Char] -> [Char]
generator :: [t2] -> [t1] -> [t] -> [(t2, t1, t)]
generator xs ys zs = [(x, y, z) | x <- xs , y <- ys , z <- zs]
generator stops vowels stops

--b.
generatorP xs ys zs = [(x, y, z) | x <- xs , y <- ys , z <- zs, x=="p"]

--2.
seekritFunc x = div (sum (map length (words x))) (length (words x))
seekritFunc :: String -> Int

--Prime Number Machine
--1.
myOr :: [Bool] -> Bool
myOr = foldr (||) False

--2.
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (||.f) False

--3.
myElem x = any (== x)

--4.
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

--5.
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

--6.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then a : b else b) []

--7.
squish :: [[a]] -> [a]
squish = foldr (++) []
--squish = foldr (flip $ foldr (:)) []

--8.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []
--squishMap f = foldr (\a b -> foldr (:) b (f a)) []

--9.
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

--10.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldr (\a b -> if f a b == GT then a else b) (last xs) xs

--11.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldr (\a b -> if f a b == LT then a else b) (last xs) xs
