-- file: applyTwice.hs
-- from Higher Order Functions of *Learn You A Haskell*
-- higher-order function that takes in a function and applies it twice!
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- higher-order programming to implement zipWith
-- first argument is function that gives single output from two parameters,
-- second two parameters are lists. Return list of [f a1 b1, f a2 b2, ...]
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- flip arguments of function
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f y x = f x y

-- map maps a function to every element in a list
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- filter takes a predicate and list and returns a list of elements satisfying
-- the predicate:
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

-- new version of quicksort using filter
quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
    smallerSorted ++ [x] ++ biggerSorted
    where smallerSorted = quicksort' (filter (<=x) xs)
          biggerSorted = quicksort' (filter (>x) xs)

-- filtering is nice and easy with functional programming!
-- find largest divisible number
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
    | f x       = x : takeWhile' f xs
    | otherwise = []

sumOddSquares = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

-- problem with Collatz sequences
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n  = n:chain (n*3 + 1)
-- my solution -- then take length of this in ghci
longChains = filter (>15) (map length (map chain [1,2..100]))
-- solution in LYAH
numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15
-- example of currying
-- mapping * over list creates [(0*),(1*),(2*)..]
-- so, (listOfFuns !! x ) y = x * y
listOfFuns = map (*) [0..]

-- rewrite numLongChains with lambda
numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))
-- makes point that lambdas don't have to be used when currying or partial
-- application will do, e.g.,
    -- map (+3) xs == map (\x -> x + 3) xs
-- are the same

-- by default, lambdas extend all the way to the right without parentheses
-- flip' :: (a -> b -> c) -> b -> a -> c
-- flip' f = \x y -> f y x
-- makes clear that function is used to be partially applied and passed to
-- a function

-- folds are functions that encapsulate the common recursive pattern of edge
-- case for empty list, action for x:xs. Fold takes binary function, starting
-- value ("accumulator") and a list to fold up.
-- Recursion: binary function is applied to starting value and head of list,
-- which produces a new accumulator. Function is applied to this and next
-- item in list, etc., until end of list is reached.

-- implement sum using fold instead of explicit recursion
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs
-- even more succinctly
sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0
-- the above works because of currying: foo a = bar b a is equiv to foo = bar b
-- do same with elem
elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys
-- notice that acc and end result has to have same type (Bool above)

-- foldr is same, except work from right, and acc is second argument for binary
-- function -- BUT, acc argument still comes before xs!
-- for example, implement map
map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs
-- make sure I can write map using foldl
map''' :: (a -> b) -> [a] -> [b]
map''' f xs = foldl (\acc x -> acc ++ [f x]) [] xs
-- notice that this isn't as good because ++ at the end of the list is slower
-- than : item to start of list. Could also do
map'''' :: (a -> b) -> [a] -> [b]
map'''' f xs = reverse (foldl (\acc x -> f x : acc) [] xs)

-- you can only foldr an infinite list (has an end working r to l) not foldl
-- foldl1 and foldr1 start with the first or last value as a starting value --
-- don't have to start off the accumulator. They cause a runtime error if you
-- pass an empty list.
sum''' = foldl1 (+)

-- present a bunch of standard functions using folds:
maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

-- scans are like folds, but report all intermediate accumulators in a list
-- can use them to count # of roots needed to get sum of 1000
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1
-- have to use takeWhile instead of filter because filter doesn't know that
-- once we pass 1000, we never will get below 1000 again

-- function application is represented by ($)
-- ($) :: (a -> b) -> a -> b
-- f $ x = f x
-- Unlike f a b c = ((f a) b) c, f $ g $ h $ i = f (g (h i))
-- also lets you do things like map ($ 3) [list of functions]

-- function application is represented by (.)
-- it's right associative, so f (g (h x)) = (f . g . z) x
-- also have to be careful about partial application, e.g.
-- sum . replicate 5 . max 6.7 $ 8.9
