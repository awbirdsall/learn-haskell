-- file: ch03ex.hs
-- end of chapter exercises
myLength :: [a] -> Int
myLength (_:xs) = 1 + myLength(xs)
myLength [] = 0

-- listMean :: [a] -> Double -- is wrong
listMean a = (sum a) / (fromIntegral (myLength a))

-- palindromize :: [a] -> [a]
palindromize a = a ++ (backwards a)

backwards a
    | (length a) >= 2 = (last a):(backwards (take (length a - 1) a))
    | otherwise = a

-- isPalindrome :: [a] -> Bool
isPalindrome a
    | (length a > 2) = (head a == last a) && (isPalindrome (inside a))
    | (length a <= 2) = True

inside a = backwards (tail (backwards (tail a)))

