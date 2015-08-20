-- file: quicksort.hs
-- can recursively implement sort by splitting a list into its head, items
-- smaller than the head, and items larger than the head.
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = smallerSorted ++ [x] ++ biggerSorted
    where smallerSorted = quicksort [a | a <- xs, a <=x]
          biggerSorted = quicksort [a | a <-xs, a > x]

