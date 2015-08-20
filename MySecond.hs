-- file: ch03/MySecond.hs
mySecond :: [a] -> a
-- can always call error, regardless of function type, terminates everything
mySecond xs = if null (tail xs)
                then error "list too short"
                else head (tail xs)
-- Maybe type handles errors without termination
safeSecond :: [a] -> Maybe a
safeSecond[] = Nothing
safeSecond xs = if null (tail xs)
    then Nothing
    else Just (head (tail xs))
-- pattern matching makes a nicer-looking function
tidySecond :: [a] -> Maybe a
-- only binds to first if there are at least two list constructors (:)
tidySecond (_:x:_) = Just x
tidySecond _ = Nothing
