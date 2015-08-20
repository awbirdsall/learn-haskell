-- file: ch03/ListADT.hs
-- define our own List a type recursively from Cons and Nil
-- every List starts with Nil, then Cons value Nil, etc...
data List a = Cons a (List a)
    | Nil
    deriving (Show)
-- prove our List is isomorphic to built-in list by going from list to List
fromList (x:xs) = Cons x (fromList xs)
fromList [] = Nil
-- write converse toList
toList (Cons x y) = (x:(toList y))
toList Nil = []
