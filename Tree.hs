-- file: Tree.hs
-- binary tree type as example of recurisve type. Tree is either node with two
-- tree children, or empty.
data Tree a = Node a (Tree a) (Tree a)
    | Empty
    deriving (Show)
-- as example, supply Empty constructor to terminate tree
simpleTree = Node "parent" (Node "left child" Empty Empty)
                            (Node "right child" Empty Empty)
