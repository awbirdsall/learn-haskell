-- file: ch02/myDrop.hs
myDrop n xs = if n <= 0 || null xs
                then xs
                else myDrop (n - 1) (tail xs)
-- new ch03 version with patterns and guards -- more readable
niceDrop n xs | n <=0 = xs
niceDrop _ []         = []
niceDrop n (_:xs)     = niceDrop (n - 1) xs
