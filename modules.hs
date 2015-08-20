-- file: modules.hs
-- Modules chapter of Learn you a Haskell

-- import at the top of a file
import Data.List

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

-- in ghci, ':m + Data.List'

-- selective import: import Data.List (nub, sort)
-- import Data.List hiding (nub)
-- import qualified Data.Map
-- import qualified Data.Map as M
