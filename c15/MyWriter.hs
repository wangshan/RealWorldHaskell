-- examples from <Learn You a Haskell For Great Good>, I think it explains
-- reader/writer/state monad better than <Real World Haskell>, but for Writer
-- monad, the mtl library has changed from 1.* to 2.* and it made the examples
-- in LYH no longer valid, the below are a fixed version

import Control.Monad.Writer

-- Writer constuctor is no longer exported by mtl
logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["Going to multiple these two"]
    return (a * b)
