{-# LANGUAGE TemplateHaskell #-}
import Data.List
import Test.QuickCheck
--import Test.QuickCheck.Batch

qsort :: (Ord a) => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort ltree ++ [x] ++ qsort rtree
             where ltree = filter (<x) xs
                   rtree = filter (>=x) xs

prop_idemponent xs = qsort (qsort xs) == qsort xs
prop_minimum xs = not (null xs) ==> head (qsort xs) == minimum xs
prop_maximum xs = not (null xs) ==> last (qsort xs) == maximum xs
prop_ordered xs = ordered (qsort xs)
    where ordered [] = True
          ordered [x] = True
          ordered (x:y:xs) = x<=y && ordered (y:xs)

prop_permutation xs = permutation xs (qsort xs)
    where permutation xs ys = null (xs \\ ys) && null (ys \\ xs)

prop_append xs ys = not (null xs) ==> 
                    not (null ys) ==>
                    min (minimum xs) (minimum ys) == head (qsort (xs++ys))

runTests = $quickCheckAll

main = runTests >>= \passed -> if passed then putStrLn "All tests passed."
                                         else putStrLn "Some tests failed."
    
{--
options = TestOptions
    { no_of_tests = 200
    , length_of_tests = 1
    , debug_tests = False
    }

main = do
    runTests "simple" options
        [ run prop_minimum
        , run prop_maximum
        , run prop_ordered
        , run prop_permutation
        , run prop_append
        ]
--}


    {--
main = do
    quickCheck (prop_minimum :: [Integer] -> Property)
    quickCheck (prop_maximum :: [Integer] -> Property)
    quickCheck (prop_ordered :: [Integer] -> Bool)
    quickCheck (prop_permutation :: [Integer] -> Bool)
    quickCheck (prop_append :: [Integer] -> [Integer] -> Property)
    --}
