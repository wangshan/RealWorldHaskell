import Control.Exception

{- again, new Exception typeclass is different from the old one described in
 - RWH, so had to modify code from the book
 -}
catchIt :: ArithException -> Maybe ()
catchIt DivideByZero = Just ()
catchIt _ = Nothing

handler :: () -> IO ()
handler _ = putStrLn "Caught Error: divide by zero"

safePrint :: Integer -> IO ()
safePrint x = handleJust catchIt handler (print $ 5 `div` x)

