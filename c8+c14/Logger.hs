module Logger
    ( Logger
    , Log
    , runLogger
    , record
    ) where

import Control.Monad (liftM, liftM2)

type Log = [String]

newtype Logger a = Logger { execLogger :: (a, Log) } deriving (Show)


-- runs the code inside the monad and unwraps its result
runLogger :: Logger a -> (a, Log)
runLogger = execLogger


-- since recording occurs in the plumbing of our monad, our action's result supplies no information
record :: String -> Logger ()
record s = Logger ((), [s])


instance Monad Logger where
    return a = Logger (a, [])

    --(>>=) :: Logger a -> (a -> Logger b) -> Logger b
    m >>= func = let (a, oldlog) = execLogger m
                     newMonad    = func a
                     (b, newlog) = execLogger newMonad
                 in Logger (b, oldlog ++ newlog)



globToRegex :: String -> Logger String
globToRegex cs = globToRegex' cs >>= \ds ->
                 return ('^' : ds)

globToRegex' :: String -> Logger String
globToRegex' "" = return "$"
globToRegex' ('?':cs) = record "any" >>
                        globToRegex' cs >>= \ds ->
                        return ('.' : ds)

globToRegex' ('*':cs) = record "star" >>
                        globToRegex' cs >>= \ds ->
                        return (".*" ++ ds)

globToRegex' ('[':'!':c:cs) = record "character class, negative" >>
                              charClass cs >>= \ds ->
                              return ("[^" ++ c : ds)

globToRegex' ('[':c:cs) = record "character class" >>
                          charClass cs >>= \ds ->
                          return ("[" ++ c : ds)

globToRegex' ('[':_) = fail "unterminated character class"


globToRegex' (c:cs) = liftM2 (++) (escape c) (globToRegex' cs)


escape :: Char -> Logger String
escape c | c `elem` regexChars = record "escape" >> return ['\\', c]
         | otherwise = return [c]
         where regexChars = "\\+()^$.{}]|"


charClass :: String -> Logger String
charClass (']':cs) = (']' :) `liftM` globToRegex' cs
charClass (c:cs)   = (c :) `liftM` charClass cs
charClass []       = fail "unterminated character class"
