import Data.List
import System.IO
import Control.Monad (when)
import System.Exit
import System.Environment (getArgs)

main = do
    args <- getArgs

    when (length args /= 2) $ do
        putStrLn "Syntax: passwd-al filename uid"
        exitFailure

    content <- readFile (args !! 0)
    --putStrLn $ show content 

    -- compute username in pure code
    let username = findByUID content (read (args !! 1))

    case username of
        Just x -> putStrLn x
        Nothing -> putStrLn "Can't find the UID"


-- given the entire input and a UID, see if we can find a username
-- add a filter to ignore lines begin with #
findByUID :: String -> Integer -> Maybe String
findByUID content uid = 
    let al = map parseline . filter (\ln -> ln !! 0 /= '#') . lines $ content
        in lookup uid al

-- convert a colon spearated line into fields
parseline input = 
    let fields = split ':' input
        in (read (fields !! 2), fields !! 0)

-- take a delimiter and a list, break up the list based on the delimiter
split :: Eq a => a -> [a] -> [[a]]
split _ [] =[[]]
split delim str = 
    let (before, remainder) = span (/= delim) str
        in before : case remainder of
            [] -> []
            x  -> split delim (tail x) -- tail x because head is delim itself
            

