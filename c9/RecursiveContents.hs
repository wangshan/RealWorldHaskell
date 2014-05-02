module RecursiveContents (getRecursiveContents) where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getCurrentDirectory, getDirectoryContents)
import System.FilePath ((</>))

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topDir = do
    names <- getDirectoryContents topDir
    let properNames = filter (`notElem` [".",".."]) names
    paths <- forM properNames $ \name -> do
        let path = topDir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then getRecursiveContents path
            else return [path]
    -- each iteration of the loop yields a list of names
    return (concat paths)
            
