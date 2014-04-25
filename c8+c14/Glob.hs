module Glob (namesMatching) where

import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, getDirectoryContents)

import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>))

import Control.Exception (handle)
import Control.Monad (forM)
import GlobRegex (matchesGlob)


isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

namesMatching pat
    | not (isPattern pat) = do
        exists <- doesNameExist pat
        return (if exists then [pat] else [])
    | otherwise = do
        case splitFileName pat of
            ("", basename) -> do
                currDir <- getCurrentDirectory
                listMatches currDir basename
            -- must check the directory name and see if it contains patterns.
            -- If it does not, we create a singleton list of the directory
            -- name. If it contains a pattern, we list all of the matching
            -- directories
            (dirname, basename) -> do
                dirs <- if isPattern dirname
                        then namesMatching (dropTrailingPathSeparator dirname)
                        else return [dirname]
                let listDir = if isPattern basename
                              then listMatches
                              else listPlain
                pathNames <- forM dirs $ \dir -> do
                    baseNames <- listDir dir basename
                    return (map (dir </>) baseNames)
                return (concat pathNames)


-- returns a list of all files matching the given glob pattern in a directory
listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = do
    dirName' <- if null dirName
                then getCurrentDirectory
                else return dirName
    names <- getDirectoryContents dirName'
    let names' = if isHidden pat
                 then filter isHidden names
                 else filter (not . isHidden) names
    return (filter (`matchesGlob` pat) names')
                {--
    handle (const (return [])) $ do
        names <- getDirectoryContents dirName'
        let names' = if isHidden pat
                     then filter isHidden names
                     else filter (not . isHidden) names
        return (filter (`matchesGlob` pat) names')
        --}


isHidden ('.':_) = True
isHidden _       = False


-- returns either an empty or singleton list, depending on whether the
-- base name it's passed exists
listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
    exists <- if null baseName
              then doesDirectoryExist dirName
              else doesNameExist (dirName </> baseName)
    return (if exists then [baseName] else [])


doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
    fileExists <- doesFileExist name
    if fileExists
    then return True
    else doesDirectoryExist name


