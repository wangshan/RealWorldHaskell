import Control.Monad (filterM, mapM, forM, liftM)
import System.Directory (Permissions(..), 
                         getPermissions,
                         getModificationTime,
                         getDirectoryContents,
                         searchable)
--new System.Directory no longer usese System.Time.Clock, it uses
--Data.Time.Clock.UTC.UTCTime instead
--import System.Time (ClockTime(..))
import Data.Time.Clock(UTCTime(..))
import System.FilePath (takeExtension, (</>))
import System.IO (IOMode(..), hFileSize, hClose, openFile)
import Control.Exception (bracket, handle, SomeException(..))

import RecursiveContents (getRecursiveContents)

type Predicate = FilePath -> Permissions -> Maybe Integer -> UTCTime -> Bool

simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
simpleFind predicate path = do
    names <- getRecursiveContents path
    return (filter predicate names)

-- must use (SomeException _), otherwise ghc complains
-- Ambiguous type variable arising from the use of `handle`
-- alternative is to define a named error handler:
{--
   errorHandler :: SomeException -> IO (Maybe Integer)
   errorHandler _ = return Nothing
   
   getFileSize path = handle errorHandler $ ....
 -}
getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle (\(SomeException _) -> return Nothing) $
    bracket (openFile path ReadMode) hClose $ \h -> do
        size <- hFileSize h
        return (Just size)

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind predicate path = getRecursiveContents path >>= filterM check
    where check name = do
            perm <- getPermissions name
            size <- getFileSize name
            modified <- getModificationTime name
            return (predicate name perm size modified) 

------------------- Controlling Traversal -------------------

data Info = Info {
    infoPath :: FilePath
    , infoPerms :: Maybe Permissions
    , infoSize :: Maybe Integer
    , infoModTime :: Maybe UTCTime
} deriving (Eq, Show, Ord)


traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
    names <- getUsefulContents path
    contents <- mapM getInfo (path: map (path </>) names) 
    liftM concat $ forM (order contents) $ \info -> do
       if isDirectory info && infoPath info /= path
           then traverse order (infoPath info)
           else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
    names <- getDirectoryContents path
    return (filter (`notElem` [".", ".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle (\(SomeException _) -> return Nothing) (Just `liftM` act)

getInfo :: FilePath -> IO Info
getInfo path = do
    perms <- maybeIO (getPermissions path)
    size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
    modified <- maybeIO (getModificationTime path)
    return (Info path perms size modified)

{-- a less expressive and less dense implementation 
traverseVerbose order path = do
    names <- getDirectoryContents path
    let usefulNames = filter (`notElem` [".", ".."]) names
    contents <- mapM getEntryName ("" : usefulNames)
    recursiveContents <- mapM recurse (order contents)
    return (concat recursiveContents)
    where getEntryName name =
        getInfo (path </> name) isDirectory info =
            case infoPerms info of Nothing -> False
                                   Just perms -> searchable perms
        recurse info = do
            if isDirectory info && infoPath info /= path
                then traverseVerbose order (infoPath info)
                else return [info]
--}
