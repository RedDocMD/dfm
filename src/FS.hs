module FS
    ( FSEntry(..)
    , isHiddenFile
    , listFSEntry
    , FileType(..)
    , copyAllFiles
    , deleteAllFiles
    , CopyConflicts(..)
    , hasNoConflicts
    ) where

import           Control.Monad.Extra (mconcatMapM)
import           Data.Default
import           System.Directory    (copyFileWithMetadata, createDirectory,
                                      doesDirectoryExist, doesFileExist,
                                      listDirectory, removeDirectory,
                                      removeFile)
import           System.FilePath
import           System.Posix        (EpochTime)
import           System.Posix.Files
import           Control.Logging               as Log
import           Data.Text                      ( pack )

data FileType = File | Directory | BlockDevice | CharDevice | NamedPipe | Socket deriving (Show, Eq)

data FSEntry = FSEntry
    { name      :: FilePath
    , fileType  :: FileType
    , isSymLink :: Bool
    , mTime     :: EpochTime
    }
    deriving (Show, Eq)

-- Check if it is hidden folder (ie, starts with dot)
isHiddenFile :: FSEntry -> Bool
isHiddenFile = startsDot . name
  where
    startsDot []      = False
    startsDot (x : _) = x == '.'

-- listDirectory but for FSEntry's
listFSEntry :: FilePath -> IO [FSEntry]
listFSEntry fp = do
    subEntries <- listDirectory fp
    let subPaths = map (fp </>) subEntries
    subTypes <- mapM fsType subPaths
    symLinks <- mapM isPathSymLink subPaths
    mTimes <- mapM fileMTime subPaths
    let entries = zipWith4
            (\nm t sl mt -> FSEntry { name = nm, fileType = t, isSymLink = sl, mTime = mt })
            subEntries
            subTypes
            symLinks
            mTimes
    return entries

fsType :: FilePath -> IO FileType
fsType path = getFileStatus path <&> fileStatusToType

isPathSymLink :: FilePath -> IO Bool
isPathSymLink path = getFileStatus path <&> isSymbolicLink

fileMTime :: FilePath -> IO EpochTime
fileMTime path = getFileStatus path <&> modificationTime

fileStatusToType :: FileStatus -> FileType
fileStatusToType fs | isBlockDevice fs     = BlockDevice
                    | isCharacterDevice fs = CharDevice
                    | isNamedPipe fs       = NamedPipe
                    | isRegularFile fs     = File
                    | isDirectory fs       = Directory
                    | isSocket fs          = Socket
                    | otherwise            = error "Unknown file type"

data CopyConflicts = CopyConflicts
       { fileToFile :: [FilePath]
       , dirToDir   :: [FilePath]
       , fileToDir  :: [FilePath]
       , dirToFile  :: [FilePath]
       }

instance Default CopyConflicts where
    def = CopyConflicts { fileToFile = []
                     , dirToDir   = []
                     , fileToDir  = []
                     , dirToFile  = []
                     }

instance Semigroup CopyConflicts where
    l <> r = CopyConflicts { fileToFile = fileToFile l ++ fileToFile r
                        , dirToDir   = dirToDir l ++ dirToDir r
                        , fileToDir  = fileToDir l ++ fileToDir r
                        , dirToFile  = dirToFile l ++ dirToFile r
                        }

instance Monoid CopyConflicts where
    mempty = def


hasNoConflicts :: CopyConflicts -> Bool
hasNoConflicts cc = null (fileToFile cc) && null (dirToDir cc) && null (fileToDir cc) && null (dirToFile cc)

copyAllFiles :: [(FilePath, FSEntry)] -> FilePath -> IO CopyConflicts
copyAllFiles xs drp = mconcatMapM (\x -> uncurry copySingleFile x drp) xs


copySingleFile :: FilePath -> FSEntry -> FilePath -> IO CopyConflicts
copySingleFile srp fe drp = do
    Log.log $ pack $ "sp = " ++ sp ++ ", dp = " ++ dp
    destFileExists      <- doesFileExist dp
    destDirectoryExists <- doesDirectoryExist dp
    case fileType fe of
        Directory -> case (destFileExists, destDirectoryExists) of
            (True , True ) -> error "dest cannot be both file and directory"
            (True , False) -> return $ def { dirToFile = [sp] }
            (False, True ) -> return $ def { dirToDir = [sp] }
            (False, False) ->
                createDirectory dp >> listFSEntry sp >>= mconcatMapM
                    (\x -> copySingleFile sp x dp)
        _ -> case (destFileExists, destDirectoryExists) of
            (True , True ) -> error "dest cannot be both file and directory"
            (True , False) -> return $ def { fileToFile = [sp] }
            (False, True ) -> return $ def { fileToDir = [sp] }
            (False, False) -> copyFileWithMetadata sp dp <&> def
  where
    sp = joinPath [srp, name fe]
    dp = joinPath [drp, name fe]

deleteAllFiles :: [(FilePath, FSEntry)] -> IO ()
deleteAllFiles = mconcatMapM (uncurry deleteSingleFile)

deleteSingleFile :: FilePath -> FSEntry -> IO ()
deleteSingleFile srp fe = case fileType fe of
    Directory ->
        listFSEntry sp
            >>= mconcatMapM (deleteSingleFile sp)
            >>  removeDirectory sp
    _ -> removeFile sp
    where sp = joinPath [srp, name fe]
