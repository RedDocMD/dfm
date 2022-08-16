module FS
    ( FSEntry(..)
    , isHiddenFile
    , listFSEntry
    , FileType(..)
    ) where

import           System.Directory               ( listDirectory )
import           System.FilePath
import           System.Posix.Files

data FileType = File | Directory | BlockDevice | CharDevice | NamedPipe | Socket deriving (Show, Eq)

data FSEntry = FSEntry
    { name      :: FilePath
    , fileType  :: FileType
    , isSymLink :: Bool
    }
    deriving Show

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
    let entries = zipWith3
            (\nm t sl -> FSEntry { name = nm, fileType = t, isSymLink = sl })
            subEntries
            subTypes
            symLinks
    return entries

fsType :: FilePath -> IO FileType
fsType path = getFileStatus path >>= \x -> return $ fileStatusToType x

isPathSymLink :: FilePath -> IO Bool
isPathSymLink path = getFileStatus path >>= \x -> return $ isSymbolicLink x

fileStatusToType :: FileStatus -> FileType
fileStatusToType fs | isBlockDevice fs     = BlockDevice
                    | isCharacterDevice fs = CharDevice
                    | isNamedPipe fs       = NamedPipe
                    | isRegularFile fs     = File
                    | isDirectory fs       = Directory
                    | otherwise            = error "Unknown file type"

