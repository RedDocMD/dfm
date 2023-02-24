module FS
    ( FSEntry(..)
    , isHiddenFile
    , listFSEntry
    , FileType(..)
    , copyAllFiles
    , deleteAllFiles
    ) where

import           Control.Monad.Extra            ( mconcatMapM )
import           System.Directory               ( copyFile
                                                , createDirectory
                                                , listDirectory
                                                , removeDirectory
                                                , removeFile
                                                )
import           System.FilePath
import           System.Posix.Files

data FileType = File | Directory | BlockDevice | CharDevice | NamedPipe | Socket deriving (Show, Eq)

data FSEntry = FSEntry
    { name      :: FilePath
    , fileType  :: FileType
    , isSymLink :: Bool
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

copyAllFiles :: [(FilePath, FSEntry)] -> FilePath -> IO ()
copyAllFiles xs drp = mconcatMapM (\x -> uncurry copySingleFile x drp) xs

copySingleFile :: FilePath -> FSEntry -> FilePath -> IO ()
copySingleFile srp fe drp = case fileType fe of
    Directory -> createDirectory dp >> listFSEntry sp >>= mconcatMapM
        (\x -> copySingleFile sp x dp)
    _ -> copyFile sp dp
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
