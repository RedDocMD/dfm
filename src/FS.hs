module FS
    ( FSEntry(..)
    , isHiddenFile
    , listFSEntry
    , FileType(..)
    , copyAllFiles
    , deleteAllFiles
    ) where

import           Control.Monad.Extra            ( mconcatMapM )
import           Data.Default
import           System.Directory               ( copyFileWithMetadata
                                                , createDirectory
                                                , doesDirectoryExist
                                                , doesFileExist
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

data CopyResult = CopyResult
    { fileToFile :: [FilePath]
    , dirToDir   :: [FilePath]
    , fileToDir  :: [FilePath]
    , dirToFile  :: [FilePath]
    }

instance Default CopyResult where
    def = CopyResult { fileToFile = []
                     , dirToDir   = []
                     , fileToDir  = []
                     , dirToFile  = []
                     }

instance Semigroup CopyResult where
    l <> r = CopyResult { fileToFile = fileToFile l ++ fileToFile r
                        , dirToDir   = dirToDir l ++ dirToDir r
                        , fileToDir  = fileToDir l ++ fileToDir r
                        , dirToFile  = dirToFile l ++ dirToFile r
                        }

instance Monoid CopyResult where
    mempty = def

copyAllFiles :: [(FilePath, FSEntry)] -> FilePath -> IO CopyResult
copyAllFiles xs drp = mconcatMapM (\x -> uncurry copySingleFile x drp) xs


copySingleFile :: FilePath -> FSEntry -> FilePath -> IO CopyResult
copySingleFile srp fe drp = do
    destFileExists      <- doesFileExist sp
    destDirectoryExists <- doesDirectoryExist sp
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
