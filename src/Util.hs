module Util
    ( pathListHeight
    , dirsBeforeFiles
    , parentPath
    , fileName
    , dirName
    , switchName
    , visibleFiles
    , FileListMode(..)
    , (!!?)
    , safeTail
    , mapLenSum
    , getUserName
    , pair
    ) where

import qualified Data.HashMap.Lazy as HM
import           Data.List.Extra   (splitOn)
import           FS
import           System.Posix.User  (userName, getRealUserID,
                                    getUserEntryForID)


-- No of lines for path list given terminal height
pathListHeight :: Int -> Int
pathListHeight h = h - 4

dirsBeforeFiles :: [FSEntry] -> [FSEntry]
dirsBeforeFiles fs =
    let isDir f = fileType f == Directory
        files = filter (not . isDir) fs
        dirs  = filter isDir fs
    in  dirs ++ files

parentPath :: FilePath -> FilePath
parentPath "/"  = "/"
parentPath path = "/" ++ intercalate "/" (init $ tail $ splitOn "/" path)

fileName :: FilePath -> FilePath
fileName = reverse . takeWhile (/= '/') . reverse

dirName :: FilePath -> String
dirName = last . splitOn "/"

switchName :: FilePath -> String -> FilePath
switchName old nName = parentPath old ++ "/" ++ nName

data FileListMode = Normal | Hidden deriving (Show, Eq)

-- List of visible files according to list mode
visibleFiles :: [FSEntry] -> FileListMode -> [FSEntry]
visibleFiles paths mode =
    let selector = if mode == Normal then not . isHiddenFile else const True
    in  filter selector paths

(!!?) :: [a] -> Int -> Maybe a
lst !!? idx = if idx >= length lst then Nothing else Just $ lst !! idx

safeTail :: [a] -> [a]
safeTail []       = []
safeTail (_ : xs) = xs

mapLenSum :: HashMap a [b] -> Int
mapLenSum = HM.foldl (\x y -> x + length y) 0

getUserName :: IO String
getUserName = do
    uid <- getRealUserID
    ue  <- getUserEntryForID uid
    return $ userName ue

pair :: a -> b -> (a, b)
pair x y = (x, y)
