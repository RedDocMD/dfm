module Util
    ( pathListHeight
    , dirsBeforeFiles
    , parentPath
    , dirName
    , visibleFiles
    , FileListMode(..)
    , (!!?)
    , safeTail
    , mapLenSum
    ) where

import qualified Data.HashMap.Lazy             as HM
import           Data.List.Extra                ( splitOn )
import           FS


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

dirName :: FilePath -> String
dirName = last . splitOn "/"

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
