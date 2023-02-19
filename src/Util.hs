module Util
    ( pathListHeight
    , dirsBeforeFiles
    ) where

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
