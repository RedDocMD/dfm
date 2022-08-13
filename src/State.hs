module State
    ( PaneState(PaneState)
    , highlightNextFile
    , highlightPrevFile
    , highlightedFile
    , enterHighlightedFile
    ) where


import           System.Directory               ( doesDirectoryExist
                                                , getDirectoryContents
                                                )
import           System.FilePath                ( (</>) )


-- Contains the current state of the app
data PaneState = PaneState
    { mainPath           :: FilePath
    , pathFiles          :: [FilePath]
    , highlightedFileIdx :: Int
    }
    deriving Show


-- Highlight the next file
highlightNextFile :: PaneState -> PaneState
highlightNextFile st = st
    { highlightedFileIdx = nextFileIdx (highlightedFileIdx st)
    }
    where nextFileIdx idx = min (length $ pathFiles st) (idx + 1)

-- Highlight the prev file
highlightPrevFile :: PaneState -> PaneState
highlightPrevFile st = st
    { highlightedFileIdx = prevFileIdx (highlightedFileIdx st)
    }
    where prevFileIdx idx = max 0 (idx - 1)

-- Get current highlighted file
highlightedFile :: PaneState -> FilePath
highlightedFile st = mainPath st </> (pathFiles st !! highlightedFileIdx st)

-- If the highlighted file is a directory, then that becomes the
-- main path. Otherwise, nothing happens.
enterHighlightedFile :: PaneState -> IO PaneState
enterHighlightedFile st = do
    let currPath = highlightedFile st
        newSt    = do
            contents <- getDirectoryContents currPath
            return
                (st { mainPath           = currPath
                    , pathFiles          = contents
                    , highlightedFileIdx = 0
                    }
                )
    isDir <- doesDirectoryExist currPath
    if isDir then return st else newSt
