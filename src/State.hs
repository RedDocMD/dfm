module State
    ( AppState(AppState)
    , highlightNextFile
    , highlightPrevFile
    , highlightedFile
    ) where

import           System.FilePath

-- Contains the current state of the app
data AppState = AppState
    { mainPath           :: FilePath
    , pathFiles          :: [FilePath]
    , highlightedFileIdx :: Int
    }
    deriving Show


-- Highlight the next file
highlightNextFile :: AppState -> AppState
highlightNextFile st = st
    { highlightedFileIdx = nextFileIdx (highlightedFileIdx st)
    }
    where nextFileIdx idx = min (length $ pathFiles st) (idx + 1)

-- Highlight the prev file
highlightPrevFile :: AppState -> AppState
highlightPrevFile st = st
    { highlightedFileIdx = prevFileIdx (highlightedFileIdx st)
    }
    where prevFileIdx idx = max 0 (idx - 1)

-- Get current highlighted file
highlightedFile :: AppState -> FilePath
highlightedFile st = mainPath st </> (pathFiles st !! highlightedFileIdx st)
