module State
    ( PaneState(PaneState)
    , pathFiles
    , highlightedFileIdx
    , highlightNextFile
    , highlightPrevFile
    , highlightedFile
    , enterHighlightedFile
    , defaultPaneState
    , AppState(AppState)
    , defaultAppState
    , currPaneState
    , visibleFiles
    ) where


import qualified Data.IntMap                   as IM
import           Data.List.Extra
import           FS
import           System.Directory
import           System.FilePath


-- Contains the current state of the app
data PaneState = PaneState
    { mainPath           :: FilePath
    , pathFiles          :: [FSEntry]
    , highlightedFileIdx :: Int
    , listMode           :: FileListMode
    }
    deriving Show

data FileListMode = Normal | Hidden deriving (Show, Eq)


-- Generate directories sorted by order
genDirs :: FilePath -> IO [FSEntry]
genDirs path = listFSEntry path >>= \x ->
    return $ sortBy (\l r -> lower (name l) `compare` lower (name r)) x

-- Starting state for pane
defaultPaneState :: IO PaneState
defaultPaneState = do
    let mode = Normal
    mp <- getHomeDirectory
    pf <- genDirs mp
    return PaneState { mainPath           = mp
                     , pathFiles          = pf
                     , highlightedFileIdx = 0
                     , listMode           = mode
                     }

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
highlightedFile st =
    mainPath st </> name (pathFiles st !! highlightedFileIdx st)

-- If the highlighted file is a directory, then that becomes the
-- main path. Otherwise, nothing happens.
enterHighlightedFile :: PaneState -> IO PaneState
enterHighlightedFile st = do
    let currPath = highlightedFile st
        newSt    = do
            contents <- genDirs currPath
            return
                (st { mainPath           = currPath
                    , pathFiles          = contents
                    , highlightedFileIdx = 0
                    }
                )
    isDir <- doesDirectoryExist currPath
    if isDir then return st else newSt


-- List of visible files according to list mode
visibleFiles :: PaneState -> [FSEntry]
visibleFiles ps =
    let selector =
            if listMode ps == Normal then not . isHiddenFile else const True
    in  filter selector (pathFiles ps)


data AppState = AppState
    { paneCnt    :: Int
    , currPane   :: Int
    , paneStates :: IM.IntMap PaneState
    }

-- Starting state of App
defaultAppState :: IO AppState
defaultAppState = defaultPaneState >>= \ps -> return AppState
    { paneCnt    = 1
    , currPane   = 1
    , paneStates = IM.fromList [(1, ps)]
    }

-- Current pane state
currPaneState :: AppState -> PaneState
currPaneState st = case IM.lookup (currPane st) (paneStates st) of
    Just ps -> ps
    Nothing -> error "invalid currPane"
