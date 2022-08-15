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
    ) where


import qualified Data.IntMap                   as IM
import           System.Directory               ( doesDirectoryExist
                                                , getHomeDirectory
                                                , listDirectory
                                                )
import           System.FilePath                ( (</>) )


data FileListMode = Normal | Hidden deriving Show

-- Contains the current state of the app
data PaneState = PaneState
    { mainPath           :: FilePath
    , pathFiles          :: [FilePath]
    , highlightedFileIdx :: Int
    , listMode           :: FileListMode
    }
    deriving Show


-- Generate directories sorted by order
genDirs :: FilePath -> FileListMode -> IO [FilePath]
genDirs path mode = do
    paths <- listDirectory path
    let sortedPaths = sort paths
        nonDotPaths = filter (not . startsDot) sortedPaths
        startsDot []      = False
        startsDot (x : _) = x == '.'
    case mode of
        Hidden -> return sortedPaths
        Normal -> return nonDotPaths

-- Starting state for pane
defaultPaneState :: IO PaneState
defaultPaneState = do
    let mode = Normal
    mp <- getHomeDirectory
    pf <- genDirs mp mode
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
highlightedFile st = mainPath st </> (pathFiles st !! highlightedFileIdx st)

-- If the highlighted file is a directory, then that becomes the
-- main path. Otherwise, nothing happens.
enterHighlightedFile :: PaneState -> IO PaneState
enterHighlightedFile st = do
    let currPath = highlightedFile st
        mode     = listMode st
        newSt    = do
            contents <- genDirs currPath mode
            return
                (st { mainPath           = currPath
                    , pathFiles          = contents
                    , highlightedFileIdx = 0
                    }
                )
    isDir <- doesDirectoryExist currPath
    if isDir then return st else newSt


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

