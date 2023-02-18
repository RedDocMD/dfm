module State
    ( PaneState(PaneState)
    , pathFiles
    , highlightedFileIdx
    , highlightNextFile
    , highlightPrevFile
    , highlightedFile
    , enterHighlightedFile
    , defaultPaneState
    , highlightedIdxOrder
    , pOffset
    , AppState(..)
    , defaultAppState
    , currPaneState
    , visibleFiles
    , currPanePath
    , scrollUp
    , scrollDown
    ) where


import qualified Data.IntMap                   as IM
import           Data.List.Extra
import           FS
import           System.Directory
import           System.FilePath
import           Util                           ( pathListHeight )


-- Contains the current state of the app
data PaneState = PaneState
    { mainPath           :: FilePath
    , pathFiles          :: [FSEntry]
    , highlightedFileIdx :: Int
    , listMode           :: FileListMode
    , pOffset            :: Int
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
                     , pOffset            = 0
                     }

-- Highlight the next file
highlightNextFile :: PaneState -> PaneState
highlightNextFile st = st
    { highlightedFileIdx = nextFileIdx (highlightedFileIdx st)
    }
    where nextFileIdx idx = min (length $ visibleFiles st) (idx + 1)

-- Highlight the prev file
highlightPrevFile :: PaneState -> PaneState
highlightPrevFile st = st
    { highlightedFileIdx = prevFileIdx (highlightedFileIdx st)
    }
    where prevFileIdx idx = max 0 (idx - 1)

-- Get current highlighted file
highlightedFile :: PaneState -> FilePath
highlightedFile st =
    mainPath st </> name (visibleFiles st !! highlightedFileIdx st)

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

-- Returns a string of the form "x/y" where file x is highlighted
-- with total of y files
highlightedIdxOrder :: PaneState -> String
highlightedIdxOrder ps = show (cs + 1) ++ "/" ++ show tot
  where
    cs  = highlightedFileIdx ps
    tot = length $ visibleFiles ps

-- Scroll down one line when height of terminal is
-- ht
paneScrollDown :: PaneState -> Int -> PaneState
paneScrollDown st ht = st { highlightedFileIdx = nIdx, pOffset = nOffset }
  where
    aht     = pathListHeight ht
    nIdx    = min (highlightedFileIdx st + 1) (length (visibleFiles st) - 1)
    offset  = pOffset st
    nOffset = if nIdx - offset + 1 > aht then offset + 1 else offset

-- Scroll up one line when height of terminal is
-- ht
paneScrollUp :: PaneState -> Int -> PaneState
paneScrollUp st _ = st { highlightedFileIdx = nIdx, pOffset = nOffset }
  where
    nIdx    = max (highlightedFileIdx st - 1) 0
    offset  = pOffset st
    nOffset = if nIdx < offset then offset - 1 else offset



data AppState = AppState
    { paneCnt    :: Int
    , currPane   :: Int
    , paneStates :: IM.IntMap PaneState
    }

-- Starting state of App
defaultAppState :: IO AppState
defaultAppState = defaultPaneState >>= \ps -> return AppState
    { paneCnt    = 4
    , currPane   = 1
    , paneStates = IM.fromList [(1, ps)]
    }

-- Current pane state
currPaneState :: AppState -> PaneState
currPaneState st = case IM.lookup (currPane st) (paneStates st) of
    Just ps -> ps
    Nothing -> error "invalid currPane"

-- Current pane path
currPanePath :: AppState -> FilePath
currPanePath = mainPath . currPaneState

-- Scroll up the current pane
scrollUp :: AppState -> Int -> AppState
scrollUp st ht =
    let ps  = currPaneState st
        nps = paneScrollUp ps ht
        pss = paneStates st
        cp  = currPane st
    in  st { paneStates = IM.insert cp nps pss }

-- Scroll down the current pane
scrollDown :: AppState -> Int -> AppState
scrollDown st ht =
    let ps  = currPaneState st
        nps = paneScrollDown ps ht
        pss = paneStates st
        cp  = currPane st
    in  st { paneStates = IM.insert cp nps pss }
