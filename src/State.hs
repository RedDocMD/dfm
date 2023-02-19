module State
    ( PaneState(PaneState)
    , pathFiles
    , highlightedFileIdx
    , highlightNextFile
    , highlightPrevFile
    , highlightedFile
    , defaultPaneState
    , highlightedIdxOrder
    , pOffset
    , AppState(..)
    , defaultAppState
    , currPaneState
    , paneVisibleFiles
    , currPanePath
    , scrollUp
    , scrollDown
    , resizeTerminal
    , recalculateOffsets
    , setCurrentPane
    , enterHighlightedFile
    , gotoParent
    ) where


-- import           Control.Logging               as Log
import qualified Data.IntMap                   as IM
import           Data.List.Extra
-- import           Data.Text                      ( pack )
import           FS
import           System.Directory
import           System.FilePath
import           Util


-- Contains the current state of the app
data PaneState = PaneState
    { mainPath           :: FilePath
    , pathFiles          :: [FSEntry]
    , highlightedFileIdx :: Int
    , listMode           :: FileListMode
    , pOffset            :: Int
    }
    deriving Show


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
highlightNextFile st = st { highlightedFileIdx = nIdx }
  where
    nextFileIdx idx = min (length $ paneVisibleFiles st) (idx + 1)
    nIdx = nextFileIdx (highlightedFileIdx st)

-- Highlight the prev file
highlightPrevFile :: PaneState -> PaneState
highlightPrevFile st = st { highlightedFileIdx = pIdx }
  where
    prevFileIdx idx = max 0 (idx - 1)
    pIdx = prevFileIdx (highlightedFileIdx st)

highlightedFileEntry :: PaneState -> FSEntry
highlightedFileEntry st =
    dirsBeforeFiles (paneVisibleFiles st) !! highlightedFileIdx st

-- Get current highlighted file
highlightedFile :: PaneState -> FilePath
highlightedFile st = mainPath st </> name (highlightedFileEntry st)

-- Open file at path
-- TODO: Actually open a file
openFile :: FilePath -> IO ()
openFile path = return ()

-- If the highlighted file is a directory, then that becomes the
-- main path. If it is a file, open it.
paneEnterHighlightedFile :: PaneState -> IO PaneState
paneEnterHighlightedFile st = do
    let currPath  = highlightedFile st
        currEntry = highlightedFileEntry st
        newSt     = do
            contents <- genDirs currPath
            return
                (st { mainPath           = currPath
                    , pathFiles          = contents
                    , highlightedFileIdx = 0
                    , pOffset            = 0
                    }
                )
        ft = fileType currEntry
    case ft of
        File      -> openFile currPath >> return st
        Directory -> newSt
        _         -> return st

paneGotoParent :: PaneState -> IO PaneState
paneGotoParent st = do
    let currPath = mainPath st
        newPath  = parentPath currPath
        dName    = dirName currPath
    if currPath == newPath
        then return st
        else do
            contents <- genDirs newPath
            let dIdx = findIndex
                    ((==) dName . name)
                    (dirsBeforeFiles $ visibleFiles contents (listMode st))
            return
                (st { mainPath           = newPath
                    , pathFiles          = contents
                    , highlightedFileIdx = fromJust dIdx
                    , pOffset            = 0
                    }
                )

-- List of visible files according to list mode
paneVisibleFiles :: PaneState -> [FSEntry]
paneVisibleFiles ps = visibleFiles (pathFiles ps) (listMode ps)

-- Returns a string of the form "x/y" where file x is highlighted
-- with total of y files
highlightedIdxOrder :: PaneState -> String
highlightedIdxOrder ps = show (cs + 1) ++ "/" ++ show tot
  where
    cs  = highlightedFileIdx ps
    tot = length $ paneVisibleFiles ps

-- Scroll down one line when height of terminal is
-- ht
paneScrollDown :: PaneState -> Int -> PaneState
paneScrollDown st ht = st { highlightedFileIdx = nIdx, pOffset = nOffset }
  where
    aht     = pathListHeight ht
    nIdx    = min (highlightedFileIdx st + 1) (length (paneVisibleFiles st) - 1)
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

paneRecalculateOffset :: PaneState -> Int -> PaneState
paneRecalculateOffset st ht =
    let aht     = pathListHeight ht
        idx     = highlightedFileIdx st
        off     = pOffset st
        offDiff = max (idx - (off + aht) + 1) 0
    in  st { pOffset = off + offDiff }



data AppState = AppState
    { paneCnt    :: Int
    , currPane   :: Int
    , paneStates :: IM.IntMap PaneState
    , tWidth     :: Int
    , tHeight    :: Int
    }

-- Starting state of App, pass in the width and height
defaultAppState :: Int -> Int -> IO AppState
defaultAppState width height = defaultPaneState >>= \ps -> return AppState
    { paneCnt    = 4
    , currPane   = 1
    , paneStates = IM.fromList $ [ (i, ps) | i <- [1 .. 4] ]
    , tWidth     = width
    , tHeight    = height
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
scrollUp :: AppState -> AppState
scrollUp st =
    let ps  = currPaneState st
        ht  = tHeight st
        nps = paneScrollUp ps ht
        pss = paneStates st
        cp  = currPane st
    in  st { paneStates = IM.insert cp nps pss }

-- Scroll down the current pane
scrollDown :: AppState -> AppState
scrollDown st =
    let ps  = currPaneState st
        ht  = tHeight st
        nps = paneScrollDown ps ht
        pss = paneStates st
        cp  = currPane st
    in  st { paneStates = IM.insert cp nps pss }

resizeTerminal :: AppState -> Int -> Int -> AppState
resizeTerminal st width height = st { tWidth = width, tHeight = height }

recalculateOffsets :: AppState -> AppState
recalculateOffsets st =
    let ht          = tHeight st
        nPaneStates = IM.map (`paneRecalculateOffset` ht) (paneStates st)
    in  st { paneStates = nPaneStates }

setCurrentPane :: AppState -> Int -> AppState
setCurrentPane st idx =
    st { currPane = if idx >= 1 && idx <= paneCnt st then idx else currPane st }

enterHighlightedFile :: AppState -> IO AppState
enterHighlightedFile st = do
    let cp  = currPane st
        ps  = currPaneState st
        pss = paneStates st
    nps <- paneEnterHighlightedFile ps
    let nPaneStates = IM.insert cp nps pss
    return $ st { paneStates = nPaneStates }

gotoParent :: AppState -> IO AppState
gotoParent st = do
    let cp  = currPane st
        ps  = currPaneState st
        pss = paneStates st
    nps <- paneGotoParent ps
    let nPaneStates = IM.insert cp nps pss
    return $ st { paneStates = nPaneStates }
