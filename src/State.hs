module State
    ( PaneState(PaneState)
    , SortOrder(..)
    , pathFiles
    , markedFiles
    , yankedFiles
    , cutFiles
    , mainPath
    , highlightedFileIdx
    , highlightNextFile
    , highlightPrevFile
    , highlightedFile
    , defaultPaneState
    , highlightedIdxOrder
    , pOffset
    , paneMarkedCount
    , paneYankedCount
    , paneCutCount
    , AppState(..)
    , AppMode(..)
    , isModeConflict
    , isModeRename
    , defaultAppState
    , resetAfterConflict
    , doRename
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
    , toggleMarkHighlightedFile
    , markAllFiles
    , unmarkAllFiles
    , clearAllMarkedFiles
    , yankMarkedFiles
    , cutMarkedFiles
    , pasteFiles
    , setSortOrder
    , enterRenameMode
    ) where


import qualified Data.HashMap.Lazy as HM
-- import           Control.Logging               as Log
import qualified Data.IntMap       as IM
import           Data.List.Extra
-- import           Data.Text                      ( pack )
import           FS
import           System.Directory
import           System.FilePath
import           System.Posix      (rename)
import           Util


data SortOrder = Name | NameDescending | Time | TimeDescending deriving (Show, Eq)

sortFnGen :: Ord a => (FSEntry -> a) -> (FSEntry -> FSEntry -> Ordering)
sortFnGen fn l r = fn l `compare` fn r

sortFnGenDesc :: Ord a => (FSEntry -> a) -> (FSEntry -> FSEntry -> Ordering)
sortFnGenDesc fn l r = fn r `compare` fn l

sortFn :: SortOrder -> (FSEntry -> FSEntry -> Ordering)
sortFn Name           = sortFnGen (lower . name)
sortFn NameDescending = sortFnGenDesc (lower . name)
sortFn Time           = sortFnGen mTime
sortFn TimeDescending = sortFnGenDesc mTime

-- Contains the current state of the app
data PaneState = PaneState
    { mainPath           :: FilePath
    , pathFiles          :: [FSEntry]
    , highlightedFileIdx :: Int
    , listMode           :: FileListMode
    , pOffset            :: Int
    , markedFiles        :: HM.HashMap FilePath [FSEntry]
    , yankedFiles        :: HM.HashMap FilePath [FSEntry]
    , cutFiles           :: HM.HashMap FilePath [FSEntry]
    , sortOrder          :: SortOrder
    }
    deriving Show


-- Generate directories sorted by order
genDirs :: FilePath -> SortOrder -> IO [FSEntry]
genDirs path so = listFSEntry path <&> dirsBeforeFiles . sortBy (sortFn so)

-- Starting state for pane
defaultPaneState :: IO PaneState
defaultPaneState = do
    let mode = Normal
        so   = Name
    mp <- getHomeDirectory
    pf <- genDirs mp so
    return PaneState { mainPath           = mp
                     , pathFiles          = pf
                     , highlightedFileIdx = 0
                     , listMode           = mode
                     , pOffset            = 0
                     , markedFiles        = HM.empty
                     , yankedFiles        = HM.empty
                     , cutFiles           = HM.empty
                     , sortOrder          = so
                     }

refreshPaneState :: PaneState -> IO PaneState
refreshPaneState st = do
  pf' <- genDirs (mainPath st) (sortOrder st)
  return $ st { pathFiles = pf' }

highlightName :: PaneState -> FilePath -> PaneState
highlightName st hName = let
  idx = find (\(_, fe) -> hName == name fe) (zip [0..] (paneVisibleFiles st))
  in case idx of
       Just (hIdx, _) -> st { highlightedFileIdx = hIdx }
       Nothing        -> st

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

highlightedFileEntry :: PaneState -> Maybe FSEntry
highlightedFileEntry st =
    let idx   = highlightedFileIdx st
        files = dirsBeforeFiles (paneVisibleFiles st)
    in  files !!? idx

-- Get current highlighted file
highlightedFile :: PaneState -> Maybe FilePath
highlightedFile st =
    fmap (\x -> mainPath st </> name x) (highlightedFileEntry st)

-- Open file at path
-- TODO: Actually open a file
openFile :: FilePath -> IO ()
openFile _ = return ()

-- If the highlighted file is a directory, then that becomes the
-- main path. If it is a file, open it.
paneEnterHighlightedFile :: PaneState -> IO PaneState
paneEnterHighlightedFile st = do
    let currPath  = highlightedFile st
        currEntry = highlightedFileEntry st
        so        = sortOrder st
        newSt path = do
            contents <- genDirs path so
            return
                (st { mainPath           = path
                    , pathFiles          = contents
                    , highlightedFileIdx = 0
                    , pOffset            = 0
                    }
                )
    case (currPath, currEntry) of
        (Just path, Just entry) -> case fileType entry of
            File      -> openFile path >> return st
            Directory -> newSt path
            _         -> return st
        _ -> return st

paneGotoParent :: PaneState -> IO PaneState
paneGotoParent st = do
    let currPath = mainPath st
        newPath  = parentPath currPath
        dName    = dirName currPath
        so       = sortOrder st
    if currPath == newPath
        then return st
        else do
            contents <- genDirs newPath so
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
    aht  = pathListHeight ht
    nIdx = max 0
        $ min (highlightedFileIdx st + 1) (length (paneVisibleFiles st) - 1)
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

paneToggleMarkHighlightedFile :: PaneState -> PaneState
paneToggleMarkHighlightedFile st =
    let file    = highlightedFileEntry st
        mp      = mainPath st
        marked  = markedFiles st
        yanked  = yankedFiles st
        cut     = cutFiles st
        mpFiles = HM.findWithDefault [] mp marked
        ypFiles = HM.findWithDefault [] mp yanked
        cpFiles = HM.findWithDefault [] mp cut
    in  case file of
            Just mFile -> if mFile `elem` mpFiles
                then st
                    { markedFiles = HM.insert mp (delete mFile mpFiles) marked
                    , yankedFiles = HM.insert mp (delete mFile ypFiles) yanked
                    , cutFiles    = HM.insert mp (delete mFile cpFiles) cut
                    }
                else st { markedFiles = HM.insert mp (mFile : mpFiles) marked }
            Nothing -> st

paneMarkAllFiles :: PaneState -> PaneState
paneMarkAllFiles st =
    st { markedFiles = HM.insert (mainPath st) (pathFiles st) (markedFiles st) }

paneUnmarkAllFiles :: PaneState -> PaneState
paneUnmarkAllFiles st = st { markedFiles = HM.insert mp [] marked
                           , yankedFiles = HM.insert mp [] yanked
                           , cutFiles    = HM.insert mp [] cut
                           }
  where
    mp     = mainPath st
    marked = markedFiles st
    yanked = yankedFiles st
    cut    = cutFiles st

paneClearAllMarkedFiles :: PaneState -> PaneState
paneClearAllMarkedFiles st =
    st { markedFiles = HM.empty, yankedFiles = HM.empty, cutFiles = HM.empty }

paneYankMarkedFiles :: PaneState -> PaneState
paneYankMarkedFiles st =
    st { yankedFiles = markedFiles st, cutFiles = HM.empty }

paneCutMarkedFiles :: PaneState -> PaneState
paneCutMarkedFiles st =
    st { yankedFiles = HM.empty, cutFiles = markedFiles st }

paneMarkedCount :: PaneState -> Int
paneMarkedCount = mapLenSum . markedFiles

paneYankedCount :: PaneState -> Int
paneYankedCount = mapLenSum . yankedFiles

paneCutCount :: PaneState -> Int
paneCutCount = mapLenSum . cutFiles

panePasteFiles :: PaneState -> IO (PaneState, CopyConflicts)
panePasteFiles st = do
    let yanked   = yankedFiles st
        cut      = cutFiles st
        mp       = mainPath st
        getPaths = concatMap (\(x, ys) -> [ (x, y) | y <- ys ]) . HM.toList
        so       = sortOrder st
    yankConflicts <- copyAllFiles (getPaths yanked) mp
    cutConflicts  <- copyAllFiles (getPaths cut)    mp
    deleteAllFiles (getPaths cut)
    contents <- genDirs mp so
    return (st { yankedFiles = HM.empty
                , cutFiles    = HM.empty
                , pathFiles   = contents
                }
            , yankConflicts <> cutConflicts)

paneSetSorderOrder :: PaneState -> SortOrder -> IO PaneState
paneSetSorderOrder st so = do
    pf <- genDirs (mainPath st) so
    return $ st { pathFiles = pf, sortOrder = so }


data AppMode = NormalMode | ConflictMode CopyConflicts | RenameMode FilePath

isModeConflict :: AppMode -> Bool
isModeConflict (ConflictMode _) = True
isModeConflict _                = False

isModeRename :: AppMode -> Bool
isModeRename (RenameMode _) = True
isModeRename _              = False

data AppState = AppState
    { paneCnt    :: Int
    , currPane   :: Int
    , paneStates :: IM.IntMap PaneState
    , tWidth     :: Int
    , tHeight    :: Int
    , tMode      :: AppMode
    }

-- Starting state of App, pass in the width and height
defaultAppState :: Int -> Int -> IO AppState
defaultAppState width height = defaultPaneState >>= \ps -> return AppState
    { paneCnt    = 4
    , currPane   = 1
    , paneStates = IM.fromList $ [ (i, ps) | i <- [1 .. 4] ]
    , tWidth     = width
    , tHeight    = height
    , tMode      = NormalMode
    }

resetAfterConflict :: AppState -> AppState
resetAfterConflict st = st { tMode = NormalMode }

doRename :: AppState -> FilePath -> FilePath -> IO AppState
doRename st old new = do
  rename old new
  let st'     = clearAllMarkedFiles st
      st''    = st' { tMode = NormalMode }
      newName = fileName new
  modifyCurrPaneIO refreshPaneState st''
    <&> modifyCurrPane (`highlightName` newName)

-- Current pane state
currPaneState :: AppState -> PaneState
currPaneState st = fromJust $ IM.lookup (currPane st) (paneStates st)

-- Current pane path
currPanePath :: AppState -> FilePath
currPanePath = mainPath . currPaneState

modifyCurrPane :: (PaneState -> PaneState) -> AppState -> AppState
modifyCurrPane fn st =
    let ps  = currPaneState st
        cp  = currPane st
        pss = paneStates st
        nps = fn ps
    in  st { paneStates = IM.insert cp nps pss }

modifyCurrPaneIO :: (PaneState -> IO PaneState) -> AppState -> IO AppState
modifyCurrPaneIO fn st = do
    let ps  = currPaneState st
        cp  = currPane st
        pss = paneStates st
    nps <- fn ps
    return $ st { paneStates = IM.insert cp nps pss }

-- Scroll up the current pane
scrollUp :: AppState -> AppState
scrollUp st = let ht = tHeight st in modifyCurrPane (`paneScrollUp` ht) st

-- Scroll down the current pane
scrollDown :: AppState -> AppState
scrollDown st = let ht = tHeight st in modifyCurrPane (`paneScrollDown` ht) st

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
enterHighlightedFile = modifyCurrPaneIO paneEnterHighlightedFile

gotoParent :: AppState -> IO AppState
gotoParent = modifyCurrPaneIO paneGotoParent

toggleMarkHighlightedFile :: AppState -> AppState
toggleMarkHighlightedFile = modifyCurrPane paneToggleMarkHighlightedFile

markAllFiles :: AppState -> AppState
markAllFiles = modifyCurrPane paneMarkAllFiles

unmarkAllFiles :: AppState -> AppState
unmarkAllFiles = modifyCurrPane paneUnmarkAllFiles

clearAllMarkedFiles :: AppState -> AppState
clearAllMarkedFiles = modifyCurrPane paneClearAllMarkedFiles

yankMarkedFiles :: AppState -> AppState
yankMarkedFiles = modifyCurrPane paneYankMarkedFiles

cutMarkedFiles :: AppState -> AppState
cutMarkedFiles = modifyCurrPane paneCutMarkedFiles

pasteFiles :: AppState -> IO AppState
pasteFiles st = do
    let ps  = currPaneState st
        cp  = currPane st
        pss = paneStates st
    (nps, mConflicts) <- panePasteFiles ps
    return $ st { paneStates = IM.insert cp nps pss
                , tMode      = ConflictMode mConflicts
                }

setSortOrder :: SortOrder -> AppState -> IO AppState
setSortOrder so = modifyCurrPaneIO (`paneSetSorderOrder` so)

enterRenameMode :: AppState -> AppState
enterRenameMode st = case hp of
  Just path -> st { tMode =  RenameMode path }
  Nothing   -> st
  where hp = highlightedFile $ currPaneState st
