module Lib
    ( renderState
    ) where

import           FS                             ( FSEntry
                                                    ( fileType
                                                    , isSymLink
                                                    , name
                                                    )
                                                , FileType(..)
                                                )
import           Graphics.Vty
import           State                          ( AppState(currPane, paneCnt)
                                                , PaneState
                                                , currPanePath
                                                , currPaneState
                                                , highlightedFile
                                                , highlightedFileIdx
                                                , highlightedIdxOrder
                                                , visibleFiles
                                                )
import           System.Posix


terminalSize :: Config -> IO DisplayRegion
terminalSize cfg = outputForConfig cfg >>= displayBounds

terminalWidth :: Config -> IO Int
terminalWidth = fmap regionWidth . terminalSize

terminalHeight :: Config -> IO Int
terminalHeight = fmap regionHeight . terminalSize


-- Primary rendering function
-- Given a Config of the terminal and an AppState,
-- it will give you an Image.
renderState :: Config -> AppState -> IO Image
renderState cfg st = do
    width  <- terminalWidth cfg
    height <- terminalHeight cfg
    let ps       = currPaneState st
        topBar   = renderTopBar st
        mainArea = renderMainArea ps height width
    bottomBar <- renderBottomBar ps
    return $ topBar `vertJoin` mainArea `vertJoin` bottomBar


renderNormalPath :: FSEntry -> Image
renderNormalPath fse
    | ft == Directory = if sl then renderDirSymLink nm else renderDir nm
    | sl              = renderSymLink nm
    | ft == File      = renderFile nm
    | otherwise       = renderSpecFile nm
  where
    ft = fileType fse
    sl = isSymLink fse
    nm = name fse

    renderDir fp =
        string (defAttr `withForeColor` blue `withStyle` bold) fp
            Graphics.Vty.<|> string defAttr "/"
    renderDirSymLink fp =
        string (defAttr `withForeColor` brightCyan `withStyle` bold) fp
            Graphics.Vty.<|> string defAttr "/"
    renderSymLink fp = string (defAttr `withForeColor` cyan) (fp ++ "@")
    renderFile     = string defAttr
    renderSpecFile = string (defAttr `withForeColor` yellow)


renderNormalPaths :: [FSEntry] -> Image
renderNormalPaths = foldl vertJoin emptyImage . map renderNormalPath


renderSelectedPath :: FSEntry -> Image
renderSelectedPath fse
    | ft == Directory = if sl then renderDirSymLink nm else renderDir nm
    | sl              = renderSymLink nm
    | ft == File      = renderFile nm
    | otherwise       = renderSpecFile nm
  where
    ft = fileType fse
    sl = isSymLink fse
    nm = name fse

    renderDir fp =
        string
                (               defAttr
                `withForeColor` black
                `withBackColor` blue
                `withStyle`     bold
                )
                fp
            Graphics.Vty.<|> string defAttr "/"
    renderDirSymLink fp =
        string
                (               defAttr
                `withForeColor` black
                `withBackColor` brightCyan
                `withStyle`     bold
                )
                fp
            Graphics.Vty.<|> string defAttr "/"
    renderSymLink fp =
        string (defAttr `withForeColor` black `withBackColor` cyan) fp
            Graphics.Vty.<|> string defAttr "@"
    renderFile = string (defAttr `withForeColor` black `withBackColor` white)
    renderSpecFile =
        string (defAttr `withForeColor` black `withBackColor` yellow)

-- Renders the list of paths
renderPathList :: PaneState -> Image
renderPathList st =
    renderNormalPaths before
        <-> renderSelectedPath sel
        <-> renderNormalPaths after
  where
    paths  = dirsBeforeFiles $ visibleFiles st
    sidx   = highlightedFileIdx st
    before = take sidx paths
    sel    = paths !! sidx
    after  = tail $ drop sidx paths

dirsBeforeFiles :: [FSEntry] -> [FSEntry]
dirsBeforeFiles fs =
    let isDir f = fileType f == Directory
        files = filter (not . isDir) fs
        dirs  = filter isDir fs
    in  dirs ++ files


-- Renders the little pane seletor list at the top
renderPaneList :: AppState -> Image
renderPaneList st = foldl horizJoin emptyImage $ intersperse spaceImg mainImgs
  where
    pc         = paneCnt st
    cp         = currPane st
    before     = take (cp - 1) [1 .. pc]
    after      = drop cp [1 .. pc]
    beforeImgs = map renderUnselectedPane before
    afterImgs  = map renderUnselectedPane after
    mainImgs   = beforeImgs ++ renderSelectedPane cp : afterImgs

    renderUnselectedPane n = string (defAttr `withForeColor` blue) (show n)
    renderSelectedPane n =
        string (defAttr `withForeColor` black `withBackColor` blue) (show n)
    spaceImg = string defAttr " "

plWidth :: Int
plWidth = 7

-- Renders the entire top bar, with panel selector and path
-- and a box around them
renderTopBar :: AppState -> Image
renderTopBar st =
    let pl       = renderPaneList st
        path     = currPanePath st
        spaceImg = string defAttr " "
        pimg =
            spaceImg
                `horizJoin` string
                                (               defAttr
                                `withForeColor` blue
                                `withStyle`     underline
                                )
                                path
    in  pl `horizJoin` pimg


verticalSpacer :: Int -> Int -> Image
verticalSpacer width n = foldl vertJoin emptyImage
    $ replicate n (string defAttr $ replicate width ' ')

-- FIXME: Handle horizontal wrapping of paths
-- FIXME: Handle vertical scrolling and offsets
renderPathListSide :: PaneState -> Int -> Int -> Image
renderPathListSide st height width =
    let pl         = renderPathList st
        leftSpacer = verticalSpacer (plWidth + 1) (height - 3)
        spaceLine  = string defAttr $ replicate width ' '
    in  spaceLine `vertJoin` (leftSpacer `horizJoin` pl)

renderMainArea :: PaneState -> Int -> Int -> Image
renderMainArea = renderPathListSide

renderBottomBar :: PaneState -> IO Image
renderBottomBar ps = do
    let hf = highlightedFile ps
    mtime <- modTime hf
    let timeImg = string attr $ formatTime defaultTimeLocale timeFormat mtime
    perm <- permissions hf
    let permImg = string attr perm
    sz <- size hf
    let szImg = string attr sz
    return $ foldl horizJoin emptyImage $ intersperse
        spaceImg
        [cntImg, timeImg, permImg, szImg]
  where
    attr       = defAttr `withForeColor` blue
    spaceImg   = string attr " "
    cntImg     = string attr $ highlightedIdxOrder ps
    timeFormat = "%d-%m-%Y %H:%M"

modTime :: FilePath -> IO UTCTime
modTime path = getFileStatus path
    >>= \x -> return (posixSecondsToUTCTime (modificationTimeHiRes x))

permissions :: FilePath -> IO String
permissions path = getFileStatus path >>= \x -> return $ permissionString x

size :: FilePath -> IO String
size path = getFileStatus path >>= \x -> return $ hrSize $ fileSize x

permissionString :: FileStatus -> String
permissionString fs = fileTypeStr
    ++ permissionsToString (fileModeToPermissions mode)
  where
    mode = fileMode fs
    isSet bp = mode .&. bp /= 0
    fileTypeStr | isSet regularFileMode      = "-"
                | isSet directoryMode        = "d"
                | isSet symbolicLinkMode     = "l"
                | isSet blockSpecialMode     = "b"
                | isSet characterSpecialMode = "c"
                | isSet socketMode           = "s"
                | isSet namedPipeMode        = "|"
                | otherwise                  = "?"

data Permissions = Permissions
    { userRead     :: Bool
    , userWrite    :: Bool
    , userExecute  :: Bool
    , groupRead    :: Bool
    , groupWrite   :: Bool
    , groupExecute :: Bool
    , otherRead    :: Bool
    , otherWrite   :: Bool
    , otherExecute :: Bool
    , setuid       :: Bool
    , setgid       :: Bool
    , sticky       :: Bool
    }

fileModeToPermissions :: FileMode -> Permissions
fileModeToPermissions fm = Permissions { userRead     = isSet ownerReadMode
                                       , userWrite    = isSet ownerWriteMode
                                       , userExecute  = isSet ownerExecuteMode
                                       , groupRead    = isSet groupReadMode
                                       , groupWrite   = isSet groupWriteMode
                                       , groupExecute = isSet groupExecuteMode
                                       , otherRead    = isSet otherReadMode
                                       , otherWrite   = isSet otherWriteMode
                                       , otherExecute = isSet otherExecuteMode
                                       , setuid       = isSet setUserIDMode
                                       , setgid       = isSet setGroupIDMode
                                       , sticky       = isSet 0o1000
                                       }
    where isSet bp = fm .&. bp /= 0

permissionsToString :: Permissions -> String
permissionsToString perm = concat
    [ cbit (userRead perm)  "r"
    , cbit (userWrite perm) "w"
    , userExecuteBit (userExecute perm) (setuid perm)
    , cbit (groupRead perm)  "r"
    , cbit (groupWrite perm) "w"
    , groupExecuteBit (groupExecute perm) (setgid perm)
    , cbit (otherRead perm)  "r"
    , cbit (otherWrite perm) "w"
    , otherExecuteBit (otherExecute perm) (sticky perm)
    ]
  where
    cbit p s = if p then s else "-"
    userExecuteBit ue suid = case (ue, suid) of
        (False, False) -> "-"
        (True , False) -> "x"
        (False, True ) -> "S"
        (True , True ) -> "s"
    groupExecuteBit ge guid = case (ge, guid) of
        (False, False) -> "-"
        (True , False) -> "x"
        (False, True ) -> "S"
        (True , True ) -> "s"
    otherExecuteBit oe st = case (oe, st) of
        (False, False) -> "-"
        (True , False) -> "x"
        (False, True ) -> "T"
        (True , True ) -> "t"

-- Human readable size
hrSize :: (Integral a, Show a) => a -> String
hrSize sz
    | sz < kib  = show sz ++ "B"
    | sz < mib  = show ((fromIntegral sz :: Double) / fromIntegral kib) ++ "K"
    | sz < gib  = show ((fromIntegral sz :: Double) / fromIntegral mib) ++ "M"
    | otherwise = show ((fromIntegral sz :: Double) / fromIntegral gib) ++ "G"
  where
    two = 2
    kib = two ^ (10 :: Int)
    mib = two ^ (20 :: Int)
    gib = two ^ (30 :: Int)
