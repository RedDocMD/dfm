module Render
    ( renderTopBar
    , renderMainArea
    , renderBottomBar
    ) where

import qualified Data.HashMap.Lazy             as HM
import           FS
import           Graphics.Vty
import           State
import           System.Posix
import           Util


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


renderNormalPaths :: [FSEntry] -> [FSEntry] -> Image
renderNormalPaths paths sel = foldl vertJoin emptyImage $ map renderFn paths
  where
    renderFn path = (selStr path) Graphics.Vty.<|> renderNormalPath path
    selStr path = if path `elem` sel
        then string (defAttr `withStyle` bold) "* "
        else emptyImage


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
renderPathList :: PaneState -> Int -> Image
renderPathList st height =
    renderNormalPaths before marked
        <-> (selMarker Graphics.Vty.<|> selBody)
        <-> renderNormalPaths after marked
  where
    paths        = dirsBeforeFiles $ paneVisibleFiles st
    off          = pOffset st
    visiblePaths = take (pathListHeight height) $ drop off paths
    sidx         = highlightedFileIdx st - off
    before       = take sidx visiblePaths
    sel          = visiblePaths !!? sidx
    after        = safeTail $ drop sidx visiblePaths
    marked       = HM.findWithDefault [] (mainPath st) (markedFiles st)
    selMarker    = case sel of
        Just mSel -> if mSel `elem` marked
            then string (defAttr `withStyle` bold) "* "
            else emptyImage
        Nothing -> emptyImage
    selBody = maybe emptyImage renderSelectedPath sel


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
renderPathListSide :: PaneState -> Int -> Int -> Image
renderPathListSide st height width =
    let pl         = renderPathList st height
        leftSpacer = verticalSpacer (plWidth + 1) (height - 3)
        spaceLine  = string defAttr $ replicate width ' '
    in  spaceLine `vertJoin` (leftSpacer `horizJoin` pl)

renderMainArea :: PaneState -> Int -> Int -> Image
renderMainArea = renderPathListSide

renderBottomBar :: PaneState -> IO Image
renderBottomBar ps = do
    let hf         = highlightedFile ps
        attr       = defAttr `withForeColor` blue
        spaceImg   = string attr " "
        timeFormat = "%d-%m-%Y %H:%M"
        markedCnt  = paneMarkedCount ps
    case hf of
        Just mHf -> do
            mtime <- modTime mHf
            perm  <- permissions mHf
            sz    <- size mHf
            let timeImg =
                    string attr $ formatTime defaultTimeLocale timeFormat mtime
                permImg   = string attr perm
                szImg     = string attr sz
                cntImg    = string attr $ highlightedIdxOrder ps
                markedImg = string attr
                    $ if markedCnt == 0 then "" else "m:" ++ show markedCnt
            return $ foldl horizJoin emptyImage $ intersperse
                spaceImg
                [cntImg, timeImg, permImg, szImg, markedImg]
        Nothing -> return emptyImage

modTime :: FilePath -> IO LocalTime
modTime path = do
    fs <- getFileStatus path
    tz <- getCurrentTimeZone
    let utcTime = posixSecondsToUTCTime (modificationTimeHiRes fs)
    return $ utcToLocalTime tz utcTime

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
    | sz < kib
    = show sz ++ "B"
    | sz < mib
    = show (round ((fromIntegral sz :: Double) / fromIntegral kib) :: Int)
        ++ "K"
    | sz < gib
    = show (round ((fromIntegral sz :: Double) / fromIntegral mib) :: Int)
        ++ "M"
    | otherwise
    = show (round ((fromIntegral sz :: Double) / fromIntegral gib) :: Int)
        ++ "G"
  where
    two = 2
    kib = two ^ (10 :: Int)
    mib = two ^ (20 :: Int)
    gib = two ^ (30 :: Int)
