{-#LANGUAGE NamedFieldPuns #-}

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
import           Safe                          (atMay)


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


data Paths = Paths
    { paths  :: [FSEntry]
    , sel    :: [FSEntry]
    , yanked :: [FSEntry]
    , cut    :: [FSEntry]
    }


renderNormalPaths :: Paths -> Image
renderNormalPaths Paths { paths, sel, yanked, cut } =
    foldl' vertJoin emptyImage $ map renderFn paths
  where
    renderFn path =
        selStr path
            Graphics.Vty.<|> yankStr path
            Graphics.Vty.<|> cutStr path
            Graphics.Vty.<|> renderNormalPath path
    selStr path = if path `elem` sel
        then string (defAttr `withStyle` bold) "* "
        else emptyImage
    yankStr path = if path `elem` yanked
        then string (defAttr `withStyle` bold `withForeColor` yellow) "y "
        else emptyImage
    cutStr path = if path `elem` cut
        then string (defAttr `withStyle` bold `withForeColor` red) "x "
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
    renderFile fp = 
        string (defAttr `withForeColor` black `withBackColor` white) fp 
            Graphics.Vty.<|> string defAttr ""
    renderSpecFile fp =
        string (defAttr `withForeColor` black `withBackColor` yellow) fp
            Graphics.Vty.<|> string defAttr ""

-- Renders the list of paths
renderPathList :: PaneState -> Int -> Image
renderPathList st height =
    renderNormalPaths Paths { paths  = before
                            , sel    = marked
                            , yanked = yanked
                            , cut    = cut
                            }
        <-> (                selMarker
            Graphics.Vty.<|> yankMarker
            Graphics.Vty.<|> cutMarker
            Graphics.Vty.<|> selBody
            )
        <-> renderNormalPaths Paths { paths  = after
                                    , sel    = marked
                                    , yanked = yanked
                                    , cut    = cut
                                    }
  where
    paths        = paneVisibleFiles st
    off          = pOffset st
    visiblePaths = take (pathListHeight height) $ drop off paths
    sidx         = highlightedFileIdx st - off
    before       = take sidx visiblePaths
    sel          = visiblePaths `atMay` sidx
    after        = safeTail $ drop sidx visiblePaths
    mp           = mainPath st
    marked       = HM.findWithDefault [] mp (markedFiles st)
    yanked       = HM.findWithDefault [] mp (yankedFiles st)
    cut          = HM.findWithDefault [] mp (cutFiles st)
    selBody      = maybe emptyImage renderSelectedPath sel
    selMarker    = case sel of
        Nothing   -> emptyImage
        Just mSel -> if mSel `elem` marked
            then string (defAttr `withStyle` bold) "* "
            else emptyImage
    yankMarker = case sel of
        Nothing   -> emptyImage
        Just mSel -> if mSel `elem` yanked
            then string (defAttr `withStyle` bold `withForeColor` yellow) "y "
            else emptyImage
    cutMarker = case sel of
        Nothing   -> emptyImage
        Just mSel -> if mSel `elem` cut
            then string (defAttr `withStyle` bold `withForeColor` red) "x "
            else emptyImage

-- Renders the little pane seletor list at the top
renderPaneList :: AppState -> Image
renderPaneList st = foldl' horizJoin emptyImage $ intersperse spaceImg mainImgs
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
verticalSpacer width n = foldl' vertJoin emptyImage
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
        yankedCnt  = paneYankedCount ps
        cutCnt     = paneCutCount ps
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
                yankedImg = string attr
                    $ if yankedCnt == 0 then "" else "y:" ++ show yankedCnt
                cutImg = string attr
                    $ if cutCnt == 0 then "" else "x:" ++ show cutCnt
            return $ foldl' horizJoin emptyImage $ intersperse
                spaceImg
                [cntImg, timeImg, permImg, szImg, markedImg, yankedImg, cutImg]
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
