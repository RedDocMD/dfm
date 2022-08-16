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
                                                , highlightedFileIdx
                                                , visibleFiles
                                                )

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
        topBar   = renderTopBar st width
        mainArea = renderPathListSide ps height width
    return $ topBar `vertJoin` mainArea


horizontalLine :: Int -> Image
horizontalLine n = string defAttr $ replicate n '─'


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
renderPaneList st =
    foldl horizJoin emptyImage
        $  [spaceImg]
        ++ intersperse spaceImg mainImgs
        ++ [spaceImg]
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
plWidth = 9

-- Renders the entire top bar, with panel selector and path
-- and a box around them
renderTopBar :: AppState -> Int -> Image
renderTopBar st width =
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
        pimgWidth = length path + 1
        topLine   = string defAttr $ concat
            [ "┌"
            , replicate plWidth '─'
            , "┬"
            , replicate (width - (plWidth + 3)) '─'
            , "┐"
            ]
        botLine = string defAttr $ concat
            [ "├"
            , replicate plWidth '─'
            , "┴"
            , replicate (width - (plWidth + 3)) '─'
            , "┤"
            ]
        vertLine = string defAttr "│"
        mainLine = foldl
            horizJoin
            emptyImage
            [ vertLine
            , pl
            , vertLine
            , pimg
            , string defAttr $ replicate (width - (plWidth + pimgWidth + 3)) ' '
            , vertLine
            ]
    in  foldl vertJoin emptyImage [topLine, mainLine, botLine]

verticalLine :: Int -> Image
verticalLine n = foldl vertJoin emptyImage $ replicate n (string defAttr "│")

verticalSpacer :: Int -> Int -> Image
verticalSpacer width n = foldl vertJoin emptyImage
    $ replicate n (string defAttr $ replicate width ' ')


renderPathListSide :: PaneState -> Int -> Int -> Image
renderPathListSide st height width =
    let pl         = renderPathList st
        leftSpacer = verticalSpacer (plWidth + 3) (height - 6)
        vl         = verticalLine (height - 6)
    in  foldl horizJoin emptyImage [vl, leftSpacer, pl]
