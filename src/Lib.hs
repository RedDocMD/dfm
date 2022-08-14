module Lib
    ( renderState
    ) where

import           Graphics.Vty
import           State                          ( AppState
                                                , PaneState
                                                , currPaneState
                                                , highlightedFileIdx
                                                , pathFiles
                                                )

-- Size of terminal
terminalSize :: Config -> IO DisplayRegion
terminalSize cfg = outputForConfig cfg >>= displayBounds

-- Width of terminal
terminalWidth :: Config -> IO Int
terminalWidth = fmap regionWidth . terminalSize

-- Region of terminal
terminalHeight :: Config -> IO Int
terminalHeight = fmap regionHeight . terminalSize


renderState :: Config -> AppState -> IO Image
renderState cfg st = do
    let ps = currPaneState st
    return $ renderPathList ps


renderNormalPath :: String -> Image
renderNormalPath = string defAttr

renderNormalPaths :: [String] -> Image
renderNormalPaths = foldl vertJoin emptyImage . map renderNormalPath

renderSelectedPath :: String -> Image
renderSelectedPath =
    string (defAttr `withForeColor` black `withBackColor` white)

renderPathList :: PaneState -> Image
renderPathList st =
    renderNormalPaths before
        <-> renderSelectedPath sel
        <-> renderNormalPaths after
  where
    paths  = pathFiles st
    sidx   = highlightedFileIdx st
    before = take sidx paths
    sel    = paths !! sidx
    after  = tail $ drop sidx paths
