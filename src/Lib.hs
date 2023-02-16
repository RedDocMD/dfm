module Lib
    ( renderState
    , updateState
    ) where

import           Graphics.Vty
import           Render
import           State

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


updateState :: Config -> AppState -> Event -> IO (AppState, Bool)
updateState cfg st ev = return (st, False)
