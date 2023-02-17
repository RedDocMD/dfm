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



-- Primary event handling function
-- Given a Config of the terminal, an AppState and the Event
-- it will give the new AppState and Bool to tell if you have to quit
updateState :: Config -> AppState -> Event -> IO (AppState, Bool)
updateState cfg st (EvKey key mods) = updateStateKey cfg st key mods
updateState _   st _                = return (st, False)


updateStateKey
    :: Config -> AppState -> Key -> [Modifier] -> IO (AppState, Bool)
updateStateKey cfg st (KChar ch) mods = updateStateCh cfg st ch mods
updateStateKey _   st _          _    = return (st, False)


updateStateCh
    :: Config -> AppState -> Char -> [Modifier] -> IO (AppState, Bool)
updateStateCh _   st 'q' [] = return (st, True)
updateStateCh cfg st 'j' [] = do
    ht <- terminalHeight cfg
    let nst = scrollDown st ht
    return (nst, False)
updateStateCh cfg st 'k' [] = do
    ht <- terminalHeight cfg
    let nst = scrollUp st ht
    return (nst, False)
updateStateCh _ st _ _ = return (st, False)
