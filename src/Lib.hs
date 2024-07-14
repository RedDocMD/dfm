module Lib
    ( renderState
    , updateState
    , terminalWidth
    , terminalHeight
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
renderState :: AppState -> IO Image
renderState st = do
    let ps       = currPaneState st
        topBar   = renderTopBar st
        height   = tHeight st
        width    = tWidth st
        mainArea = renderMainArea ps height width
    bottomBar <- renderBottomBar ps
    return $ topBar `vertJoin` mainArea `vertJoin` bottomBar



-- Primary event handling function
-- Given a Config of the terminal, an AppState and the Event
-- it will give the new AppState and Bool to tell if you have to quit
updateState :: AppState -> Event -> IO AppState
updateState st (EvKey key mods) = updateStateKey st key mods
updateState st (EvResize width height) =
    return $ recalculateOffsets $ resizeTerminal st width height
updateState st _ = return st


updateStateKey :: AppState -> Key -> [Modifier] -> IO AppState
updateStateKey st (KChar  ch) mods = updateStateCh st ch mods
updateStateKey st (KFun  num) mods = updateStateFKey st num mods
updateStateKey st KEnter      []   = enterHighlightedFile st
updateStateKey st _           _    = return st

updateStateFKey :: AppState -> Int -> [Modifier] -> IO AppState
updateStateFKey st 2 [] = return $ enterRenameMode st
updateStateFKey st 3 [] = return $ enterMkdirMode st
updateStateFKey st _ _  = return st

updateStateCh :: AppState -> Char -> [Modifier] -> IO AppState
updateStateCh st 'j' [] = return $ scrollDown st
updateStateCh st 'k' [] = return $ scrollUp st
updateStateCh st 'l' [] = enterHighlightedFile st
updateStateCh st 'h' [] = gotoParent st
updateStateCh st ' ' [] = return $ toggleMarkHighlightedFile st
updateStateCh st 'm' [] = return $ markAllFiles st
updateStateCh st 'c' [] = return $ clearAllMarkedFiles st
updateStateCh st 'u' [] = return $ unmarkAllFiles st
updateStateCh st 'y' [] = return $ yankMarkedFiles st
updateStateCh st 'x' [] = return $ cutMarkedFiles st
updateStateCh st 'p' [] = pasteFiles st
updateStateCh st 'P' [] = pasteAllPaneFiles st
updateStateCh st 'a' [] = setSortOrder Name st
updateStateCh st 'A' [] = setSortOrder NameDescending st
updateStateCh st 't' [] = setSortOrder Time st
updateStateCh st 'T' [] = setSortOrder TimeDescending st
-- updateStateCh st 'r' [] = getUserName >>= \un -> gotoDir ("/run/media/" ++ un) st
updateStateCh st 'r' [] = refreshCurrPane st
updateStateCh st '1' [] = return $ setCurrentPane st 1
updateStateCh st '2' [] = return $ setCurrentPane st 2
updateStateCh st '3' [] = return $ setCurrentPane st 3
updateStateCh st '4' [] = return $ setCurrentPane st 4
updateStateCh st 'd' [MCtrl] = return $ scrollDownHalfPage st
updateStateCh st 'u' [MCtrl] = return $ scrollUpHalfPage st
updateStateCh st _   _  = return st
