module Main
    ( main
    ) where

import           Control.Logging               as Log
import           Control.Monad.RWS
import           Graphics.Vty
import           Lib
import           State

type App = RWST Vty () AppState IO

main :: IO ()
main = Log.withFileLogging "/tmp/dfm.log" $ do
    cfg    <- standardIOConfig
    width  <- terminalWidth cfg
    height <- terminalHeight cfg
    as     <- defaultAppState width height
    vty    <- mkVty cfg
    _      <- execRWST (mainLoop False) vty as
    shutdown vty


mainLoop :: Bool -> App ()
mainLoop shouldExit = do
    updateDisplay
    unless shouldExit $ handleNextEvent >>= mainLoop

updateDisplay :: App ()
updateDisplay = do
    vty <- ask
    as  <- get
    img <- liftIO $ renderState as
    let pic = picForImage img
    liftIO $ update vty pic

handleNextEvent :: App Bool
handleNextEvent = do
    vty <- ask
    ev  <- liftIO $ nextEvent vty
    handleEvent ev
  where
    handleEvent :: Event -> App Bool
    handleEvent ev = do
        as  <- get
        nas <- liftIO $ updateState as ev
        put nas
        return $ ev == EvKey (KChar 'q') []

