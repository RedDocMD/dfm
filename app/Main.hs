module Main
    ( main
    ) where

import           Control.Logging   as Log
import           Control.Monad.RWS
import           FS
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
    conflictGuard cfg as

conflictGuard :: Config -> AppState -> IO ()
conflictGuard cfg as = do
    vty    <- mkVty cfg
    (nas, _)      <- execRWST (mainLoop Continue) vty as
    shutdown vty
    when (appStateHasConflict nas)
        $ handleConflicts (conflicts nas) >> conflictGuard cfg (resetAfterConflict nas)

handleConflicts :: CopyConflicts -> IO ()
handleConflicts cfg = do
    putStrLn "Got some conflicts"
    threadDelay 2000000

mainLoop :: Action -> App ()
mainLoop action = do
    updateDisplay
    when (action == Continue) $ handleNextEvent >>= mainLoop

updateDisplay :: App ()
updateDisplay = do
    vty <- ask
    as  <- get
    img <- liftIO $ renderState as
    let pic = picForImage img
    liftIO $ update vty pic

data Action = Continue | Quit | Conflict deriving Eq

handleNextEvent :: App Action
handleNextEvent = do
    vty <- ask
    ev  <- liftIO $ nextEvent vty
    handleEvent ev
  where
    handleEvent :: Event -> App Action
    handleEvent ev = do
        as  <- get
        nas <- liftIO $ updateState as ev
        put nas
        let action
              | ev == EvKey (KChar 'q') [] = Quit
              | appStateHasConflict nas    = Conflict
              | otherwise                  = Continue
        return action

