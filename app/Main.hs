module Main
  ( main,
  )
where

import           Control.Logging   as Log
import           Control.Monad.RWS (RWST, execRWST)
import           FS
import           Graphics.Vty
import           Lib
import           State
import System.Posix (installHandler, Handler (..), sigINT)

type App = RWST Vty () AppState IO

main :: IO ()
main = Log.withFileLogging "/tmp/dfm.log" $ do
  _ <- installHandler sigINT Ignore Nothing
  cfg <- standardIOConfig
  width <- terminalWidth cfg
  height <- terminalHeight cfg
  as <- defaultAppState width height
  conflictGuard cfg as

conflictGuard :: Config -> AppState -> IO ()
conflictGuard cfg as = do
  vty <- mkVty cfg
  (nas, _) <- execRWST (mainLoop Continue) vty as
  shutdown vty
  when (appStateHasConflict nas) $
    printConflicts (conflicts nas) >> conflictGuard cfg (resetAfterConflict nas)

printConflicts :: CopyConflicts -> IO ()
printConflicts cc = do
  let printFileToFileConflict fp = putStrLn $ "Cannot copy file " ++ fp ++ " as it already exists as a file"
      printFileToDirConflict fp = putStrLn $ "Cannot copy file " ++ fp ++ " as it already exists as a directory"
      printDirToFileConflict fp = putStrLn $ "Cannot copy directory " ++ fp ++ " as it already exists as a file"
      printDirToDirConflict fp = putStrLn $ "Cannot copy directory " ++ fp ++ " as it already exists as a directory"
  mapM_ printFileToFileConflict (fileToFile cc)
  mapM_ printFileToDirConflict (fileToDir cc)
  mapM_ printDirToFileConflict (dirToFile cc)
  mapM_ printDirToDirConflict (dirToDir cc)
  putStrLn "\nWaiting for any key ..."
  _ <- getChar
  return ()

mainLoop :: Action -> App ()
mainLoop action = do
  updateDisplay
  when (action == Continue) $ handleNextEvent >>= mainLoop

updateDisplay :: App ()
updateDisplay = do
  vty <- ask
  as <- get
  img <- liftIO $ renderState as
  let pic = picForImage img
  liftIO $ update vty pic

data Action = Continue | Quit | Conflict deriving (Eq)

handleNextEvent :: App Action
handleNextEvent = do
  vty <- ask
  ev <- liftIO $ nextEvent vty
  handleEvent ev
  where
    handleEvent :: Event -> App Action
    handleEvent ev = do
      as <- get
      nas <- liftIO $ updateState as ev
      put nas
      let action
            | ev == EvKey (KChar 'q') [] = Quit
            | appStateHasConflict nas = Conflict
            | otherwise = Continue
      return action
