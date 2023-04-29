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
import           System.Posix      (Handler (..), installHandler, sigINT)
import           Util

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
  (nas, _) <- execRWST (mainLoop False) vty as
  shutdown vty
  case tMode nas of
    ConflictMode conf  -> printConflicts conf >> conflictGuard cfg (resetAfterConflict nas)
    RenameMode oldPath -> do
      newPath <- getNewName oldPath
      if newPath == oldPath
        then conflictGuard cfg nas
        else doRename nas oldPath newPath >>= conflictGuard cfg
    MkdirMode -> do
      newDirName <- getNewDirectoryName
      as' <- mkdir newDirName as
      putStrLn "\nWaiting for any key ..."
      _ <- getChar
      conflictGuard cfg as'
    NormalMode         -> return ()

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

getNewName :: FilePath -> IO FilePath
getNewName from = do
  putStrLn $ "Renaming " ++ from ++ " to:"
  endName <- getLine
  let newName = if null endName then from else switchName from endName
  return newName

getNewDirectoryName :: IO FilePath
getNewDirectoryName = do
  putStrLn "Enter new directory name:"
  getLine

mainLoop :: Bool -> App ()
mainLoop shouldExit = do
  updateDisplay
  unless shouldExit $ handleNextEvent >>= mainLoop

updateDisplay :: App ()
updateDisplay = do
  vty <- ask
  as <- get
  img <- liftIO $ renderState as
  let pic = picForImage img
  liftIO $ update vty pic

handleNextEvent :: App Bool
handleNextEvent = do
  vty <- ask
  ev <- liftIO $ nextEvent vty
  handleEvent ev
  where
    handleEvent :: Event -> App Bool
    handleEvent ev = do
      as <- get
      nas <- liftIO $ updateState as ev
      put nas
      let quit
            | ev == EvKey (KChar 'q') [] = True
            | isModeConflict (tMode nas) = True
            | isModeRename   (tMode nas) = True
            | isModeMkdir    (tMode nas) = True
            | otherwise                  = False
      return quit
