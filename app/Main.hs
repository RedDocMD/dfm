module Main
    ( main
    ) where

import           Graphics.Vty
import           Lib
import           State

main :: IO ()
main = do
    cfg <- standardIOConfig
    as  <- defaultAppState
    mainLoop cfg as

mainLoop :: Config -> AppState -> IO ()
mainLoop cfg as = do
    vty <- mkVty cfg
    img <- renderState cfg as
    update vty $ picForImage img
    ev            <- nextEvent vty
    (newAs, quit) <- updateState cfg as ev
    if quit then shutdown vty else mainLoop cfg newAs
