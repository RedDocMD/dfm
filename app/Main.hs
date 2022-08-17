module Main
    ( main
    ) where

import           Graphics.Vty
import           Lib
import           State

main :: IO ()
main = do
    cfg <- standardIOConfig
    vty <- mkVty cfg
    as  <- defaultAppState
    img <- renderState cfg as
    update vty $ picForImage img
    e <- nextEvent vty
    shutdown vty
    print ("Last event was: " ++ show e)
