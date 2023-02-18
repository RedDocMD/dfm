{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

import           Control.Logging                ( withFileLogging )
import qualified Control.Logging               as Log
import           Data.Text                      ( pack )
import           Graphics.Vty                   ( Config
                                                , mkVty
                                                , nextEvent
                                                , picForImage
                                                , shutdown
                                                , standardIOConfig
                                                , update
                                                )
import           Lib                            ( renderState
                                                , updateState
                                                )
import           State                          ( AppState
                                                , defaultAppState
                                                )

main :: IO ()
main = withFileLogging "/tmp/dfm.log" $ do
    cfg <- standardIOConfig
    as  <- defaultAppState
    mainLoop cfg as

mainLoop :: Config -> AppState -> IO ()
mainLoop cfg as = do
    Log.log "At the beginning of new loop"
    vty <- mkVty cfg
    img <- renderState cfg as
    update vty $ picForImage img
    Log.log "Waiting for event"
    ev <- nextEvent vty
    Log.log (pack $ show ev)
    (newAs, quit) <- updateState cfg as ev
    if quit then shutdown vty else mainLoop cfg newAs
