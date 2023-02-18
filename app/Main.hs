{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

import           Control.Monad.RWS
import           Graphics.Vty
import           Lib
import           State

data VtyState = VtyState
    { mVty :: Vty
    , mCfg :: Config
    }

type App = RWST VtyState () AppState IO

main :: IO ()
main = do
    cfg <- standardIOConfig
    as  <- defaultAppState
    vty <- mkVty cfg
    let ts = VtyState { mVty = vty, mCfg = cfg }
    _ <- execRWST (mainLoop False) ts as
    shutdown vty


mainLoop :: Bool -> App ()
mainLoop shouldExit = do
    updateDisplay
    unless shouldExit $ handleNextEvent >>= mainLoop

updateDisplay :: App ()
updateDisplay = do
    vs  <- ask
    as <- get
    img <- liftIO $ renderState (mCfg vs) as
    let pic = picForImage img
    liftIO $ update (mVty vs) pic

handleNextEvent :: App Bool
handleNextEvent = do
    vs <- ask
    ev <- liftIO $ nextEvent (mVty vs)
    handleEvent (mCfg vs) ev
  where
    handleEvent :: Config -> Event -> App Bool
    handleEvent cfg ev = do
        as  <- get
        nas <- liftIO $ updateState cfg as ev
        put nas
        return $ ev == EvKey (KChar 'q') []

