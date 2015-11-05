{-# LANGUAGE OverloadedStrings #-}
module Main where
import SDL
import Linear (V4(..))
import Control.Monad (unless)

sdlInit :: IO Renderer
sdlInit = do
  initializeAll
  window <- createWindow "Discordant Note" defaultWindow
  createRenderer window (-1) defaultRenderer

loop :: Renderer -> IO ()
loop r = do
   events <- pollEvents
   present r
   unless (any isWinClose events) $ loop r
  where isWinClose (Event _ (WindowClosedEvent _)) = True
        isWinClose _ = False

main :: IO ()
main = loop =<< sdlInit
