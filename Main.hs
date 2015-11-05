{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
module Main where
import SDL hiding (Play)
import Game
import Linear (V4(..), V2(..), lerp)
import Linear.Affine
import Control.Monad (unless, forM_)
import Data.List (maximumBy)
import Data.Maybe (catMaybes)
import Data.Ord

sdlInit :: IO Renderer
sdlInit = do
  initializeAll
  window <- createWindow "Discordant Note" defaultWindow
  createRenderer window (-1) defaultRenderer

data GameEvent = Play Input | Quit
processEvents :: [Event] -> GameEvent
processEvents = maximumBy (comparing priority) . (Play Wait :) . catMaybes . map decode
  where decode (Event _ payload) = decode' payload
        decode' (KeyboardEvent (KeyboardEventData _ _ _ (Keysym _ KeycodeQ _))) = Just Quit
        decode' (KeyboardEvent (KeyboardEventData _ Pressed False _)) = Just $ Play Tap
        decode' (WindowClosedEvent _) = Just $ Quit
        decode' _ = Nothing
        priority (Play Wait) = 0
        priority (Play Tap) = 1
        priority Quit = 10

loop :: GameState -> Renderer -> IO ()
loop gs r = do
   t <- time
   event <- processEvents `fmap` pollEvents
   draw gs r
   present r
   case event of
     Quit -> return ()
     Play i -> loop (step i gs (t - now gs)) r

shieldColor :: Num a => [Tone] -> Time -> V4 a
shieldColor s t = sigmoidColor (evalShield s t)

sigmoidColor :: Num a => Double -> V4 a
sigmoidColor x = fmap (fromInteger . round) $ lerp alpha pureRed pureBlue
  where pureRed :: V4 Double
        pureRed = V4 255 0 0 255
        pureBlue = V4 0 0 255 255
        alpha = sigmoid x * 0.5
        sigmoid x = 1 / (1 + 2.718**(-x))

drawEnemy :: Enemy -> Time -> Double -> Renderer -> IO ()
drawEnemy (Bolt engine bias dist) t xOffset r = do
  rendererDrawColor r $= V4 0 255 0 255
  let radius = round ((eval engine t)**2 * 50)
      x = round xOffset
      y = round (dist * 100)
  fillRect r (Just (Rectangle (P (V2 x y)) (V2 radius radius)))

draw :: GameState -> Renderer -> IO ()
draw GS{shield, now, enemies} r = do
  rendererDrawColor r $= shieldColor shield now
  clear r
  rendererDrawColor r $= V4 0 0 0 200
  let shieldHeight = round $ evalShield shield now * 15
  fillRect r (Just (Rectangle (P (V2 310 200)) (V2 20 shieldHeight)))
  forM_  (zip enemies [50, 150..]) $ \(enemy, column) ->
    drawEnemy enemy now column r

initialEnemies :: [Enemy]
initialEnemies = map enemy [0..7]
  where enemy :: Double -> Enemy
        enemy n = Bolt (tone n) 0.2 ((7 - n)^2/7)
        tone n = T { freq = sin n/2, amp = 0.8 + (3.5 - n)/35, off = cos (9 * n)}

main :: IO ()
main = do
  start <- time
  let initialState = GS { shield = [],
                          enemies = initialEnemies,
                          lastTap = start,
                          now = start}
  loop initialState =<< sdlInit
