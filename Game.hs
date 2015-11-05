{-# LANGUAGE NamedFieldPuns #-}
module Game where

type Time = Double

data Tone = T { freq :: Double, amp :: Double, off :: Double }

eval :: Tone -> Time -> Double
eval T{freq, amp, off} t = cos (2 * pi * freq * t + off) * amp

evalShield :: [Tone] -> Time -> Double
evalShield shield t = sum (map (flip eval t) shield)

data Enemy = Bolt Tone Double Double

data GameState =
  GS { shield :: [Tone],
       enemies :: [Enemy],
       lastTap :: Time,
       now :: Time }

data Input = Tap | Wait

tapped :: Input -> Bool
tapped Tap = True
tapped Wait = False

step :: Input -> GameState -> Time -> GameState
step i GS{shield, enemies, lastTap, now} delta = newState where
  newState = GS {shield = newShield,
                 enemies = newEnemies,
                 lastTap = newLastTap,
                 now = newNow}
  newShield = stepShield i lastTap newNow delta shield
  newEnemies = map (moveEnemy pointShield newNow delta) enemies
  pointShield = evalShield newShield newNow
  newLastTap = if tapped i then newNow else lastTap
  newNow = now + delta

stepShield :: Input -> Time -> Time -> Time -> [Tone] -> [Tone]
stepShield i lastTap now delta shield = tapTone i lastTap now ++ updateExisting shield
  where updateExisting = filter bigEnough . map (decay delta)
        bigEnough (T {amp}) = amp >= 0.01

decay :: Time -> Tone -> Tone
decay delta T{freq, amp, off} = T{freq, off, amp = amp*factor}
  where factor = 0.85**(delta*freq)

tapTone :: Input -> Time -> Time -> [Tone]
tapTone Tap lastTap now = [T{freq = 1/(now - lastTap), off = lastTap, amp = 1}]
tapTone Wait _ _ = []

moveEnemy :: Double -> Time -> Time -> Enemy -> Enemy
moveEnemy shield now delta (Bolt engine bias dist) = Bolt engine bias (dist + offset)
  where offset = -(shield * pointEngine * pointEngine + bias) * delta
        pointEngine = eval engine now
