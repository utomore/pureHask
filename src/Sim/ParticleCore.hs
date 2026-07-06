-- | The particle system's pure core: the particle record, the integration
--   step and the deterministic emitters. "Sim.Particles" is the thin Apecs
--   glue that stores the buffer globally; "Render.Draw" draws it.
--
--   Determinism: emitters use an index-based pseudo-spread ('wobble') instead
--   of a random generator, so every burst is reproducible in tests and the
--   simulation stays replayable.
module Sim.ParticleCore
  ( Particle(..)
  , stepParticles
  , burstFor
  , landingBurst
  , pickupBurst
  , hitBurst
  ) where

import Data.Maybe (mapMaybe)
import Linear (V2(..))

import Core.Config (landingDustMinSpeed)
import Core.Types (VFXType(..), Direction(..))

-- | One particle: a coloured square with velocity, gravity and a lifetime.
--   Rendering fades alpha with @pLife / pMaxLife@.
data Particle = Particle
  { pPos     :: !(V2 Double)
  , pVel     :: !(V2 Double)
  , pLife    :: !Double            -- ^ seconds remaining
  , pMaxLife :: !Double
  , pSize    :: !Double            -- ^ square edge, pixels
  , pColor   :: !(Int, Int, Int)
  , pGravity :: !Double            -- ^ pixels/sec^2 (0 = floaty)
  } deriving (Eq, Show)

-- | Integrate one fixed sub-step and cull expired particles.
stepParticles :: Double -> [Particle] -> [Particle]
stepParticles dt = mapMaybe step
  where
    step p
      | life' <= 0.0 = Nothing
      | otherwise = Just p
          { pPos  = pPos p + pVel p * pure dt
          , pVel  = pVel p + V2 0.0 (pGravity p * dt)
          , pLife = life'
          }
      where life' = pLife p - dt

-- | Deterministic 0.0–0.9 jitter from an index (tiny LCG-ish hash).
wobble :: Int -> Double
wobble i = fromIntegral ((i * 37 + 11) `mod` 10) / 10.0

-- | @t@ in 0..1 across the burst, for symmetric fans.
spread :: Int -> Int -> Double
spread i n = fromIntegral i / fromIntegral (max 1 (n - 1))

-- | Dust kicked up from a point: a horizontal fan with a small upward pop.
dustAt :: V2 Double -> Int -> Double -> [Particle]
dustAt pos n power =
  [ Particle pos (V2 vx vy) life life (2.0 + 2.0 * wobble (i + 3))
      (150, 140, 118) 700.0
  | i <- [0 .. n - 1]
  , let vx = (spread i n - 0.5) * 2.0 * power
        vy = negate (power * (0.35 + 0.3 * wobble i))
        life = 0.3 + 0.25 * wobble (i + 7)
  ]

-- | Landing dust, scaled by impact speed; below the threshold no dust at
--   all (small hops stay clean).
landingBurst :: Double -> V2 Double -> [Particle]
landingBurst impactSpeed feet
  | impactSpeed < landingDustMinSpeed = []
  | otherwise = dustAt feet 8 (min 260.0 (impactSpeed * 0.4))

-- | Golden sparkle when an item is picked up: floaty, no gravity.
pickupBurst :: V2 Double -> [Particle]
pickupBurst center =
  [ Particle center (V2 vx vy) life life 2.5 (255, 226, 120) 0.0
  | i <- [0 .. 9 :: Int]
  , let ang = spread i 10 * 2.0 * pi
        speed = 45.0 + 40.0 * wobble i
        vx = cos ang * speed
        vy = sin ang * speed - 30.0
        life = 0.45 + 0.3 * wobble (i + 5)
  ]

-- | Impact sparks (weapon hits, enemy deaths): a directional orange spray.
hitBurst :: V2 Double -> Maybe Direction -> [Particle]
hitBurst center mDir =
  [ Particle center (V2 (side * vx) vy) life life 2.5 (255, 170, 60) 900.0
  | i <- [0 .. 11 :: Int]
  , let vx = 60.0 + 220.0 * wobble i
        vy = negate (40.0 + 180.0 * wobble (i + 4))
        life = 0.25 + 0.2 * wobble (i + 8)
  ]
  where
    side = case mDir of
      Just DirLeft -> -1.0
      Just DirRight -> 1.0
      Nothing -> if even (round (fst3 center) :: Int) then 1.0 else -1.0
    fst3 (V2 x _) = x

-- | The particle burst accompanying each VFX request, keyed by its type.
--   Pure table: the glue ("Sim.Combat" / "Sim.Physics") just calls it.
burstFor :: VFXType -> V2 Double -> Maybe Direction -> [Particle]
burstFor vfxType pos mDir = case vfxType of
  VFXDashGhost ->
    [ Particle pos (V2 (back * (30.0 + 40.0 * wobble i))
                       ((wobble (i + 2) - 0.45) * 80.0))
        life life 2.0 (110, 235, 255) 0.0
    | i <- [0 .. 2 :: Int]
    , let life = 0.2 + 0.15 * wobble (i + 6)
    ]
  VFXMelee     -> hitBurst pos mDir
  VFXThrust    -> hitBurst pos mDir
  VFXShockwave -> dustAt pos 14 240.0
  where
    back = case mDir of
      Just DirRight -> -1.0
      _             -> 1.0
