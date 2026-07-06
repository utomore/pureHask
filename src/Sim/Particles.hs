-- | Apecs glue for the particle system: the global buffer lives in
--   'Core.Components.ParticleStore'; stepping and emission both delegate to
--   the pure "Sim.ParticleCore". No decisions here.
module Sim.Particles
  ( emitParticles
  , tickParticles
  ) where

import Apecs

import Core.Components
import Core.Config (maxParticles)
import Sim.ParticleCore

-- | Prepend a burst, keeping the buffer under the global cap (newest win).
emitParticles :: [Particle] -> Game ()
emitParticles [] = return ()
emitParticles new = do
  ParticleStore ps <- get global
  set global (ParticleStore (take maxParticles (new ++ ps)))

-- | Advance every particle by one fixed sub-step.
tickParticles :: Double -> Game ()
tickParticles dt = do
  ParticleStore ps <- get global
  set global (ParticleStore (stepParticles dt ps))
