module ParticleSpec (spec) where

import Linear (V2(..))
import Test.Hspec

import Core.Config (landingDustMinSpeed)
import Core.Types (VFXType(..), Direction(..))
import Sim.ParticleCore

mkParticle :: V2 Double -> V2 Double -> Double -> Double -> Particle
mkParticle pos vel life grav = Particle pos vel life life 2.0 (255, 255, 255) grav

spec :: Spec
spec = do
  describe "stepParticles" $ do
    it "integrates position by velocity" $ do
      let p = mkParticle (V2 0.0 0.0) (V2 100.0 0.0) 1.0 0.0
          [p'] = stepParticles 0.1 [p]
      pPos p' `shouldBe` V2 10.0 0.0

    it "applies per-particle gravity to the velocity" $ do
      let p = mkParticle (V2 0.0 0.0) (V2 0.0 0.0) 1.0 700.0
          [p'] = stepParticles 0.1 [p]
      pVel p' `shouldBe` V2 0.0 70.0

    it "culls particles whose life expires" $ do
      let p = mkParticle (V2 0.0 0.0) (V2 0.0 0.0) 0.05 0.0
      stepParticles 0.1 [p] `shouldBe` []

    it "a burst is deterministic (same input, same particles)" $ do
      let a = burstFor VFXShockwave (V2 10.0 20.0) Nothing
          b = burstFor VFXShockwave (V2 10.0 20.0) Nothing
      a `shouldBe` b

  describe "landingBurst" $ do
    it "no dust below the speed threshold" $
      landingBurst (landingDustMinSpeed - 1.0) (V2 0.0 0.0) `shouldBe` []

    it "dust above the speed threshold" $
      landingBurst (landingDustMinSpeed + 1.0) (V2 0.0 0.0)
        `shouldSatisfy` (not . null)

    it "dust fans out both left and right" $ do
      let ps = landingBurst 600.0 (V2 0.0 0.0)
          vxs = [ x | p <- ps, let V2 x _ = pVel p ]
      any (< 0.0) vxs `shouldBe` True
      any (> 0.0) vxs `shouldBe` True

  describe "emitters" $ do
    it "pickup sparkles are floaty (no gravity)" $
      pickupBurst (V2 0.0 0.0) `shouldSatisfy` all ((== 0.0) . pGravity)

    it "hit sparks respect the facing direction" $ do
      let right = hitBurst (V2 0.0 0.0) (Just DirRight)
          left  = hitBurst (V2 0.0 0.0) (Just DirLeft)
      right `shouldSatisfy` all (\p -> let V2 x _ = pVel p in x > 0.0)
      left  `shouldSatisfy` all (\p -> let V2 x _ = pVel p in x < 0.0)

    it "dash ghosts trail opposite to the dash direction" $ do
      let ps = burstFor VFXDashGhost (V2 0.0 0.0) (Just DirRight)
      ps `shouldSatisfy` all (\p -> let V2 x _ = pVel p in x < 0.0)