module CombatCoreSpec (spec) where

import Linear (V2(..))
import Test.Hspec

import Core.Config
import Core.Types
import Sim.CombatCore

-- | A grounded, idle player with no input.
baseIn :: CombatIn
baseIn = CombatIn
  { ciInput        = emptyFrameInput
  , ciDt           = 1.0 / 120.0
  , ciState        = StateIdle 0.0
  , ciGrounded     = True
  , ciFacing       = DirRight
  , ciVel          = V2 0.0 0.0
  , ciDoubleJumped = False
  , ciHook         = HookRetracted
  , ciPos          = V2 100.0 100.0
  , ciSize         = V2 playerSize playerSize
  , ciStamina      = playerMaxStamina
  , ciSpeedMult    = 1.0
  }

withIntents :: [Intent] -> CombatIn -> CombatIn
withIntents is ci = ci { ciInput = (ciInput ci) { fiIntents = is } }

withHeld :: (HeldKeys -> HeldKeys) -> CombatIn -> CombatIn
withHeld f ci =
  ci { ciInput = (ciInput ci) { fiHeld = f (fiHeld (ciInput ci)) } }

vy :: CombatOut -> Double
vy out = let V2 _ y = coVel out in y

vx :: CombatOut -> Double
vx out = let V2 x _ = coVel out in x

spec :: Spec
spec = do
  describe "running and jumping" $ do
    it "held direction produces run speed" $ do
      let out = combatStep (withHeld (\h -> h { heldLeft = True }) baseIn)
      vx out `shouldBe` (-playerSpeed)
      coFacing out `shouldBe` DirLeft

    it "equipment speed multiplier scales the run speed" $ do
      let fast = baseIn { ciSpeedMult = 1.15 }
          out = combatStep (withHeld (\h -> h { heldRight = True }) fast)
      vx out `shouldBe` playerSpeed * 1.15

    it "grounded jump launches upward and clears grounded" $ do
      let out = combatStep (withIntents [IntentJump] baseIn)
      vy out `shouldBe` jumpSpeed
      coGrounded out `shouldBe` False

    it "mid-air jump spends the double jump" $ do
      let airborne = baseIn { ciGrounded = False, ciVel = V2 0.0 50.0 }
          out = combatStep (withIntents [IntentJump] airborne)
      vy out `shouldBe` jumpSpeed
      coDoubleJumped out `shouldBe` True

    it "a second mid-air jump does nothing" $ do
      let spent = baseIn { ciGrounded = False, ciDoubleJumped = True, ciVel = V2 0.0 50.0 }
          out = combatStep (withIntents [IntentJump] spent)
      vy out `shouldBe` 50.0
      coDoubleJumped out `shouldBe` True

  describe "dash" $ do
    it "a dash intent starts dashing at dash speed" $ do
      let out = combatStep (withIntents [IntentDash DirLeft] baseIn)
      coState out `shouldBe` StateDashing dashDuration
      vx out `shouldBe` (-dashSpeed)

    it "dash is blocked while the cooldown is running" $ do
      let cooling = baseIn { ciState = StateIdle 0.5 }
          out = combatStep (withIntents [IntentDash DirLeft] cooling)
      coState out `shouldSatisfy` \s -> case s of
        StateDashing _ -> False
        _              -> True

    it "an expired dash enters the cooldown" $ do
      let dashing = baseIn { ciState = StateDashing 0.001 }
          out = combatStep dashing
      coState out `shouldBe` StateIdle dashCooldownDuration

  describe "attack" $ do
    it "grounded attack starts charging" $ do
      let out = combatStep (withIntents [IntentAttackPress] baseIn)
      coState out `shouldBe` StateCharging 0.0

    it "air attack plunges" $ do
      let out = combatStep (withIntents [IntentAttackPress] baseIn { ciGrounded = False })
      coState out `shouldBe` StatePlunge
      vy out `shouldBe` plungeSpeed

    it "a short charge releases into a melee swing" $ do
      let charging = baseIn { ciState = StateCharging 0.1 }
          out = combatStep (withIntents [IntentAttackRelease] charging)
      coState out `shouldBe` StateMelee meleeDuration

    it "a full charge releases into a thrust" $ do
      let charged = baseIn { ciState = StateCharging chargeThreshold }
          out = combatStep (withIntents [IntentAttackRelease] charged)
      coState out `shouldBe` StateThrust thrustDuration
      vx out `shouldBe` thrustSpeed

    it "melee requests a swing VFX" $ do
      let charging = baseIn { ciState = StateCharging 0.1 }
          out = combatStep (withIntents [IntentAttackRelease] charging)
      coVfx out `shouldSatisfy` any (\(VfxRequest _ _ t _) -> t == VFXMelee)

  describe "grappling hook" $ do
    it "hook button shoots the hook up-forward" $ do
      let out = combatStep (withIntents [IntentHook] baseIn)
      case coHook out of
        HookFlying _ (V2 hvx hvy) -> do
          hvx `shouldSatisfy` (> 0.0)   -- facing right
          hvy `shouldSatisfy` (< 0.0)   -- upward
        other -> expectationFailure ("expected HookFlying, got " <> show other)

    it "hook button cancels a flying hook" $ do
      let flying = baseIn { ciHook = HookFlying (V2 0 0) (V2 0 0) }
          out = combatStep (withIntents [IntentHook] flying)
      coHook out `shouldBe` HookRetracted

    it "hook button on an anchored hook starts pulling" $ do
      let anchor = V2 200.0 50.0
          anchored = baseIn { ciHook = HookAnchored anchor }
          out = combatStep (withIntents [IntentHook] anchored)
      coState out `shouldBe` StateHookPulling anchor

    it "steering while pulling cancels the pull" $ do
      let pulling = baseIn { ciState = StateHookPulling (V2 200.0 50.0) }
          out = combatStep (withHeld (\h -> h { heldLeft = True }) pulling)
      coState out `shouldBe` StateIdle 0.0
      coHook out `shouldBe` HookRetracted

    it "jumping off a hang launches and restores the double jump" $ do
      let hanging = baseIn { ciState = StateHookHanging (V2 200.0 50.0)
                           , ciDoubleJumped = True }
          out = combatStep (withIntents [IntentJump] hanging)
      coState out `shouldBe` StateIdle 0.0
      vy out `shouldBe` jumpSpeed
      coDoubleJumped out `shouldBe` False
      coHook out `shouldBe` HookRetracted

  describe "stamina" $ do
    it "dashing costs stamina" $ do
      let out = combatStep (withIntents [IntentDash DirRight] baseIn)
      coStamina out `shouldBe` playerMaxStamina - dashStaminaCost

    it "dash is blocked when stamina is too low" $ do
      let tired = baseIn { ciStamina = dashStaminaCost - 1.0 }
          out = combatStep (withIntents [IntentDash DirRight] tired)
      coState out `shouldSatisfy` \s -> case s of
        StateDashing _ -> False
        _              -> True

    it "double jump costs stamina and is blocked when exhausted" $ do
      let airborne = baseIn { ciGrounded = False, ciVel = V2 0.0 50.0 }
          okOut = combatStep (withIntents [IntentJump] airborne)
      coStamina okOut `shouldBe` playerMaxStamina - doubleJumpStaminaCost
      let tired = airborne { ciStamina = doubleJumpStaminaCost - 1.0 }
          out = combatStep (withIntents [IntentJump] tired)
      vy out `shouldBe` 50.0
      coDoubleJumped out `shouldBe` False

    it "shooting the hook costs stamina and is blocked when exhausted" $ do
      let okOut = combatStep (withIntents [IntentHook] baseIn)
      coStamina okOut `shouldBe` playerMaxStamina - hookStaminaCost
      let tired = baseIn { ciStamina = hookStaminaCost - 1.0 }
          out = combatStep (withIntents [IntentHook] tired)
      coHook out `shouldBe` HookRetracted

    it "stamina regenerates while grounded and idle" $ do
      let tired = baseIn { ciStamina = 50.0 }
          out = combatStep tired
      coStamina out `shouldBe` 50.0 + staminaRegen * ciDt baseIn

    it "stamina does not regenerate in the air" $ do
      let tired = baseIn { ciStamina = 50.0, ciGrounded = False }
          out = combatStep tired
      coStamina out `shouldBe` 50.0

    it "stamina never exceeds the maximum" $ do
      let out = combatStep baseIn
      coStamina out `shouldBe` playerMaxStamina
