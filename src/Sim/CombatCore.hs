-- | The player combat state machine as a pure step function.
--
--   "Sim.Combat" is the thin Apecs glue that reads the player's components
--   into a 'CombatIn', calls 'combatStep', and writes the 'CombatOut' back.
--   Everything gameplay-relevant — state transitions, velocities, VFX
--   requests — is decided here, purely, so the test suite can drive the
--   machine through whole scenarios without an ECS world.
--
--   Extension guide: a new player move is (1) a new 'CombatState'
--   constructor in "Core.Types", (2) its transitions here, (3) its visuals in
--   "Render.Draw". Nothing else needs to change.
module Sim.CombatCore
  ( CombatIn(..)
  , CombatOut(..)
  , VfxRequest(..)
  , combatStep
  ) where

import Linear (V2(..))

import Core.Config
import Core.Types

-- | Snapshot of everything the combat machine may read this sub-step.
data CombatIn = CombatIn
  { ciInput        :: !FrameInput   -- ^ intents fire on the first sub-step only
  , ciDt           :: !Double
  , ciState        :: !CombatState
  , ciGrounded     :: !Bool
  , ciFacing       :: !Direction
  , ciVel          :: !(V2 Double)
  , ciDoubleJumped :: !Bool
  , ciHook         :: !HookState
  , ciPos          :: !(V2 Double)  -- ^ top-left, for hook origin / VFX
  , ciSize         :: !(V2 Double)
  , ciStamina      :: !Double       -- ^ gates dash / double jump / hook
  , ciSpeedMult    :: !Double       -- ^ movement multiplier from equipment
  } deriving (Eq, Show)

-- | A visual effect the machine wants spawned (position, life, type, facing).
data VfxRequest = VfxRequest !(V2 Double) !Double !VFXType !(Maybe Direction)
  deriving (Eq, Show)

-- | Everything the combat machine decided this sub-step.
data CombatOut = CombatOut
  { coState        :: !CombatState
  , coVel          :: !(V2 Double)
  , coFacing       :: !Direction
  , coDoubleJumped :: !Bool
  , coHook         :: !HookState
  , coVfx          :: ![VfxRequest]
  , coGrounded     :: !Bool    -- ^ may clear grounded on the jump sub-step
  , coStamina      :: !Double  -- ^ after costs and regeneration
  } deriving (Eq, Show)

has :: Intent -> FrameInput -> Bool
has i = elem i . fiIntents

dashIntent :: FrameInput -> Maybe Direction
dashIntent fi = case [ d | IntentDash d <- fiIntents fi ] of
  (d : _) -> Just d
  []      -> Nothing

-- | One sub-step of the combat machine.
combatStep :: CombatIn -> CombatOut
combatStep ci = CombatOut
  { coState        = state'
  , coVel          = V2 vx' vy'
  , coFacing       = facing'
  , coDoubleJumped = dj'
  , coHook         = hook'
  , coVfx          = vfx
  , coGrounded     = grounded'
  , coStamina      = stamina'
  }
  where
    input          = ciInput ci
    held           = fiHeld input
    dt             = ciDt ci
    grounded       = ciGrounded ci
    V2 vx vy       = ciVel ci
    pos            = ciPos ci
    size           = ciSize ci

    -- 1. Facing may turn only in states that allow steering.
    facing' = case ciState ci of
      StateIdle _        -> steer
      StateCharging _    -> steer
      StateHookHanging _ -> steer
      _                  -> ciFacing ci
      where
        steer
          | heldLeft held  = DirLeft
          | heldRight held = DirRight
          | otherwise      = ciFacing ci

    signed d v = if d == DirLeft then -v else v

    -- 2. Hook button: shoot / cancel / start pulling. Shooting costs stamina.
    (hookAfterPress, stateAfterHook, hookCost)
      | has IntentHook input = case ciHook ci of
          HookRetracted
            | ciStamina ci >= hookStaminaCost ->
                let ang = 1.0 / sqrt 2.0
                    hVel = V2 (signed facing' ang * hookSpeed) (negate ang * hookSpeed)
                    hPos = pos + size / 2.0
                in (HookFlying hPos hVel, ciState ci, hookStaminaCost)
            | otherwise -> (HookRetracted, ciState ci, 0.0)
          HookFlying _ _ -> (HookRetracted, ciState ci, 0.0)
          HookAnchored anchor
            | heldLeft held || heldRight held -> (HookRetracted, ciState ci, 0.0)
            | otherwise -> (HookAnchored anchor, StateHookPulling anchor, 0.0)
      | otherwise = (ciHook ci, ciState ci, 0.0)

    -- 3. The state machine proper. The last tuple slot is the stamina cost
    --    this sub-step spent (dash, double jump).
    (state', vx', vy', dj', hook', vfx, moveCost) = case stateAfterHook of

      StateIdle cd ->
        let cd' = max 0.0 (cd - dt)
        in case dashIntent input of
          Just dashDir | cd' <= 0.0 && ciStamina ci >= dashStaminaCost ->
            ( StateDashing dashDuration
            , signed dashDir dashSpeed, 0.0
            , ciDoubleJumped ci, hookAfterPress
            , [VfxRequest pos 0.15 VFXDashGhost (Just dashDir)]
            , dashStaminaCost )
          _
            | has IntentAttackPress input ->
                if grounded
                  then (StateCharging 0.0, 0.0, vy, ciDoubleJumped ci, hookAfterPress, [], 0.0)
                  else (StatePlunge, 0.0, plungeSpeed, ciDoubleJumped ci, hookAfterPress, [], 0.0)
            | otherwise ->
                let moveSpeed = playerSpeed * ciSpeedMult ci
                    runSpeed = case (heldLeft held, heldRight held) of
                      (True, False) -> -moveSpeed
                      (False, True) -> moveSpeed
                      _             -> 0.0
                    (jumpVel, djNew, jumpVfx, jumpCost)
                      | has IntentJump input =
                          if grounded
                            then (jumpSpeed, ciDoubleJumped ci, [], 0.0)
                            else if not (ciDoubleJumped ci)
                                    && ciStamina ci >= doubleJumpStaminaCost
                              then ( jumpSpeed, True
                                   , [VfxRequest pos 0.15 VFXDashGhost Nothing]
                                   , doubleJumpStaminaCost )
                              else (vy, ciDoubleJumped ci, [], 0.0)
                      | otherwise = (vy, ciDoubleJumped ci, [], 0.0)
                in (StateIdle cd', runSpeed, jumpVel, djNew, hookAfterPress, jumpVfx, jumpCost)

      StateDashing t ->
        let t' = t - dt
        in if t' <= 0.0
             then (StateIdle dashCooldownDuration, 0.0, vy, ciDoubleJumped ci, hookAfterPress, [], 0.0)
             else ( StateDashing t'
                  , signed facing' dashSpeed, 0.0
                  , ciDoubleJumped ci, hookAfterPress
                  , [VfxRequest pos 0.12 VFXDashGhost (Just facing')], 0.0 )

      StateMelee t ->
        let t' = t - dt
        in if t' <= 0.0
             then (StateIdle 0.0, 0.0, vy, ciDoubleJumped ci, hookAfterPress, [], 0.0)
             else (StateMelee t', 0.0, vy, ciDoubleJumped ci, hookAfterPress, [], 0.0)

      StateCharging t ->
        let t' = t + dt
        in if has IntentAttackRelease input
             then if t' < chargeThreshold
               then ( StateMelee meleeDuration, 0.0, vy
                    , ciDoubleJumped ci, hookAfterPress
                    , [VfxRequest pos meleeDuration VFXMelee (Just facing')], 0.0 )
               else ( StateThrust thrustDuration
                    , signed facing' thrustSpeed, 0.0
                    , ciDoubleJumped ci, hookAfterPress
                    , [VfxRequest pos thrustDuration VFXThrust (Just facing')], 0.0 )
             else (StateCharging t', 0.0, 0.0, ciDoubleJumped ci, hookAfterPress, [], 0.0)

      StateThrust t ->
        let t' = t - dt
        in if t' <= 0.0
             then (StateIdle 0.0, 0.0, vy, ciDoubleJumped ci, hookAfterPress, [], 0.0)
             else ( StateThrust t'
                  , signed facing' thrustSpeed, 0.0
                  , ciDoubleJumped ci, hookAfterPress, [], 0.0 )

      -- Landing (plunge -> idle + shockwave) is physics-driven and handled
      -- in "Sim.Physics"; here plunge just keeps forcing the dive velocity.
      StatePlunge ->
        (StatePlunge, 0.0, plungeSpeed, ciDoubleJumped ci, hookAfterPress, [], 0.0)

      StateHookPulling anchor
        | heldLeft held || heldRight held || has IntentJump input ->
            (StateIdle 0.0, vx, vy, ciDoubleJumped ci, HookRetracted, [], 0.0)
        | otherwise ->
            (StateHookPulling anchor, vx, vy, ciDoubleJumped ci, hookAfterPress, [], 0.0)

      StateHookHanging anchor
        | has IntentJump input ->
            -- Launch off the hook; hanging restores the double jump.
            (StateIdle 0.0, 0.0, jumpSpeed, False, HookRetracted, [], 0.0)
        | otherwise ->
            (StateHookHanging anchor, 0.0, 0.0, ciDoubleJumped ci, hookAfterPress, [], 0.0)

    -- Stamina: pay this sub-step's costs, then regenerate while grounded and
    -- not spending.
    totalCost = hookCost + moveCost
    stamina' =
      let afterCost = ciStamina ci - totalCost
          regen = if grounded && totalCost <= 0.0 then staminaRegen * dt else 0.0
      in max 0.0 (min playerMaxStamina (afterCost + regen))

    -- 4. Jumping off the ground clears grounded immediately so the next
    --    sub-step cannot treat the lift-off frame as still grounded.
    grounded' =
      if has IntentJump input && grounded && state' == StateIdle 0.0
        then False
        else grounded
