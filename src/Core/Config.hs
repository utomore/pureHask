-- | Every tunable constant in one place. Balance / game-feel iteration should
--   only ever touch this module.
module Core.Config where

--------------------------------------------------------------------------------
-- Screen / timing
--------------------------------------------------------------------------------

-- | Window width in pixels.
screenWidth :: Double
screenWidth = 800.0

-- | Window height in pixels.
screenHeight :: Double
screenHeight = 600.0

-- | Fixed simulation time step (seconds). The renderer runs once per frame;
--   the simulation runs zero or more fixed steps per frame (accumulator
--   pattern in "Main"). Never feed a variable dt into the simulation.
simStep :: Double
simStep = 1.0 / 120.0

-- | Upper bound on the per-frame accumulated time, so that a dragged window
--   or a debugger pause does not trigger a huge catch-up burst.
maxFrameTime :: Double
maxFrameTime = 0.25

--------------------------------------------------------------------------------
-- World
--------------------------------------------------------------------------------

-- | Tile size in pixels.
tileSize :: Double
tileSize = 32.0

-- | Gravity acceleration (pixels/sec^2).
gravityAccel :: Double
gravityAccel = 1500.0

-- | Maximum downward falling speed (pixels/sec).
terminalVelocity :: Double
terminalVelocity = 900.0

--------------------------------------------------------------------------------
-- Player movement
--------------------------------------------------------------------------------

-- | Horizontal movement speed (pixels/sec).
playerSpeed :: Double
playerSpeed = 300.0

-- | Jump vertical speed (pixels/sec). Negative because y grows downward.
jumpSpeed :: Double
jumpSpeed = -550.0

-- | Player collider size (pixels).
playerSize :: Double
playerSize = 24.0

--------------------------------------------------------------------------------
-- Player vitals
--------------------------------------------------------------------------------

playerMaxHp :: Double
playerMaxHp = 100.0

playerMaxMp :: Double
playerMaxMp = 50.0

playerMaxStamina :: Double
playerMaxStamina = 100.0

-- | Stamina regeneration per second while grounded.
staminaRegen :: Double
staminaRegen = 25.0

dashStaminaCost :: Double
dashStaminaCost = 20.0

doubleJumpStaminaCost :: Double
doubleJumpStaminaCost = 10.0

hookStaminaCost :: Double
hookStaminaCost = 15.0

--------------------------------------------------------------------------------
-- Combat
--------------------------------------------------------------------------------

dashDuration :: Double
dashDuration = 0.15

dashCooldownDuration :: Double
dashCooldownDuration = 0.6

dashSpeed :: Double
dashSpeed = 800.0

-- | Fraction of the dash speed carried into a dash jump (jump pressed while
--   dashing). 0.85 * 800 = 680 px/s, well above the 300 px/s run speed, so
--   the technique is worth mastering.
dashJumpCarryFactor :: Double
dashJumpCarryFactor = 0.85

meleeDuration :: Double
meleeDuration = 0.18

-- | Holding attack at least this long turns the release into a thrust.
chargeThreshold :: Double
chargeThreshold = 0.45

thrustDuration :: Double
thrustDuration = 0.2

thrustSpeed :: Double
thrustSpeed = 900.0

plungeSpeed :: Double
plungeSpeed = 1100.0

projectileSpeed :: Double
projectileSpeed = 700.0

--------------------------------------------------------------------------------
-- Grappling hook
--------------------------------------------------------------------------------

hookSpeed :: Double
hookSpeed = 1200.0

maxHookDist :: Double
maxHookDist = 320.0

hookPullSpeed :: Double
hookPullSpeed = 850.0

-- | Distance to the anchor below which pulling turns into hanging.
hookArriveDist :: Double
hookArriveDist = 12.0

--------------------------------------------------------------------------------
-- Damage
--------------------------------------------------------------------------------

-- | The player's unarmed attack damage; weapon @atk@ stats add on top.
playerBaseAtk :: Double
playerBaseAtk = 12.0

-- | Damage multiplier of the charged thrust relative to a normal swing.
thrustDamageMult :: Double
thrustDamageMult = 1.6

-- | Damage multiplier of the plunge relative to a normal swing.
plungeDamageMult :: Double
plungeDamageMult = 1.3

-- | Seconds of invulnerability after the player takes a hit.
playerInvulnDuration :: Double
playerInvulnDuration = 0.8

-- | Seconds an enemy cannot be damaged again after taking a hit, so one
--   swing does not land on every 1/120s sub-step.
enemyHurtCooldown :: Double
enemyHurtCooldown = 0.3

-- | Enemy projectile collider edge (pixels).
enemyProjectileSize :: Double
enemyProjectileSize = 10.0

--------------------------------------------------------------------------------
-- Particles
--------------------------------------------------------------------------------

-- | Hard cap on live particles; the oldest are dropped past it.
maxParticles :: Int
maxParticles = 600

-- | Minimum landing speed (pixels/sec) that kicks up dust.
landingDustMinSpeed :: Double
landingDustMinSpeed = 260.0

--------------------------------------------------------------------------------
-- Input semantics
--------------------------------------------------------------------------------

-- | Two presses of the same direction within this window trigger a dash.
doubleTapWindow :: Double
doubleTapWindow = 0.22

--------------------------------------------------------------------------------
-- Talents
--------------------------------------------------------------------------------

-- | Enemies defeated per talent point (kill-milestone acquisition).
talentKillsPerPoint :: Int
talentKillsPerPoint = 5

-- | Talent points granted for clearing a level.
talentPointsPerClear :: Int
talentPointsPerClear = 1

--------------------------------------------------------------------------------
-- Game flow
--------------------------------------------------------------------------------

-- | Seconds spent on the death screen before respawning.
deathPauseDuration :: Double
deathPauseDuration = 0.9

-- | Seconds spent on the level-complete card before loading the next level.
levelCompleteDuration :: Double
levelCompleteDuration = 1.2
