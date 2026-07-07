module EnemyCoreSpec (spec) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Linear (V2(..))
import Test.Hspec

import Core.Config (tileSize)
import Core.Types
import Enemy.Script
import Script.Sexp (parseSexps, formsNamed)
import Sim.EnemyCore

-- | A contact slime patrolling 2 tiles around x=320 (via the shorthand's
--   sugar tree, so these tests also pin the sugar's semantics).
slime :: EnemyDef
slime = EnemyDef
  { edId = EnemyId "slime", edName = "SLIME", edColor = (90, 200, 90)
  , edSize = V2 24.0 18.0
  , edHp = 30.0, edDamage = 10.0, edSpeed = 60.0
  , edTree = sugarTree (2.0 * tileSize) 140.0 60.0 AttackContact
  , edSpawns = [("03-hollow", V2 320.0 416.0)]
  }

archer :: EnemyDef
archer = slime
  { edId = EnemyId "archer"
  , edTree = sugarTree (2.0 * tileSize) 260.0 60.0 (AttackRanged 2.0 300.0)
  }

-- | Full behavior-tree enemy: flees below 35% hp, else chase, else patrol.
warden :: EnemyDef
warden = slime
  { edId = EnemyId "warden"
  , edHp = 90.0
  , edTree = BTSelect
      [ BTSequence [BTCond (CondHpBelow 0.35), BTAct (ActFlee 130.0)]
      , BTSequence [BTCond (CondPlayerWithin 220.0), BTAct (ActChase 80.0)]
      , BTAct (ActPatrol (2.0 * tileSize) 40.0)
      ]
  }

home :: V2 Double
home = V2 320.0 416.0

farAway :: V2 Double
farAway = V2 5000.0 416.0

ai0 :: EnemyAi
ai0 = initialEnemyAi home

dt :: Double
dt = 1.0 / 120.0

-- | Step with full hp.
stepFull :: EnemyDef -> V2 Double -> V2 Double -> EnemyAi -> EnemyStep
stepFull def self player = stepEnemyAi def dt 1.0 self player

spec :: Spec
spec = do
  describe "patrol (sugar tree)" $ do
    it "walks right from the spawn" $ do
      let step = stepFull slime home farAway ai0
      esVx step `shouldBe` 60.0
      esShoot step `shouldBe` Nothing

    it "turns around (with a pause) past the patrol edge" $ do
      let atEdge = home + V2 (2.0 * tileSize + 1.0) 0.0
          step = stepFull slime atEdge farAway ai0
      esVx step `shouldBe` 0.0
      eaPatrolDir (esAi step) `shouldBe` DirLeft
      eaPatrolPause (esAi step) `shouldSatisfy` (> 0.0)

    it "resumes walking left after the pause" $ do
      let paused = ai0 { eaPatrolDir = DirLeft, eaPatrolPause = 0.001 }
          step1 = stepFull slime home farAway paused
          step2 = stepFull slime home farAway (esAi step1)
      esVx step2 `shouldBe` (-60.0)

  describe "chase (sugar tree)" $ do
    it "chases the player inside the aggro range" $ do
      let step = stepFull slime home (home + V2 100.0 0.0) ai0
      esVx step `shouldBe` 60.0

    it "chases towards the player's side" $ do
      let step = stepFull slime home (home - V2 100.0 0.0) ai0
      esVx step `shouldBe` (-60.0)

    it "patrols instead when the player is outside the aggro range" $ do
      let step = stepFull slime home (home + V2 200.0 0.0) ai0
      esVx step `shouldBe` 60.0  -- patrol from spawn walks right

    it "aggro 0 never chases" $ do
      let passive = slime { edTree = sugarTree (2.0 * tileSize) 0.0 60.0 AttackContact }
          step = stepFull passive home (home + V2 10.0 0.0) ai0
      -- patrol output, not chase (identical vx here, so pin via the tree)
      edTree passive `shouldBe` BTAct (ActPatrol (2.0 * tileSize) 60.0)
      esVx step `shouldBe` 60.0

  describe "ranged attack (sugar tree)" $ do
    it "keeps a standoff distance instead of hugging the player" $ do
      let step = stepFull archer home (home + V2 80.0 0.0) ai0
      esVx step `shouldBe` 0.0

    it "shoots when the cooldown is ready and then rearms it" $ do
      let step = stepFull archer home (home + V2 100.0 0.0) ai0
      case esShoot step of
        Just (V2 sx _, speed) -> do
          sx `shouldSatisfy` (> 0.0)
          speed `shouldBe` 300.0
        Nothing -> expectationFailure "expected a shot"
      eaCooldown (esAi step) `shouldBe` 2.0

    it "does not shoot while the cooldown is running" $ do
      let hot = ai0 { eaCooldown = 1.5 }
          step = stepFull archer home (home + V2 100.0 0.0) hot
      esShoot step `shouldBe` Nothing

    it "contact enemies never shoot" $ do
      let step = stepFull slime home (home + V2 50.0 0.0) ai0
      esShoot step `shouldBe` Nothing

  describe "behavior tree (full)" $ do
    it "select takes the FIRST succeeding rule: low hp flees even in aggro" $ do
      let step = stepEnemyAi warden dt 0.2 home (home + V2 100.0 0.0) ai0
      esVx step `shouldBe` (-130.0)  -- flee away from the player

    it "healthy inside aggro chases" $ do
      let step = stepEnemyAi warden dt 1.0 home (home + V2 100.0 0.0) ai0
      esVx step `shouldBe` 80.0

    it "healthy and alone patrols" $ do
      let step = stepEnemyAi warden dt 1.0 home farAway ai0
      esVx step `shouldBe` 40.0

    it "a failed sequence leaves no partial writes" $ do
      -- (sequence (chase …) (player-within 0)) writes vx then fails; the
      -- transaction must roll back to the pre-sequence outputs.
      let tree = BTSelect
            [ BTSequence [BTAct (ActChase 99.0), BTCond (CondPlayerWithin 0.0)]
            , BTAct ActStop
            ]
          def = slime { edTree = tree }
          step = stepFull def home (home + V2 100.0 0.0) ai0
      esVx step `shouldBe` 0.0

  describe "enemy DSL compiler" $ do
    it "compiles the (behavior …) shorthand into the sugar tree" $ do
      let src = unlines
            [ "(enemy archer"
            , "  (name \"深淵射手\")"
            , "  (stats (hp 20) (damage 8) (speed 40))"
            , "  (behavior (patrol 3) (aggro 260) (attack ranged 2.0 300))"
            , "  (spawn-at 04-dunes 12 10))"
            ]
      case parseAndCompile src of
        Right ed -> do
          edTree ed `shouldBe`
            sugarTree (3.0 * tileSize) 260.0 40.0 (AttackRanged 2.0 300.0)
          edSpawns ed `shouldBe` [("04-dunes", V2 (12.0 * tileSize) (10.0 * tileSize))]
        Left err -> expectationFailure err

    it "compiles a full (behavior-tree …)" $ do
      let src = unlines
            [ "(enemy warden"
            , "  (name \"巨衛\")"
            , "  (stats (hp 90) (damage 22) (speed 80))"
            , "  (behavior-tree"
            , "    (select"
            , "      (sequence (hp-below 0.35) (flee 130))"
            , "      (sequence (player-within 220) (chase 80))"
            , "      (patrol 2 40)))"
            , "  (spawn-at 05-abyssgate 50 12))"
            ]
      case parseAndCompile src of
        Right ed -> edTree ed `shouldBe` BTSelect
          [ BTSequence [BTCond (CondHpBelow 0.35), BTAct (ActFlee 130.0)]
          , BTSequence [BTCond (CondPlayerWithin 220.0), BTAct (ActChase 80.0)]
          , BTAct (ActPatrol (2.0 * tileSize) 40.0)
          ]
        Left err -> expectationFailure err

    it "rejects an unknown tree node with its name" $ do
      let src = "(enemy x (name \"x\") (stats (hp 1) (damage 1) (speed 1)) (behavior-tree (summon-dragons)) (spawn-at 01-a 1 1))"
      parseAndCompile src `shouldSatisfy` \r -> case r of
        Left e  -> "summon-dragons" `T.isInfixOf` T.pack e
        Right _ -> False

    it "rejects an enemy without spawn points" $ do
      let src = "(enemy ghost (name \"鬼\") (stats (hp 1) (damage 1) (speed 1)))"
      parseAndCompile src `shouldSatisfy` \r -> case r of
        Left e  -> not (null e)
        Right _ -> False

  describe "player hitboxes" $ do
    it "melee opens a hitbox in front of the player" $ do
      let Just (V2 hx _, _) =
            playerHitbox (StateMelee 0.1) DirRight (V2 100.0 100.0) (V2 24.0 24.0)
      hx `shouldSatisfy` (> 100.0)

    it "melee facing left opens the hitbox on the left" $ do
      let Just (V2 hx _, _) =
            playerHitbox (StateMelee 0.1) DirLeft (V2 100.0 100.0) (V2 24.0 24.0)
      hx `shouldSatisfy` (< 100.0)

    it "idle has no hitbox" $
      playerHitbox (StateIdle 0.0) DirRight (V2 0.0 0.0) (V2 24.0 24.0)
        `shouldBe` Nothing

    it "thrust hits harder than melee, plunge in between" $ do
      let melee  = playerAttackDamage (StateMelee 0.1) 0
          thrust = playerAttackDamage (StateThrust 0.1) 0
          plunge = playerAttackDamage StatePlunge 0
      thrust `shouldSatisfy` (> plunge)
      plunge `shouldSatisfy` (> melee)

    it "weapon atk adds to the damage" $
      playerAttackDamage (StateMelee 0.1) 5
        `shouldBe` playerAttackDamage (StateMelee 0.1) 0 + 5.0

parseAndCompile :: String -> Either String EnemyDef
parseAndCompile src = do
  forms <- parseSexps (T.pack src)
  case formsNamed "enemy" forms of
    (body : _) -> compileEnemy M.empty body
    []         -> Left "no (enemy …) form"