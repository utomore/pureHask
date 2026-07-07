module FlowSpec (spec) where

import Test.Hspec

import Core.Config (deathPauseDuration, levelCompleteDuration)
import Core.Types
import Flow.Machine

start :: FlowState
start = initialFlow 2  -- a two-level game

playing :: FlowState
playing = start { fsMode = ModePlaying }

inMenu :: FlowState
inMenu = playing { fsMode = ModeMenu }

-- | A frame with the default (empty) menu env.
frame :: Double -> [Intent] -> FlowIn
frame dt is = FlowFrame dt is emptyMenuEnv emptyWorldSnapshot

-- | A frame with a given menu env.
frameEnv :: MenuEnv -> [Intent] -> FlowIn
frameEnv env is = FlowFrame 0.016 is env emptyWorldSnapshot

sampleEnv :: MenuEnv
sampleEnv = MenuEnv
  { meBackpack  = [ (ItemId "gold-key", CatQuest, 1)
                  , (ItemId "potion-hp-s", CatPotion, 2)
                  , (ItemId "sword-rusty", CatEquip SlotWeapon, 1)
                  ]
  , meEquipped  = (SlotWeapon, Just (ItemId "sword-old"))
                    : [ (slot, Nothing) | slot <- [SlotBody ..] ]
  , meSaveSlots = [Just "LV 1  00:30", Nothing, Nothing]
  , meAutoSave  = Just "LV 2  10:00"
  , meSettings  = [("SHOW FPS", "OFF"), ("FULLSCREEN", "OFF"), ("FONT", "PIXEL"), ("LANGUAGE", "EN")]
  }

spec :: Spec
spec = do
  describe "title screen" $ do
    it "jump starts a fresh run" $ do
      let (fs, cmds) = stepFlow (frame 0.016 [IntentJump]) start
      fsMode fs `shouldBe` ModePlaying
      cmds `shouldBe` [CmdNewGame]

    it "starting a run resets the stats" $ do
      let dirty = start { fsStats = RunStats 5 3 99.0 }
          (fs, _) = stepFlow (frame 0.016 [IntentJump]) dirty
      fsStats fs `shouldBe` emptyRunStats

    it "menu key quits" $ do
      let (_, cmds) = stepFlow (frame 0.016 [IntentMenu]) start
      cmds `shouldBe` [CmdQuit]

  describe "menu open/close" $ do
    it "escape opens the menu at the status page" $ do
      let (fs, _) = stepFlow (frame 0.016 [IntentMenu]) playing
      fsMode fs `shouldBe` ModeMenu
      fsMenu fs `shouldBe` initialCursor

    it "escape closes the menu" $ do
      let (fs, _) = stepFlow (frame 0.016 [IntentMenu]) inMenu
      fsMode fs `shouldBe` ModePlaying

    it "time does not advance while the menu is open" $ do
      let (fs, _) = stepFlow (frame 1.0 []) inMenu
      statTime (fsStats fs) `shouldBe` statTime (fsStats inMenu)

  describe "menu navigation" $ do
    it "right cycles to the next page and resets the row" $ do
      let (fs, _) = stepFlow (frameEnv sampleEnv [IntentNavRight]) inMenu
      mcPage (fsMenu fs) `shouldBe` PageBackpack
      mcRow (fsMenu fs) `shouldBe` 0

    it "left from the first page wraps to the last" $ do
      let (fs, _) = stepFlow (frameEnv sampleEnv [IntentNavLeft]) inMenu
      mcPage (fsMenu fs) `shouldBe` PageSettings

    it "down moves the cursor and clamps at the list end" $ do
      let onBag = inMenu { fsMenu = MenuCursor PageBackpack 1 }
          (fs1, _) = stepFlow (frameEnv sampleEnv [IntentNavDown]) onBag
      mcRow (fsMenu fs1) `shouldBe` 2
      let (fs2, _) = stepFlow (frameEnv sampleEnv [IntentNavDown]) fs1
      mcRow (fsMenu fs2) `shouldBe` 2  -- three rows; clamped

    it "up clamps at zero" $ do
      let onBag = inMenu { fsMenu = MenuCursor PageBackpack 0 }
          (fs, _) = stepFlow (frameEnv sampleEnv [IntentNavUp]) onBag
      mcRow (fsMenu fs) `shouldBe` 0

  describe "menu confirm" $ do
    it "confirming a potion row uses the potion" $ do
      let onPotion = inMenu { fsMenu = MenuCursor PageBackpack 1 }
          (_, cmds) = stepFlow (frameEnv sampleEnv [IntentJump]) onPotion
      cmds `shouldBe` [CmdUseItem (ItemId "potion-hp-s")]

    it "confirming a quest item does nothing" $ do
      let onKey = inMenu { fsMenu = MenuCursor PageBackpack 0 }
          (_, cmds) = stepFlow (frameEnv sampleEnv [IntentJump]) onKey
      cmds `shouldBe` []

    it "confirming a gear row equips it" $ do
      let onSword = inMenu { fsMenu = MenuCursor PageBackpack 2 }
          (_, cmds) = stepFlow (frameEnv sampleEnv [IntentJump]) onSword
      cmds `shouldBe` [CmdEquip (ItemId "sword-rusty")]

    it "confirming an occupied equip slot unequips it" $ do
      let onSlot = inMenu { fsMenu = MenuCursor PageEquip 0 }
          (_, cmds) = stepFlow (frameEnv sampleEnv [IntentJump]) onSlot
      cmds `shouldBe` [CmdUnequip SlotWeapon]

    it "confirming an empty equip slot does nothing" $ do
      let onEmpty = inMenu { fsMenu = MenuCursor PageEquip 2 }
          (_, cmds) = stepFlow (frameEnv sampleEnv [IntentJump]) onEmpty
      cmds `shouldBe` []

    it "confirming a settings row toggles it" $ do
      let onSetting = inMenu { fsMenu = MenuCursor PageSettings 1 }
          (_, cmds) = stepFlow (frameEnv sampleEnv [IntentJump]) onSetting
      cmds `shouldBe` [CmdToggleSetting 1]

    it "confirming a save row writes the 1-based slot" $ do
      let onSave = inMenu { fsMenu = MenuCursor PageSave 1 }
          (_, cmds) = stepFlow (frameEnv sampleEnv [IntentJump]) onSave
      cmds `shouldBe` [CmdSaveGame 2]

    it "load row 0 loads the autosave" $ do
      let onAuto = inMenu { fsMenu = MenuCursor PageLoad 0 }
          (_, cmds) = stepFlow (frameEnv sampleEnv [IntentJump]) onAuto
      cmds `shouldBe` [CmdLoadGame 0]

    it "loading an existing manual slot works, an empty one does not" $ do
      let onSlot1 = inMenu { fsMenu = MenuCursor PageLoad 1 }
          (_, cmds1) = stepFlow (frameEnv sampleEnv [IntentJump]) onSlot1
      cmds1 `shouldBe` [CmdLoadGame 1]
      let onEmpty = inMenu { fsMenu = MenuCursor PageLoad 2 }
          (_, cmds2) = stepFlow (frameEnv sampleEnv [IntentJump]) onEmpty
      cmds2 `shouldBe` []

    it "a restored run adopts the saved level and stats" $ do
      let saved = RunStats 9 9 300.0
          (fs, _) = stepFlow (FlowEvents [EvRunRestored 1 saved emptyQuestLog]) inMenu
      fsMode fs `shouldBe` ModePlaying
      fsLevel fs `shouldBe` 1
      fsStats fs `shouldBe` saved

  describe "dialogue" $ do
    let dlg = playing { fsMode = ModeDialogue [("ELDER", "HI"), ("ELDER", "BYE")] }

    it "jump advances to the next line" $ do
      let (fs, _) = stepFlow (frame 0.016 [IntentJump]) dlg
      fsMode fs `shouldBe` ModeDialogue [("ELDER", "BYE")]

    it "the last line closes the dialogue" $ do
      let (mid, _) = stepFlow (frame 0.016 [IntentJump]) dlg
          (fs, _) = stepFlow (frame 0.016 [IntentJump]) mid
      fsMode fs `shouldBe` ModePlaying

    it "escape closes the dialogue immediately" $ do
      let (fs, _) = stepFlow (frame 0.016 [IntentMenu]) dlg
      fsMode fs `shouldBe` ModePlaying

    it "openDialogue with no lines is a no-op" $
      fsMode (openDialogue [] playing) `shouldBe` ModePlaying

  describe "death" $ do
    it "a death event enters ModeDead and counts the death" $ do
      let (fs, _) = stepFlow (FlowEvents [EvPlayerDied]) playing
      fsMode fs `shouldBe` ModeDead deathPauseDuration
      statDeaths (fsStats fs) `shouldBe` 1

    it "after the pause a respawn command fires" $ do
      let (dead, _) = stepFlow (FlowEvents [EvPlayerDied]) playing
          (fs, cmds) = stepFlow (frame (deathPauseDuration + 0.1) []) dead
      fsMode fs `shouldBe` ModePlaying
      cmds `shouldBe` [CmdRespawnPlayer]

    it "death events are ignored outside ModePlaying (no double count)" $ do
      let (dead, _) = stepFlow (FlowEvents [EvPlayerDied]) playing
          (fs, _) = stepFlow (FlowEvents [EvPlayerDied]) dead
      statDeaths (fsStats fs) `shouldBe` 1

  describe "level progression" $ do
    it "reaching the goal enters the level-complete card" $ do
      let (fs, _) = stepFlow (FlowEvents [EvGoalReached]) playing
      fsMode fs `shouldBe` ModeLevelComplete levelCompleteDuration

    it "after the card, the next level loads and autosaves" $ do
      let (done, _) = stepFlow (FlowEvents [EvGoalReached]) playing
          (fs, cmds) = stepFlow (frame (levelCompleteDuration + 0.1) []) done
      fsMode fs `shouldBe` ModePlaying
      fsLevel fs `shouldBe` 1
      cmds `shouldBe` [CmdLoadLevel 1, CmdSaveGame 0]

    it "finishing the last level rolls the ending" $ do
      let lastLevel = playing { fsLevel = 1 }
          (done, _) = stepFlow (FlowEvents [EvGoalReached]) lastLevel
          (fs, cmds) = stepFlow (frame (levelCompleteDuration + 0.1) []) done
      fsMode fs `shouldBe` ModeEnding
      cmds `shouldBe` []

    it "ending returns to title on jump" $ do
      let ending = start { fsMode = ModeEnding }
          (fs, _) = stepFlow (frame 0.016 [IntentJump]) ending
      fsMode fs `shouldBe` ModeTitle

  describe "stats" $ do
    it "item pickups are counted" $ do
      let (fs, _) = stepFlow
            (FlowEvents [EvItemPicked (ItemId "gold-key"), EvItemPicked (ItemId "potion-hp-s")])
            playing
      statItems (fsStats fs) `shouldBe` 2

    it "play time accumulates only in ModePlaying" $ do
      let (fs, _) = stepFlow (frame 0.5 []) playing
      statTime (fsStats fs) `shouldBe` 0.5
