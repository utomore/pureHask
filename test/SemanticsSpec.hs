module SemanticsSpec (spec) where

import Test.Hspec

import Core.Config (doubleTapWindow)
import Core.Types
import Input.Semantics

-- | Run a sequence of (dt, raw) frames, collecting the intents of each frame.
runFrames :: [(Double, RawInput)] -> [[Intent]]
runFrames = go initialTracker
  where
    go _ [] = []
    go trk ((dt, raw) : rest) =
      let (trk', fi) = stepIntents dt raw trk
      in fiIntents fi : go trk' rest

key :: (RawInput -> RawInput) -> RawInput
key f = f emptyRawInput

spec :: Spec
spec = do
  describe "edge detection" $ do
    it "fires IntentJump on the press frame only" $ do
      let frames = runFrames
            [ (0.016, key (\r -> r { rawJump = True }))
            , (0.016, key (\r -> r { rawJump = True }))  -- still held
            , (0.016, emptyRawInput)
            ]
      frames `shouldBe` [[IntentJump], [], []]

    it "fires IntentInteract on the E press frame only" $ do
      -- Regression: this rule was missing, so E never talked to NPCs.
      let frames = runFrames
            [ (0.016, key (\r -> r { rawInteract = True }))
            , (0.016, key (\r -> r { rawInteract = True }))  -- still held
            , (0.016, emptyRawInput)
            ]
      frames `shouldBe` [[IntentInteract], [], []]

    it "fires attack press and release separately" $ do
      let frames = runFrames
            [ (0.016, key (\r -> r { rawAttack = True }))
            , (0.016, key (\r -> r { rawAttack = True }))
            , (0.016, emptyRawInput)
            ]
      frames `shouldBe` [[IntentAttackPress], [], [IntentAttackRelease]]

  describe "double-tap dash" $ do
    it "two quick presses of the same direction fire a dash" $ do
      let frames = runFrames
            [ (0.016, key (\r -> r { rawLeft = True }))
            , (0.016, emptyRawInput)                       -- release
            , (0.016, key (\r -> r { rawLeft = True }))    -- re-press quickly
            ]
      last frames `shouldSatisfy` elem (IntentDash DirLeft)

    it "a slow second press does not dash" $ do
      let frames = runFrames
            [ (0.016, key (\r -> r { rawLeft = True }))
            , (doubleTapWindow + 0.1, emptyRawInput)       -- window expires
            , (0.016, key (\r -> r { rawLeft = True }))
            ]
      last frames `shouldSatisfy` notElem (IntentDash DirLeft)

    it "alternating directions do not dash" $ do
      let frames = runFrames
            [ (0.016, key (\r -> r { rawLeft = True }))
            , (0.016, emptyRawInput)
            , (0.016, key (\r -> r { rawRight = True }))
            ]
      last frames `shouldSatisfy` \is ->
        notElem (IntentDash DirLeft) is && notElem (IntentDash DirRight) is

  describe "menu navigation intents" $ do
    it "direction presses also emit nav intents" $ do
      let (_, fi) = stepIntents 0.016 (key (\r -> r { rawLeft = True })) initialTracker
      fiIntents fi `shouldSatisfy` elem IntentNavLeft

    it "up/down keys emit nav intents" $ do
      let (_, fi) = stepIntents 0.016 (key (\r -> r { rawUp = True, rawDown = True }))
                                initialTracker
      fiIntents fi `shouldSatisfy` elem IntentNavUp
      fiIntents fi `shouldSatisfy` elem IntentNavDown

  describe "held keys" $ do
    it "reports held state each frame regardless of edges" $ do
      let (_, fi) = stepIntents 0.016 (key (\r -> r { rawLeft = True, rawJump = True }))
                                initialTracker
      heldLeft (fiHeld fi) `shouldBe` True
      heldJump (fiHeld fi) `shouldBe` True
      heldRight (fiHeld fi) `shouldBe` False
