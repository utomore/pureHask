module SettingsSpec (spec) where

import Test.Hspec

import Core.Settings

spec :: Spec
spec = do
  describe "parse/format roundtrip" $ do
    it "defaults survive a roundtrip" $
      parseSettings (formatSettings defaultSettings) `shouldBe` defaultSettings

    it "every font choice survives a roundtrip" $
      mapM_
        (\f -> let s = defaultSettings { setFont = f }
               in parseSettings (formatSettings s) `shouldBe` s)
        [minBound .. maxBound]

    it "every language survives a roundtrip" $
      mapM_
        (\l -> let s = defaultSettings { setLang = l }
               in parseSettings (formatSettings s) `shouldBe` s)
        [minBound .. maxBound]

    it "all flags on survive a roundtrip" $ do
      let s = Settings True True FontPixel LangEn
      parseSettings (formatSettings s) `shouldBe` s

  describe "parseSettings" $ do
    it "empty input yields the defaults" $
      parseSettings "" `shouldBe` defaultSettings

    it "unknown keys and malformed lines are ignored" $
      parseSettings "bogus=true\nnot a pair\nshowFps=true"
        `shouldBe` defaultSettings { setShowFps = True }

    it "an unknown font value keeps the default" $
      setFont (parseSettings "font=comic-sans") `shouldBe` setFont defaultSettings

  describe "toggleSetting" $ do
    it "row 0 toggles SHOW FPS" $
      setShowFps (toggleSetting 0 defaultSettings) `shouldBe` True

    it "row 1 toggles FULLSCREEN" $
      setFullscreen (toggleSetting 1 defaultSettings) `shouldBe` True

    it "row 2 cycles the font through every choice and wraps" $ do
      let states = iterate (toggleSetting 2) defaultSettings { setFont = minBound }
          n = length ([minBound .. maxBound] :: [FontChoice])
      map setFont (take n states) `shouldBe` [minBound .. maxBound]
      setFont (states !! n) `shouldBe` minBound

    it "row 3 cycles the language and wraps" $ do
      let s1 = toggleSetting 3 defaultSettings
      setLang s1 `shouldBe` LangEn
      setLang (toggleSetting 3 s1) `shouldBe` LangZhTW

    it "an out-of-range row is a no-op" $
      toggleSetting 99 defaultSettings `shouldBe` defaultSettings

  describe "settingsRows" $ do
    it "has one row per setting, in cursor order" $
      map fst (settingsRows defaultSettings)
        `shouldBe` ["SHOW FPS", "FULLSCREEN", "FONT", "LANGUAGE"]

    it "shows the active font label" $ do
      let s = defaultSettings { setFont = FontChinese }
      lookup "FONT" (settingsRows s) `shouldBe` Just (fontLabel FontChinese)
