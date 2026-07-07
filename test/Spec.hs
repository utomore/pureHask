module Main where

import Test.Hspec

import qualified AudioSpec
import qualified CodecSpec
import qualified CombatCoreSpec
import qualified EnemyCoreSpec
import qualified EquipCoreSpec
import qualified ExprSpec
import qualified FlowSpec
import qualified HudSpec
import qualified InheritSpec
import qualified ItemsSpec
import qualified LangSpec
import qualified NpcSpec
import qualified ParticleSpec
import qualified QuestSpec
import qualified LevelSpec
import qualified RegistrySpec
import qualified SceneSpec
import qualified SemanticsSpec
import qualified SettingsSpec
import qualified SexpSpec
import qualified SpriteSpec
import qualified TalentSpec
import qualified TilemapSpec

main :: IO ()
main = hspec $ do
  describe "Script.Sexp"      SexpSpec.spec
  describe "Core.Lang"        LangSpec.spec
  describe "Script.Inherit"   InheritSpec.spec
  describe "Script.Expr"      ExprSpec.spec
  describe "Items.Registry"   RegistrySpec.spec
  describe "Sim.Items"        ItemsSpec.spec
  describe "World.Tilemap"    TilemapSpec.spec
  describe "World.Level"      LevelSpec.spec
  describe "World.Scene"      SceneSpec.spec
  describe "Input.Semantics"  SemanticsSpec.spec
  describe "Core.Settings"    SettingsSpec.spec
  describe "Flow.Machine"     FlowSpec.spec
  describe "Sim.CombatCore"   CombatCoreSpec.spec
  describe "Sim.EnemyCore"    EnemyCoreSpec.spec
  describe "Sim.EquipCore"    EquipCoreSpec.spec
  describe "Talent"           TalentSpec.spec
  describe "Sprite"           SpriteSpec.spec
  describe "Audio"            AudioSpec.spec
  describe "Sim.ParticleCore" ParticleSpec.spec
  describe "Save.Codec"       CodecSpec.spec
  describe "Quest"            QuestSpec.spec
  describe "Npc"              NpcSpec.spec
  describe "Hud.Machine"      HudSpec.spec
