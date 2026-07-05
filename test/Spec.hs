module Main where

import Test.Hspec

import qualified CodecSpec
import qualified CombatCoreSpec
import qualified EquipCoreSpec
import qualified ExprSpec
import qualified FlowSpec
import qualified HudSpec
import qualified ItemsSpec
import qualified NpcSpec
import qualified QuestSpec
import qualified LevelSpec
import qualified RegistrySpec
import qualified SceneSpec
import qualified SemanticsSpec
import qualified SexpSpec
import qualified TilemapSpec

main :: IO ()
main = hspec $ do
  describe "Script.Sexp"      SexpSpec.spec
  describe "Script.Expr"      ExprSpec.spec
  describe "Items.Registry"   RegistrySpec.spec
  describe "Sim.Items"        ItemsSpec.spec
  describe "World.Tilemap"    TilemapSpec.spec
  describe "World.Level"      LevelSpec.spec
  describe "World.Scene"      SceneSpec.spec
  describe "Input.Semantics"  SemanticsSpec.spec
  describe "Flow.Machine"     FlowSpec.spec
  describe "Sim.CombatCore"   CombatCoreSpec.spec
  describe "Sim.EquipCore"    EquipCoreSpec.spec
  describe "Save.Codec"       CodecSpec.spec
  describe "Quest"            QuestSpec.spec
  describe "Npc"              NpcSpec.spec
  describe "Hud.Machine"      HudSpec.spec
