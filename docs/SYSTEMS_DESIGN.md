# pureHask 系統擴充設計書 — RPG 化七大系統

> 版本:v1 已實作(2026-07-05 設計並完成 S1–S9)
> 前置閱讀:[ARCHITECTURE.md](ARCHITECTURE.md)(三條鐵律)、[GAME_DESIGN_REPORT.md](GAME_DESIGN_REPORT.md)(遊戲願景)
> 本文件描述七大系統的**建構方式**與**組合方式**。

## ✅ 實作狀態:S1–S9 全部完成

全部九個里程碑已實作並通過測試(170+ 測例、-Wall 零警告)。與原設計的偏差:

| 偏差 | 原因 |
|---|---|
| 背包頁為單一分組清單(含分類標籤欄),非四分頁 | v1 UI 簡化;儲存層完整支援四分類,分頁 UI 是純渲染改動 |
| 設定檔為 `config/settings.cfg`(key=value),非 JSON | 兩個布林設定不值得 JSON;若設定變複雜再遷移 |
| 對話(`say`)由 `Game.Logic` 直接開 `ModeDialogue`,未走 WorldCommand | 對話是模式轉換(邏輯層職權),不是世界變更 |
| 任務腳本中的 `say` 顯示為 toast,NPC 對話才用對話框 | 任務完成當下常在移動中,toast 干擾較小 |
| 自動存檔位於 slot 0(`saves/auto.json`),讀檔頁第 0 列 | 不覆寫玩家手動槽位 |
| 玩家跨關卡狀態(背包/裝備/血量)由 `PlayerPersist` 攜帶 | 原型的「world 重建即重置」與 RPG 進度矛盾,實作時修正 |

---

## 0. 總覽:新系統在架構中的位置

七大系統全部沿用三條鐵律。新增的核心概念只有兩個:

1. **Script DSL 基座**(`Script.*`):一個 S-expression 語法的外部腳本語言,任務與 NPC 共用同一套「運算式/條件/動作」核心,各自疊上自己的 schema。**加任務、加 NPC = 加文字檔,不重編譯。**
2. **WorldCommand 通道**:流程層/任務層對模擬世界的干預指令(給道具、開門、生成 NPC),與現有 `FlowCommand` 同構——純機器決定 WHEN,殼層執行 WHAT。

擴充後的資料流(粗體為新增):

```
SDL ──RawInput──► FRP.Network ──Intent──► Apecs 模擬 ──GameEvent──► FRP.Network
                  ├ Input.Semantics(現有)              ▲              ├ Flow.Machine(擴充:選單頁)
                  ├ Flow.Machine   (現有)              │              ├ **Quest.Runtime**(新:任務機)
                  └ **Hud.Machine**(新:提示/跑馬燈)   │              └ 產出 FlowCommand + **WorldCommand**
                                                        │                        │
        殼層(Main)執行:載關卡/重生/離開 + **存讀檔 + WorldCommand 寫入世界** ◄──┘
                                                        │
   啟動時載入並交叉驗證:**Items.Registry / Quest 腳本 / NPC 腳本 / Scene 圖層檔**
```

新增資產目錄:

```
assets/
  levels/*.txt        (現有)關卡地形
  scenes/*.scene      (新)七層視差佈景,對應同名關卡
  items/items.def     (新)道具資料庫
  quests/*.quest      (新)任務腳本
  npcs/*.npc          (新)NPC 腳本
saves/slotN.json      (新)存檔(gitignore)
config/settings.json  (新)遊戲設定
```

---

## 1. Script DSL 基座(任務與 NPC 的共同地基)

### 1.1 為什麼自製 S-expression DSL,不用 Lua/JSON

- **不重編譯**是硬需求 → 排除 Haskell EDSL。
- Lua(hslua)引入原生依賴與不可測的副作用面;JSON 寫邏輯(條件/分支)極其醜陋。
- S-expr parser 約 120 行、零依賴、語法均勻,而且**AST 即資料**——直譯器是純函式,完全可測,與本專案哲學一致。

### 1.2 分層

```
Script.Sexp     -- S-expression 型別 + parser(純,零依賴)
Script.Expr     -- 共用語意層:值(Int/Bool/String)、條件、動作 的 AST 與求值器
Script.Env      -- 求值環境:唯讀世界快照(旗標、任務狀態、背包、玩家位置…)
Quest.Script    -- 任務 schema:Sexp → QuestDef(編譯,啟動時執行並驗證)
Npc.Script      -- NPC schema:Sexp → NpcDef
```

**求值器簽名(純)**:

```haskell
evalCond :: ScriptEnv -> Cond -> Bool
-- 動作不執行副作用,只「翻譯」成指令,由外層機器收集:
runAction :: Action -> [ScriptEffect]   -- ScriptEffect ⊂ WorldCommand ∪ 內部效果
```

### 1.3 共用條件/動作詞彙(v1 範圍)

| 條件 | 意義 |
|---|---|
| `(flag NAME)` / `(not …)` / `(and …)` / `(or …)` | 全域旗標邏輯 |
| `(has-item ID N)` | 背包內有 N 個道具 |
| `(quest-state QID active/done/available)` | 任務狀態 |
| `(player-near X Y R)` | 玩家在半徑內(格為單位) |
| `(level-is NAME)` | 目前關卡 |

| 動作 | 意義 → 對應 WorldCommand |
|---|---|
| `(set-flag NAME)` / `(clear-flag NAME)` | 改旗標(任務機內部狀態) |
| `(give-item ID N)` / `(take-item ID N)` | `WcGiveItem` / `WcTakeItem` |
| `(say SPEAKER TEXT…)` | 開對話框(HUD 指令) |
| `(toast TEXT)` | HUD 跑馬燈提示 |
| `(offer-quest QID)` / `(advance QID)` / `(complete QID)` | 任務機內部轉換 |
| `(spawn-npc ID X Y)` / `(despawn-npc ID)` | `WcSpawnNpc` / `WcDespawnNpc` |

**驗證原則(fail loudly)**:啟動時所有腳本編譯成 AST,並交叉檢查——引用的 ItemId 必須存在於 items.def、QuestId/NpcId 必須存在、關卡名必須存在。任何 dangling reference = 啟動失敗並列出檔名行號。**絕不在遊玩中才發現腳本打錯字。**

### 1.4 測試策略

`Script.Sexp` parser 與 `Script.Expr` 求值器是純函式:parser 往返測試(parse→print→parse)、求值器以手工 `ScriptEnv` 表格驅動。這層測穩了,任務/NPC 的 bug 面就只剩 schema 轉換。

---

## 2. 道具資料庫與背包系統(調整)

### 2.1 從 sum type 到 registry(架構原則的演進)

現有 `ItemType`(2 個建構子)在內容規模化後不可維護——每加一個藥水都重編譯,違反「內容迭代不碰編譯器」。**v1 起道具改為資料驅動**:

```
;; assets/items/items.def
(item gold-key     (name "金鑰")   (category quest)              (desc "開啟深淵之門"))
(item potion-hp-s  (name "小紅藥") (category potion)  (stack 9)  (use (heal 30)))
(item sword-rusty  (name "鏽劍")   (category equip weapon) (stats (atk 5)))
(item boots-swift  (name "疾風靴") (category equip shoes)  (stats (spd 15) (def 1)))
```

```haskell
newtype ItemId = ItemId Text          -- 驗證過的 ID(啟動時對 registry 檢查)
data ItemCategory = CatGeneral | CatPotion | CatQuest | CatEquip EquipSlot
data ItemDef = ItemDef { idName, idCategory, idStackLimit, idStats, idUse, idDesc … }
newtype ItemRegistry = ItemRegistry (Map ItemId ItemDef)  -- 啟動時建立,唯讀
```

> 遷移註記:`Core.Types.ItemType` 廢除,`Backpack` 元件改為 `Backpack (Map ItemId Int)`(堆疊)。
> 「sum type 不用字串」鐵則的正確詮釋:**程式層分類**(EquipSlot、ItemCategory)仍是 sum type,編譯器把關;**內容層實例**(哪些道具存在)是資料,由啟動期驗證把關。兩道防線,各司其職。

### 2.2 背包規則

- 四個分頁 = 四個 `ItemCategory`;任務道具**不可丟棄、不可販售**(category 決定行為)。
- 藥水可堆疊(`stack` 上限)、可快捷使用(遊戲中按 `Q` 使用第一個藥水,v2 再做快捷欄)。
- `use` 效果走 Script 動作詞彙(`(heal 30)`、`(restore-mp 20)`),由 `Sim.Items` 純函式結算 → 又是可測純邏輯。
- 撿取流程不變(`Sim.Rules`),只是 `EvItemPicked` 帶 `ItemId`。

---

## 3. 玩家屬性與裝備系統(新)

### 3.1 Vitals 與衍生屬性

```haskell
data Vitals = Vitals { hp, maxHp, mp, maxMp, stamina, maxStamina :: !Double }  -- 元件
data BaseStats = BaseStats { bsAtk, bsDef, bsSpd … }                            -- Config 起始值
data DerivedStats = DerivedStats { dsAtk, dsDef, dsSpeedMult … }                -- 純計算結果
```

- **耐力**:衝刺 -20、鉤索 -15、二段跳 -10,站立每秒回 25;不足時招式不可發動(`CombatCore` 檢查,加測試)。
- **魔力**:v1 保留欄位與 UI,消耗者(法術)屬於未來戰鬥擴充。
- 計算是純函式:`computeStats :: BaseStats -> [ItemDef] -> DerivedStats`(基礎 + 全裝備加成總和),在裝備變動時重算並快取到元件。

### 3.2 裝備五部位

```haskell
data EquipSlot = SlotWeapon | SlotBody | SlotShoes | SlotGloves | SlotHead
  deriving (Eq, Ord, Enum, Bounded)          -- 固定五格,程式層 sum type
newtype Equipped = Equipped (Map EquipSlot ItemId)   -- 玩家元件
```

- 穿脫規則(純函式 `Sim.EquipCore`):只有 `CatEquip slot` 且 slot 相符可穿;穿上時舊裝備回背包;背包滿(v1 不限格數,先不處理)。
- 裝備影響:`dsSpeedMult` 乘上 `playerSpeed`、未來 `dsAtk` 進傷害管線。**CombatCore 讀 DerivedStats,不知道裝備的存在**——保持狀態機輸入是純資料。

---

## 4. 存檔/讀檔系統(新)

### 4.1 存什麼:單一 `SaveGame` record

```haskell
data SaveGame = SaveGame
  { svVersion   :: !Int              -- schema 版本,不符時明確拒載
  , svLevel     :: !Int              -- 關卡索引
  , svCheckpoint:: !(V2 Double)      -- 重生點
  , svStats     :: !RunStats
  , svVitals    :: !Vitals
  , svBackpack  :: !(Map ItemId Int)
  , svEquipped  :: !(Map EquipSlot ItemId)
  , svFlags     :: !(Set Text)       -- 腳本旗標
  , svQuests    :: !(Map QuestId QuestProgress)
  }
```

- 格式:JSON(aeson ≥ 2.3,與 GHC 9.14 相容;架構文件 §5 已註記)。三個槽位 `saves/slot{1,2,3}.json`。
- **收集是純的**:`snapshot :: FlowState -> QuestLog -> PlayerRecord -> SaveGame`;讀檔反向 `restore`,兩者互為反函數 → **roundtrip 測試**(`restore . snapshot ≡ id`)是這個系統的核心測試。
- 寫檔時機:選單「存檔」頁手動存 + 過關自動存 slot 0(autosave)。由 `FlowCommand`(`CmdSaveGame n` / `CmdLoadGame n`)觸發,殼層執行 IO。
- 讀檔 = `CmdLoadLevel svLevel` 的加強版:重建 world 後由殼層把 `svBackpack/svEquipped/svVitals` 寫回玩家、把 `svQuests/svFlags` 餵給任務機的 `restore` 入口。
- 損壞/版本不符:拒載並在選單顯示「存檔毀損」,**不得半載**。

---

## 5. 主選單系統(新)

### 5.1 模式與導航:Flow.Machine 的直接擴充

```haskell
data GameMode = … | ModeMenu !MenuPage        -- 取代單純的 ModePaused(保留舊模式為快速暫停)
data MenuPage = PageStatus | PageBackpack | PageEquip | PageQuests
              | PageMap | PageSave | PageLoad | PageSettings
data MenuCursor = MenuCursor { mcPage :: !MenuPage, mcRow :: !Int, mcCol :: !Int }
```

- `Esc` 開選單(進 `ModeMenu PageStatus`,模擬凍結——沿用 `ModePaused` 的凍結語意);`Esc` 再按關閉;`←/→`(或 Tab)切頁、`↑/↓` 移動游標、`Space` 確認。
- **導航全部是 `Flow.Machine` 的純轉換**(游標存在 `FlowState`),`FlowSpec` 直接測「在背包頁按下確認會發出 `CmdUseItem`」這類劇本。
- 選單各頁需要世界資料(背包內容、裝備、Vitals)→ **渲染時讀取**,導航機只管游標語意(「第 3 列」),渲染層把游標對映到實際道具列表。游標越界由渲染回報的列表長度夾住(每幀傳入 `FlowFrame` 的 env)。

### 5.2 各頁內容(v1)

| 頁 | 內容 | 互動 |
|---|---|---|
| 狀態 | HP/MP/耐力數值、五圍(Atk/Def/Spd)、遊玩時間、死亡數 | 唯讀 |
| 背包 | 四分頁(一般/藥水/裝備/任務),格狀清單+說明欄 | 確認=使用/裝備;任務道具不可操作 |
| 裝備 | 五部位欄+目前加成明細 | 確認=卸下回背包 |
| 任務 | 進行中/已完成清單,選中顯示目前目標 | 唯讀(v1) |
| 地圖 | Tilemap 縮圖(每格 2px)+玩家點+任務標記 | 唯讀 |
| 存檔/讀檔 | 三槽位+autosave 資訊(關卡/時間/日期) | 確認=`CmdSaveGame/CmdLoadGame` |
| 設定 | 音量(預留)、顯示 FPS、全螢幕 | 改動即寫 `config/settings.json` |

---

## 6. 七層視差渲染(新)

### 6.1 資料:`.scene` 佈景檔(同 S-expr 語法)

```
;; assets/scenes/01-training.scene(與關卡同名,可缺省=無佈景)
(scene
  (layer back-3 (parallax 0.2) (tint 24 30 48)
    (prop silhouette-hills   0  260 640 200))   ;; 名稱 x y w h(先用純色幾何,未來換貼圖)
  (layer back-1 (parallax 0.7)
    (prop pillar 300 200 40 280) (prop pillar 900 180 40 300))
  (layer front-1 (parallax 1.15)
    (prop hanging-vine 150 0 24 120))
  (layer front-3 (parallax 1.6) (alpha 180)
    (prop fog-band 0 480 2000 80)))
```

- 七層固定:`back-3/2/1` → **主層(tilemap+全部實體,現有 renderScene)** → `front-1/2/3`。
- `parallax` 係數乘上攝影機位移:`<1` 後景移得慢(遠)、`>1` 前景移得快(近)、主層恆為 1。
- `prop` v1 = 具名純色幾何(顏色表在 `Render.Props`);未來換 sprite 時**只改渲染端的名稱→貼圖對映,scene 檔完全不動**——這就是把美術位置資料先外部化的價值。
- 前景層可帶 `alpha` 做霧氣/遮擋;層可帶 `tint` 做深度氛圍。

### 6.2 實作

```haskell
data SceneLayer = SceneLayer { slDepth :: !LayerDepth, slParallax :: !Double, slProps :: [Prop] … }
loadScene :: FilePath -> IO (Either String SceneDef)   -- LevelData 加掛 ldScene
renderLayers :: Renderer -> V2 Double -> [SceneLayer] -> IO ()  -- camOffset 傳入
```

`Render.Draw.renderScene` 改為:清屏 → 畫 back 三層 → 現有主層 → 畫 front 三層。純解析部分(`Scene.Parser`)照例進測試。**效能註記**:props 依攝影機視野裁剪(同磁磚的可見範圍計算)。

---

## 7. NPC 系統(新)

### 7.1 職責切分(嚴格遵守鐵律二)

NPC 有兩個「腦」,分屬兩層,**不可混**:

- **身體(Apecs)**:`Npc` 元件(id、目前 AI 狀態)+ Position/Velocity/Collider。移動 AI 是純狀態機 `Npc.Core`(比照 CombatCore 的 In/Out 模式):巡邏、閒晃、面向玩家、站定。每個模擬子步執行,吃 `NpcDef` 的 movement 參數。
- **劇本(腳本層)**:對話、給任務、旗標反應——寫在 `.npc` 檔的 `dialogue` 規則裡,玩家按 `E`(新 Intent:`IntentInteract`)時由 `Sim.Rules` 發 `EvTalkedTo npcId`,任務機/對話機求值當下該說哪句。

```
(npc elder
  (name "長老")  (color 200 180 120)  (size 24 32)
  (movement (patrol (12 14) (20 14)) (speed 40) (pause 2.0))   ;; 或 (idle) / (wander R)
  (personality (chatter 8 "唉,腰好痠…" "年輕真好"))            ;; 閒聊氣泡,個性=參數
  (dialogue
    (rule (quest-state find-key done)  (say elder "你找回了鑰匙!") (advance find-key))
    (rule (quest-state find-key active)(say elder "鑰匙在上層平台。"))
    (default (say elder "深淵並不歡迎訪客。") (offer-quest find-key))))
```

- `dialogue` 規則**由上而下取第一個條件成立者**(決定性、可測)。
- 生成:關卡檔加 `N` 記號?否——NPC 的位置屬於腳本(`(spawn-at LEVEL X Y)` 欄位),由殼層在 `CmdLoadLevel` 後查詢「此關卡的 NPC」並生成。同一 NPC 可依旗標出現/消失(`(present-when COND)`,載入時求值)。
- 對話 UI:畫面下方對話框(HUD 機的一種狀態),對話中模擬凍結(新 `ModeDialogue`,Flow 轉換,附測試)。

### 7.2 個性(v1 的務實詮釋)

「個性」= 行為參數 + 閒聊語料,不是 AI 模擬:移動風格(巡邏/閒晃/站樁)、步速、停頓、隨機氣泡語錄與頻率。這些全是 `.npc` 檔資料,改檔即改個性。v2 若要狀態化情緒(好感度),加一個 `(mood …)` 旗標維度即可,DSL 詞彙已預留 `flag` 機制。

---

## 8. GUI/HUD(新)

### 8.1 常駐 HUD(遊戲中)

- 左上:HP(紅)/MP(藍)/耐力(黃)三條——底框+比例填充,數值變化時 0.2s 平滑過渡(渲染側插值,非模擬狀態)。
- 右上:追蹤中任務的目前目標一行(「收集 金鑰 0/1」),由 QuestLog 直接讀取。
- 下中:互動提示(靠近 NPC 顯示「E 交談」;靠近道具顯示「X 撿取」)——`Sim.Rules` 計算鄰近性寫入全域 `HudHints` 元件,渲染讀取。
- 跑馬燈 toast(撿到道具、任務更新):**`Hud.Machine` 純機器**(佇列+TTL,folded 在 Reflex,吃 GameEvent),`HudSpec` 測「三秒後提示消失」「同時多條會排隊」。

### 8.2 繪製基座

`Render.Widgets`:條(bar)、面板(panel)、格線清單(grid list)、對話框——所有選單頁與 HUD 共用的繪製原語,建立在現有 `Render.Font` 上。純色幾何風格延續。

---

## 9. 組合:模組與元件全景

### 9.1 新模組(依層)

```
第 1 層(純,必附測試):
  Script.Sexp / Script.Expr        -- DSL 基座
  Quest.Script / Quest.Runtime     -- 任務 schema + 任務機(QuestLog 轉換)
  Npc.Script / Npc.Core            -- NPC schema + 移動 AI 機
  Items.Registry / Items.Def       -- 道具資料庫載入與查詢
  Sim.EquipCore / Sim.StatsCore    -- 穿脫規則、屬性計算
  Scene.Parser                     -- 佈景檔解析
  Save.Codec                       -- SaveGame ↔ JSON(roundtrip 測試)
  Hud.Machine                      -- toast/提示佇列機
第 2 層(膠水):
  Sim.Npc(Npc.Core 的 Apecs 膠水)、Sim.Items(使用藥水)、Sim.Interact(E 鍵/鄰近)
  Render.Widgets / Render.Menu / Render.Hud / Render.Layers / Render.Props
第 3 層(殼):
  Main 擴充:WorldCommand 執行器、存讀檔 IO、啟動期資產載入與交叉驗證
```

### 9.2 新元件(Core.Components 註冊)

| 元件 | 內容 | 掛在 |
|---|---|---|
| `Vitals` | hp/mp/stamina | 玩家 |
| `Equipped` | Map EquipSlot ItemId | 玩家 |
| `DerivedCache` | DerivedStats | 玩家(裝備變動時重算) |
| `Backpack`(改) | Map ItemId Int | 玩家 |
| `Npc` | NpcId + AI 狀態 | NPC 實體 |
| `HudHints` | 鄰近互動提示 | global |

### 9.3 新事件/指令詞彙

```haskell
data GameEvent = … | EvTalkedTo !NpcId | EvZoneEntered !Text | EvItemUsed !ItemId
               | EvEquipChanged | EvVitalsChanged            -- HUD/任務機消費
data FlowCommand = … | CmdSaveGame !Int | CmdLoadGame !Int | CmdUseItem !ItemId
                 | CmdEquip !ItemId | CmdUnequip !EquipSlot
data WorldCommand = WcGiveItem !ItemId !Int | WcTakeItem !ItemId !Int
                  | WcSpawnNpc !NpcId !(V2 Double) | WcDespawnNpc !NpcId
                  | WcOpenDialogue ![DialogueLine]           -- 任務機/腳本 → 殼層 → 世界
```

`netFrame`/`netEvents` 的回傳擴充為同時攜帶 `[FlowCommand]` 與 `[WorldCommand]`;殼層新增 `runWorldCommand :: ItemRegistry -> World -> WorldCommand -> IO ()`。

### 9.4 每幀順序(擴充後,變動處粗體)

1. SDL → RawInput → `netFrame`(意圖、模式、指令、**HUD 狀態**)
2. 執行 FlowCommand(**含存讀檔**)與 **WorldCommand**
3. `ModePlaying` 時固定時步:controlPlayer(**讀 DerivedStats/耐力**)→ **Sim.Npc** → stepPhysics → tickVFX → projectiles;每幀一次:applyIntents(**+Interact/用藥水**)→ checkRules
4. drain GameEvent → `netEvents`(流程機 + **任務機 + HUD 機**同時消費)→ 再執行產出的指令
5. 渲染:**back×3** → 主層 → **front×3** → **HUD(條/追蹤/提示)** → 模式覆蓋(**選單頁/對話框**)

---

## 10. 建造順序(每步結束遊戲可玩、測試全綠)

| # | 里程碑 | 內容 | 為什麼先做它 |
|---|---|---|---|
| S1 | 道具地基 | Items.Registry + items.def + Backpack 改造 + 遷移現有兩道具 | 其後所有系統都引用 ItemId |
| S2 | 屬性+HUD | Vitals + 耐力消耗 + StatsCore + 三條 HUD + Widgets 基座 | 選單/裝備頁要顯示它 |
| S3 | 主選單 | ModeMenu + 導航機 + 狀態/背包/設定頁 | 之後每個系統交付一個頁 |
| S4 | 裝備 | EquipCore + 裝備頁 + DerivedStats 接入移動速度 | 靠 S1/S2/S3 全部就緒 |
| S5 | 存讀檔 | Save.Codec + 槽位頁 + 過關 autosave | SaveGame 需要 S1–S4 的欄位 |
| S6 | DSL 基座 | Script.Sexp/Expr + 測試 | 任務/NPC 的共同地基,獨立可測 |
| S7 | 任務系統 | Quest.Script/Runtime + 任務頁 + HUD 追蹤 + 首個任務檔 | 先於 NPC:撿取型任務不需要 NPC 即可全流程驗證 |
| S8 | NPC | Npc.Script/Core + 對話框 + E 互動 + offer-quest 接通 | 補上任務的「給予者」拼圖 |
| S9 | 視差七層 | Scene.Parser + Render.Layers + 兩關的 .scene 檔 | 純視覺,無依賴,最後打磨 |

**每個里程碑的定義完成(DoD)**:`cabal build all` 零警告、`cabal test` 全綠且新機器有 spec、遊戲從標題完整跑一輪、ARCHITECTURE.md 的擴充指南若受影響需同步更新。

## 11. 風險與明確的不做

- **DSL 範圍蔓延**是最大風險:v1 詞彙表(§1.3)即是全部,新增詞彙需先在本文件登記語意再實作。禁止在腳本裡實作迴圈/變數/算術以外的圖靈完備特性——需要複雜邏輯時,寫成新的「條件/動作」原語(Haskell 側,有測試),讓腳本保持宣告式。
- **不做(v1)**:商店/金錢、NPC 尋路(巡邏點之間直線)、任務分支樹(線性 stage)、快捷欄、鍵位自訂、多語系。
- 存檔向前相容:`svVersion` 不符直接拒載(v1);做遷移器是 v2 的事。
