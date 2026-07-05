# pureHask 架構規範 — 給未來開發者（人類與 AI）的設計文件

> 版本:0.2.0(2026-07-05 全面重構)
> 本文件是本專案的「憲法」。**在動手改任何程式之前，先讀完「三條鐵律」與你要動的那一層的章節。**
> 對應的遊戲設計願景(一小時遊戲《迴響深淵》)見 [GAME_DESIGN_REPORT.md](GAME_DESIGN_REPORT.md)。

---

## 0. 一分鐘總覽

技術棧:**SDL2**(視窗/輸入/繪圖) + **Apecs**(ECS 模擬世界) + **Reflex**(FRP 事件網路) + **hspec**(測試)。

```
                    ┌─────────────────────────────────────────┐
   SDL 鍵盤事件      │  app/Main.hs(效果殼層,只做接線與執行)   │   畫面
──────────────────► │  + 啟動期載入/驗證所有 assets/ 資料檔     │ ──────►
                    └──┬───────────▲──────────┬──────────▲────┘
                RawInput│    FrameOut│   GameEvent│  world 讀取│
                MenuEnv ▼            │           ▼           │
              ┌────────────────────────┐  ┌──────────────────────┐
              │ FRP.Network(Reflex)    │  │ Apecs World(模擬)    │
              │ ├ Input.Semantics(純) │  │ ├ Sim.Combat/Core    │
              │ └ Game.Logic(純組合)  │  │ ├ Sim.Npc(巡邏 AI)  │
              │   ├ Flow.Machine 流程  │  │ ├ Sim.Physics        │
              │   ├ Quest.Runtime 任務 │  │ ├ Sim.Rules(發事件) │
              │   └ Hud.Machine toast  │  │ └ Sim.Spawn          │
              └────────────────────────┘  └──────────────────────┘
```

每幀資料流(單向,無回路):

1. `Main` 把 SDL 事件翻譯成 `RawInput`(純資料),並打包 `MenuEnv`(選單可操作的世界快照)與 `WorldSnapshot`(腳本條件可讀的世界事實)。
2. `netFrame` 將它們打進 Reflex → 得到 `FrameOut`:玩家意圖 `Intent`、`GameMode`、選單游標、`QuestLog`、HUD toast、要執行的 `FlowCommand`。
3. `Main` 執行指令(載關卡/重生/存讀檔/用道具/穿脫裝備…),然後**只在 `ModePlaying` 時**以固定時步驅動 Apecs 模擬(玩家戰鬥 → NPC AI → 物理 → VFX)。
4. 模擬透過 `Sim.Rules.emitEvent` 把事實(死亡、達陣、撿取、交談)寫進全域 `EventQueue`;`Main` 每幀 drain 一次,經 `netEvents` 打回 Reflex——流程機轉換模式、任務機推進 QuestLog 並產出 `WorldCommand`(給道具、生成 NPC)、對話規則求值後開啟對話框。
5. 渲染:後景視差×3 → 主場景(磁磚/道具/NPC/玩家)→ 前景視差×3 → HUD(血條/任務追蹤/toast)→ 模式覆蓋(標題/選單/對話框/結算)。

---

## 1. 三條鐵律(不可協商)

### 鐵律一:純核心、效果殼層(Pure Core, Effectful Shell)

所有**決策邏輯**必須是純函式;IO、SDL、Apecs 副作用只能出現在薄薄的膠水層。本專案的純狀態機是所有玩法邏輯的家:

| 狀態機 | 模組 | 職責 | 對應膠水層 |
|---|---|---|---|
| 輸入語意 | `Input.Semantics` | 原始按鍵 → `Intent`(邊緣偵測、雙擊) | `FRP.Network` fold |
| 遊戲流程 | `Flow.Machine` | `GameMode`、選單導航、對話推進、`FlowCommand` | `Game.Logic` → `FRP.Network` fold |
| 任務 | `Quest.Runtime` | `QuestLog` 推進、腳本動作 → `WorldCommand` | 同上 |
| HUD 提示 | `Hud.Machine` | toast 佇列與壽命 | 同上 |
| 戰鬥 | `Sim.CombatCore` | `CombatState` 轉換、速度、耐力、VFX 請求 | `Sim.Combat`(Apecs 讀寫) |
| 裝備 | `Sim.EquipCore` | 穿脫規則、`DerivedStats` 計算 | `Sim.Rules` |
| NPC | `Npc.Core` | 巡邏 AI、閒聊氣泡、對話規則求值 | `Sim.Npc` / `Game.Logic` |

`Game.Logic` 是流程/任務/HUD 三機的**純組合器**(只做排序與輸出合併,規則各自住在自己的模組),由 `FRP.Network` 摺疊。

**判斷準則:如果一段邏輯值得測試,它就必須寫成純函式。** 膠水層(`Sim.Combat`、`FRP.Network`、`Main`)只准做「讀取 → 呼叫純函式 → 寫回」,不准出現任何 if/計時器/狀態判斷。看到膠水層長出邏輯就是架構在腐化。

### 鐵律二:Reflex 只有兩份工作,Apecs 只有一份

- **Reflex 擁有**:輸入語意層、遊戲邏輯層(`GameMode`、`RunStats`、`QuestLog`、HUD toast 的唯一真相)。
- **Apecs 擁有**:模擬世界的一切(位置、速度、戰鬥狀態、NPC 身體、實體)。
- **Reflex 永遠不碰 ECS world;模擬程式碼永遠看不到 Reflex 型別。** 兩者只透過純資料溝通:`Intent`(FRP→Sim)、`GameEvent`(Sim→FRP)、`FlowCommand`/`WorldCommand`(FRP→Shell→Sim)、`MenuEnv`/`WorldSnapshot`(Shell→FRP 的世界快照)。
- 想在 Reflex 裡做 NPC 移動、物理、碰撞?**禁止。** 那是模擬,屬於 Apecs(NPC 的「身體」在 `Sim.Npc`,只有對話「劇本」在邏輯層)。想在模擬裡切換遊戲模式、改任務進度?**禁止。** 發一個 `GameEvent`,讓 `Game.Logic` 裡的機器決定。

單向街:**模擬發布事實(它死了)、流程層決定後果(進入死亡畫面)、殼層執行動作(重生實體)。** 三者不可互換。

### 鐵律三:模擬只吃固定時步

物理與戰鬥只以 `Core.Config.simStep`(1/120s)前進,由 `Main` 的 accumulator 驅動。**任何模擬程式碼不得接收可變 dt。** `Intent` 是一次性的,只交給該幀的第一個子步(`Main` 的 `pending` 機制保證高幀率下不丟失);`HeldKeys` 則每個子步都有效。渲染每幀一次,與模擬頻率無關。

---

## 2. 模組地圖與依賴方向

依賴只能由下往上(上層可 import 下層,反之禁止):

```
第 0 層(純資料,零依賴):
  Core.Config      -- 所有可調參數。改手感只准改這裡。
  Core.Types       -- 共享詞彙:Direction/ItemId/EquipSlot/RawInput/Intent/
                      CombatState/Vitals/GameEvent/GameMode/Flow&WorldCommand/
                      Menu*/Quest*/WorldSnapshot…
                      ★ 禁止 import Apecs/SDL/Reflex
  Script.Sexp      -- S-expression parser(所有外部資料檔的語法)

第 1 層(純邏輯,只依賴第 0 層;每個模組都有對應 Spec):
  Input.Semantics  -- 意圖機(edge/雙擊 → Intent)
  Flow.Machine     -- 流程機(GameMode/選單導航/對話推進)
  Sim.CombatCore   -- 戰鬥機(含耐力把關)
  Sim.EquipCore    -- 穿脫規則 + DerivedStats 計算
  Sim.Items        -- 用藥效果、背包純操作
  Script.Expr      -- DSL 條件/動作 AST + 求值器(任務/NPC 共用詞彙)
  Quest.Script / Quest.Runtime -- 任務 schema + 任務機(QuestLog)
  Npc.Script / Npc.Core        -- NPC schema + 巡邏 AI 機 + 對話規則求值
  Hud.Machine      -- toast 佇列機
  Game.Logic       -- ★ 上述 流程/任務/HUD 機的「組合器」,被 FRP 摺疊
  Save.Codec       -- SaveGame ↔ JSON(roundtrip 不變量)
  Core.Settings    -- 設定檔純解析
  World.Tilemap / World.Level / World.Scene -- 地形、關卡檔、視差佈景檔

第 2 層(綁定框架的膠水,禁止決策邏輯):
  Core.Components  -- 唯一的 Apecs Component 註冊點(makeWorld)
  FRP.Network      -- 唯一碰 Reflex 的模組(fold 意圖機 + Game.Logic)
  Sim.Combat / Sim.Physics / Sim.Rules / Sim.Spawn / Sim.Npc -- Apecs 讀寫
  Render.Font / Widgets / Layers / Draw / Hud / Menu / UI    -- 繪製

第 3 層(殼):
  app/Main.hs      -- SDL 初始化、主迴圈、Flow/WorldCommand 執行、
                      啟動期資產載入與交叉驗證。禁止邏輯。
```

外部資料(改檔即改遊戲,不重編譯):`assets/levels/*.txt`(關卡)、
`assets/items/items.def`(道具)、`assets/quests/*.quest`(任務)、
`assets/npcs/*.npc`(NPC)、`assets/scenes/*.scene`(視差佈景)、
`config/settings.cfg`(設定)、`saves/*.json`(存檔,gitignore)。
**全部在啟動期交叉驗證,dangling reference = 啟動失敗。**

測試(test/,hspec,170+ 測例):每個第 1 層純模組一個 Spec。**測試永遠不 import SDL 或 Reflex 模組。**

指令:`cabal build all` 建置、`cabal test` 測試、`cabal run pureHask` 執行(工作目錄需在專案根,關卡檔以相對路徑 `assets/levels/` 尋找)。

---

## 3. 擴充指南(常見任務的標準作法)

每個作法都列出「要碰的檔案」。**如果你發現自己碰到清單以外的檔案,先停下來重讀鐵律。**

### 3.1 新增一個關卡 ★最常見

1. 在 `assets/levels/` 放一個 `NN-名字.txt`(NN 決定順序,檔名排序即關卡順序)。
2. 使用記號:`#` 實心、`P` 出生點(必要)、`G` 終點(必要)、`K`/`H` 道具、空白為空氣。
3. 完成。**不需要改任何程式、不需要重新編譯。** 啟動時 `discoverLevels` 自動發現,流程機自動把最後一關接到結局畫面。
4. 缺 `P` 或 `G` 會在載入時以明確錯誤訊息失敗(絕不靜默補預設值)。

### 3.2 新增一種道具 ★零程式碼

1. 在 `assets/items/items.def` 加一個 `(item …)` 表單:name/category/color/
   stack/use/stats/desc(格式見檔頭與 "Items.Registry" 模組文件)。
2. 想直接擺進關卡?在 `World.Level.itemMarkers` 加一個字元對應(這是唯一
   需要重編譯的情況;由任務 `give-item` 給的道具完全不用)。
3. 啟動期驗證會抓出打錯的 category/slot/效果;`RegistrySpec` 已覆蓋格式。

> 原則註記:**程式層分類**(`ItemCategory`/`EquipSlot`)是 sum type,編譯器
> 把關;**內容層實例**(哪些道具存在)是資料,啟動期驗證把關。詳見
> SYSTEMS_DESIGN.md §2.1。

### 3.3 新增一個玩家招式

1. `Core.Types`:`CombatState` 加建構子。
2. `Core.Config`:加調參常數(持續時間、速度、冷卻)。
3. `Sim.CombatCore.combatStep`:寫進入/離開轉換。若需要新按鍵,先照 3.5 加 `Intent`。
4. 若招式與地形互動(如落地觸發),物理側轉換寫在 `Sim.Physics`(參考 `StatePlunge` 落地→衝擊波)。
5. `Sim.Physics` 的 `applyGravity` 表決定該狀態是否受重力。
6. `Render.Draw` 給它顏色/形狀。
7. **先寫 `CombatCoreSpec` 測試再實作轉換**(招式是純函式,TDD 成本極低)。

### 3.4 新增敵人(尚未有敵人系統,這是規劃好的路徑)

1. `Core.Components`:加 `Enemy` 元件(種類、AI 狀態),註冊進 `makeWorld`。
2. 新模組 `Sim.EnemyCore`(純 AI 狀態機,比照 `CombatCore` 的 In/Out 模式)+ `Sim.Enemy`(Apecs 膠水,掛進 `Main` 的子步序列)。
3. `World.Level`:加敵人記號(如 `1`/`2`/`3`),`LevelData` 加 `ldEnemies` 欄位,`Sim.Spawn` 生成。
4. 傷害結算:新增 `Sim.Damage`,收集 Hitbox×Hurtbox 相交後**統一結算**(避免同幀互打的順序 bug);玩家死亡改為發 `EvPlayerDied`。
5. 遠程敵人請直接使用現成的 `Projectile` 元件與 `updateProjectiles`(它們就是為此保留的)。
6. `EnemyCoreSpec` 測 AI 轉換。

### 3.5 新增一個按鍵/意圖

1. `Core.Types`:`RawInput` 加欄位、`Intent` 加建構子(若需持續按住,`HeldKeys` 也加)。
2. `app/Main.hs` `processEvents`:綁 SDL keycode(這是唯一碰 SDL 事件的地方)。
3. `Input.Semantics.stepIntents`:產生意圖(邊緣/放開/雙擊皆在此)。
4. 消費端:戰鬥用 → `Sim.CombatCore`;世界互動 → `Sim.Rules.applyIntents`;流程用(選單)→ `Flow.Machine`。
5. `SemanticsSpec` 加測試。

### 3.6 新增遊戲模式(如商店、地圖畫面)

1. `Core.Types`:`GameMode` 加建構子;需要新的殼層動作就加 `FlowCommand`。
2. `Flow.Machine.stepFlow`:寫進入/離開轉換 + `FlowSpec` 測試。
3. `Render.UI.renderOverlay`:畫該模式的畫面。
4. `app/Main.hs`:決定該模式下模擬是否凍結(比照 `ModePaused`)。

### 3.7 新增磁磚種類(尖刺、單向平台)

1. `World.Tilemap`:`Tile` 加建構子;若影響碰撞語意,改 `isTileSolid` 或新增查詢函式(如 `isTileHazard`)。
2. `World.Level`:字元對應(`toTile`)。
3. 危險磁磚的判定寫在 `Sim.Rules`(發 `EvPlayerDied` 或未來的傷害事件),不要寫在物理裡。
4. `Render.Draw` 上色;`TilemapSpec` 加測試。

### 3.8 新增音效/音樂(規劃好的路徑)

事件驅動:訂閱的正確位置是 `Main` 中 `netEvents`/`netFrame` 回傳處——`GameEvent` 與 `GameMode` 轉換就是播放時機(死亡音、撿取音、換 BGM)。新增 `Audio.hs` 封裝 sdl2-mixer,由 `Main` 呼叫。**不要在模擬或 Reflex 裡播音效。**

### 3.9 存檔系統(規劃好的路徑)

`RunStats`/進度的唯一真相在 `Flow.Machine`——序列化 `FlowState` 即可。寫檔時機由 `FlowCommand`(如 `CmdSaveGame`)觸發,`Main` 執行。建議 aeson(注意:與 GHC 9.14 的相容性需選 2.3+)。

---

## 4. 設計理念(為什麼是這樣)

1. **為什麼三個狀態機要純?** 遊戲邏輯的 bug 幾乎都是狀態轉換 bug。純函式讓「蓄力 0.44 秒放開會怎樣」變成一行測試,而不是手動玩十分鐘。目前 55 個測試在 0.002 秒內跑完。
2. **為什麼 Reflex 不碰模擬?** ECS 與 FRP 都想擁有狀態。讓兩者共管同一份狀態(如玩家位置)會產生「誰是真相」問題與大量跨界接線。切分準則:**逐實體、空間性、每步迭代的 → Apecs;跨幀的事件組合、全域流程 → Reflex。**
3. **為什麼指令(FlowCommand)不直接在流程機裡執行?** 流程機要保持純函式才能測「死亡 0.9 秒後會下重生指令」。執行(改 IORef、建 world)是殼層的事。
4. **為什麼換關卡是整個 world 重建?** Apecs 沒有「清空世界」;逐實體刪除必然漏。World 很便宜,重建 = 零殘留 bug。
5. **為什麼關卡是外部文字檔?** 內容迭代不應碰編譯器。檔名排序即順序,讓「加關卡」成為零程式碼操作。
6. **為什麼道具是 sum type 不是字串?** 加新道具時,編譯器的 pattern 警告就是你的待辦清單;字串比對則是靜默錯誤。
7. **為什麼自製 3x5 字型?** 避免 sdl2-ttf 的原生依賴與字型檔資產,用 fillRect 拼字。幾何極簡美術方向下這就夠了。
8. **邊界情況要 fail loudly**:關卡缺標記回 `Left`,不補預設值。半錯的狀態比崩潰更貴。

## 5. 已知的保留與待辦

- `Projectile`/`updateProjectiles`/`projectileSpeed` 目前無生成者——**刻意保留**給遠程敵人(3.4)。
- 尚無:敵人、HP/傷害、檢查點、存檔、音效、手把。皆有規劃路徑(見第 3 節與 GAME_DESIGN_REPORT.md 的里程碑 M2–M5)。
- **RPG 化七大系統**(主選單、任務 DSL、存讀檔、背包/裝備、七層視差、NPC DSL、HUD)的完整設計見 [SYSTEMS_DESIGN.md](SYSTEMS_DESIGN.md);其建造順序 S1–S9 與本文件的擴充指南互補,實作時兩者皆須遵守。注意其中一項原則演進:內容層道具將由 `ItemType` sum type 遷移為啟動期驗證的 `ItemId` registry(SYSTEMS_DESIGN.md §2.1 有完整論證)。
- `cabal.project` 用 `allow-newer` 放寬 reflex 生態對 base-4.22/template-haskell-2.24 的上限(GHC 9.14);若升級 reflex 後可移除。
- 行為修正紀錄:重構時修了「衝刺無視冷卻」——`StateIdle` 的 `dashCooldown` 原本只倒數不把關,現在雙擊在冷卻中不觸發衝刺(有測試鎖定)。

## 6. 交付前檢查清單(每個 PR 必過)

- [ ] `cabal build all` 零警告(-Wall 是底線)
- [ ] `cabal test` 全綠;新邏輯有新測試
- [ ] 純模組(第 0/1 層)沒有新的 Apecs/SDL/Reflex import
- [ ] 膠水層沒有長出決策邏輯
- [ ] 新常數放在 `Core.Config`,不散落
- [ ] 跑一次遊戲:標題 → 玩 → 死一次 → 過關 → 結局,流程完整
