# pureHask 專案報告：現況結構分析 與 一小時完整遊戲設計方向

> 撰寫日期:2026-07-05
> 對象:pureHask — Haskell 2D 平台動作遊戲原型(SDL2 + Apecs + Reflex)

---

## 第一部分:目前專案整體結構

### 1.1 技術棧

| 層 | 套件 | 用途 |
|---|---|---|
| 視窗 / 輸入 / 繪圖 | `sdl2` | 視窗、鍵盤事件、硬體加速 renderer(VSync) |
| ECS 遊戲狀態 | `apecs` | 所有遊戲實體與元件(TemplateHaskell 生成 World) |
| FRP 事件網路 | `reflex` (Spider host) | 目前僅將 InputState 以 Behavior 掛在 tick 事件上 |
| 數學 | `linear` | `V2 Double` 向量 |
| 其他 | `vector`, `containers`, `dependent-sum` | Tilemap 儲存、Reflex host 所需 |

### 1.2 模組地圖(src/,共 6 個模組、約 1,200 行)

```
Main.hs    (165 行)  SDL 初始化、事件輪詢、遊戲主迴圈、實體生成
Types.hs   (202 行)  全部 ECS 元件定義 + makeWorld + 遊戲常數(調參中心)
Map.hs     (171 行)  ASCII 關卡定義、Tilemap 解析、AABB 對磁磚碰撞解算
Physics.hs (394 行)  InputState、玩家操控(戰鬥狀態機)、物理步進、投射物、勝敗判定
Render.hs  (302 行)  攝影機、磁磚/實體/VFX/鉤索/背包 UI 繪製(純色矩形美術)
FRP.hs     ( 55 行)  Reflex Spider host 網路:input Behavior + tick Event
```

### 1.3 每幀資料流

```
SDL.pollEvents
   → processEvents (Main.hs) 產生 InputState(含上一幀按鍵,供 edge detection)
   → fireInput / fireTick (FRP.hs, Reflex 把 InputState attach 到 dt)
   → runSystem (Apecs):
        controlPlayer     -- 輸入 → 戰鬥狀態機轉移、速度決定
        stepPhysics       -- 重力、鉤索飛行、依狀態解算碰撞
        tickVFX           -- 特效生命週期
        updateProjectiles -- 投射物移動與撞牆銷毀
        checkWinLoss      -- 落死重生 / 抵達終點
        renderGame        -- 攝影機 + 全部繪製 + present
   → 遞迴呼叫 gameLoop
```

### 1.4 已實作的玩法功能

- **移動系統**:跑、跳、二段跳(含殘影 VFX)、方向朝向指示
- **戰鬥狀態機**(`CombatState`,8 個狀態):
  - 雙擊方向鍵 → 衝刺(Dash,含冷卻與殘影)
  - Z 短按 → 近戰斬擊;長按蓄力(≥0.45s)→ 突刺(Thrust)
  - 空中按 Z → 下墜猛擊(Plunge,落地產生衝擊波 VFX)
  - A → 45° 鉤索(飛行/錨定/拉取/懸掛四階段,懸掛可跳躍脫離)
- **道具系統**:地圖擺放 Gold Key / Healing Potion,X 撿取進背包,I 開背包 UI
- **關卡**:ASCII 硬編碼地圖(64×19 格),P/G/K/H 記號解析出生點、終點、道具
- **物理**:AABB 對磁磚、X/Y 分軸解算 + 貼齊、terminal velocity、dt 上限 0.1s
- **攝影機**:跟隨玩家、地圖邊界 clamp、只繪可見磁磚

### 1.5 架構優點

1. **元件切割乾淨**:Position/Velocity/Collider/Gravity 等正交元件,Player/Goal 用 `Unique` storage,符合 Apecs 慣例。
2. **調參集中**:所有手感常數(重力、衝刺速度、蓄力閾值……)集中在 Types.hs,迭代手感很方便。
3. **狀態機明確**:`CombatState` 是 sum type,每個狀態的轉移條件在 `controlPlayer` 一處收斂,不會有布林旗標地獄。
4. **碰撞解算正確**:分軸解算 + 邊界貼齊(±0.001 epsilon)是平台遊戲標準做法。
5. **無美術依賴**:純色矩形 + VFX 已有辨識度,solo 開發可以走「幾何極簡美術」路線到底(參考 Thomas Was Alone / N++)。

### 1.6 目前的結構性問題(未來擴充前建議先處理)

| # | 問題 | 說明 | 建議 |
|---|---|---|---|
| 1 | **Reflex 層形同虛設** | FRP.hs 只做「hold input、attach 到 tick」,等價於直接把 `(InputState, dt)` 傳進去。付出 Spider host 的複雜度卻沒得到 FRP 的好處。 | 二選一:(a) 移除 Reflex,簡化為純 game loop;(b) 真正把狀態機/計時器/連段偵測搬進 Reflex 事件網路。建議 (a),ECS 已足夠。 |
| 2 | **Physics.hs 職責過重** | InputState 定義、輸入 edge detection、戰鬥狀態機、物理步進、投射物、勝敗判定全在一個模組,`controlPlayer` 一個函式近 190 行。 | 拆為 `Input.hs`、`Combat.hs`(狀態機)、`Physics.hs`(純物理)、`Rules.hs`(勝敗)。 |
| 3 | **沒有遊戲流程狀態** | `checkWinLoss` 抵達終點只是把玩家傳回出生點,沒有標題畫面、死亡畫面、通關畫面。 | 加入頂層 `GameMode` 狀態機(見第二部分 5.2)。 |
| 4 | **關卡硬編碼在原始碼** | 換關 = 改 Haskell 重編譯。 | 地圖移到外部檔案(`assets/levels/*.txt`),啟動時讀入;之後可升級 Tiled JSON(專案已 vendor aeson-2.2.4.0 可直接用)。 |
| 5 | **道具是裸字串** | `Item String`、渲染與撿取邏輯到處比對 `"Gold Key"`。 | 改為 `data ItemType = GoldKey | HealingPotion | ...` sum type。 |
| 6 | **可變時步** | 物理直接吃 frame dt,幀率波動會影響手感與穿牆風險(dash 800px/s 時 0.1s 上限一幀可位移 80px > 2.5 格磁磚)。 | 固定時步(accumulator,例如 1/120s),渲染插值可後補。 |
| 7 | **投射物無來源** | `updateProjectiles`、`Projectile`、`projectileSpeed` 都在,但沒有任何程式生成投射物 — 是半途的死碼。 | 設計定案後決定保留(給遠程敵人用)或刪除。 |
| 8 | **螢幕尺寸魔法數字** | 800×600 在 Main.hs 與 Render.hs 各寫一次。 | 提到 Types.hs 常數。 |
| 9 | **repo 內有 vendored 依賴** | 根目錄有完整 `aeson-2.2.4.0/` 與 `dist-newstyle/src/*`,污染版本庫。 | 確認 cabal.project 的用途後,將建置產物加入 .gitignore。 |

---

## 第二部分:設計方向 — 一小時完整遊戲

### 2.0 一句話目標

> **《迴響深淵》(Echo Depths,暫名):一款約 60 分鐘、能力鎖(ability-gated)的極簡幾何風動作平台遊戲 —— 玩家墜入深淵,沿途尋回六種失落的能力,擊敗深淵之心後逃出。**

選這個方向的理由:

1. **現有原型 90% 的機制直接變成「可解鎖能力」**:二段跳、衝刺、蓄力突刺、下墜猛擊、鉤索 —— 全部已經寫好了。設計工作是「把它們排進節奏」而不是「從零做新系統」。
2. **能力鎖天然撐起 1 小時**:每個能力 = 一個教學區 + 一個考驗區 + 一次回頭開新路,5~6 個能力就是 50~70 分鐘的骨架,業界驗證過的公式(Metroid / Hollow Knight 的微縮版)。
3. **幾何極簡美術可以走到底**:不需要 sprite、不需要動畫師,顏色+形狀+VFX 就是美術語言,solo Haskell 開發最現實的路線。

### 2.1 遊戲流程總覽(玩家 60 分鐘體驗)

```
標題畫面
  └─ 第 1 章「墜落」   (~8 min)  教學:跑、跳 | 解鎖:二段跳
  └─ 第 2 章「迴廊」   (~12 min) 敵人登場、近戰教學 | 解鎖:衝刺
  └─ 第 3 章「豎井」   (~12 min) 垂直攀升關 | 解鎖:鉤索
  └─ 第 4 章「熔窟」   (~10 min) 蓄力突刺解謎(撞碎牆) | 解鎖:下墜猛擊
  └─ 第 5 章「回音道」 (~8 min)  全能力綜合考驗 + 三把鑰匙收集
  └─ 第 6 章「深淵之心」(~8 min)  Boss 戰(三階段)
  └─ 結局:逃出動畫 + 通關統計(時間/死亡數/收集率)
```

- 每章 = 一張獨立地圖檔,章與章之間單向門(進入下一章後可從存檔點續玩,不做完整 backtracking,控制範圍)。
- 每章內 2~3 個檢查點(checkpoint),死亡回到最近檢查點,不清進度。
- 選收集品:每章藏 3 個「迴響碎片」,全收有隱藏結局文字 —— 給重玩價值,成本極低(就是會發光的矩形)。

### 2.2 外部系統(玩家看得到的)

#### A. 移動與戰鬥系統(現有,轉為可解鎖)
- 初始能力:只有 跑 + 單跳。
- 解鎖順序:**二段跳 → 衝刺 → 鉤索 → 下墜猛擊**(近戰 Z 與蓄力突刺第 2 章一起給)。
- 實作:玩家掛 `Abilities` 元件(record of Bool),`controlPlayer` 每個分支前檢查對應旗標。改動極小。
- 新增手感必備項:**土狼時間(coyote time,~0.1s)** 與 **跳躍緩衝(jump buffer,~0.12s)** —— 平台遊戲手感的兩個最高性價比投資。

#### B. 敵人與傷害系統(全新,最大的一塊)
- 玩家:HP 5 點(心形格 UI),受擊 → 擊退 + 0.8s 無敵閃爍。
- 敵人三種就夠一小時:
  1. **爬行者**:地面來回巡邏,碰到掉頭(1 HP,教學沙包)
  2. **飛行者**:定點漂浮,玩家進範圍後緩慢追蹤(2 HP,逼玩家用空中攻擊)
  3. **射手**:定點,朝玩家發射投射物(2 HP,**回收現成的 Projectile 系統**)
- 攻擊判定:近戰/突刺/下墜產生短命 `Hitbox` 實體(現有 VFX 生成模式直接複用),與敵人 `Hurtbox` AABB 相交即造成傷害。衝刺帶 i-frame。
- Boss:大型幾何體,三階段(地面衝撞 → 天降彈幕 → 狂暴連段),每階段用一個 sum type 狀態機 —— 與 `CombatState` 同構,寫法已驗證。

#### C. 關卡與進度系統
- 地圖:外部 ASCII 檔(每章一檔),新增記號:`C`=檢查點、`D`=鎖門、`E1/E2/E3`=敵人、`A`=能力解鎖點、`F`=碎片、尖刺 `^`(碰到=1 傷害+彈回)。
- 鎖門與鑰匙:沿用現有 Item/Backpack,第 5 章三把鑰匙開最終門 —— 背包系統從裝飾變成玩法。
- 檢查點:碰觸點亮,死亡重生於此;同時觸發自動存檔。

#### D. UI / HUD 系統
- 常駐 HUD:左上 HP 心形格、右上已解鎖能力圖示、蓄力時玩家腳下進度環(現有蓄力變色升級版)。
- 畫面:標題(開始/繼續/離開)、暫停選單(Esc)、死亡轉場(暗屏 0.5s)、章節標題卡、通關統計畫面。
- 文字渲染:引入 `sdl2-ttf`;或先用「點陣矩形拼字」過渡(零依賴)。

#### E. 回饋與氛圍系統(Juice)
- 音訊:`sdl2-mixer`,每章一首 loop BGM + 10 個左右音效(跳、揮、命中、受傷、撿取、開門)。免費素材(freesound / OpenGameArt)即可。
- 畫面回饋:受擊螢幕震動(camera offset 抖動 0.15s)、命中頓幀(hitstop 0.05s)、每章不同背景色調(深藍→暗紫→熔橙→…)標示進度。
- 現有 VFX 系統直接沿用,只加種類。

### 2.3 內部系統(引擎層)

#### 1. 頂層遊戲模式狀態機(最優先)
```haskell
data GameMode
  = ModeTitle
  | ModePlaying   { currentLevel :: Int }
  | ModePaused
  | ModeDead      { respawnTimer :: Double }
  | ModeLevelDone { nextLevel :: Int }
  | ModeEnding    { stats :: RunStats }
```
存為 Apecs `global` 元件;`gameLoop` 依模式分派 update/render。這是「原型 → 遊戲」的分水嶺。

#### 2. 模組重構(配合上述拆分)
```
src/
  Main.hs        -- 只剩初始化 + 主迴圈
  Types.hs       -- 元件 + 常數(可再拆 Config.hs)
  Input.hs       -- InputState + edge detection(自 Physics.hs 抽出)
  Combat.hs      -- CombatState 狀態機(自 Physics.hs 抽出)
  Physics.hs     -- 純物理步進
  Enemy.hs       -- 敵人 AI 狀態機 + 生成
  Damage.hs      -- Hitbox/Hurtbox/HP/無敵幀 解算
  Level.hs       -- 地圖載入、關卡切換、實體生成(取代 Map.hs 的一半)
  Map.hs         -- Tilemap 資料結構 + 碰撞查詢(保留)
  Save.hs        -- 存檔(aeson JSON:目前章節/能力/碎片)
  Audio.hs       -- sdl2-mixer 封裝
  UI.hs          -- HUD + 選單繪製
  Render.hs      -- 場景繪製
  GameMode.hs    -- 頂層狀態機
```

#### 3. 固定時步物理
Accumulator 模式:渲染每幀一次,物理以固定 `1/120s` 步進 0~N 次。消除穿牆與手感飄移,是加入戰鬥判定前的必要地基(hitbox 只活 2~3 個物理步,可變 dt 會讓判定不穩定)。

#### 4. 傷害事件管線(ECS 內部)
每幀順序:`AI 決策 → 玩家輸入 → 物理 → Hitbox×Hurtbox 相交收集 DamageEvent → 統一結算(扣血/擊退/無敵/死亡) → VFX/音效 → 渲染`。
結算集中一處,避免「A 打 B 同幀 B 打 A」順序 bug。

#### 5. 資料驅動關卡管線
`assets/levels/chapter1.txt` + 同名 `.json`(章節名、BGM、背景色、敵人參數)。aeson 已在手邊。目標:**調關卡不重編譯**。

#### 6. 存檔系統
`save.json`:`{chapter, abilities, fragments, deaths, playTimeSec}`。寫入時機:檢查點、章節完成。標題畫面「繼續」讀此檔。

#### 7. Reflex 的去留(建議:移除)
現況 Reflex 只是 55 行的 pass-through。除非你想把「連段偵測/計時器」重寫成 FRP 事件網路作為學習目標,否則移除它可少一個重依賴、簡化 main loop。**若學習 FRP 本身是專案目的,則反向操作:把 double-tap 偵測與 CombatState 計時搬進 Reflex,讓它名副其實。**(這是唯一需要你拍板的方向性決定。)

### 2.4 建議里程碑(每個結束時遊戲都可玩)

| 里程碑 | 內容 | 產出 |
|---|---|---|
| **M1 地基** | GameMode 狀態機、固定時步、模組拆分、地圖外部化、移除(或坐實)Reflex | 有標題/暫停/死亡畫面的原型 |
| **M2 戰鬥** | HP/Hitbox/Hurtbox/無敵幀、敵人×3、傷害管線、coyote+jump buffer | 可以打怪、會死、有手感 |
| **M3 進度** | Abilities 解鎖、檢查點、存檔、鎖門鑰匙、章節切換 | 完整遊戲循環跑通 |
| **M4 內容** | 6 張章節地圖、碎片收集、尖刺等機關 | 60 分鐘流程可從頭玩到尾 |
| **M5 Boss+收尾** | 三階段 Boss、結局統計、音訊、螢幕震動/hitstop、UI 美化 | 可發佈的 1.0 |

依賴新增:`sdl2-ttf`(文字)、`sdl2-mixer`(音訊)、`aeson`(存檔/關卡設定,已 vendor)。

### 2.5 範圍控制原則(solo 開發護欄)

- **不做**:完整 metroidvania 大地圖回溯、裝備/商店/經濟、對話系統、動畫 sprite、手把支援(1.0 後再說)。
- 敵人固定 3 種 + 1 Boss,能力固定 6 個,章節固定 6 章 —— 內容量封頂,靠關卡編排做出變化。
- 每個里程碑結束遊戲必須「可從標題玩到當前內容盡頭」,避免長期處於不可玩狀態。
