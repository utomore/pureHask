# pureHask 內容 DSL 中文教學

> 本文件教你**不寫一行 Haskell、不重新編譯**,就能新增關卡、道具、任務、
> NPC、敵人與視差佈景。所有內容檔都在啟動時交叉驗證——打錯字遊戲會
> 拒絕啟動並印出明確錯誤,**這是特性不是缺陷**(fail loudly)。
>
> 架構背景見 [ARCHITECTURE.md](ARCHITECTURE.md);本文只講「怎麼寫檔案」。

**內容工作流三件套**:

- **熱重載**:遊戲運行中存檔即生效(約 1 秒內)。道具/任務/NPC/敵人/
  字串表直接換掉定義;**關卡與佈景**也支援——當前關卡的 world 會重建
  (背包/裝備/血量保留,人回到出生點)。改壞了?遊戲**不會當掉**——
  保留舊資料並在主控台印出錯誤,修好存檔就恢復。唯一例外:**新增/刪除
  關卡檔**需要重啟(關卡數烙進流程機與存檔)。
- **`pureHask --validate`**:不開視窗、只跑完整驗證(所有語言),
  exit code 0/1——給內容作者快速檢查,也適合掛 CI。
- **`(inherit BASE)` 繼承**:道具與敵人可繼承較早定義者,只寫差異
  (見 §3 / §6.4)。

## 目錄

0. [文本 key 與多語言 `assets/lang/*.lang`](#0-文本-key-與多語言)
1. [S-expression 基礎語法](#1-s-expression-基礎語法)
2. [關卡 `assets/levels/*.txt`](#2-關卡)
3. [道具 `assets/items/items.def`](#3-道具)
4. [任務 `assets/quests/*.quest`](#4-任務)
5. [NPC `assets/npcs/*.npc`](#5-npc)
6. [敵人與行為樹 `assets/enemies/*.enemy`](#6-敵人與行為樹)
7. [共用條件/動作詞彙表](#7-共用條件動作詞彙表)
8. [視差佈景 `assets/scenes/*.scene`](#8-視差佈景)
9. [常見錯誤與排查](#9-常見錯誤與排查)
10. [天賦樹 `assets/talents/talents.def`](#10-天賦樹)
11. [音效與音樂 `assets/audio/`](#11-音效與音樂)
12. [Sprite Sheet 動畫 `assets/sprites/` + `assets/textures/`](#12-sprite-sheet-動畫)

---

## 0. 文本 key 與多語言

所有玩家看得到的文字都住在**字串表** `assets/lang/語言.lang`
(目前有 `zh-TW.lang` 與 `en.lang`),格式是一行一條:

```
;; 註解
item.gold-key.name = 金鑰匙
npc.elder.hello.1  = 歡迎,旅行者。
```

內容檔的**顯示文字位置**寫「裸符號」就是引用 key:

```lisp
(name item.gold-key.name)          ; ← key,啟動期查表(缺 key = 啟動失敗)
(name "直接寫字面值也可以")         ; ← 字串,打原型時方便
```

規則:

- **id 用 ASCII 符號、顯示文字用 key**——翻譯只動 `.lang` 檔,內容/邏輯不動。
- 換語言:設定頁 LANGUAGE 列(重啟生效),或 `config/settings.cfg` 的
  `lang=zh-TW|en`。
- 新語言 = 複製一份 `.lang` 翻譯 + 在 `Core.Settings.Language` 加一個建構子。
- 引擎自產的字串(如任務 toast 前綴)也走表:`ui.quest.started`、
  `ui.quest.completed`,兩個 key 是**必要**的。

---

## 1. S-expression 基礎語法

除了關卡(純文字圖)之外,所有內容檔都用同一種語法:**S-expression**
(小括號表單),由 `Script.Sexp` 解析。

```lisp
;; 分號到行尾是註解
(表單名 參數1 參數2 …)      ; 一個表單 = 一對小括號
(name "顯示名稱")            ; 字串用雙引號,可以是中文
(spawn-at 03-hollow 20 13)   ; 符號(識別字)與數字不用引號
(stats (hp 30) (damage 10))  ; 表單可以巢狀
```

三種值:

| 型 | 例子 | 用途 |
|---|---|---|
| 符號 symbol | `slime`、`03-hollow`、`contact` | 識別字:id、關卡名、枚舉值 |
| 字串 string | `"史萊姆"`、`"歡迎,旅人。"` | 顯示文字(支援繁體中文,檔案存 UTF-8) |
| 數字 number | `30`、`2.0`、`-4` | 數值(座標多以「磁磚」為單位,1 磚 = 32px) |

**規則:id 一律用 ASCII 符號(如 `slime`),顯示文字才用中文字串。**
id 會被程式比對;顯示文字只給玩家看。

---

## 2. 關卡

`assets/levels/NN-名字.txt` — 純文字,一個字元一塊磁磚(32×32px)。
**檔名排序 = 關卡順序**,所以用 `01-`、`02-` 開頭。

```
################################################################
#                                                              #
#              H                 K                             #
#            #####             ######                    G     #
#   P                                                   ####   #
################################################################
```

| 字元 | 意義 |
|---|---|
| `#` | 實心磚 |
| `P` | 玩家出生點(**必要**) |
| `G` | 終點(**必要**) |
| `K` `H` `S` `B` | 道具(金鑰匙/藥水/劍/靴;對應表在 `World.Level.itemMarkers`) |
| 空白 | 空氣 |

- 缺 `P` 或 `G` → 啟動失敗並指出檔名。
- 掉出地圖底部 = 死亡,所以「坑洞」就是挖穿最下面幾列。
- 每張關卡**必須**有同名的 `.scene` 佈景檔(見第 8 節)。
- 行寬不必完全一致(會自動補空白),但建議左右都圍 `#` 牆。

---

## 3. 道具

全部道具都寫在 `assets/items/items.def`,一個 `(item …)` 一種道具。

```lisp
(item potion-hp-s
  (name "小型藥水")          ; 顯示名(可中文)
  (category potion)          ; general | potion | quest | equip SLOT
  (stack 9)                  ; 疊加上限(省略 = 1)
  (color 220 40 40)          ; 地上與圖示的顏色 R G B
  (use (heal 30))            ; 使用效果(potion 才有意義)
  (desc "回復 30 點生命"))   ; 選單裡的描述
```

**category(必填)**:

| 寫法 | 意義 |
|---|---|
| `(category general)` | 一般雜物 |
| `(category potion)` | 可用 Q 鍵或選單使用 |
| `(category quest)` | 任務道具 |
| `(category equip weapon\|body\|shoes\|gloves\|head)` | 裝備,佔一個部位 |

**use 效果**(可多個):`(heal N)`、`(restore-mp N)`、`(restore-stamina N)`。

**stats 裝備數值**(可多個):`(atk N)` 攻擊、`(def N)` 防禦、
`(spd N)` 移速百分比(15 = +15%)。`atk` 會加進玩家攻擊傷害。

**繼承**:`(inherit BASE)` 複製較早定義的道具,只寫差異;`stats` 逐項
合併,其餘欄位整個覆蓋:

```lisp
(item potion-hp-l
  (inherit potion-hp-s)
  (name "大型藥水")
  (use (heal 80)))       ; category/stack/color 全部沿用小藥水
```

想把道具直接擺進關卡,需要在 `World.Level.itemMarkers` 加一個字元對應
(唯一需要重編譯的情況);由任務 `give-item` 給的道具完全不用。

---

## 4. 任務

`assets/quests/*.quest`,一檔可放多個 `(quest …)`。

```lisp
(quest cull-slimes
  (name "清剿史萊姆")
  (auto-start)                        ; 選填:開新遊戲即啟動
  (stage cull                         ; 依序推進的階段
    (goal "擊敗 2 隻史萊姆 - Z 攻擊")  ; HUD 右上角追蹤文字
    (objective (kill slime 2))        ; 這一階段等待的目標
    (on-complete                      ; 目標達成時執行的動作
      (toast "幽谷安全多了")
      (give-item potion-hp-s 1)
      (complete-quest cull-slimes))))
```

**objective 目標種類**:

| 寫法 | 完成條件 |
|---|---|
| `(collect ITEM N)` | 撿到 N 個道具(`(collect ITEM)` = 1 個) |
| `(reach-goal)` | 抵達終點 G |
| `(talk-to NPC)` | 與該 NPC 交談 |
| `(kill ENEMY N)` | 擊敗 N 隻該種敵人(`(kill ENEMY)` = 1 隻) |

**on-complete 動作**:見[第 7 節詞彙表](#7-共用條件動作詞彙表)。
多階段任務用多個 `(stage …)`,以 `(advance-quest 自己)` 推進到下一階段,
最後一階段用 `(complete-quest 自己)` 收尾。

引用的道具 id、敵人 id 都會在啟動期驗證。

---

## 5. NPC

`assets/npcs/*.npc`,一檔一個(或多個)`(npc …)`。

```lisp
(npc miner
  (name "老礦工")                     ; 頭上名牌(可中文)
  (color 170 140 90)                  ; 身體顏色
  (size 24 32)                        ; 碰撞箱(省略 = 24 32)
  (spawn-at 03-hollow 7 13)           ; 關卡名 磚x 磚y(必要)
  (movement (patrol 5 10) (speed 30) (pause 2.0))  ; 或 (movement (idle))
  (chatter 10 "礦脈都被堵住了……" "小心腳下。")     ; 選填:每 10 秒輪播氣泡
  (dialogue                           ; E 鍵交談;規則由上而下,第一個成立的贏
    (rule (quest-state cull-slimes done)
      (say miner "谷裡清淨多了,謝謝你。"))
    (default
      (say miner "這些史萊姆吞了我的礦車!"
                 "替我清掉牠們吧。"))))
```

- `movement`:`(idle)` 站著不動;`(patrol X1 X2)` 在磚 x 座標 X1–X2 間
  來回,`(speed px/s)`、`(pause 秒)` 調步調。
- `dialogue` 的每條 `(rule 條件 動作…)` 由上而下嘗試;`(default 動作…)`
  永遠成立,放最後。條件與動作詞彙見第 7 節。
- **繼承**:`(inherit BASE)`(基底須定義在前)。`dialogue` 規則**串接**
  ——自己的規則排最前(優先),基底的墊底;自己帶 `(default …)` 時基底的
  default 讓位。`movement` 逐項合併;`spawn-at` 永不繼承;其餘欄位整個
  覆蓋。範例見 `assets/npcs/elder.npc` 的「長老的回聲」——繼承的台詞
  仍以長老的聲音說出,正合迴響主題。
- `(say 說話者 "第一句" "第二句" …)` 逐句顯示在下方對話框,Space 翻頁。
  說話者寫 NPC id,顯示時自動換成 `name`。
- NPC 個性 = 巡邏參數 + 閒聊台詞 + 對話規則,全部住在資料裡。

---

## 6. 敵人與行為樹

`assets/enemies/*.enemy`。敵人的行為是一棵**資料驅動的行為樹**——
S-expression 本身就是樹,不需要 GUI 編輯器。

### 6.1 完整寫法:`(behavior-tree …)`

```lisp
(enemy brute
  (name enemy.brute.name)
  (color 220 100 70)
  (size 34 40)
  (stats (hp 90) (damage 22) (speed 80))   ; 三項必填
  (behavior-tree
    (select                                 ; 由上而下,第一條成立的規則獲勝
      (sequence (hp-below 0.35) (flee 130)) ; 血量 < 35% → 逃跑
      (sequence (player-within 220) (chase 80)) ; 玩家靠近 → 追擊
      (patrol 2 40)))                       ; 否則巡邏(半徑磚 速度)
  (spawn-at 05-abyssgate 50 12))            ; 一行一隻,可跨關卡
```

**節點詞彙**:

| 類別 | 寫法 | 語意 |
|---|---|---|
| 複合 | `(select 子…)` | 依序嘗試,第一個成功的獲勝(優先序規則) |
| 複合 | `(sequence 子…)` | 依序全部成功才成功(條件 AND 動作) |
| 條件 | `(player-within D)` / `(player-beyond D)` | 玩家距離 ≤ / > D px |
| 條件 | `(hp-below F)` | 自身血量比例 < F(0..1) |
| 條件 | `(cooldown-ready)` | 射擊冷卻已就緒 |
| 動作 | `(chase 速)` / `(flee 速)` | 朝向/背向玩家移動 |
| 動作 | `(patrol 半徑磚 速)` | 以出生點為中心來回巡邏 |
| 動作 | `(stop)` | 停止水平移動 |
| 動作 | `(shoot 彈速 冷卻秒)` | 冷卻就緒時朝玩家射彈(總是成功) |

**求值模型(反應式)**:整棵樹**每個模擬子步重新評估**,節點沒有自己的
記憶——巡邏方向、冷卻等狀態住在黑板(`EnemyAi`)裡。條件會成功/失敗,
動作一律成功;`sequence` 中途失敗會**回滾**整段的輸出(交易語意)。

### 6.2 簡寫:`(behavior …)`(編譯成等價的樹)

三參數老寫法照舊可用,適合「普通的巡邏+追擊」敵人:

```lisp
(behavior (patrol 3) (aggro 280) (attack ranged 2.0 320))
;; 等價於:
;; (select (sequence (player-within 280)
;;                   (shoot 320 2.0)
;;                   (select (sequence (player-beyond 168) (chase 速))
;;                           (stop)))
;;         (patrol 3 速))
```

### 6.3 繼承:`(inherit BASE)`

變體敵人只寫差異。基底必須定義在**前面**(同檔較早或檔名較早),
`stats` 逐項合併,`spawn-at` **永不繼承**(擺放是每個定義自己的事),
其他欄位(color/size/behavior-tree…)整個覆蓋:

```lisp
(enemy slime-red
  (inherit slime)
  (name enemy.slime-red.name)
  (color 220 80 80)
  (stats (hp 50) (damage 14))   ; speed 沿用基底的 60
  (spawn-at 05-abyssgate 30 13))
```

### 6.4 設計慣例

- 想要新「行為形狀」→ **先組樹**(站樁砲台 = `(select (sequence (player-within 300) (shoot …)) (stop))`)。
- 想要新「能力動詞」(飛行、召喚、跳擊)→ 動程式:`Enemy.Script` 加節點 +
  `Sim.EnemyCore` 求值分支 + `EnemyCoreSpec` 測試,然後所有敵人都能用。
- 玩家的反擊:Z 揮劍、長按 Z 蓄力刺(1.6 倍)、空中 Z 下砸(1.3 倍),
  傷害 = 基礎值 + 武器 `atk`;`damage` 同時是接觸與投射物傷害。
  敵人死亡發 `EvEnemyKilled`,任務 `(kill …)` 靠它推進。

---

## 7. 共用條件/動作詞彙表

NPC 對話規則與任務 on-complete 共用同一套詞彙(`Script.Expr`)。
**DSL 刻意保持宣告式:沒有變數、迴圈、算術**;需要複雜邏輯時,
是在 Haskell 端新增一個「典型原語」,不是把腳本寫成程式。

**條件**(用在 NPC `(rule 條件 …)`):

| 寫法 | 成立時機 |
|---|---|
| `(flag 名字)` | 旗標已被 `set-flag` 設起 |
| `(has-item ITEM N)` | 背包裡至少 N 個(省 N = 1) |
| `(quest-state QUEST available\|active\|done)` | 任務處於該階段 |
| `(player-near 磚x 磚y 半徑磚)` | 玩家在該點附近 |
| `(level-is "01-training")` | 目前關卡名(注意是字串) |
| `(and c…)` `(or c…)` `(not c)` | 邏輯組合 |

**動作**(用在 `(rule …)`、`(default …)`、`(on-complete …)`):

| 寫法 | 效果 |
|---|---|
| `(say 說話者 "…" "…")` | 開對話框逐句顯示(任務裡用會變 toast) |
| `(toast "…")` | 螢幕下方跳提示 |
| `(set-flag 名)` / `(clear-flag 名)` | 設/清全域旗標 |
| `(give-item ITEM N)` / `(take-item ITEM N)` | 給/收道具 |
| `(offer-quest Q)` | 啟動一個尚未開始的任務 |
| `(advance-quest Q)` / `(complete-quest Q)` | 推進/完成任務 |
| `(spawn-npc NPC 磚x 磚y)` / `(despawn-npc NPC)` | 生成/移除 NPC |
| `(give-talent-points N)` | 給 N 點天賦點(任務獎勵常用,見 §10) |
| `(respec-talents)` | 全額退還已花費的天賦點(放在 NPC 對話 = 重洗地點) |

---

## 8. 視差佈景

每張關卡必須有 `assets/scenes/關卡名.scene`。七層渲染順序:
`back-3 → back-2 → back-1 → 主場景 → front-1 → front-2 → front-3`。

```lisp
(scene
  (layer back-3 (parallax 0.2)          ; 視差係數:越小越遠
    (prop silhouette-hills -300 340 1400 300))
  (layer back-1 (parallax 0.75)
    (prop glow 260 320 180 100))
  (layer front-3 (parallax 1.5) (alpha 140)  ; alpha:整層透明度
    (prop fog-band -400 430 3200 90)))
```

`(prop 名字 x y 寬 高)` 座標是世界像素。可用的 prop 名字(顏色/形狀
定義在 `Render.Layers`,加新形狀才需要動程式):
`silhouette-hills`、`silhouette-spire`、`pillar`、`crystal`、`glow`、
`hanging-vine`、`fog-band`、`dust-band`。

---

## 9. 常見錯誤與排查

啟動失敗時,錯誤訊息會直接指出檔案與原因。常見狀況:

| 錯誤訊息片段 | 原因 | 解法 |
|---|---|---|
| `missing required marker 'P'` | 關卡沒放出生點 | 加 `P` |
| `unknown item 'potion-hp'` | 道具 id 打錯 | 對照 items.def 的 id |
| `references unknown quest` | NPC 對話引用不存在的任務 | 檢查 quest id |
| `references unknown enemy` | 任務 `(kill …)` 的敵人 id 不存在 | 檢查 .enemy 檔 |
| `spawns in unknown level` | NPC/敵人 `spawn-at` 的關卡名打錯 | 用檔名去掉 `.txt` |
| `stats missing (hp N)` | 敵人漏了必填數值 | 補 `(stats (hp …) (damage …) (speed …))` |
| `needs at least one (spawn-at …)` | 敵人一隻都沒放 | 至少一行 spawn-at |
| `unknown text key` | 顯示文字引用了 .lang 沒有的 key | 在所有語言的 .lang 補上 |
| `unknown ui text key` | .lang 缺引擎必要 key | 補 `ui.quest.started/completed` |
| `unknown behavior-tree node` | 樹節點拼錯或參數數量不對 | 對照 §6.1 表格 |
| `inherits unknown …` | 基底不存在或定義在後面 | 基底要排前面(檔名/順序) |
| `not valid UTF-8` | 檔案編碼不對 | 以 UTF-8(無 BOM 佳)存檔 |
| `scene load failed` | 關卡缺同名 .scene | 建一個(可先抄現有的) |
| `unknown condition/action` | 詞彙拼錯或參數型別不對 | 對照第 7 節表格 |

提醒:

- **座標單位**:`spawn-at`、`player-near`、`spawn-npc` 都是**磚**
  (1 磚 = 32px);`.scene` 的 prop 是**像素**。
- 敵人與 NPC 受重力,出生點放在地面上方一兩磚即可,會自己落地。
- 中文由 TTF 字體(預設)顯示;就算在設定切回 `PIXEL (ASCII)` 像素
  字型,含非 ASCII 字元的字串也會**自動回退 TTF**,文字不會消失
  (像素字型只涵蓋 A-Z/0-9,純英文字串才用它)。

---

## 10. 天賦樹

檔案:`assets/talents/talents.def`(單一檔;檔案順序 = 選單 TALENT 頁顯示順序)。

```lisp
(talent blade-touch
  (name talent.blade-touch.name)   ; 顯示名(lang key 或字串)
  (desc talent.blade-touch.desc)   ; 選單底部說明
  (max-rank 3)                     ; 可買幾階(省略 = 1)
  (cost 1)                         ; 每階幾點(省略 = 1)
  (effect (atk 2)))                ; 每階疊加的效果,至少一項

(talent blade-edge
  (name talent.blade-edge.name)
  (requires blade-touch 2)         ; 需要「較早定義」的節點達 2 階
  (max-rank 2) (cost 2)
  (effect (atk 4) (spd 5)))
```

**效果詞彙**(打錯 = 啟動失敗):`(atk N)`、`(def N)`、`(spd N)`(移速 +N%)、
`(max-hp N)`、`(max-stamina N)`。

**規則**:

- `(requires 節點 [階數])` 只能引用**檔案中較早定義**的節點(省略階數 = 1)。
  前向引用是錯誤——這一條讓天賦圖天生無環。
- 天賦加成是屬性的**底層**,裝備疊在上面;`max-hp`/`max-stamina` 會即時
  調整血條/耐力上限。

**天賦點怎麼來**(`Core.Config` 可調):

| 來源 | 預設 |
|---|---|
| 擊殺里程碑 | 每 5 殺 +1(`talentKillsPerPoint`) |
| 過關 | 每關 +1(`talentPointsPerClear`) |
| 腳本 `(give-talent-points N)` | 任務獎勵/NPC 對話,見 §7 |

**重洗(到指定地點重新配點)**:把 `(respec-talents)` 放進某個 NPC 的對話,
那個 NPC 站的地方就是重洗點——全額退還花費的點數,再去選單重配。
現成範例 `assets/npcs/echo-shrine.npc`(關卡 03 的「回聲石碑」):

```lisp
(dialogue
  (default
    (say echo-shrine npc.echo-shrine.respec.1)
    (respec-talents)
    (toast npc.echo-shrine.toast)))
```

---

## 11. 音效與音樂

檔案:`assets/audio/audio.def` + 音檔放 `assets/audio/sfx/`、`assets/audio/music/`
(路徑寫相對於 `assets/audio/`;WAV 最穩)。**聲音是事件驅動的**:模擬發生
什麼(撿到道具、敵人死亡),就播綁定的音;遊戲邏輯完全不知道音訊存在。

```lisp
(sfx pickup (file sfx/pickup.wav) (volume 96))   ; volume 0-128,省略 = 96
(music cave (file music/cave.wav) (volume 48))

(on-event item-picked pickup)        ; 事件 → 音效
(music-for-mode title title-theme)   ; 模式 → 音樂
(music-for-mode playing overworld)
(music-for-level 03-hollow cave)     ; 玩這關時蓋過 playing 的綁定
```

**可綁的事件名**(固定詞彙,綁錯啟動失敗):
`player-died`、`goal-reached`、`item-picked`、`item-used`、`equip-changed`、
`talked-to`、`enemy-killed`、`talent-learned`、`talents-respec`。

**可綁的模式名**:`title`、`playing`、`menu`、`dialogue`、`dead`、
`level-complete`、`ending`。**沒綁的模式維持現在的音樂**——這是特性:
開選單/進對話不會重播關卡曲。音樂全部循環播放、切換帶 0.35 秒淡入。

沒有 `audio.def` = 整個遊戲靜音運行;沒有音訊裝置的機器也會自動靜音,
不影響遊戲。目前的 wav 都是合成佔位音,**直接覆蓋同名檔即可換音**(熱重載)。

---

## 12. Sprite Sheet 動畫

兩個目錄:圖放 `assets/textures/*.bmp`,定義放 `assets/sprites/*.sprite`。

```lisp
(sprite player
  (sheet player.bmp)          ; assets/textures/ 下的檔名
  (frame-size 24 24)          ; 網格一格的像素大小
  (color-key 255 0 255)       ; 選填:這個顏色視為透明(慣用洋紅)
  (anim idle (row 0) (frames 2) (fps 3))
  (anim run  (row 1) (frames 4) (fps 10)))
```

- 一個 `(anim …)` = 讀網格**同一列**、由左至右 `frames` 格、每秒 `fps` 格、循環。
  `row`/`frames`/`fps` 都可省略(0 / 1 / 8)。
- **誰用這張圖看 sprite 的名字**(命名慣例,不用改任何檔):
  `player` = 玩家、`npc-elder` = NPC elder、`enemy-slime` = 敵人 slime。
  沒有 sheet 的實體維持現在的色塊,美術可以一張一張慢慢換。
- 動畫名有 fallback:玩家會依序找 `dash`/`attack`/`charge`/`thrust`/`plunge`/
  `hook`/`jump`/`run`/`idle`——sheet 只有一列 `idle` 也能動;NPC/敵人找
  `run`→`walk`→`idle`。面向用水平鏡射,畫面朝右畫即可。
- 圖用 **BMP**(SDL 內建支援,免裝新函式庫)。用 Aseprite/GIMP/PS 匯出
  sprite sheet 後另存 BMP,或任何工具 PNG→BMP;透明用洋紅色 color-key
  最穩(32 位元 BMP 的 alpha 不一定被讀)。
- 啟動與 `--validate` 會直接讀 BMP 表頭驗證:格數超出圖的寬高、檔案不存在、
  上下顛倒(top-down)的 BMP 都會被抓出來。`.sprite` 與 `.bmp` 都支援熱重載。
