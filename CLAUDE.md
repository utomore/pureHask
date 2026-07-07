# pureHask — 2D Platformer (Haskell: SDL2 + Apecs + Reflex)

**先讀 [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) 再動手。** 它定義了本專案的三條鐵律
(純核心/效果殼層、Reflex 與 Apecs 的職責邊界、固定時步)與所有常見擴充任務的標準作法
(加關卡、加道具、加招式、加敵人、加模式…)。違反鐵律的修改一律視為錯誤。

遊戲設計願景(一小時流程《迴響深淵》)見 [docs/GAME_DESIGN_REPORT.md](docs/GAME_DESIGN_REPORT.md)。
RPG 系統(主選單/任務 DSL/存讀檔/背包裝備/七層視差/NPC DSL/HUD)的設計與建造順序見
[docs/SYSTEMS_DESIGN.md](docs/SYSTEMS_DESIGN.md)——實作這些系統前必讀,里程碑 S1–S9 依序進行。

## 指令

```
cabal build all                  # 建置(必須零警告)
cabal test                       # hspec 測試(250+,必須全綠)
cabal run pureHask               # 執行(工作目錄需為專案根)
cabal run pureHask -- --validate # 無視窗驗證全部內容資產(所有語言;CI 可用)
```

內容資產在遊戲運行中**熱重載**(存檔約 1 秒生效;改壞保留舊資料印錯誤):
定義檔直接換掉;關卡/佈景變更重建當前關卡(玩家物品保留、回出生點)。
新增/刪除關卡檔需重啟。

## 快速地圖

- 純邏輯(可測試,禁 import SDL/Reflex):`Input.Semantics`、`Flow.Machine`、
  `Quest.Runtime`、`Npc.Core`、`Hud.Machine`、`Game.Logic`(組合器)、
  `Sim.CombatCore/EquipCore/Items`、`Script.Sexp/Expr`、`Save.Codec`、
  `World.Tilemap/Level/Scene`
- 膠水(禁寫決策邏輯):`FRP.Network`(唯一碰 Reflex)、`Sim.*`、`Render.*`、
  `Core.Components`(唯一的元件註冊點)、`app/Main.hs`(SDL、主迴圈、指令執行)
- **內容迭代免編譯**:關卡 `assets/levels/*.txt`、道具 `assets/items/items.def`、
  任務 `assets/quests/*.quest`、NPC `assets/npcs/*.npc`、敵人
  `assets/enemies/*.enemy`、視差佈景 `assets/scenes/*.scene`——全部啟動期
  交叉驗證,打錯字直接啟動失敗。**寫法教學:[docs/DSL_GUIDE.md](docs/DSL_GUIDE.md)。**
- 調參只改 `Core.Config`。

## 操作鍵

方向鍵移動(雙擊=衝刺)、Space 跳/確認、Z 攻擊(長按蓄力)、A 鉤索、
X 撿取、Q 用藥、E 對話、I 快捷背包、Esc 主選單(狀態/背包/裝備/任務/
地圖/存檔/讀檔/設定)。視窗可自由縮放(邏輯解析度 800x600 等比縮放);
設定頁可切換 UI 字體(PIXEL 像素字型 / CHINESE 繁中 TTF,見
`assets/fonts/` 與 ARCHITECTURE.md §3.10)。

## 慣例

- 一律附測試:三個狀態機(意圖/流程/戰鬥)與關卡解析、碰撞都是純函式,先寫測試再改轉換。
- 模擬與流程溝通只走 `GameEvent`(Sim→Flow)/`FlowCommand`(Flow→Shell)/`Intent`(Input→Sim),不得繞道。
- 環境:GHC 9.14 + cabal;`cabal.project` 以 allow-newer 放寬 reflex 生態的 base 上限;SDL2 與 SDL2_ttf 路徑設定在 `cabal.project.local`(原生庫來自 msys2 pacman:`mingw-w64-x86_64-SDL2{,_ttf}`;執行需 `C:\ghcup\msys64\mingw64\bin` 在 PATH 上)。
