# assets/fonts

UI 字體資產。啟動期載入,缺檔即啟動失敗(fail loudly)。

| 檔案 | 來源 | 授權 |
|---|---|---|
| `NotoSansTC.ttf` | Noto Sans TC (Variable Font), Google Noto Fonts | SIL Open Font License 1.1 |

新增字體的步驟見 `docs/ARCHITECTURE.md` §3.10:擴充 `Core.Settings.FontChoice`
與 `Render.Font.Fonts`,並在 `app/Main.hs` 的 `loadFonts` 傳入路徑。
