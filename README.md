# 📊 台灣家庭看護移工勞動待遇與雇主滿意度政策分析專案

本專案致力於透過數據分析，深入剖析**「家庭看護移工的勞動待遇困境」**、**「雇主滿意度與痛點」**以及**「事業面移工失聯風險之跨界實證」**三大核心議題。透過整合勞動部「113年移工管理及運用調查」事業面與家庭面原始微觀主數據、移民署統計與監察院調查報告，我們使用 R 語言進行資料清洗、加權計算與分析，並設計出一系列符合**《報導者 (The Reporter)》**專業新聞風格的高品質圖表。

---

## 📂 專案資料夾結構說明 (Directory Structure)

專案目前已進行全面整理，結構非常清晰：

```
c:\R4CSS-master\報告用資料\
├── data/                            # 原始數據與問卷相關說明
│   ├── data113.csv                  # 113年移工管理及運用調查（家庭看護工主數據）
│   ├── ques113.pdf                  # 113年移工調查問卷 PDF
│   ├── 109年移工/ ~ 113年移工/      # 歷年官方調查之原始數據與統計報告
│   └── 事業面問卷/、家庭面問卷/        # 官方問卷結構與資料說明文件
│
├── scripts/                         # 數據清洗、統計分析與繪圖 R 腳本
│   ├── generate_plots.R             # 核心繪圖腳本（生成正式圖表 1 - 5：失聯與勞動條件）
│   ├── generate_plots_sat.R         # 雇主滿意度與痛點分析（生成正式圖表 6 - 7）
│   ├── generate_plots_sat2.R        # 雇主滿意度與薪資雙軸疊加（生成正式圖表 8）
│   ├── generate_plots_sat3.R        # 雇主滿意度與溝通雙軸疊加（生成正式圖表 9）
│   ├── generate_plots_sat_continuous.R # 薪資連續變數與滿意度散佈分析（生成正式圖表 10 - 12）
│   ├── thai_worker_charts.R         # 🇹🇭 泰國移工產業結構論證圖（生成圖表 A - F）
│   ├── generate_business_heatmap.R  # 🔥 事業面×家庭面大整合熱力矩陣圖腳本（NotoSans 標準）
│   ├── generate_clear_correlation_chart.R # 📈 仲介費與失聯率「債務枷鎖」關係散佈圖腳本
│   ├── generate_news_chart.R        # 新聞移工分析圖表腳本
│   ├── industry_vs_caregiver_chart.R # 產業移工 vs 家庭看護工對比圖表
│   ├── hourly_wage_chart.R          # 時薪與失聯率分析圖表
│   ├── oppression_charts.R          # 移工群體壓迫因素深度分析圖表
│   ├── missing_rate_heatmap.R       # 📊 移工失聯率熱力圖範本（NotoSans 與經典視覺格式標準）
│   ├── compare_industry_home.R      # 產業面與家庭面交叉比較輔助腳本
│   ├── explore_business_data.R      # 事業面資料探索腳本
│   ├── explore_business_data2.R     # 事業面資料進階探索腳本
│   ├── explore_biz_salary.R         # 事業面薪資探索腳本
│   ├── explore_nationality.R        # 國籍交叉分析探索腳本
│   ├── final家庭面.R                # 數據探索與早期分析草稿腳本
│   └── draft_satisfaction_salary.R  # 滿意度與薪資分析的臨時程式碼片段
│
├── plots/                           # 視覺化產出目錄 (PNG 圖檔)
│   ├── 1_trend.png                  # 圖1：歷年失聯率趨勢比較 (產業 vs 看護)
│   ├── 2_nationality.png            # 圖2：各國籍失聯率對比 (越南籍高失聯)
│   ├── 3_micro_profile.png          # 圖3：微觀特徵 (國籍 × 年齡逃跑率)
│   ├── 4_reasons.png                # 圖4：雇主認知的移工失聯主因
│   ├── 5_conditions.png             # 圖5：待遇落差對比 (製造業 vs 看護工)
│   ├── 6_satisfaction_painpoints.png # 圖6：雇主滿意度痛點啞鈴圖 (溝通與護理)
│   ├── 7_satisfaction_nationality.png # 圖7：國籍滿意度熱力圖 (菲律賓溝通最佳)
│   ├── 8_satisfaction_salary.png    # 圖8：薪資與「配合度」不滿意雙軸疊加圖
│   ├── 9_satisfaction_communication.png # 圖9：薪資與「溝通能力」不滿意雙軸疊加圖
│   ├── 10_satisfaction_salary_continuous.png # 圖10：總薪資與「配合度」散佈趨勢圖
│   ├── 11_satisfaction_comm_continuous.png # 圖11：總薪資與「華語溝通」散佈趨勢圖
│   ├── 12_satisfaction_salary_bar.png # 圖12：不滿意度 vs 平均薪水長條圖
│   ├── A_nationality_industry.png   # 圖A：各國籍移工產業結構差異 (泰國 99.6% 在產業)
│   ├── B_caregiver_nationality.png  # 圖B：家庭看護工國籍組成 (泰國僅 0.1%)
│   ├── C_caregiver_lost_rate.png    # 圖C：家庭看護工失聯率×國籍
│   ├── D_apples_oranges.png         # 圖D：產業 vs 看護結構對比概念圖
│   ├── E_leave_by_nationality.png   # 圖E：各國籍看護工放假頻率
│   ├── F_exposure_vs_lost.png       # 圖F：家庭看護占比 vs 失聯率散佈圖
│   ├── G_industry_vs_caregiver.png  # 圖G：產業移工 vs 家庭看護工全方位對比
│   ├── H_labor_law_comparison.png   # 圖H：勞基法保障範疇對比
│   ├── I_hourly_wage_vs_lost.png    # 圖I：時薪與失聯率關係
│   ├── industry_scale_runaway_heatmap.png  # 🔥 事業面×家庭面大整合熱力矩陣圖
│   ├── nationality_fee_vs_runaway.png      # 📈 仲介費與失聯率「債務枷鎖」關係圖
│   ├── news_migration_analysis.png         # 新聞移工趨勢分析圖
│   ├── oppression_regression_or.png        # 壓迫因素 Logistic 迴歸勝算比圖
│   ├── oppression_salary_gap.png           # 壓迫因素薪資落差圖
│   ├── oppression_work_hours.png           # 壓迫因素工時分析圖
│   ├── satisfaction_trend_110_113.png      # 雇主滿意度 110-113 年趨勢變化
│   └── exploratory/                 # 歷史草稿與探索性視覺化圖表備份
│
├── reports/                         # 分支專題研究與章節報告 (Rmarkdown)
│   ├── 事業面移工失聯率熱力圖分析.Rmd / .html # 🔥【重點】事業面×家庭面跨界大整合熱力矩陣實證報告
│   ├── 移工仲介費與失聯率關係分析.Rmd / .html  # 📈【重點】債務枷鎖雙重實證報告 (Pearson + Logistic)
│   ├── 103年移工仲介費用分析.Rmd / .html       # 歷年仲介費用變化分析
│   ├── 移工群體壓迫因素深度分析.Rmd / .html     # 壓迫因素迴歸與結構因果分析
│   ├── 雇主滿意度趨勢分析_110_113.Rmd         # 滿意度跨年趨勢分析
│   ├── 失聯移工專題報導.Rmd / .html  # 失聯專題深入草稿
│   ├── 家庭面分析_整理.Rmd            # 家庭面問卷指標整理
│   ├── 家庭移工問卷分析.Rmd / .html  # 移工問卷完整統計草稿
│   ├── 家庭移工困境報導.Rmd          # 專題報導故事草稿
│   ├── 移工假別深度分析.Rmd / .html  # 假別與休假深度報告
│   ├── 移工勞動待遇分析.Rmd / .html  # 薪資與福利分析報告
│   ├── 移工地區別分析.Rmd            # 六都與非六都地區差異分析報告
│   ├── 雇主行為與政策效果分析.Rmd/.html # 雇主行為對滿意度的影響分析
│   └── 休假分析發現摘要.md           # 核心數據洞察摘要文字
│
├── assignments/                     # 外部課堂作業與研究備份 (與本分析獨立)
│   └── AS07/                        # 網頁爬蟲與 104 職缺薪資分析作業
│
├── 家庭看護移工政策報告.Rmd         # 📄【核心 deliverables】主政策分析報告 Rmd
├── 家庭看護移工政策報告.html        # 🌐 主政策分析報告編譯產出 (HTML 可直開瀏覽)
├── LOG.md                           # 📝 專案開發與執行日誌
└── README.md                        # ℹ️ 本專案說明文件
```

---

## 🎯 核心 deliverable 簡介

根目錄下的 **[家庭看護移工政策報告.Rmd](file:///c:/R4CSS-master/報告用資料/家庭看護移工政策報告.Rmd)**（與編譯好的 **[家庭看護移工政策報告.html](file:///c:/R4CSS-master/報告用資料/家庭看護移工政策報告.html)**）為本專案的核心整合成果。報告結構完整，包含：
1. **宏觀背景**：長照需求（2035年將達 130 萬失能人口）與照顧人力缺口。
2. **失聯移工趨勢分析**：利用歷年累計數據探討失聯移工的暴增與國籍分布特徵。
3. **制度性根源**：比較雇主認知與移工實質面對的高額仲介費等推力。
4. **勞動條件分析**：以統計數據實證家庭看護工的薪資（月薪 2.1 萬）僅產業移工（2.9 萬）的 72%，且每月平均休假僅 1.5 天。
5. **政策建議**：包含擴大喘息服務、推動薪資銀行轉帳透明化等具體方向。

---

## 🚀 重新運行指南 (Execution Guide)

所有的 R 腳本與 RMarkdown 檔案中已完成**動態路徑修復**。不論您在 RStudio 中將工作目錄設在專案根目錄還是 `scripts/` (或 `reports/`) 目錄，皆可一鍵直接執行，無須手動調整路徑。

### 1. 重新生成全部新聞圖表
您只需在 R 環境中執行 `scripts/` 下的對應腳本，新生成的圖檔會自動更新至根目錄的 `plots/`：
* **核心失聯與勞動待遇分析圖 (圖 1 - 5)**：執行 `scripts/generate_plots.R`
* **雇主不滿意度痛點與熱力圖 (圖 6 - 7)**：執行 `scripts/generate_plots_sat.R`
* **配合度與華語溝通雙軸分析圖 (圖 8 - 9)**：執行 `scripts/generate_plots_sat2.R` 與 `scripts/generate_plots_sat3.R`
* **薪資連續變數與長條圖分析 (圖 10 - 12)**：執行 `scripts/generate_plots_sat_continuous.R`
* **泰國移工產業結構論證圖 (圖 A - F)**：執行 `scripts/thai_worker_charts.R`
* **產業 vs 看護工對比、勞基法、時薪圖 (圖 G - I)**：執行 `scripts/industry_vs_caregiver_chart.R` 與 `scripts/hourly_wage_chart.R`
* **🔥 事業面×家庭面大整合熱力矩陣圖**：執行 `scripts/generate_business_heatmap.R`
* **📈 仲介費與失聯率「債務枷鎖」關係圖**：執行 `scripts/generate_clear_correlation_chart.R`
* **壓迫因素深度分析圖（迴歸、薪資、工時）**：執行 `scripts/oppression_charts.R`

### 2. 重新編譯政策分析報告
* **主政策報告**：在 RStudio 中開啟根目錄的 `家庭看護移工政策報告.Rmd`，點擊上方工具列的 **Knit** 按鈕，即可自動讀取 `data/` 中的最新數據，並完美重新編譯出 `家庭看護移工政策報告.html`。
* **事業面跨界熱力矩陣專題**：在 RStudio 中開啟 `reports/事業面移工失聯率熱力圖分析.Rmd`，點擊 **Knit** 即可編譯。
* **仲介費與失聯率雙重實證專題**：在 RStudio 中開啟 `reports/移工仲介費與失聯率關係分析.Rmd`，點擊 **Knit** 即可編譯。

---

## 🎨 統一繪圖字型與格式規範 (Theme & Typography Standards)
為了維持未來所有產出圖表之視覺專業感與高質感，專案已引進 **Google Noto Sans TC (NotoSans)** 中文字型，並訂定以下統一格式規範：

### 1. 字型載入與註冊方式
在所有繪圖腳本起手式，必須調用 `showtext` 套件自動下載並註冊 Google 雲端字型：
```r
library(showtext)
font_add_google("Noto Sans TC", "NotoSans")
showtext_auto()
```

### 2. ggplot2 經典主題格式要求 (Theme Classic Standards)
為了實現一致且精緻的極簡風格，繪圖主題需以 `theme_classic` 為基礎，並配合以下微調：
*   **字型與大小**：基礎字型設為 `"NotoSans"`，基礎大小設為 `20`
*   **標題樣式**：加粗、大小設為 `25` (`plot.title = element_text(face = "bold", size = 25)`)
*   **圖表說明 (Caption)**：顏色調淺設為 `#aaaaaa` (`plot.caption = element_text(color = "#aaaaaa")`)
*   **圖例位置**：置於右側 (`legend.position = "right"`)
*   **無邊界軸線與刻度**：完全隱去 X/Y 軸線與刻度線，突顯圖表本身的主體感 (`axis.line = element_blank()`, `axis.ticks = element_blank()`)
*   **Y 軸標題文字方向**：ggplot2 預設將 Y 軸標題旋轉 90°，中文字會橫躺而無法正常閱讀。必須強制設為水平顯示：
    ```r
    axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = 1)
    ```
*   **Y 軸語意翻正**：凡使用「數值愈小 = 愈正面」的量尺（如 1=很滿意、5=很不滿意），繪圖前須將分數反轉，使視覺上「高 = 好」，避免讀者誤讀。有兩種做法，二選一：
    ```r
    # 方法 A：直接轉換資料欄（建議用於折線圖、散佈圖）
    mutate(score_pos = 6 - score)   # 原始 1→5、5→1；調整後 1=很不滿意、5=很滿意

    # 方法 B：反轉座標軸（建議用於長條圖，不改動原始值）
    scale_y_reverse()
    ```

標準範例程式碼已完整儲存於 [scripts/missing_rate_heatmap.R](file:///c:/R4CSS-master/%E5%A0%B1%E5%91%8A%E7%94%A8%E8%B3%87%E6%96%99/scripts/missing_rate_heatmap.R)，開發新圖表時請以此為基底進行擴充。

---

### 3. 堆疊長條圖規範 (Stacked Bar Chart Standards)

適用場景：呈現類別變數的**結構比例**（例如費用區間、職業類別、國籍分布），且需要在色塊內顯示百分比標籤時。

#### 3.1 資料前處理原則

```r
library(scales)  # 提供 percent() 函數

df <- df %>%
  mutate(
    # 原則：佔比 < 6% 的區塊不顯示標籤，避免文字擠壓
    label_text = if_else(pct < 0.06, "", percent(pct, 0.1)),
    # 原則：淺色填充區塊用黑字，深色填充區塊用白字
    #       依實際使用的 palette 調整哪些類別屬於「淺色」
    text_color = if_else(類別欄位 %in% c("最淺色類別", "次淺色類別"), "black", "white")
  )
```

**關鍵決策點**：
*   **標籤隱藏閾值**：預設 `pct < 0.06`（6%），可依圖表寬度調整（寬圖可降至 3%）。
*   **黑白字判斷**：以 `YlOrRd` 為例，前兩個分段（淺黃、橘黃）用黑字；其餘深色用白字。更換 palette 時需重新確認分段色彩的明暗。

#### 3.2 ggplot2 繪圖標準模板

```r
ggplot(df, aes(x = 分組欄位, y = pct, fill = 類別欄位)) +
  geom_col(position = "fill", width = 0.6) +
  geom_text(
    aes(label = label_text, color = text_color),
    position = position_stack(vjust = 0.5),
    size = 6.5, fontface = "bold", family = "NotoSans"
  ) +
  scale_color_identity() +                        # 必須加：讀取資料欄中的顏色字串
  scale_fill_brewer(palette = "YlOrRd") +         # 預設色系：由淺黃至深紅
  scale_y_continuous(labels = percent) +
  labs(
    title = "圖表標題",
    x = NULL, y = "結構占比 (%)", fill = "圖例標題",
    caption = "數據來源：XXX"
  ) +
  theme_classic(base_family = "NotoSans", base_size = 20) +
  theme(
    plot.title   = element_text(face = "bold", size = 25),
    plot.caption = element_text(color = "#aaaaaa"),
    legend.position = "right",
    axis.line    = element_blank(),
    axis.ticks   = element_blank()
  )
```

#### 3.3 各參數說明

| 參數 | 預設值 | 說明 |
|------|--------|------|
| `width` | `0.6` | 長條寬度；多組時可調寬至 `0.75` |
| `size`（標籤字級） | `6.5` | 對應輸出解析度 150–200 dpi；低解析輸出時可縮至 `5` |
| `fontface` | `"bold"` | 標籤加粗以提高對比可讀性 |
| `vjust` | `0.5` | 標籤垂直置中於色塊內 |
| `palette` | `"YlOrRd"` | 連續序列資料首選；名目類別資料改用 `"Set2"` 或 `"Paired"` |
| 標籤隱藏閾值 | `0.06` | 低於此比例不顯示文字標籤 |

#### 3.4 常見替換色系對照

| 資料特性 | 建議 palette | 淺色端（用黑字）的類別數 |
|----------|-------------|--------------------------|
| 連續序列（費用、薪資分級） | `YlOrRd` | 前 2 個分段 |
| 連續序列（強調差異） | `Blues` / `Greens` | 前 2 個分段 |
| 名目類別（無順序） | `Set2` / `Paired` | 依實際色彩目視判斷 |
| 雙向對比（正/負） | `RdBu`（反向） | 中間淺色分段 |

---

## 📊 數據來源與引用 (References)
* **主數據**：勞動部（2025），[113年移工管理及運用調查統計結果](https://www.mol.gov.tw/1607/1632/1633/77749/)
* **失聯數據**：內政部移民署歷年[失聯移工統計](https://www.immigration.gov.tw/5385/7344/7350/8943/)
* **政策脈絡**：中華民國監察院（2023），[影響移工失聯之結構性問題調查報告](https://www.cy.gov.tw/News_Content.aspx?n=125&s=26792)
* **長照需求**：衛生福利部（2025），長期照顧十年計畫3.0（115～124年）核定本
