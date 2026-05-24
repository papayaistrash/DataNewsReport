# 台灣移工失聯風暴之結構性因子剖析：仲介費用與行業推拉力實證分析
# 
# 目的：分析各個不同產業的失聯率與他們給付的仲介費、來源國是否有關聯，並產出可供新聞發布的高解析度專業圖表。
# 數據來源：監察院「影響移工失聯之結構性問題調查報告」、勞動部及移民署 112-113 年統計資料整理

library(tidyverse)
library(scales)
library(patchwork)
library(showtext)
library(ggplot2)

# 動態設定工作目錄，確保不論在根目錄或 scripts 目錄下執行都能正常運作
root_dir <- getwd()
if (basename(root_dir) == "scripts") {
  setwd(dirname(root_dir))
}

# 建立 plots 目錄（若不存在）
if (!dir.exists("plots")) {
  dir.create("plots")
}

# 載入並註冊 Google 雲端中文字型 Noto Sans TC
font_add_google("Noto Sans TC", "NotoSans")
showtext_auto()

# 定義專業新聞圖表主題 (Theme News)
theme_news <- function() {
  theme_classic(base_family = "NotoSans", base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 16, color = "#1F3145", margin = margin(b = 6)),
      plot.subtitle = element_text(size = 11, color = "#555555", margin = margin(b = 12)),
      plot.caption = element_text(size = 9, color = "#aaaaaa", hjust = 1, margin = margin(t = 10)),
      axis.title = element_text(face = "bold", size = 11, color = "#2c3e50"),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.text = element_text(size = 10, color = "#333333"),
      axis.line = element_line(color = "#cccccc"),
      axis.ticks = element_line(color = "#cccccc"),
      legend.position = "right",
      legend.title = element_text(face = "bold", size = 10),
      legend.text = element_text(size = 9),
      panel.grid.major.y = element_line(color = "#eeeeee", linewidth = 0.5)
    )
}

# ==============================================================================
# 數據集 A：國籍別之仲介費用與失聯率關聯 (2023-2024年平均統計)
# ==============================================================================
df_nationality <- tibble(
  國家 = c("越南", "印尼", "泰國", "菲律賓"),
  平均仲介費 = c(170000, 110000, 95000, 70000),      # 新台幣 (NTD)
  年均失聯率 = c(6.5, 3.8, 1.8, 0.9),                # 百分比 (%)
  在台人數 = c(260000, 255000, 68000, 152000)        # 移工人數規模
)

# 國籍專屬配色系統
pal_nation <- c("越南" = "#E74C3C", "印尼" = "#F39C12", "泰國" = "#2980B9", "菲律賓" = "#2ECC71")

p1 <- ggplot(df_nationality, aes(x = 平均仲介費, y = 年均失聯率)) +
  # 繪製線性趨勢線
  geom_smooth(method = "lm", se = FALSE, color = "#7f8c8d", linetype = "dashed", size = 0.8) +
  # 繪製氣泡圖，點大小代表在台移工人數
  geom_point(aes(fill = 國家, size = 在台人數), shape = 21, color = "white", stroke = 1.2, show.legend = TRUE) +
  # 點標籤
  geom_text(aes(label = 國家), vjust = -1.5, fontface = "bold", family = "NotoSans", size = 4.5, show.legend = FALSE) +
  scale_fill_manual(values = pal_nation) +
  scale_size_continuous(
    labels = comma,
    range = c(6, 15), 
    name = "在台移工總人數"
  ) +
  scale_x_continuous(
    labels = label_comma(suffix = " 元"), 
    limits = c(50000, 200000),
    breaks = seq(60000, 180000, 30000)
  ) +
  scale_y_continuous(
    labels = percent_format(scale = 1), 
    limits = c(0, 8),
    breaks = seq(0, 8, 2)
  ) +
  labs(
    title = "一、債務枷鎖：仲介費用與失聯率之強烈正相關",
    subtitle = "越南移工支付全球最高昂仲介費（約17萬台幣），其失聯率高達 6.5% 亦居各國之首",
    x = "移工來台平均支付仲介費（新台幣元）",
    y = "移工年均失聯發生率 (%)",
    fill = "移工來源國"
  ) +
  theme_news() +
  guides(fill = guide_legend(override.aes = list(size = 5)))

# ==============================================================================
# 數據集 B：不同產業之失聯率差異 (2023-2024年平均統計)
# ==============================================================================
df_industry <- tibble(
  產業別 = c("營造業", "農業", "家事看護", "漁業", "製造業"),
  失聯率 = c(6.2, 5.0, 4.0, 3.5, 3.1)
)

# 排序產業
df_industry <- df_industry %>%
  mutate(產業別 = fct_reorder(產業別, -失聯率))

p2 <- ggplot(df_industry, aes(x = 產業別, y = 失聯率, fill = 產業別)) +
  geom_col(width = 0.55, show.legend = FALSE, fill = "#1F3145") +
  # 在長條圖上方印出數值
  geom_text(aes(label = percent(失聯率/100, 0.1)), vjust = -0.6, fontface = "bold", family = "NotoSans", size = 4.5) +
  scale_y_continuous(
    labels = percent_format(scale = 1), 
    limits = c(0, 7.5),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title = "二、行業推拉力：高風險與法外環境推高失聯率",
    subtitle = "營造業與農業高溫且多為 remote 工地，黑工市場每日現領現金形成強烈拉力",
    x = "引進移工之產業類別",
    y = "產業年均失聯發生率 (%)"
  ) +
  theme_news() +
  theme(panel.grid.major.y = element_line(color = "#eeeeee", linewidth = 0.5))

# ==============================================================================
# 組合圖表與輸出
# ==============================================================================
combined <- p1 + p2 + 
  plot_layout(widths = c(1.1, 0.9)) +
  plot_annotation(
    title = "台灣移工失聯風暴之結構性因子剖析",
    subtitle = "數據實證：高額跨國仲介債務與行業勞動待遇（黑工拉力）為推拉移工遁入地下黑工之核心機制",
    caption = "數據整理：監察院移工失聯專案調查報告、勞動部及移民署統計月報\n圖表製作：Antigravity 新聞數據小組",
    theme = theme(
      plot.title = element_text(face = "bold", size = 20, family = "NotoSans", color = "#1F3145", hjust = 0.5, margin = margin(t = 15, b = 5)),
      plot.subtitle = element_text(size = 13, family = "NotoSans", color = "#555555", hjust = 0.5, margin = margin(b = 20)),
      plot.caption = element_text(size = 9, family = "NotoSans", color = "#888888", hjust = 1, margin = margin(t = 15))
    )
  )

# 儲存為高畫質新聞圖表
ggsave(
  filename = "plots/news_migration_analysis.png",
  plot = combined,
  width = 15,
  height = 8.5,
  dpi = 300,
  device = "png"
)

cat("成功產出新聞分析圖表：plots/news_migration_analysis.png\n")
