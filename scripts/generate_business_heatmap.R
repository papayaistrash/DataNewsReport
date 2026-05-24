# ==============================================================================
# 📊 整合版移工失聯率熱力圖生成腳本（家庭面與事業面雙重實證）
# ==============================================================================
# 數據來源：勞動部 113 年移工管理及運用調查（事業面與家庭面原始微觀數據）
# 分析維度：大類行業別 (7類) x 事業單位與雇主規模 (6類，新增「1人」)
# 權重選用：
#   - 事業面：家數權數 (w1) - 適用於事業單位層次之比例推估
#   - 家庭面：雇主權數 (w) - 適用於家庭戶層次之比例推估
# ==============================================================================

library(tidyverse)
library(haven)
library(showtext)
library(ggplot2)
library(scales)

# 設定工作目錄
root_dir <- getwd()
if (basename(root_dir) == "scripts") {
  setwd(dirname(root_dir))
}

# 確保 plots 目錄存在
if (!dir.exists("plots")) {
  dir.create("plots")
}

# 載入並註冊 Google 雲端字型 Noto Sans TC
font_add_google("Noto Sans TC", "NotoSans")
showtext_auto()

# ── 1. 讀取數據 ──
biz_path <- "data/113年移工/事業面/data113.sav"
home_path <- "data/113年移工/家庭面/data113.sav"

if (!file.exists(biz_path) || !file.exists(home_path)) {
  stop("找不到必要的數據檔案。請確保 data/113年移工/ 下之事業面與家庭面 sav 檔存在。")
}

biz_sav <- read_sav(biz_path)
home_sav <- read_sav(home_path)

# ── 2. 整理家庭面數據 (家庭看護工) ──
home_clean <- home_sav %>%
  mutate(
    runaway = ifelse(q5 == 2, 1, 0),
    ind_name = "家庭看護工",
    scale_name = "1人"
  ) %>%
  filter(!is.na(runaway)) %>%
  group_by(ind_name, scale_name) %>%
  summarise(
    n = n(),
    w_total = sum(w, na.rm = TRUE),
    w_lost = sum(w[runaway == 1], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    rate = w_lost / w_total * 100
  )

# ── 3. 整理與分類事業面數據 (合併為 5 個製造業 + 1 個營建工程業) ──
biz_clean <- biz_sav %>%
  mutate(
    runaway = ifelse(q4 == 2, 1, 0),
    # 將 26 個子行業分類為 5 大類製造業 + 1 個營建工程業
    ind_name = case_when(
      industry %in% c(18, 19, 20) ~ "電子資訊與電力工業",
      industry %in% c(16, 17, 21, 22, 23) ~ "金屬與機械製造業",
      industry %in% c(9, 10, 11, 12, 13, 14, 15) ~ "化學與材料工業",
      industry == 26 ~ "營建工程業",
      industry %in% c(1, 2, 3, 4, 5, 6, 7, 8, 24, 25) ~ "民生與其他製造業",
      TRUE ~ "民生與其他製造業"
    ),
    scale_name = as.character(as_factor(scale))
  ) %>%
  filter(!is.na(runaway), !is.na(ind_name), !is.na(scale_name)) %>%
  group_by(ind_name, scale_name) %>%
  summarise(
    n = n(),
    w_total = sum(w1, na.rm = TRUE),
    w_lost = sum(w1[runaway == 1], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    rate = w_lost / w_total * 100
  )

# ── 4. 合併兩者並完成矩陣補白 (Make Complete Grid) ──
combined_data <- bind_rows(home_clean, biz_clean) %>%
  mutate(
    scale_name = factor(scale_name, levels = c("1人", "1～29人", "30～99人", "100～199人", "200～499人", "500人及以上"))
  ) %>%
  # 完成矩陣補齊，將沒有調查數據的儲存格補為 NA
  complete(ind_name, scale_name)

# 計算各類別的加權總體失聯經驗率，用以排序 Y 軸
overall_rates <- combined_data %>%
  filter(!is.na(rate)) %>%
  group_by(ind_name) %>%
  summarise(
    overall_rate = sum(w_lost, na.rm = TRUE) / sum(w_total, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  arrange(overall_rate)

# 將整體失聯率合併回去，設定排序因子
heatmap_data <- combined_data %>%
  left_join(overall_rates, by = "ind_name") %>%
  mutate(
    ind_name = factor(ind_name, levels = overall_rates$ind_name),
    # 準備儲存格標籤
    label_text = case_when(
      is.na(rate) ~ "－\n(不適用)",
      TRUE ~ paste0(round(rate, 1), "%\n(N=", n, ")")
    ),
    # 準備儲存格文字顏色
    text_color = case_when(
      is.na(rate) ~ "#BBBBBB",
      rate > 55 ~ "white",
      TRUE ~ "#1F3145"
    )
  )

# ── 5. 繪製熱力圖 ──
p <- ggplot(heatmap_data, aes(x = scale_name, y = ind_name, fill = rate)) +
  # 設定 geom_tile() 的外邊框與空值填充顏色
  geom_tile(color = "white", linewidth = 1.2) +
  geom_text(aes(label = label_text, color = text_color), fontface = "bold", size = 3.6, lineheight = 0.95) +
  scale_color_identity() +
  # 設定空值為極淺灰，有效值為暖橘紅漸層
  scale_fill_gradientn(
    colors = c("#FFF9F2", "#FEE8D6", "#FDBB84", "#FC8D59", "#EF6548", "#D7301F", "#990000", "#7F0000"),
    values = rescale(c(0, 5, 20, 45, 65, 80, 92, 100)),
    labels = function(x) paste0(x, "%"),
    limits = c(0, 100),
    na.value = "#FAFAFA" # 空值顯示極淺灰，代表不適用
  ) +
  theme_classic(base_family = "NotoSans", base_size = 20) +
  labs(
    title = "臺灣外籍移工失聯風險雙重維度整合熱力矩陣圖",
    subtitle = "各宏觀行業與僱用規模下「雇主/事業單位過去六年曾發生移工失聯」之加權比例 (%)",
    x = "雇主與事業單位之移工僱用規模（員工人數）",
    y = "宏觀行業類別（依總體加權失聯經驗率由低至高排序）",
    fill = "加權失聯經驗率",
    caption = "數據來源：勞動部 113 年移工管理及運用調查（事業面微觀 N=4,538, 家庭面微觀 N=4,016 原始加權數據）\n製圖說明：\n1. 本圖事業面採用家數權數 (w1)、家庭面採用雇主權數 (w) 進行全國推估。\n2. N 代表該儲存格之實際調查有效樣本數；「－(不適用)」代表我國現行制度與調查中無此僱用規模之配對格。"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 25, color = "#1F3145", hjust = 0, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 13, color = "#555555", hjust = 0, margin = margin(b = 20)),
    plot.caption = element_text(color = "#aaaaaa", lineheight = 1.15, margin = margin(t = 15)),
    axis.title.x = element_text(face = "bold", size = 14, color = "#2c3e50", margin = margin(t = 12)),
    axis.title.y = element_text(face = "bold", size = 14, color = "#2c3e50", margin = margin(r = 12)),
    axis.text.x = element_text(face = "bold", size = 11, color = "#2c3e50"),
    axis.text.y = element_text(face = "bold", size = 11, color = "#2c3e50"),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 12, color = "#1F3145"),
    legend.text = element_text(size = 11, color = "#2c3e50"),
    legend.key.height = unit(1.6, "cm"),
    legend.key.width = unit(0.35, "cm"),
    panel.grid = element_blank(),
    plot.margin = margin(t = 20, r = 15, b = 20, l = 15)
  )

# 儲存高解析度圖表
ggsave(
  filename = "plots/industry_scale_runaway_heatmap.png",
  plot = p,
  width = 11,
  height = 7,
  dpi = 300,
  device = "png"
)

cat("成功生成並儲存整合版熱力圖至 plots/industry_scale_runaway_heatmap.png\n")
