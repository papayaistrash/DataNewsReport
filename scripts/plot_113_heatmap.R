# ============================================================================
# 📊 薪資分組 vs 各項滿意度 熱力圖 (113年)
# ============================================================================
library(tidyverse)
library(haven)
library(scales)
library(showtext)

font_add_google("Noto Sans TC", "NotoSans")
showtext_auto()

base_dir <- "."
if (basename(getwd()) == "scripts") { base_dir <- ".." }
dir.create(file.path(base_dir, "plots"), showWarnings = FALSE)

# 讀取 113 年資料
d113 <- read_dta(file.path(base_dir, "data/113年移工/家庭面/data113.dta"))

# 清理薪資與滿意度變數
heatmap_data <- d113 %>%
  mutate(
    salary = as.numeric(nq10a),
    w = as.numeric(w),
    # 將薪資分組
    salary_group = case_when(
      salary < 20000 ~ "1. 2萬以下",
      salary >= 20000 & salary < 22000 ~ "2. 2萬~2.2萬",
      salary >= 22000 & salary < 25000 ~ "3. 2.2萬~2.5萬",
      salary >= 25000 & salary <= 50000 ~ "4. 2.5萬以上",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(salary_group), !is.na(w)) %>%
  # 挑選並翻正滿意度指標 (原本 1=很滿意~5=很不滿意，翻正為 5=很滿意)
  mutate(
    sat_技術 = 6 - as.numeric(q20_1),
    sat_態度 = 6 - as.numeric(q20_2),
    sat_情緒 = 6 - as.numeric(q20_3),
    sat_關係 = 6 - as.numeric(q20_5),
    sat_衛生 = 6 - as.numeric(q20_6),
    sat_整體 = 6 - as.numeric(q20_7)
  ) %>%
  # 轉換成長格式方便繪圖
  select(salary_group, w, starts_with("sat_")) %>%
  pivot_longer(cols = starts_with("sat_"), names_to = "indicator", values_to = "score") %>%
  filter(!is.na(score)) %>%
  group_by(salary_group, indicator) %>%
  summarise(
    mean_score = weighted.mean(score, w, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    # 清理指標名稱
    indicator_label = str_replace(indicator, "sat_", "滿意度：")
  )

# 繪製熱力圖
p <- ggplot(heatmap_data, aes(x = salary_group, y = indicator_label, fill = mean_score)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%.2f", mean_score)), 
            color = ifelse(heatmap_data$mean_score > mean(heatmap_data$mean_score), "white", "black"),
            size = 6, family = "NotoSans", fontface = "bold") +
  # 使用漸層色，越滿意越偏藍，較低則偏橘黃
  scale_fill_gradientn(colors = c("#FFF5EB", "#FDB863", "#4393C3", "#2166AC"),
                       name = "翻正分數\n(滿分5分)") +
  scale_y_discrete(limits = rev(c("滿意度：技術", "滿意度：態度", "滿意度：情緒", 
                                  "滿意度：衛生", "滿意度：關係", "滿意度：整體"))) +
  scale_x_discrete(labels = function(x) str_remove(x, "^\\d+\\. ")) +
  labs(
    title = "薪資愈高，雇主愈滿意嗎？",
    subtitle = "各項表現滿意度翻正分數（5=很滿意，1=很不滿意），依雇主給付月薪分組",
    x = "雇主給付月薪 (不扣除健保等費用)",
    y = NULL,
    caption = "資料來源：勞動部 113 年移工管理及運用調查（家庭面）"
  ) +
  theme_minimal(base_family = "NotoSans", base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 25, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 14, color = "#555555", margin = margin(b = 15)),
    plot.caption = element_text(color = "#aaaaaa", margin = margin(t = 15)),
    axis.text.x = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 15)),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 14)
  )

output_path <- file.path(base_dir, "plots/P_113_salary_sat_heatmap.png")
ggsave(output_path, plot = p, width = 12, height = 8, dpi = 150)
cat(sprintf("✅ 已輸出圖表至：%s\n", output_path))
