# ============================================================================
# 📊 家庭看護移工「縣市別」失聯率 × 整體滿意度分析
#    合併 109–113 年家庭面問卷微觀數據（N ≈ 20,110）
# ============================================================================
library(tidyverse)
library(haven)
library(scales)
library(showtext)

font_add_google("Noto Sans TC", "NotoSans")
showtext_auto()

# ── 動態路徑 ──────────────────────────────────────────────────
base_dir <- "."
if (basename(getwd()) == "scripts") { base_dir <- ".." }
dir.create(file.path(base_dir, "plots"), showWarnings = FALSE)

# ── 縣市對照表（21 碼，五年一致）─────────────────────────────
city_map <- c(
  "1"  = "新北市", "2"  = "臺北市", "3"  = "桃園市",
  "4"  = "臺中市", "5"  = "臺南市", "6"  = "高雄市",
  "7"  = "宜蘭縣", "8"  = "新竹縣", "9"  = "苗栗縣",
  "10" = "彰化縣", "11" = "南投縣", "12" = "雲林縣",
  "13" = "嘉義縣", "14" = "屏東縣", "15" = "臺東縣",
  "16" = "花蓮縣", "17" = "澎湖縣", "18" = "基隆市",
  "19" = "新竹市", "20" = "嘉義市", "21" = "金門縣及連江縣"
)

six_cities <- c("新北市", "臺北市", "桃園市", "臺中市", "臺南市", "高雄市")

# ── 讀取並統一各年資料 ────────────────────────────────────────
read_year <- function(year) {
  path <- file.path(base_dir, "data",
                    paste0(year, "年移工/家庭面/data", year, ".dta"))
  d <- read_dta(path)

  # 縣市欄位：109/112 = t1, 110/111 = county, 113 = j1
  city_col <- if (year %in% c("109", "112")) "t1"
              else if (year %in% c("110", "111")) "county"
              else "j1"

  # 失聯欄位：109/110/111 = q1, 112/113 = q5
  run_col <- if (year %in% c("109", "110", "111")) "q1" else "q5"

 # 滿意度欄位：109 = q20g, 110/111/112 = q19g, 113 = q20_7
  sat_col <- if (year == "109") "q20g"
             else if (year %in% c("110", "111", "112")) "q19g"
             else "q20_7"

  d %>%
    transmute(
      year     = as.integer(year),
      city_code = as.character(.data[[city_col]]),
      city     = recode(city_code, !!!city_map),
      runaway  = as.integer(.data[[run_col]]),   # 1=無, 2=有
      sat_raw  = as.numeric(.data[[sat_col]]),    # 1=很滿意 ~ 5=很不滿意
      w        = as.numeric(w)
    )
}

d_all <- bind_rows(lapply(c("109", "110", "111", "112", "113"), read_year))
cat("合併後總列數:", nrow(d_all), "\n")

# ── 失聯率（加權）by 縣市 ────────────────────────────────────
run_city <- d_all %>%
  filter(!is.na(runaway), !is.na(w)) %>%
  group_by(city) %>%
  summarise(
    n_total   = n(),
    w_total   = sum(w),
    w_run     = sum(w * (runaway == 2)),
    run_rate  = w_run / w_total,
    .groups   = "drop"
  ) %>%
  mutate(
    is_six    = city %in% six_cities,
    city_type = ifelse(is_six, "六都", "非六都")
  )

overall_run <- sum(run_city$w_run) / sum(run_city$w_total)
cat("全國加權失聯率:", percent(overall_run, 0.1), "\n\n")

# ── 整體滿意度（加權）by 縣市 ────────────────────────────────
sat_city <- d_all %>%
  filter(!is.na(sat_raw), !is.na(w)) %>%
  group_by(city) %>%
  summarise(
    n_total   = n(),
    w_total   = sum(w),
    mean_sat  = weighted.mean(sat_raw, w, na.rm = TRUE),
    # 翻正：6 - score → 5 = 很滿意，1 = 很不滿意
    mean_sat_pos = weighted.mean(6 - sat_raw, w, na.rm = TRUE),
    # 滿意比例（回答 1 或 2）
    pct_satisfied = weighted.mean(sat_raw <= 2, w, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  mutate(
    is_six    = city %in% six_cities,
    city_type = ifelse(is_six, "六都", "非六都")
  )

overall_sat <- weighted.mean(
  d_all$sat_raw[!is.na(d_all$sat_raw)],
  d_all$w[!is.na(d_all$sat_raw)],
  na.rm = TRUE
)
cat("全國加權平均滿意度（原始 1=很滿意~5=很不滿意）:", round(overall_sat, 2), "\n")

# ============================================================================
# 🎨 繪圖 —— 圖 J：各縣市家庭看護工加權失聯率（109–113 合併）
# ============================================================================
pJ <- ggplot(
  run_city %>% mutate(city = fct_reorder(city, run_rate)),
  aes(x = city, y = run_rate, fill = city_type)
) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = overall_run, linetype = "dashed",
             color = "grey30", linewidth = 0.6) +
  geom_text(aes(label = percent(run_rate, 0.1)),
            hjust = -0.15, size = 5, family = "NotoSans") +
  annotate("text", x = 3, y = overall_run + 0.003,
           label = paste0("全國 ", percent(overall_run, 0.1)),
           color = "grey30", size = 5, family = "NotoSans") +
  scale_fill_manual(values = c("六都" = "#3566A5", "非六都" = "#DE4429")) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1),
                     expand = expansion(mult = c(0, 0.18))) +
  coord_flip() +
  labs(
    title    = "各縣市家庭看護移工加權失聯率",
    subtitle = "合併 109–113 年家庭面問卷（N ≈ 20,110），以雇主權重 w 計算",
    x        = NULL,
    y        = "加權失聯率",
    fill     = NULL,
    caption  = "資料來源：勞動部 109–113 年移工管理及運用調查（家庭面）"
  ) +
  theme_classic(base_family = "NotoSans", base_size = 20) +
  theme(
    plot.title    = element_text(face = "bold", size = 25),
    plot.subtitle = element_text(size = 14, color = "#555555", margin = margin(b = 12)),
    plot.caption  = element_text(color = "#aaaaaa"),
    legend.position = "right",
    axis.line     = element_blank(),
    axis.ticks    = element_blank(),
    axis.title.y  = element_text(angle = 0, vjust = 0.5, hjust = 1)
  )

ggsave(file.path(base_dir, "plots/J_city_runaway_rate.png"),
       plot = pJ, width = 12, height = 9, dpi = 150)
cat("✅ 已輸出：plots/J_city_runaway_rate.png\n")

# ============================================================================
# 🎨 繪圖 —— 圖 K：各縣市雇主整體滿意度（翻正分數）
# ============================================================================
overall_sat_pos <- 6 - overall_sat

pK <- ggplot(
  sat_city %>% mutate(city = fct_reorder(city, mean_sat_pos)),
  aes(x = city, y = mean_sat_pos, fill = city_type)
) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = overall_sat_pos, linetype = "dashed",
             color = "grey30", linewidth = 0.6) +
  geom_text(aes(label = round(mean_sat_pos, 2)),
            hjust = -0.15, size = 5, family = "NotoSans") +
  annotate("text", x = 3, y = overall_sat_pos + 0.03,
           label = paste0("全國 ", round(overall_sat_pos, 2)),
           color = "grey30", size = 5, family = "NotoSans") +
  scale_fill_manual(values = c("六都" = "#3566A5", "非六都" = "#DE4429")) +
  scale_y_continuous(limits = c(0, max(sat_city$mean_sat_pos) * 1.12),
                     expand = expansion(mult = c(0, 0.05))) +
  coord_flip() +
  labs(
    title    = "各縣市雇主對家庭看護移工整體表現滿意度",
    subtitle = "翻正分數（5 = 很滿意，1 = 很不滿意）｜109–113 年合併加權",
    x        = NULL,
    y        = "翻正滿意分數",
    fill     = NULL,
    caption  = "資料來源：勞動部 109–113 年移工管理及運用調查（家庭面）"
  ) +
  theme_classic(base_family = "NotoSans", base_size = 20) +
  theme(
    plot.title    = element_text(face = "bold", size = 25),
    plot.subtitle = element_text(size = 14, color = "#555555", margin = margin(b = 12)),
    plot.caption  = element_text(color = "#aaaaaa"),
    legend.position = "right",
    axis.line     = element_blank(),
    axis.ticks    = element_blank(),
    axis.title.y  = element_text(angle = 0, vjust = 0.5, hjust = 1)
  )

ggsave(file.path(base_dir, "plots/K_city_satisfaction.png"),
       plot = pK, width = 12, height = 9, dpi = 150)
cat("✅ 已輸出：plots/K_city_satisfaction.png\n")

# ============================================================================
# 🎨 繪圖 —— 圖 L：失聯率 × 滿意度散佈圖（縣市層級）
# ============================================================================
scatter_data <- run_city %>%
  select(city, run_rate, is_six, city_type, n_total) %>%
  left_join(sat_city %>% select(city, mean_sat_pos, pct_satisfied), by = "city")

cor_val <- cor(scatter_data$run_rate, scatter_data$mean_sat_pos, use = "complete.obs")

pL <- ggplot(scatter_data,
             aes(x = mean_sat_pos, y = run_rate)) +
  geom_smooth(method = "lm", se = TRUE, color = "#DE4429",
              linetype = "dashed", linewidth = 0.8, alpha = 0.15) +
  geom_point(aes(size = n_total, color = city_type), alpha = 0.8) +
  geom_text(aes(label = city), vjust = -1.2, size = 5,
            family = "NotoSans", check_overlap = FALSE) +
  scale_color_manual(values = c("六都" = "#3566A5", "非六都" = "#DE4429")) +
  scale_size_continuous(name = "樣本數", range = c(3, 12),
                        labels = comma) +
  scale_x_continuous(name = "翻正滿意分數\n（愈高＝愈滿意）") +
  scale_y_continuous(name = "加權\n失聯率",
                     labels = percent_format(accuracy = 0.1)) +
  annotate("label", x = max(scatter_data$mean_sat_pos) - 0.05,
           y = max(scatter_data$run_rate) * 0.95,
           label = paste0("r = ", round(cor_val, 3)),
           size = 6, family = "NotoSans", fontface = "bold",
           fill = "white", label.size = 0.5, color = "#DE4429") +
  labs(
    title    = "各縣市「滿意度」與「失聯率」的關係",
    subtitle = "氣泡大小 = 五年合併樣本數｜虛線 = 線性趨勢",
    color    = NULL,
    caption  = paste0("Pearson r = ", round(cor_val, 3),
                      "｜資料來源：勞動部 109–113 年移工管理及運用調查（家庭面）")
  ) +
  theme_classic(base_family = "NotoSans", base_size = 20) +
  theme(
    plot.title    = element_text(face = "bold", size = 25),
    plot.subtitle = element_text(size = 14, color = "#555555", margin = margin(b = 12)),
    plot.caption  = element_text(color = "#aaaaaa"),
    legend.position = "right",
    axis.line     = element_blank(),
    axis.ticks    = element_blank(),
    axis.title.y  = element_text(angle = 0, vjust = 0.5, hjust = 1)
  )

ggsave(file.path(base_dir, "plots/L_city_scatter_run_sat.png"),
       plot = pL, width = 13, height = 9, dpi = 150)
cat("✅ 已輸出：plots/L_city_scatter_run_sat.png\n")

# ============================================================================
# 📋 輸出統計摘要表
# ============================================================================
summary_table <- run_city %>%
  select(city, city_type, n_total, run_rate) %>%
  left_join(sat_city %>% select(city, mean_sat_pos, pct_satisfied), by = "city") %>%
  arrange(desc(run_rate))

cat("\n╔══════════════════════════════════════════════════════════════╗\n")
cat("║          各縣市家庭看護工 失聯率 × 滿意度 摘要表           ║\n")
cat("╠══════════════════════════════════════════════════════════════╣\n")
cat(sprintf("║ %-14s %-6s %6s %8s %8s %8s ║\n",
            "縣市", "類型", "樣本", "失聯率", "翻正分數", "滿意比例"))
cat("╠══════════════════════════════════════════════════════════════╣\n")
for (i in 1:nrow(summary_table)) {
  r <- summary_table[i, ]
  cat(sprintf("║ %-14s %-6s %6d %7.1f%% %8.2f %7.1f%% ║\n",
              r$city, r$city_type, r$n_total,
              r$run_rate * 100, r$mean_sat_pos, r$pct_satisfied * 100))
}
cat("╠══════════════════════════════════════════════════════════════╣\n")
cat(sprintf("║ %-14s %-6s %6d %7.1f%% %8.2f %7.1f%% ║\n",
            "全國", "—", sum(summary_table$n_total),
            overall_run * 100, overall_sat_pos,
            weighted.mean(d_all$sat_raw <= 2, d_all$w, na.rm = TRUE) * 100))
cat("╚══════════════════════════════════════════════════════════════╝\n")

cat("\nPearson 相關係數（失聯率 vs 翻正滿意分數）:", round(cor_val, 3), "\n")
cor_test <- cor.test(scatter_data$run_rate, scatter_data$mean_sat_pos)
cat("p-value:", format.pval(cor_test$p.value, digits = 4), "\n")

cat("\n✅ 分析完成！共產出 3 張圖表：\n")
cat("  plots/J_city_runaway_rate.png\n")
cat("  plots/K_city_satisfaction.png\n")
cat("  plots/L_city_scatter_run_sat.png\n")
