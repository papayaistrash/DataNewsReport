# ============================================================================
# 📊 各縣市家庭看護移工失聯率差異的「成因分析」
#    合併 109–113 年（N ≈ 20,110）→ Logistic 迴歸 + 縣市特徵剖面
# ============================================================================
library(tidyverse)
library(haven)
library(scales)
library(broom)
library(showtext)

font_add_google("Noto Sans TC", "NotoSans")
showtext_auto()

# ── 動態路徑 ──────────────────────────────────────────────────
base_dir <- "."
if (basename(getwd()) == "scripts") { base_dir <- ".." }
dir.create(file.path(base_dir, "plots"), showWarnings = FALSE)

# ── 縣市對照表 ──────────────────────────────────────────────
city_map <- c(
  "1"="新北市","2"="臺北市","3"="桃園市","4"="臺中市","5"="臺南市","6"="高雄市",
  "7"="宜蘭縣","8"="新竹縣","9"="苗栗縣","10"="彰化縣","11"="南投縣","12"="雲林縣",
  "13"="嘉義縣","14"="屏東縣","15"="臺東縣","16"="花蓮縣","17"="澎湖縣","18"="基隆市",
  "19"="新竹市","20"="嘉義市","21"="金門縣及連江縣"
)
six_cities <- c("新北市","臺北市","桃園市","臺中市","臺南市","高雄市")

# ── 各年讀取並統一解釋變數 ────────────────────────────────────
read_year_full <- function(year) {
  path <- file.path(base_dir, "data",
                    paste0(year, "年移工/家庭面/data", year, ".dta"))
  d <- read_dta(path)

  # 縣市
  city_col <- switch(year,
    "109"=, "112"= "t1",
    "110"=, "111"= "county",
    "113"= "j1")

  # 失聯（1=無, 2=有）
  run_col <- if (year %in% c("109","110","111")) "q1" else "q5"

  # 薪資（總薪資）
  sal_col <- switch(year,
    "109"= "q9_0_1",
    "110"=, "111"=, "112"= "q11",
    "113"= "nq10a")

  # 放假 → 統一為 no_leave (0/1)
  if (year == "109") {
    # q15: 1=都有, 2=部分, 3=都不放
    d$no_leave <- as.integer(d$q15 == 3)
  } else if (year %in% c("110", "111")) {
    # q12: 1=都有, 2=部分, 3=都不放
    d$no_leave <- as.integer(d$q12 == 3)
  } else if (year == "112") {
    # q12a: 1~4=有放, 5=都不放
    d$no_leave <- as.integer(d$q12a == 5)
  } else {
    # 113: q13a: 1~4=有放, 5=都不放
    d$no_leave <- as.integer(d$q13a == 5)
  }

  d %>%
    transmute(
      year      = as.integer(year),
      city_code = as.character(.data[[city_col]]),
      city      = recode(city_code, !!!city_map),
      is_six    = city %in% six_cities,
      runaway   = as.integer(.data[[run_col]]),       # 1=無, 2=有
      run_01    = as.integer(runaway == 2),
      salary    = as.numeric(.data[[sal_col]]),
      nationality = factor(q8d, levels = 1:4,
                           labels = c("印尼","菲律賓","泰國","越南")),
      age_group = factor(q8b, levels = 1:4,
                         labels = c("未滿25歲","25-34歲","35-44歲","45歲以上")),
      education = factor(q8c, levels = 1:3,
                         labels = c("國中以下","高中職","專科以上")),
      nursing   = factor(q8e, levels = 1:2,
                         labels = c("有護理訓練","無護理訓練")),
      no_leave  = no_leave,
      w         = as.numeric(w)
    )
}

d_all <- bind_rows(lapply(c("109","110","111","112","113"), read_year_full))
cat("合併後總列數:", nrow(d_all), "\n")

# ── 清除極端薪資 ─────────────────────────────────────────────
d_all <- d_all %>%
  mutate(salary = ifelse(salary < 10000 | salary > 50000, NA, salary))

# ============================================================================
# 📊 Part 1：各縣市「組成差異」描述性統計
# ============================================================================
city_profile <- d_all %>%
  filter(!is.na(run_01)) %>%
  group_by(city) %>%
  summarise(
    n            = n(),
    run_rate     = weighted.mean(run_01, w, na.rm = TRUE),
    pct_vn       = weighted.mean(nationality == "越南", w, na.rm = TRUE),
    pct_id       = weighted.mean(nationality == "印尼", w, na.rm = TRUE),
    pct_ph       = weighted.mean(nationality == "菲律賓", w, na.rm = TRUE),
    pct_young    = weighted.mean(age_group %in% c("未滿25歲","25-34歲"), w, na.rm = TRUE),
    mean_salary  = weighted.mean(salary, w, na.rm = TRUE),
    pct_no_leave = weighted.mean(no_leave, w, na.rm = TRUE),
    pct_low_edu  = weighted.mean(education == "國中以下", w, na.rm = TRUE),
    pct_no_nurse = weighted.mean(nursing == "無護理訓練", w, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(is_six = city %in% six_cities)

cat("\n=== 縣市特徵與失聯率相關矩陣 ===\n")
cor_vars <- city_profile %>%
  select(run_rate, pct_vn, pct_young, mean_salary, pct_no_leave, pct_low_edu, pct_no_nurse)
cor_mat <- cor(cor_vars, use = "complete.obs")
print(round(cor_mat, 3))

# ============================================================================
# 📊 Part 2：個體層級 Logistic 迴歸（控制多重因素）
# ============================================================================
d_model <- d_all %>%
  filter(!is.na(run_01), !is.na(nationality), !is.na(age_group),
         !is.na(salary), !is.na(no_leave), !is.na(education), !is.na(w))

cat("\n=== 迴歸分析樣本數:", nrow(d_model), "===\n")

model_full <- glm(
  run_01 ~ nationality + age_group + salary + no_leave + education + nursing +
           is_six + factor(year),
  data = d_model, family = binomial, weights = w
)

# 使用 Wald CI 而非 profile CI（避免加權 GLM 收斂問題）
or_table <- tidy(model_full, exponentiate = TRUE) %>%
  mutate(
    conf.low  = exp(log(estimate) - 1.96 * std.error),
    conf.high = exp(log(estimate) + 1.96 * std.error)
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  filter(term != "(Intercept)")

cat("\n=== Logistic 迴歸結果（勝算比 OR，Wald CI）===\n")
print(or_table %>% select(term, estimate, conf.low, conf.high, p.value))

# ============================================================================
# 🎨 繪圖 —— 圖 M：Logistic 迴歸 OR 森林圖
# ============================================================================
or_plot_data <- or_table %>%
  filter(!str_detect(term, "factor\\(year\\)")) %>%
  mutate(
    label = recode(term,
      "nationality菲律賓"   = "菲律賓（vs 印尼）",
      "nationality泰國"     = "泰國（vs 印尼）",
      "nationality越南"     = "越南（vs 印尼）",
      "age_group25-34歲"    = "25-34歲（vs <25）",
      "age_group35-44歲"    = "35-44歲（vs <25）",
      "age_group45歲以上"   = "45歲以上（vs <25）",
      "salary"              = "月薪（每增千元）",
      "no_leave"            = "完全不放假",
      "education高中職"     = "高中職（vs 國中以下）",
      "education專科以上"   = "專科以上（vs 國中以下）",
      "nursing無護理訓練"   = "無護理訓練",
      "is_sixTRUE"          = "六都（vs 非六都）"
    ),
    sig = ifelse(p.value < 0.05, "顯著", "不顯著"),
    group = case_when(
      str_detect(term, "nationality") ~ "國籍",
      str_detect(term, "age_group")   ~ "年齡",
      str_detect(term, "salary")      ~ "薪資",
      str_detect(term, "leave")       ~ "休假",
      str_detect(term, "education")   ~ "教育",
      str_detect(term, "nursing")     ~ "訓練",
      str_detect(term, "six")         ~ "地區"
    )
  ) %>%
  # 特別處理薪資：改為「每增加1,000元」的 OR
  mutate(
    estimate = ifelse(term == "salary", estimate^1000, estimate),
    conf.low = ifelse(term == "salary", conf.low^1000, conf.low),
    conf.high = ifelse(term == "salary", conf.high^1000, conf.high)
  )

pM <- ggplot(or_plot_data, aes(x = estimate, y = fct_rev(fct_inorder(label)),
                                color = sig)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.25, linewidth = 0.9) +
  geom_point(size = 4) +
  geom_text(aes(label = round(estimate, 2)),
            vjust = -0.8, size = 5, family = "NotoSans", show.legend = FALSE) +
  scale_color_manual(values = c("顯著" = "#DE4429", "不顯著" = "#999999")) +
  scale_x_log10() +
  labs(
    title    = "哪些因素影響家庭看護移工的失聯風險？",
    subtitle = "Logistic 迴歸勝算比（OR）｜109–113 年合併加權（控制年度固定效果）",
    x        = "勝算比 OR（對數刻度，1 = 無影響）",
    y        = NULL,
    color    = "p < 0.05",
    caption  = "資料來源：勞動部 109–113 年移工管理及運用調查（家庭面）\n參照組：印尼、未滿25歲、國中以下、非六都"
  ) +
  theme_classic(base_family = "NotoSans", base_size = 20) +
  theme(
    plot.title    = element_text(face = "bold", size = 25),
    plot.subtitle = element_text(size = 14, color = "#555555", margin = margin(b = 12)),
    plot.caption  = element_text(color = "#aaaaaa"),
    legend.position = "right",
    axis.line     = element_blank(),
    axis.ticks    = element_blank(),
    axis.title.y  = element_text(angle = 0, vjust = 0.5, hjust = 1),
    panel.grid.major.x = element_line(color = "#eeeeee")
  )

ggsave(file.path(base_dir, "plots/M_logistic_or_forest.png"),
       plot = pM, width = 14, height = 9, dpi = 150)
cat("\n✅ 已輸出：plots/M_logistic_or_forest.png\n")

# ============================================================================
# 🎨 繪圖 —— 圖 N：高失聯 vs 低失聯縣市的「組成剖面」雷達比較
#    改用平行座標/長條比較（ggplot 原生）
# ============================================================================
# 將縣市分為高、中、低失聯三組
quantiles <- quantile(city_profile$run_rate, probs = c(1/3, 2/3))
city_profile <- city_profile %>%
  mutate(
    run_group = case_when(
      run_rate >= quantiles[2] ~ "高失聯（前1/3）",
      run_rate <= quantiles[1] ~ "低失聯（後1/3）",
      TRUE                     ~ "中失聯"
    ),
    run_group = factor(run_group, levels = c("高失聯（前1/3）","中失聯","低失聯（後1/3）"))
  )

profile_compare <- city_profile %>%
  group_by(run_group) %>%
  summarise(
    越南籍占比     = mean(pct_vn),
    `未滿35歲占比` = mean(pct_young),
    不放假比例     = mean(pct_no_leave),
    平均月薪       = mean(mean_salary),
    國中以下比例   = mean(pct_low_edu),
    無護理訓練比例 = mean(pct_no_nurse),
    .groups = "drop"
  )

cat("\n=== 高/中/低失聯縣市組成比較 ===\n")
print(profile_compare)

# 標準化繪圖
profile_long <- profile_compare %>%
  pivot_longer(-run_group, names_to = "指標", values_to = "值") %>%
  group_by(指標) %>%
  mutate(標準化 = (值 - min(值)) / (max(值) - min(值) + 1e-10)) %>%
  ungroup()

pN <- ggplot(profile_long %>% filter(指標 != "平均月薪"),
             aes(x = 指標, y = 標準化, fill = run_group)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = case_when(
    指標 == "平均月薪" ~ comma(round(值)),
    TRUE ~ percent(值, 0.1)
  )), position = position_dodge(0.7), vjust = -0.3,
  size = 5, family = "NotoSans") +
  scale_fill_manual(values = c(
    "高失聯（前1/3）" = "#DE4429",
    "中失聯"          = "#E8A343",
    "低失聯（後1/3）" = "#3566A5"
  )) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(
    title    = "高失聯 vs 低失聯縣市：移工組成有何不同？",
    subtitle = "依加權失聯率將 21 縣市分為三組比較｜數字為原始比例",
    x        = NULL, y = "標準化比較值", fill = NULL,
    caption  = "資料來源：勞動部 109–113 年移工管理及運用調查（家庭面）"
  ) +
  theme_classic(base_family = "NotoSans", base_size = 20) +
  theme(
    plot.title    = element_text(face = "bold", size = 25),
    plot.subtitle = element_text(size = 14, color = "#555555", margin = margin(b = 12)),
    plot.caption  = element_text(color = "#aaaaaa"),
    legend.position = "top",
    axis.line     = element_blank(),
    axis.ticks    = element_blank(),
    axis.title.y  = element_text(angle = 0, vjust = 0.5, hjust = 1),
    axis.text.x   = element_text(size = 17)
  )

ggsave(file.path(base_dir, "plots/N_city_profile_compare.png"),
       plot = pN, width = 14, height = 8, dpi = 150)
cat("✅ 已輸出：plots/N_city_profile_compare.png\n")

# ============================================================================
# 🎨 繪圖 —— 圖 O：縣市失聯率 × 越南籍占比 散佈圖
# ============================================================================
cor_vn <- cor(city_profile$run_rate, city_profile$pct_vn, use = "complete.obs")
cor_test_vn <- cor.test(city_profile$run_rate, city_profile$pct_vn)

pO <- ggplot(city_profile, aes(x = pct_vn, y = run_rate)) +
  geom_smooth(method = "lm", se = TRUE, color = "#DE4429",
              linetype = "dashed", linewidth = 0.8, alpha = 0.15) +
  geom_point(aes(size = n, color = ifelse(is_six, "六都", "非六都")), alpha = 0.8) +
  geom_text(aes(label = city), vjust = -1.2, size = 5,
            family = "NotoSans", check_overlap = FALSE) +
  scale_color_manual(values = c("六都" = "#3566A5", "非六都" = "#DE4429")) +
  scale_size_continuous(name = "樣本數", range = c(3, 12)) +
  scale_x_continuous(labels = percent_format(accuracy = 1), name = "越南籍占比") +
  scale_y_continuous(labels = percent_format(accuracy = 0.1), name = "加權\n失聯率") +
  annotate("label", x = max(city_profile$pct_vn) * 0.85,
           y = max(city_profile$run_rate) * 0.95,
           label = paste0("r = ", round(cor_vn, 3), "\np = ", format.pval(cor_test_vn$p.value, digits=3)),
           size = 5.5, family = "NotoSans", fontface = "bold",
           fill = "white", color = "#DE4429") +
  labs(
    title    = "越南籍移工占比愈高的縣市，失聯率愈高",
    subtitle = "以五年合併數據計算各縣市越南籍家庭看護工加權占比 vs 加權失聯率",
    color    = NULL,
    caption  = paste0("Pearson r = ", round(cor_vn, 3),
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
    axis.title.y  = element_text(angle = 0, vjust = 0.5, hjust = 1),
    panel.grid.major = element_line(color = "#eeeeee")
  )

ggsave(file.path(base_dir, "plots/O_city_vietnam_vs_runaway.png"),
       plot = pO, width = 13, height = 9, dpi = 150)
cat("✅ 已輸出：plots/O_city_vietnam_vs_runaway.png\n")

# ============================================================================
# 📋 最終摘要
# ============================================================================
cat("\n══════════════════════════════════════════════════════════\n")
cat("                    分析核心結論摘要\n")
cat("══════════════════════════════════════════════════════════\n")

cat("\n【迴歸結果重點】\n")
key_or <- or_table %>%
  filter(p.value < 0.05, !str_detect(term, "year")) %>%
  arrange(desc(estimate))
for (i in 1:nrow(key_or)) {
  cat(sprintf("  • %s: OR = %.2f (p = %s)\n",
              key_or$term[i], key_or$estimate[i],
              format.pval(key_or$p.value[i], digits=3)))
}

cat("\n【縣市層級相關】\n")
cat(sprintf("  • 失聯率 × 越南籍占比: r = %.3f (p = %s)\n",
            cor_vn, format.pval(cor_test_vn$p.value, digits=3)))

cat("\n✅ 分析完成！共產出 3 張新圖表：\n")
cat("  plots/M_logistic_or_forest.png      — 迴歸 OR 森林圖\n")
cat("  plots/N_city_profile_compare.png    — 高/低失聯縣市組成比較\n")
cat("  plots/O_city_vietnam_vs_runaway.png — 越南籍占比 vs 失聯率\n")
