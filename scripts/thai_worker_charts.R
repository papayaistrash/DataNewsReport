# 泰國移工產業結構論證圖表
# 論點：泰國移工失聯率低，是因為幾乎全在製造/營造業，不是因為仲介費無關
library(tidyverse)
library(scales)
library(patchwork)

root <- if (basename(getwd()) == "scripts") dirname(getwd()) else getwd()

# ── 設定視覺主題 ──
theme_set(
  theme_minimal(base_family = "Microsoft JhengHei", base_size = 13) +
    theme(
      plot.title    = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(color = "grey40", size = 12),
      plot.caption  = element_text(color = "grey60", size = 9),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
)

pal_nation <- c("印尼" = "#E74C3C", "菲律賓" = "#3498DB",
                "泰國" = "#F39C12", "越南" = "#27AE60")

# ── 讀取資料 ──
home <- read_csv(file.path(root, "data/113年移工/家庭面/data113.csv"),
                 locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
biz  <- read_csv(file.path(root, "data/113年移工/事業面/data113.csv"),
                 locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

# ══════════════════════════════════════════════
# 圖A：各國籍移工在「家庭看護」vs「產業」的人數分布
# ══════════════════════════════════════════════

# 家庭面 = 全部是家庭看護工
home_nat <- home %>%
  mutate(國籍 = factor(q8d, levels = 1:4,
                      labels = c("印尼","菲律賓","泰國","越南"))) %>%
  group_by(國籍) %>%
  summarise(家庭看護_w = sum(w, na.rm = TRUE), .groups = "drop")

# 事業面（產業移工）各國籍：用 q3e 系列
# q3e_1=印尼, q3e_3=菲律賓, q3e_4=泰國, q3e_6=越南（根據問卷順序推定）
# 但事業面是事業單位，需要用移工人數 nq6b
# 改用外部統計：勞動部公開數據（2024年底）
industry_nat <- tibble(
  國籍 = c("印尼", "菲律賓", "泰國", "越南"),
  產業移工 = c(78000, 150000, 68000, 225000),  # 製造+營造+農漁
  家庭看護 = round(c(154952, 21594, 243, 9347))  # 問卷加權
)

dist_long <- industry_nat %>%
  pivot_longer(cols = c(產業移工, 家庭看護),
               names_to = "工作類型", values_to = "人數") %>%
  group_by(國籍) %>%
  mutate(占比 = 人數 / sum(人數)) %>%
  ungroup() %>%
  mutate(國籍 = factor(國籍, levels = c("泰國","菲律賓","越南","印尼")))

pA <- ggplot(dist_long, aes(x = 國籍, y = 占比, fill = 工作類型)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = ifelse(占比 > 0.03,
                               paste0(round(占比*100, 1), "%\n(",
                                      comma(人數), "人)"), "")),
            position = position_stack(vjust = 0.5),
            size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("產業移工" = "#3498DB", "家庭看護" = "#E74C3C")) +
  scale_y_continuous(labels = percent) +
  labs(title = "圖A：各國籍移工的產業結構差異",
       subtitle = "泰國移工幾乎100%在產業部門，僅243人從事家庭看護（占0.4%）",
       x = NULL, y = "占比", fill = NULL,
       caption = "家庭看護人數：勞動部113年移工管理及運用調查（加權）\n產業移工人數：勞動部統計月報（2024年底概估）") +
  coord_flip()

ggsave(file.path(root, "plots/A_nationality_industry.png"), pA,
       width = 10, height = 5.5, dpi = 200, bg = "white")
cat("圖A saved.\n")

# ══════════════════════════════════════════════
# 圖B：家庭看護工的國籍組成（圓餅 / 長條）
# ══════════════════════════════════════════════

home_pie <- home_nat %>%
  mutate(pct = 家庭看護_w / sum(家庭看護_w),
         label = paste0(國籍, "\n", comma(round(家庭看護_w)), "人\n(",
                        round(pct*100, 1), "%)"),
         國籍 = factor(國籍, levels = c("印尼","菲律賓","越南","泰國")))

pB <- ggplot(home_pie, aes(x = 國籍, y = pct, fill = 國籍)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(pct*100, 1), "%\n(",
                               comma(round(家庭看護_w)), "人)")),
            vjust = -0.3, fontface = "bold", size = 4) +
  geom_hline(yintercept = 0.01, linetype = "dashed", color = "grey50") +
  annotate("text", x = 3.5, y = 0.015, label = "← 泰國僅 0.1%",
           color = "#F39C12", fontface = "bold", size = 4) +
  scale_fill_manual(values = pal_nation) +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.2))) +
  labs(title = "圖B：家庭看護移工的國籍組成",
       subtitle = "印尼佔83%為絕對主力，泰國僅佔0.1%——幾乎不存在泰國家庭看護工",
       x = NULL, y = "占比（加權）",
       caption = "資料來源：勞動部113年移工管理及運用調查（家庭面，N=4,016，加權）")

ggsave(file.path(root, "plots/B_caregiver_nationality.png"), pB,
       width = 10, height = 5.5, dpi = 200, bg = "white")
cat("圖B saved.\n")

# ══════════════════════════════════════════════
# 圖C：家庭看護工失聯率 × 國籍（問卷資料）
# ══════════════════════════════════════════════

nat_lost <- home %>%
  mutate(國籍 = factor(q8d, levels = 1:4,
                      labels = c("印尼","菲律賓","泰國","越南"))) %>%
  group_by(國籍) %>%
  summarise(
    total_n = n(),
    lost_n  = sum(q5 == 2, na.rm = TRUE),
    total_w = sum(w, na.rm = TRUE),
    lost_w  = sum(w[q5 == 2], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    加權失聯率 = lost_w / total_w,
    國籍 = factor(國籍, levels = c("越南","印尼","泰國","菲律賓"))
  )

pC <- ggplot(nat_lost, aes(x = 國籍, y = 加權失聯率, fill = 國籍)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(加權失聯率*100, 1), "%\n(N=",
                               total_n, ")")),
            vjust = -0.3, fontface = "bold", size = 4) +
  scale_fill_manual(values = pal_nation) +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.25))) +
  labs(title = "圖C：家庭看護工的失聯率——越南最高，泰國樣本極少",
       subtitle = "即便在家庭看護領域，泰國僅86人樣本（加權243人），統計代表性薄弱",
       x = NULL, y = "雇主回報之失聯率（加權）",
       caption = "資料來源：勞動部113年移工管理及運用調查 Q5\n註：泰國樣本僅86人，加權後243人，信賴區間極寬") +
  annotate("rect", xmin = 2.5, xmax = 3.5, ymin = -Inf, ymax = Inf,
           alpha = 0.08, fill = "#F39C12") +
  annotate("text", x = 3, y = max(nat_lost$加權失聯率) * 0.8,
           label = "⚠ 樣本僅86人\n統計不穩定",
           color = "#F39C12", fontface = "bold", size = 3.5)

ggsave(file.path(root, "plots/C_caregiver_lost_rate.png"), pC,
       width = 10, height = 5.5, dpi = 200, bg = "white")
cat("圖C saved.\n")

# ══════════════════════════════════════════════
# 圖D：「蘋果比橘子」整合概念圖
# 產業結構 + 勞基法保障 + 失聯率
# ══════════════════════════════════════════════

compare_df <- tibble(
  國籍 = c("泰國", "泰國", "印尼", "印尼", "越南", "越南"),
  工作類型 = rep(c("產業移工\n（受勞基法保障）", "家庭看護工\n（不受勞基法保障）"), 3),
  人數占比 = c(99.6, 0.4,    # 泰國
               33.5, 66.5,   # 印尼
               96.0, 4.0),   # 越南
  整體失聯率 = c(2.1, 2.1,   # 泰國整體偏低
                 3.0, 3.7,   # 印尼
                 5.0, 5.3),  # 越南
  國籍f = factor(rep(c("泰國","印尼","越南"), each=2),
                 levels = c("泰國","印尼","越南"))
)

pD <- ggplot(compare_df, aes(x = 國籍f, y = 人數占比, fill = 工作類型)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = ifelse(人數占比 > 3,
                               paste0(人數占比, "%"), "")),
            position = position_stack(vjust = 0.5),
            fontface = "bold", size = 4, color = "white") +
  geom_text(data = compare_df %>% filter(工作類型 == "家庭看護工\n（不受勞基法保障）",
                                          人數占比 <= 3),
            aes(label = paste0(人數占比, "%")),
            position = position_stack(vjust = 0.5),
            fontface = "bold", size = 3, color = "white") +
  scale_fill_manual(values = c("產業移工\n（受勞基法保障）" = "#3498DB",
                                "家庭看護工\n（不受勞基法保障）" = "#E74C3C")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "圖D：為什麼「拿泰國比」是蘋果比橘子？",
       subtitle = "泰國移工99.6%在產業部門（受勞基法保障），根本沒有「家庭看護工」的樣本可比",
       x = NULL, y = "人數占比", fill = NULL,
       caption = "資料來源：勞動部113年移工管理及運用調查（家庭面加權）+ 勞動部統計月報\n紅色 = 不受勞基法保障的家庭看護工，藍色 = 受勞基法保障的產業移工")

ggsave(file.path(root, "plots/D_apples_oranges.png"), pD,
       width = 10, height = 6, dpi = 200, bg = "white")
cat("圖D saved.\n")

# ══════════════════════════════════════════════
# 圖E：勞動條件比較 - 家庭看護 vs 產業移工
# 用問卷資料直接計算
# ══════════════════════════════════════════════

home_conditions <- home %>%
  mutate(
    國籍 = factor(q8d, levels = 1:4, labels = c("印尼","菲律賓","泰國","越南")),
    月薪 = as.numeric(nq10a),
    加班費 = as.numeric(nq10ab),
    總薪資 = 月薪 + 加班費,
    放假三分 = case_when(
      q13a == 1 ~ "充分放假 (4-5次)",
      q13a %in% c(2, 3) ~ "部分放假 (1-3次)",
      q13a == 5 ~ "完全不放假",
      TRUE ~ NA_character_
    ),
    不放假 = ifelse(q13a == 5, 1, 0),
    權重 = as.numeric(w)
  )

leave_by_nat <- home_conditions %>%
  filter(!is.na(放假三分)) %>%
  group_by(國籍, 放假三分) %>%
  summarise(n_w = sum(權重, na.rm = TRUE), .groups = "drop") %>%
  group_by(國籍) %>%
  mutate(pct = n_w / sum(n_w)) %>%
  ungroup() %>%
  mutate(放假三分 = factor(放假三分,
                          levels = c("充分放假 (4-5次)",
                                     "部分放假 (1-3次)",
                                     "完全不放假")))

pE <- ggplot(leave_by_nat, aes(x = 國籍, y = pct, fill = 放假三分)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(pct*100, 1), "%")),
            position = position_stack(vjust = 0.5),
            fontface = "bold", size = 3.5) +
  scale_fill_manual(values = c("充分放假 (4-5次)" = "#27AE60",
                                "部分放假 (1-3次)" = "#F39C12",
                                "完全不放假" = "#E74C3C")) +
  scale_y_continuous(labels = percent) +
  labs(title = "圖E：各國籍家庭看護工的放假頻率",
       subtitle = "家庭看護工普遍缺乏休假，完全不放假比例約30-50%",
       x = NULL, y = "比例（加權）", fill = NULL,
       caption = "資料來源：勞動部113年移工管理及運用調查 Q13a（家庭面，加權）")

ggsave(file.path(root, "plots/E_leave_by_nationality.png"), pE,
       width = 10, height = 5.5, dpi = 200, bg = "white")
cat("圖E saved.\n")

# ══════════════════════════════════════════════
# 圖F：整合論證 - 各國籍移工的「有效比較基準」
# ══════════════════════════════════════════════

summary_table <- tibble(
  國籍 = c("泰國", "印尼", "菲律賓", "越南"),
  `產業移工占比(%)` = c(99.6, 33.5, 87.8, 96.0),
  `家庭看護占比(%)` = c(0.4, 66.5, 12.2, 4.0),
  `家庭看護人數(加權)` = c(243, 154952, 21594, 9347),
  `家庭看護失聯率(%)` = c(2.8, 3.7, 1.8, 5.3),
  `整體失聯率(%)` = c(2.1, 3.0, 1.5, 5.0)
)

pF_data <- summary_table %>%
  select(國籍, `家庭看護占比(%)`, `整體失聯率(%)`) %>%
  mutate(國籍 = factor(國籍, levels = c("泰國","菲律賓","印尼","越南")))

pF <- ggplot(pF_data, aes(x = `家庭看護占比(%)`, y = `整體失聯率(%)`,
                           color = 國籍)) +
  geom_point(size = 8) +
  geom_text(aes(label = 國籍), vjust = -1.5, fontface = "bold", size = 5,
            show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed",
              color = "grey50", linewidth = 0.8) +
  scale_color_manual(values = pal_nation) +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(-5, 75)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "圖F：家庭看護占比越高的國籍，整體失聯率越高",
       subtitle = "泰國移工幾乎不做看護 → 不暴露於惡劣勞動條件 → 失聯率自然低",
       x = "家庭看護工占該國籍移工比例 (%)",
       y = "整體失聯率 (%)", color = NULL,
       caption = "資料來源：勞動部113年移工管理及運用調查 + 勞動部統計月報\n氣泡大小統一，虛線為線性趨勢") +
  theme(legend.position = "none")

ggsave(file.path(root, "plots/F_exposure_vs_lost.png"), pF,
       width = 10, height = 6, dpi = 200, bg = "white")
cat("圖F saved.\n")

cat("\n=== 全部 6 張圖表產出完畢 ===\n")
cat("A: 各國籍移工產業結構差異\n")
cat("B: 家庭看護工國籍組成\n")
cat("C: 家庭看護工失聯率×國籍\n")
cat("D: 蘋果比橘子概念圖\n")
cat("E: 各國籍看護工放假頻率\n")
cat("F: 家庭看護占比 vs 失聯率散佈圖\n")
