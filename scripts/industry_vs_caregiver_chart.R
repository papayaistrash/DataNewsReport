# ══════════════════════════════════════════════════════════════
# 產業移工 vs 家庭看護工：待遇差距與失聯率比較
# ══════════════════════════════════════════════════════════════
library(tidyverse)
library(scales)
library(patchwork)

root <- if (basename(getwd()) == "scripts") dirname(getwd()) else getwd()

# ── 主題設定 ──
theme_report <- theme_minimal(base_family = "Microsoft JhengHei", base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 17, color = "#1a1a2e"),
    plot.subtitle   = element_text(color = "grey40", size = 12),
    plot.caption    = element_text(color = "grey60", size = 9),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 12)
  )
theme_set(theme_report)

col_industry <- "#3498DB"
col_care     <- "#E74C3C"

# ── 讀取資料 ──
home <- read_csv(file.path(root, "data/113年移工/家庭面/data113.csv"),
                 locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
biz  <- read_csv(file.path(root, "data/113年移工/事業面/data113.csv"),
                 locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

# ══════════════════════════════════════════════
# 計算各項指標
# ══════════════════════════════════════════════

# --- 事業面（產業移工）---
biz_w <- biz$w3  # 權重

# 基本月薪 q7ca
biz_salary_base <- weighted.mean(biz$q7ca, biz_w, na.rm = TRUE)
# 加班費 q7cb
biz_overtime_pay <- weighted.mean(biz$q7cb, biz_w, na.rm = TRUE)
# 總薪資 = 基本月薪 + 加班費 + 津貼
biz_salary_total <- biz_salary_base + biz_overtime_pay +
  weighted.mean(biz$nq7d, biz_w, na.rm = TRUE)

# 每月正常工時 q7b (小時) + 加班時數 q7bb
biz_monthly_hours <- weighted.mean(biz$q7b, biz_w, na.rm = TRUE)
biz_ot_hours_raw <- weighted.mean(biz$q7bb, biz_w, na.rm = TRUE)
# 每日實際工時 ≈ (正常月工時 + 加班月時數) / 工作天數
biz_daily_hours <- (biz_monthly_hours + biz_ot_hours_raw) / 22  # 約22工作日

# 每月加班時數
biz_ot_hours <- weighted.mean(biz$q7bb, biz_w, na.rm = TRUE)

# 休假：q8=1 表示「週休二日」，q8=2「每月排休」，q8=3「彈性」，q8=4「不一定」
# 事業面大部分適用勞基法 → 每月至少4天假（週休一日起跳）
# 保守估計：q8=1 → 8天/月；q8=2 → 排休約 4-6天
biz_leave_days <- 8  # 受勞基法保障，週休制

# 失聯率：使用移民署累計失聯率（per-worker）
# 事業面 q4 是「事業單位曾有失聯經驗的比例」(26.8%)，非個人失聯率
# 累計失聯率（移民署2025）：製造業約10.5%，看護工約12.2%
biz_lost_rate <- 10.5  # 製造業累計失聯率

# --- 家庭面（家庭看護工）---
home_w <- home$w

# 基本月薪
home_salary_base <- weighted.mean(as.numeric(home$nq10a), home_w, na.rm = TRUE)
# 加班費
home_overtime_pay <- weighted.mean(as.numeric(home$nq10ab), home_w, na.rm = TRUE)
# 總薪資
home_salary_total <- home_salary_base + home_overtime_pay

# 每日工時
home_hours <- home %>%
  mutate(
    start = as.numeric(nq9a_1),
    end   = as.numeric(nq9a_2) + 12,
    rest_h = as.numeric(nq9b_1),
    rest_m = as.numeric(nq9b_2),
    work  = (end - start) - (rest_h + rest_m / 60)
  ) %>%
  filter(work > 0 & work < 24)
home_daily_hours <- weighted.mean(home_hours$work, home_hours$w, na.rm = TRUE)

# 每月放假天數
# q13a: 1=4-5次, 2=1次, 3=2-3次, 5=完全不放假
home_leave <- home %>%
  mutate(
    leave_days = case_when(
      q13a == 1 ~ 4.5,
      q13a == 2 ~ 1,
      q13a == 3 ~ 2.5,
      q13a == 5 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(leave_days))
home_leave_days <- weighted.mean(home_leave$leave_days, home_leave$w, na.rm = TRUE)

# 不放假比例
home_no_leave_pct <- sum(home_w[home$q13a == 5], na.rm = TRUE) /
  sum(home_w[!is.na(home$q13a)], na.rm = TRUE) * 100

# 失聯率：使用移民署累計失聯率（per-worker）
# 家庭面 q5 是雇主過去6年回報比例（3.5%），低估（僅限現有雇主回顧）
# 累計失聯率（移民署2025）：看護工約12.2%
home_lost_rate <- 12.2  # 看護工累計失聯率

# ── 輸出數據確認 ──
cat("=== 產業移工 ===\n")
cat("基本月薪:", round(biz_salary_base), "\n")
cat("加班費:", round(biz_overtime_pay), "\n")
cat("總薪資:", round(biz_salary_total), "\n")
cat("每月正常工時:", round(biz_monthly_hours, 1), "\n")
cat("每日工時:", round(biz_daily_hours, 1), "\n")
cat("每月加班時數:", round(biz_ot_hours, 1), "\n")
cat("每月休假:", biz_leave_days, "天\n")
cat("失聯率:", round(biz_lost_rate, 1), "%\n")

cat("\n=== 家庭看護工 ===\n")
cat("基本月薪:", round(home_salary_base), "\n")
cat("加班費:", round(home_overtime_pay), "\n")
cat("總薪資:", round(home_salary_total), "\n")
cat("每日工時:", round(home_daily_hours, 1), "\n")
cat("每月休假:", round(home_leave_days, 1), "天\n")
cat("完全不放假比例:", round(home_no_leave_pct, 1), "%\n")
cat("失聯率:", round(home_lost_rate, 1), "%\n")

# ══════════════════════════════════════════════
# 繪圖：整合式對比圖
# ══════════════════════════════════════════════

# 子圖1：月薪比較
salary_df <- tibble(
  類別 = factor(c("產業移工", "家庭看護工"), levels = c("產業移工", "家庭看護工")),
  基本月薪 = c(biz_salary_base, home_salary_base),
  加班費 = c(biz_overtime_pay, home_overtime_pay)
) %>%
  pivot_longer(cols = c(基本月薪, 加班費), names_to = "項目", values_to = "金額") %>%
  mutate(項目 = factor(項目, levels = c("加班費", "基本月薪")))

p1 <- ggplot(salary_df, aes(x = 類別, y = 金額, fill = 項目)) +
  geom_col(width = 0.55) +
  geom_text(data = salary_df %>% group_by(類別) %>% summarise(total = sum(金額)),
            aes(x = 類別, y = total, fill = NULL,
                label = paste0(comma(round(total)), " 元")),
            vjust = -0.5, fontface = "bold", size = 4.5) +
  scale_fill_manual(values = c("基本月薪" = "#5DADE2", "加班費" = "#F5B041"),
                    guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.18)),
                     limits = c(0, NA)) +
  labs(title = "💰 月薪", x = NULL, y = "元", fill = NULL) +
  theme(legend.position = "top", legend.text = element_text(size = 10))

# 子圖2：每日工時
hours_df <- tibble(
  類別 = factor(c("產業移工", "家庭看護工"), levels = c("產業移工", "家庭看護工")),
  工時 = c(biz_daily_hours, home_daily_hours),
  顏色 = c(col_industry, col_care)
)

p2 <- ggplot(hours_df, aes(x = 類別, y = 工時, fill = 類別)) +
  geom_col(width = 0.55, show.legend = FALSE) +
  geom_hline(yintercept = 8, linetype = "dashed", color = "grey50", linewidth = 0.6) +
  annotate("text", x = 1.5, y = 8.4, label = "勞基法標準 8 小時",
           color = "grey40", size = 3.5, fontface = "italic") +
  geom_text(aes(label = paste0(round(工時, 1), " hr")),
            vjust = -0.5, fontface = "bold", size = 4.5) +
  scale_fill_manual(values = c("產業移工" = col_industry, "家庭看護工" = col_care)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)), limits = c(0, 13)) +
  labs(title = "⏰ 每日工時", x = NULL, y = "小時")

# 子圖3：每月休假天數
leave_df <- tibble(
  類別 = factor(c("產業移工", "家庭看護工"), levels = c("產業移工", "家庭看護工")),
  天數 = c(biz_leave_days, home_leave_days),
  備註 = c("週休制保障", paste0(round(home_no_leave_pct, 0), "%完全不放假"))
)

p3 <- ggplot(leave_df, aes(x = 類別, y = 天數, fill = 類別)) +
  geom_col(width = 0.55, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(天數, 1), " 天\n", 備註)),
            vjust = -0.3, fontface = "bold", size = 3.8, lineheight = 0.9) +
  scale_fill_manual(values = c("產業移工" = col_industry, "家庭看護工" = col_care)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25)), limits = c(0, NA)) +
  labs(title = "🏖️ 每月休假", x = NULL, y = "天")

# 子圖4：累計失聯率
lost_df <- tibble(
  類別 = factor(c("產業移工", "家庭看護工"), levels = c("產業移工", "家庭看護工")),
  失聯率 = c(biz_lost_rate, home_lost_rate)
)

p4 <- ggplot(lost_df, aes(x = 類別, y = 失聯率, fill = 類別)) +
  geom_col(width = 0.55, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(失聯率, 1), "%")),
            vjust = -0.5, fontface = "bold", size = 4.5) +
  scale_fill_manual(values = c("產業移工" = col_industry, "家庭看護工" = col_care)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.2)), limits = c(0, 16)) +
  labs(title = "🚨 累計失聯率", x = NULL, y = "%")

# 組合
combined <- (p1 | p2 | p3 | p4) +
  plot_annotation(
    title    = "產業移工 vs 家庭看護工：制度性待遇落差與失聯率",
    subtitle = paste0(
      "家庭看護工月薪僅產業移工的 ",
      round(home_salary_total / biz_salary_total * 100), "%，",
      "每日多工作 ", round(home_daily_hours - biz_daily_hours, 1), " 小時，",
      "每月休假少 ", round(biz_leave_days - home_leave_days, 1), " 天"
    ),
    caption  = "薪資/工時/休假：勞動部113年移工管理及運用調查（事業面N=4,538；家庭面N=4,016，加權）\n失聯率：移民署2025年底累計統計｜藍色＝受勞基法保障之產業移工；紅色＝不受勞基法保障之家庭看護工",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 18, color = "#1a1a2e",
                                   family = "Microsoft JhengHei"),
      plot.subtitle = element_text(color = "grey30", size = 13,
                                   family = "Microsoft JhengHei"),
      plot.caption  = element_text(color = "grey50", size = 10,
                                   family = "Microsoft JhengHei")
    )
  )

ggsave(file.path(root, "plots/G_industry_vs_caregiver.png"), combined,
       width = 16, height = 6.5, dpi = 200, bg = "white")
cat("\n圖G saved: plots/G_industry_vs_caregiver.png\n")

# ══════════════════════════════════════════════
# 第二張圖：勞基法保障對照表
# ══════════════════════════════════════════════

law_df <- tibble(
  項目 = factor(c("法定工時上限", "強制加班費", "每週至少一日休假",
                  "勞保/健保/勞退", "轉換雇主權利"),
                levels = rev(c("法定工時上限", "強制加班費", "每週至少一日休假",
                               "勞保/健保/勞退", "轉換雇主權利"))),
  產業移工 = c(1, 1, 1, 1, 0.7),
  家庭看護工 = c(0, 0, 0, 0.5, 0.3)
) %>%
  pivot_longer(cols = c(產業移工, 家庭看護工),
               names_to = "類別", values_to = "保障程度") %>%
  mutate(
    label = case_when(
      保障程度 >= 0.8 ~ "✓ 有保障",
      保障程度 >= 0.5 ~ "△ 部分",
      保障程度 >= 0.3 ~ "△ 有限",
      TRUE ~ "✗ 無保障"
    ),
    label_color = case_when(
      保障程度 >= 0.8 ~ "#27AE60",
      保障程度 >= 0.5 ~ "#F39C12",
      保障程度 >= 0.3 ~ "#F39C12",
      TRUE ~ "#E74C3C"
    )
  )

p_law <- ggplot(law_df, aes(x = 類別, y = 項目)) +
  geom_tile(aes(fill = 保障程度), color = "white", linewidth = 2) +
  geom_text(aes(label = label), fontface = "bold", size = 4.5) +
  scale_fill_gradient2(low = "#E74C3C", mid = "#F39C12", high = "#27AE60",
                       midpoint = 0.5, limits = c(0, 1),
                       guide = "none") +
  labs(title = "勞動法規保障比較：家庭看護工是制度的「化外之民」",
       subtitle = "家庭看護工不適用《勞動基準法》，工時、休假、加班費皆無法律保障",
       x = NULL, y = NULL,
       caption = "綠色 = 有法律保障；橘色 = 部分保障；紅色 = 無法律保障") +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 13, face = "bold"))

ggsave(file.path(root, "plots/H_labor_law_comparison.png"), p_law,
       width = 10, height = 5.5, dpi = 200, bg = "white")
cat("圖H saved: plots/H_labor_law_comparison.png\n")
