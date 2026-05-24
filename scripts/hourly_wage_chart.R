# ══════════════════════════════════════════════════════════════
# 平均時薪比較 + 累計失聯率（報導者風格）
# ══════════════════════════════════════════════════════════════
library(tidyverse)
library(scales)

root <- if (basename(getwd()) == "scripts") dirname(getwd()) else getwd()

# ── 讀取資料 ──
home <- read_csv(file.path(root, "data/113年移工/家庭面/data113.csv"),
  locale = locale(encoding = "UTF-8"), show_col_types = FALSE
)
biz <- read_csv(file.path(root, "data/113年移工/事業面/data113.csv"),
  locale = locale(encoding = "UTF-8"), show_col_types = FALSE
)

# ══════════════════════════════════════════════
# 計算時薪
# ══════════════════════════════════════════════

# --- 產業移工 ---
biz_w <- biz$w3

biz_salary_base  <- weighted.mean(biz$q7ca, biz_w, na.rm = TRUE)
biz_overtime_pay <- weighted.mean(biz$q7cb, biz_w, na.rm = TRUE)
biz_bonus        <- weighted.mean(biz$nq7d, biz_w, na.rm = TRUE)
biz_total_salary <- biz_salary_base + biz_overtime_pay + biz_bonus

biz_monthly_hours <- weighted.mean(biz$q7b, biz_w, na.rm = TRUE)
biz_ot_hours      <- weighted.mean(biz$q7bb, biz_w, na.rm = TRUE)
biz_total_hours   <- biz_monthly_hours + biz_ot_hours

biz_hourly <- biz_total_salary / biz_total_hours

# --- 家庭看護工 ---
home_w <- home$w

home_salary_base  <- weighted.mean(as.numeric(home$nq10a), home_w, na.rm = TRUE)
home_overtime_pay <- weighted.mean(as.numeric(home$nq10ab), home_w, na.rm = TRUE)
home_total_salary <- home_salary_base + home_overtime_pay

home_calc <- home %>%
  mutate(
    start = as.numeric(nq9a_1),
    end = as.numeric(nq9a_2) + 12,
    rest_h = as.numeric(nq9b_1),
    rest_m = as.numeric(nq9b_2),
    daily_work = (end - start) - (rest_h + rest_m / 60)
  ) %>%
  filter(daily_work > 0 & daily_work < 24)
home_daily_hours <- weighted.mean(home_calc$daily_work, home_calc$w, na.rm = TRUE)

home_leave <- home %>%
  mutate(leave_days = case_when(
    q13a == 1 ~ 4.5, q13a == 2 ~ 1, q13a == 3 ~ 2.5, q13a == 5 ~ 0,
    TRUE ~ NA_real_
  )) %>%
  filter(!is.na(leave_days))
home_leave_days <- weighted.mean(home_leave$leave_days, home_leave$w, na.rm = TRUE)
home_work_days  <- 30 - home_leave_days

home_total_hours <- home_daily_hours * home_work_days
home_hourly <- home_total_salary / home_total_hours

# ── 確認 ──
cat("產業移工 時薪:", round(biz_hourly, 1), "元\n")
cat("家庭看護工 時薪:", round(home_hourly, 1), "元\n")

# ══════════════════════════════════════════════
# 繪圖：報導者風格
# ══════════════════════════════════════════════

min_hourly <- 183

# 報導者配色
col_biz  <- "#2D6A8F"
col_care <- "#C0392B"

plot_df <- tibble(
  類別 = factor(c("產業移工", "家庭看護工"),
    levels = c("產業移工", "家庭看護工")
  ),
  時薪 = c(biz_hourly, home_hourly),
  失聯率 = c(10.5, 12.2),
  法規 = c("受《勞基法》保障", "不適用《勞基法》")
)

p <- ggplot(plot_df, aes(x = 類別, y = 時薪)) +

  # ── 柱體 ──
  geom_col(aes(fill = 類別), width = 0.52, show.legend = FALSE) +

  # ── 基本時薪參考線 ──
  geom_hline(yintercept = min_hourly, color = "#aaaaaa",
             linewidth = 0.45, linetype = "longdash") +
  annotate("text", x = 2.42, y = min_hourly,
           label = paste0("基本時薪 ", min_hourly, " 元"),
           color = "#999999", size = 3.5, hjust = 1, vjust = -0.7,
           family = "Microsoft JhengHei") +

  # ── 時薪數值（柱頂） ──
  geom_text(aes(label = paste0(round(時薪, 1))),
            vjust = -1.6, size = 8, fontface = "bold", color = "#1a1a1a",
            family = "Microsoft JhengHei") +
  geom_text(aes(label = "元/每小時"),
            vjust = -0.4, size = 3.8, color = "#777777",
            family = "Microsoft JhengHei") +

  # ── 失聯率標籤（柱內白底標籤） ──
  geom_label(aes(y = 30, label = paste0("失聯率 ", 失聯率, "%")),
             fill = "white", label.size = 0, alpha = 0.85,
             size = 4.2, fontface = "bold", color = "#333333",
             family = "Microsoft JhengHei") +

  # ── 法規標注（柱內底部） ──
  geom_text(aes(y = 10, label = 法規),
            size = 3.3, color = "white",
            family = "Microsoft JhengHei") +

  # ── 配色 ──
  scale_fill_manual(values = c("產業移工" = col_biz,
                                "家庭看護工" = col_care)) +
  # ── Y 軸 ──
  scale_y_continuous(
    limits = c(0, 275),
    breaks = seq(0, 250, 50),
    labels = function(x) ifelse(x == 0, "0", x),
    expand = expansion(mult = c(0, 0))
  ) +

  # ── 標題 ──
  labs(
    title = "家庭看護工的實質時薪，不到產業移工的一半",
    subtitle = paste0(
      "以月薪、每日工時、每月休假天數推估，",
      "家庭看護工平均時薪僅 ", round(home_hourly, 1), " 元，",
      "遠低於法定基本時薪 ", min_hourly, " 元"
    ),
    x = NULL, y = NULL,
    caption = "時薪＝月總薪資÷月總工時　資料來源：勞動部113年移工管理及運用調查（加權）　失聯率：移民署累計統計"
  ) +

  # ── 報導者主題 ──
  theme_minimal(base_family = "Microsoft JhengHei", base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold", size = 20, color = "#1a1a1a",
      margin = margin(b = 4)
    ),
    plot.subtitle = element_text(
      size = 12.5, color = "#555555", lineheight = 1.4,
      margin = margin(b = 22)
    ),
    plot.caption = element_text(
      size = 9, color = "#aaaaaa", hjust = 0,
      margin = margin(t = 16)
    ),
    axis.text.x = element_text(size = 15, face = "bold", color = "#1a1a1a"),
    axis.text.y = element_text(size = 10.5, color = "#bbbbbb"),
    axis.ticks = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#eeeeee", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 20, r = 20, b = 12, l = 12)
  )

ggsave(file.path(root, "plots/I_hourly_wage_vs_lost.png"), p,
  width = 9, height = 7.5, dpi = 200, bg = "white"
)
cat("\n圖I saved: plots/I_hourly_wage_vs_lost.png\n")
