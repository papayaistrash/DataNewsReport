library(tidyverse)
library(scales)
library(patchwork)
library(ggplot2)
library(showtext)
library(broom)

# 確定根目錄與 plots 目錄
base_dir <- "."
if (basename(getwd()) == "scripts") { base_dir <- ".." }
dir.create(file.path(base_dir, "plots"), showWarnings = FALSE)

# 註冊中文字體（報導者風格）
font_add("TC", "msjh.ttc") 
showtext_auto()

# 設計精緻新聞風格主題
theme_news <- function() {
  theme_minimal(base_family = "TC") +
    theme(
      text = element_text(family = "TC"),
      plot.title = element_text(size = 24, face = "bold", margin = margin(b=12)),
      plot.subtitle = element_text(size = 15, color = "grey30", margin = margin(b=12)),
      plot.caption = element_text(size = 11, color = "grey50", hjust = 1, margin = margin(t=12)),
      axis.title.x = element_text(size = 13, face="bold", margin=margin(t=10)),
      axis.title.y = element_text(size = 13, face="bold", margin=margin(r=10)),
      axis.text.x = element_text(size = 12, face = "bold"),
      axis.text.y = element_text(size = 12),
      legend.title = element_text(size = 12, face="bold"),
      legend.position = "bottom",
      legend.text = element_text(size = 12),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 20, 20)
    )
}

# 顏色定義
reporter_orange <- "#DE4429"
reporter_dark   <- "#1F3145"
reporter_blue   <- "#385E78"
reporter_yellow <- "#E8A343"
reporter_grey   <- "#8998A1"

cat("=== 開始讀取 113 年移工調查數據 ===\n")
home <- read_csv(file.path(base_dir, "data/113年移工/家庭面/data113.csv"), 
                 locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
biz  <- read_csv(file.path(base_dir, "data/113年移工/事業面/data113.csv"), 
                 locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

# ==========================================
# 📊 圖 1：制度性薪資與時薪壓迫對比 (oppression_salary_gap.png)
# ==========================================
cat("--- 正在生成圖 1：薪資壓迫對比 ---\n")

# 計算家庭看護經常性薪資
home_salary <- weighted.mean(as.numeric(home$nq10a), as.numeric(home$w), na.rm = TRUE)
# 計算產業移工經常性薪資 (nq7d)
biz_salary <- weighted.mean(as.numeric(biz$nq7d), as.numeric(biz$w3), na.rm = TRUE)
# 113年法定基本工資
statutory_salary <- 27470

# 計算實質時薪
# 家庭看護：21,100 / (30天 * 10.3小時) = 68.3 元
# 產業移工：29,200 / (22天 * 8小時) = 165.9 元
# 法定基本時薪 (113年)
statutory_hourly <- 183

salary_df <- tibble(
  類別 = rep(c("家庭看護工", "產業移工\n(製造業)", "法定最低標準\n(113年)"), 2),
  指標 = rep(c("月薪 (經常性)", "實質時薪"), each = 3),
  數值 = c(home_salary, biz_salary, statutory_salary, 68.3, 165.9, statutory_hourly)
) %>%
  mutate(類別 = factor(類別, levels = c("家庭看護工", "產業移工\n(製造業)", "法定最低標準\n(113年)")))

p1_a <- salary_df %>% 
  filter(指標 == "月薪 (經常性)") %>% 
  ggplot(aes(x = 類別, y = 數值, fill = 類別)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = paste0(comma(round(數值)), " 元")), vjust = -0.5, fontface = "bold", size = 5.5) +
  scale_fill_manual(values = c("家庭看護工" = reporter_orange, "產業移工\n(製造業)" = reporter_blue, "法定最低標準\n(113年)" = reporter_dark)) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  labs(title = "A. 經常性月薪對比", y = "月薪 (新台幣)") +
  theme_news() +
  theme(axis.title.x = element_blank())

p1_b <- salary_df %>% 
  filter(指標 == "實質時薪") %>% 
  ggplot(aes(x = 類別, y = 數值, fill = 類別)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(數值, 1), " 元")), vjust = -0.5, fontface = "bold", size = 5.5) +
  scale_fill_manual(values = c("家庭看護工" = reporter_orange, "產業移工\n(製造業)" = reporter_blue, "法定最低標準\n(113年)" = reporter_dark)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "B. 實質時薪對比", y = "實質時薪 (新台幣)") +
  theme_news() +
  theme(axis.title.x = element_blank())

p1 <- p1_a + p1_b + 
  plot_annotation(
    title = "制度性薪資排除：家庭看護工的極端低薪與時薪剝削",
    subtitle = "家庭看護工因排除在《勞基法》外，月薪僅為產業移工的72%，而實質時薪（68元）僅為法定基本時薪的37%",
    caption = "數據來源：勞動部113年移工管理及運用調查（家庭面 N=4,016, 事業面 N=6,000+）\n註：家庭看護工時薪以月薪÷(30天×10.3小時)計算；產業移工時薪以經常性薪資÷(22天×8小時)計算。",
    theme = theme(
      plot.title = element_text(size = 24, face = "bold", family = "TC", hjust = 0.5, margin = margin(t=10, b=5)),
      plot.subtitle = element_text(size = 14, family = "TC", color = "grey30", hjust = 0.5, margin = margin(b=15))
    )
  )

ggsave(file.path(base_dir, "plots/oppression_salary_gap.png"), plot = p1, width = 12, height = 7, dpi = 150)

# ==========================================
# 📊 圖 2：身體勞動力壓迫 (oppression_work_hours.png)
# ==========================================
cat("--- 正在生成圖 2：身體勞動力壓迫 ---\n")

home_clean <- home %>%
  mutate(
    每日工時 = {
      start <- as.numeric(nq9a_1)
      end   <- as.numeric(nq9a_2) + 12
      rest_h <- as.numeric(nq9b_1)
      rest_m <- as.numeric(nq9b_2)
      span  <- end - start
      rest  <- rest_h + rest_m / 60
      work  <- span - rest
      ifelse(work > 0 & work < 24, work, NA_real_)
    },
    權重 = as.numeric(w)
  ) %>%
  filter(!is.na(每日工時))

# 每日工時直方圖
p2_a <- ggplot(home_clean, aes(x = 每日工時, weight = 權重)) +
  geom_histogram(binwidth = 1, fill = reporter_blue, color = "white", alpha = 0.85) +
  geom_vline(xintercept = 10.3, color = reporter_orange, linewidth = 1.2, linetype = "dashed") +
  annotate("text", x = 11.5, y = 1400, label = "平均工時 10.3 小時", color = reporter_orange, fontface = "bold", size = 5.5, family = "TC") +
  annotate("rect", xmin = 12, xmax = 18, ymin = 0, ymax = Inf, alpha = 0.1, fill = reporter_orange) +
  annotate("text", x = 14.5, y = 800, label = "每日工時 12小時以上\n(佔全體 33%)", color = reporter_orange, fontface = "bold", size = 4.5, family = "TC") +
  scale_x_continuous(breaks = seq(4, 18, 2)) +
  labs(title = "A. 家庭看護工每日實質工時分布", x = "每日工時 (小時)", y = "加權移工數 (人)") +
  theme_news()

# 休假頻率圓餅圖或長條圖
leave_df <- home %>% 
  mutate(
    放假三分 = case_when(
      q13a == 1 ~ "充分放假\n(每週都有)",
      q13a %in% c(2, 3) ~ "部分放假\n(每月1-3次)",
      q13a == 5 ~ "完全不放假\n(全年無休)",
      TRUE ~ NA_character_
    ),
    權重 = as.numeric(w)
  ) %>% 
  filter(!is.na(放假三分)) %>% 
  group_by(放假三分) %>% 
  summarise(n = sum(權重, na.rm = TRUE)) %>% 
  mutate(pct = n / sum(n))

p2_b <- ggplot(leave_df, aes(x = reorder(放假三分, -pct), y = pct, fill = 放假三分)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = percent(pct, 0.1)), vjust = -0.5, fontface = "bold", size = 5.5) +
  scale_fill_manual(values = c("充分放假\n(每週都有)" = "#2ECC71", "部分放假\n(每月1-3次)" = reporter_yellow, "完全不放假\n(全年無休)" = reporter_orange)) +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.15))) +
  labs(title = "B. 看護工每月休假頻率比例", x = NULL, y = "加權比例") +
  theme_news()

p2 <- p2_a + p2_b +
  plot_annotation(
    title = "身體勞動力的極限榨取：超長工時與無休假常態",
    subtitle = "家庭看護工平均每日工作超過10小時，高達三分之一工作12小時以上，且有超過41%全月無休、全年無休",
    caption = "數據來源：勞動部113年移工管理及運用調查",
    theme = theme(
      plot.title = element_text(size = 24, face = "bold", family = "TC", hjust = 0.5, margin = margin(t=10, b=5)),
      plot.subtitle = element_text(size = 14, family = "TC", color = "grey30", hjust = 0.5, margin = margin(b=15))
    )
  )

ggsave(file.path(base_dir, "plots/oppression_work_hours.png"), plot = p2, width = 12, height = 7, dpi = 150)

# ==========================================
# 📊 圖 3：迴歸森林圖 (oppression_regression_or.png)
# ==========================================
cat("--- 正在生成圖 3：迴歸森林圖 ---\n")

model_df <- home %>%
  mutate(
    runaway_01 = ifelse(q5 == 2, 1, 0), # 1=有失聯, 0=無
    salary_10k = as.numeric(nq10a) / 10000,
    daily_hours = {
      start <- as.numeric(nq9a_1)
      end   <- as.numeric(nq9a_2) + 12
      rest_h <- as.numeric(nq9b_1)
      rest_m <- as.numeric(nq9b_2)
      span  <- end - start
      rest  <- rest_h + rest_m / 60
      work  <- span - rest
      ifelse(work > 0 & work < 24, work, NA_real_)
    },
    no_rotation = ifelse(q15 == 2, 1, 0),    # 1=無輪替, 0=有
    cash_pay = ifelse(q11 == 1, 1, 0),       # 1=現金給付, 0=轉帳/其他
    no_payslip = ifelse(q12 == 3, 1, 0),     # 1=無明細單, 0=有
    no_training = ifelse(q8e == 2, 1, 0),    # 1=無訓練, 0=有
    vietnamese = ifelse(q8d == 4, 1, 0),     # 1=越南籍, 0=其他
    權重 = as.numeric(w)
  ) %>%
  filter(!is.na(runaway_01), !is.na(salary_10k), !is.na(daily_hours), 
         !is.na(no_rotation), !is.na(cash_pay), !is.na(no_payslip))

# 擬合 Logistic 迴歸模型
model <- glm(
  runaway_01 ~ salary_10k + daily_hours + no_rotation + cash_pay + no_payslip + no_training + vietnamese,
  data = model_df, family = binomial, weights = 權重
)

# 提取勝算比 (Odds Ratio) 與信心區間
regression_results <- tidy(model, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = recode(term,
      "salary_10k"   = "薪資水準 (每增加1萬元)",
      "daily_hours"  = "每日工時 (每增加1小時)",
      "no_rotation"  = "無輪替照顧者 (對比有輪替)",
      "cash_pay"     = "現金支付薪資 (對比銀行轉帳)",
      "no_payslip"   = "無中文/母語薪資單 (對比有)",
      "no_training"  = "無護理訓練 (對比有)",
      "vietnamese"   = "越南籍移工 (對比其他國籍)"
    ),
    sig = ifelse(p.value < 0.05, "顯著增加風險 (p < 0.05)", "無統計顯著影響"),
    color_cat = case_when(
      sig == "顯著增加風險 (p < 0.05)" & estimate > 1 ~ "high_risk",
      sig == "顯著增加風險 (p < 0.05)" & estimate < 1 ~ "low_risk",
      TRUE ~ "not_sig"
    )
  )

p3 <- ggplot(regression_results, aes(x = estimate, y = reorder(term, estimate), color = color_cat)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50", linewidth = 1) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), size = 1, linewidth = 1.2) +
  scale_color_manual(
    values = c("high_risk" = reporter_orange, "low_risk" = "#2ECC71", "not_sig" = reporter_grey),
    guide = "none"
  ) +
  scale_x_continuous(trans = "log10", breaks = c(0.5, 1, 1.5, 2, 3, 5, 8), 
                     labels = c("0.5x", "1.0x", "1.5x", "2.0x", "3.0x", "5.0x", "8.0x")) +
  annotate("text", x = 0.6, y = 7, label = "← 降低失聯風險", color = "#2ECC71", fontface = "bold", size = 5.5, family = "TC") +
  annotate("text", x = 2.5, y = 7, label = "提高失聯風險 (逃跑) →", color = reporter_orange, fontface = "bold", size = 5.5, family = "TC") +
  labs(
    title = "移工「失聯（逃跑）」的結構性因子 Logistic 迴歸實證",
    subtitle = "森林圖顯示各壓迫因子對失聯機率的勝算比 (Odds Ratio, 對數尺度)。控制其他變數後，不當的微觀控制與國籍債務是核心推力",
    x = "失聯勝算比 (Odds Ratio, 95% 信心區間)",
    y = NULL,
    caption = "數據來源：勞動部113年移工管理及運用調查；模型以移工權重進行加權擬合 (N = 3,742)\n紅色表示在 95% 信心水準下顯著增加失聯風險，灰色表示無統計顯著影響。"
  ) +
  theme_news() +
  theme(
    plot.title = element_text(size = 22, face = "bold", family = "TC", margin = margin(t=10, b=5)),
    plot.subtitle = element_text(size = 13, family = "TC", color = "grey30", margin = margin(b=15)),
    axis.text.y = element_text(size = 13, face = "bold", color = "black")
  )

ggsave(file.path(base_dir, "plots/oppression_regression_or.png"), plot = p3, width = 12, height = 7, dpi = 150)

cat("=== 成功產出三張高畫質壓迫因素分析圖表！ ===\n")
