# 產業移工 vs 家庭看護工：待遇與失聯率比較
library(tidyverse)
library(scales)
library(patchwork)

root <- if (basename(getwd()) == "scripts") dirname(getwd()) else getwd()

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

# ── 讀取資料 ──
home <- read_csv(file.path(root, "data/113年移工/家庭面/data113.csv"),
                 locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
biz  <- read_csv(file.path(root, "data/113年移工/事業面/data113.csv"),
                 locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

# ── 探索事業面薪資/工時欄位 ──
cat("=== 事業面薪資相關欄位 ===\n")
cat("nq6b (移工人數?):", head(biz$nq6b, 10), "\n")
cat("nq6c_1:", head(biz$nq6c_1, 10), "\n")
cat("nq6c_2:", head(biz$nq6c_2, 10), "\n")
cat("nq7d (薪資?):", summary(as.numeric(biz$nq7d)), "\n")
cat("nq7db:", summary(as.numeric(biz$nq7db)), "\n")
cat("nq7dc:", summary(as.numeric(biz$nq7dc)), "\n")

# q7a, q7b 工時相關
cat("\n=== 事業面工時欄位 ===\n")
cat("q7a (每日工時?):", table(biz$q7a, useNA="ifany")[1:min(10,length(table(biz$q7a)))], "\n")
cat("q7b:", table(biz$q7b, useNA="ifany")[1:min(10,length(table(biz$q7b)))], "\n")

# 事業面薪資
cat("\n=== 事業面薪資分布 ===\n")
cat("nq7d summary:\n")
print(summary(as.numeric(biz$nq7d)))
cat("nq7db summary:\n")
print(summary(as.numeric(biz$nq7db)))
cat("nq7dc summary:\n")
print(summary(as.numeric(biz$nq7dc)))

# q7ca, q7cb 加班費
cat("\n=== 事業面加班費 ===\n")
cat("q7ca:", table(biz$q7ca, useNA="ifany")[1:min(5,length(table(biz$q7ca)))], "\n")
cat("q7cb:", table(biz$q7cb, useNA="ifany")[1:min(5,length(table(biz$q7cb)))], "\n")

# 事業面休假 q8
cat("\n=== 事業面休假 ===\n")
cat("q8:", table(biz$q8, useNA="ifany"), "\n")

# 家庭面薪資/工時
cat("\n=== 家庭面薪資 ===\n")
cat("nq10a (月薪):\n")
print(summary(as.numeric(home$nq10a)))
cat("nq10ab (加班費):\n")
print(summary(as.numeric(home$nq10ab)))

cat("\n=== 家庭面工時 ===\n")
cat("nq9a_1 (起始):\n")
print(summary(as.numeric(home$nq9a_1)))
cat("nq9a_2 (結束):\n")
print(summary(as.numeric(home$nq9a_2)))

# 家庭面休假
cat("\n=== 家庭面休假 q13a ===\n")
print(table(home$q13a, useNA = "ifany"))

# 事業面失聯
cat("\n=== 事業面失聯 q4 ===\n")
print(table(biz$q4, useNA = "ifany"))
cat("加權失聯率:", round(sum(biz$w3[biz$q4==1], na.rm=TRUE) / sum(biz$w3, na.rm=TRUE) * 100, 1), "%\n")

# 家庭面失聯
cat("\n=== 家庭面失聯 q5 ===\n")
print(table(home$q5, useNA = "ifany"))
cat("加權失聯率:", round(sum(home$w[home$q5==2], na.rm=TRUE) / sum(home$w, na.rm=TRUE) * 100, 1), "%\n")
