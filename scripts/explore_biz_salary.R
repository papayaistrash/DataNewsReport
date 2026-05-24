# 進一步確認事業面薪資欄位含義
library(tidyverse)

root <- if (basename(getwd()) == "scripts") dirname(getwd()) else getwd()
biz  <- read_csv(file.path(root, "data/113年移工/事業面/data113.csv"),
                 locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

# nq7d 中位數=0 → 可能是「額外津貼」
# nq7db 中位數=5000 → 可能是「加班費」
# 看看基本薪資在哪裡

# 先看有沒有其他 q6/q7 欄位
cat("=== q7 相關欄位名 ===\n")
q7_cols <- grep("^q7|^nq7", names(biz), value = TRUE)
cat(paste(q7_cols, collapse = ", "), "\n\n")

for (col in q7_cols) {
  vals <- as.numeric(biz[[col]])
  cat(col, ": min=", min(vals,na.rm=T), " median=", median(vals,na.rm=T),
      " mean=", round(mean(vals,na.rm=T),0), " max=", max(vals,na.rm=T),
      " NA=", sum(is.na(vals)), "\n")
}

# q8 休假
cat("\n=== q8 (休假方式) ===\n")
print(table(biz$q8, useNA = "ifany"))

# q8a2, q8a3
cat("\n=== q8a2 (每月休假天數?) ===\n")
cat("非空值:", sum(!is.na(biz$q8a2) & biz$q8a2 != " ", na.rm=TRUE), "\n")
vals_8a2 <- as.numeric(trimws(biz$q8a2))
cat("有效值summary:\n")
print(summary(vals_8a2[!is.na(vals_8a2)]))

cat("\n=== q8a3 ===\n")
vals_8a3 <- as.numeric(trimws(biz$q8a3))
print(summary(vals_8a3[!is.na(vals_8a3)]))

# 工時 q7a, q7b
cat("\n=== q7a (工時制度?) ===\n")
print(table(biz$q7a, useNA = "ifany"))

cat("\n=== q7b (每日工時?) ===\n")
print(table(biz$q7b, useNA = "ifany"))

cat("\n=== q7bb (每日實際工時?) ===\n")
print(summary(as.numeric(biz$q7bb)))

# 家庭面工時計算
home <- read_csv(file.path(root, "data/113年移工/家庭面/data113.csv"),
                 locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

cat("\n=== 家庭面工時計算 ===\n")
home_hours <- home %>%
  mutate(
    start = as.numeric(nq9a_1),
    end   = as.numeric(nq9a_2) + 12,  # PM
    rest_h = as.numeric(nq9b_1),
    rest_m = as.numeric(nq9b_2),
    span  = end - start,
    rest  = rest_h + rest_m / 60,
    work  = span - rest
  ) %>%
  filter(work > 0 & work < 24)

cat("家庭面每日工時 summary:\n")
print(summary(home_hours$work))
cat("加權平均:", round(weighted.mean(home_hours$work, home_hours$w, na.rm=TRUE), 1), "小時\n")

# 事業面每日工時
cat("\n=== 事業面 q7b 分布 ===\n")
# q7b 可能是 1=8hr, 2=比8hr多... 
print(table(biz$q7b, useNA = "ifany"))

# 嘗試 q7bb
cat("\n=== 事業面 q7bb (可能是小時數) ===\n")
q7bb_vals <- as.numeric(trimws(biz$q7bb))
print(summary(q7bb_vals[!is.na(q7bb_vals) & q7bb_vals > 0]))

# 加權平均薪資 - 事業面
# nq7d = 獎金/津貼?, nq7db = 月薪/加班費?
# 看前幾筆完整資料
cat("\n=== 事業面前10筆薪資相關 ===\n")
biz %>%
  select(industry, nq7d, nq7db, nq7dc, q7ca, q7cb) %>%
  head(20) %>%
  print()
