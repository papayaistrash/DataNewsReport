# 探索事業面資料的產業與國籍結構
library(tidyverse)

# 動態找到專案根目錄
root <- if (basename(getwd()) == "scripts") dirname(getwd()) else getwd()

# 讀取事業面 113 年資料
biz <- read_csv(file.path(root, "data/113年移工/事業面/data113.csv"),
                locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

cat("=== 事業面資料 ===\n")
cat("總列數:", nrow(biz), "\n")
cat("欄位名:", paste(names(biz), collapse = ", "), "\n\n")

cat("=== industry 產業類別分布 ===\n")
print(table(biz$industry, useNA = "ifany"))

cat("\n=== scale 規模 ===\n")
print(table(biz$scale, useNA = "ifany"))

# 檢查國籍相關欄位 - q3c 系列（僱用的移工國籍）
cat("\n=== Q3C 國籍欄位（有僱用哪些國籍）===\n")
for (col in paste0("q3c_", 1:8)) {
  if (col %in% names(biz)) {
    cat(col, ": 有=", sum(biz[[col]] == 1, na.rm = TRUE),
        ", 無=", sum(biz[[col]] == 0, na.rm = TRUE), "\n")
  }
}

# 檢查國籍相關欄位 - q3e 系列
cat("\n=== Q3E 國籍欄位 ===\n")
for (col in paste0("q3e_", 1:8)) {
  if (col %in% names(biz)) {
    cat(col, ": 有=", sum(biz[[col]] == 1, na.rm = TRUE),
        ", 無=", sum(biz[[col]] == 0, na.rm = TRUE), "\n")
  }
}

# 看看 q4 相關欄位（失聯相關）
cat("\n=== Q4 欄位（失聯） ===\n")
if ("q4" %in% names(biz)) print(table(biz$q4, useNA = "ifany"))

# 讀取家庭面資料
home <- read_csv(file.path(root, "data/113年移工/家庭面/data113.csv"),
                 locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

cat("\n\n=== 家庭面資料 ===\n")
cat("總列數:", nrow(home), "\n")

# 國籍 q8d
cat("\n=== Q8D 國籍分布（家庭面）===\n")
print(table(home$q8d, useNA = "ifany"))

# 失聯 q5
cat("\n=== Q5 是否有失聯經驗（家庭面）===\n")
print(table(home$q5, useNA = "ifany"))

# 產業交叉 - 事業面的 industry × q3c（國籍）
cat("\n\n=== 事業面: industry × 國籍交叉 ===\n")
cat("（q3c_1=印尼, q3c_2=馬來西亞, q3c_3=菲律賓, q3c_4=泰國, q3c_5=越南）\n")
for (nat_col in c("q3c_1", "q3c_3", "q3c_4", "q3c_5")) {
  cat("\n---", nat_col, "---\n")
  sub <- biz %>% filter(.data[[nat_col]] == 1)
  cat("有此國籍移工的事業數:", nrow(sub), "\n")
  print(table(sub$industry, useNA = "ifany"))
}

# 事業面 失聯 × 國籍
cat("\n\n=== 事業面: 失聯(q4) × 國籍 ===\n")
for (nat_col in c("q3c_1", "q3c_3", "q3c_4", "q3c_5")) {
  cat("\n---", nat_col, "---\n")
  sub <- biz %>% filter(.data[[nat_col]] == 1)
  cat("有失聯(q4=1):", sum(sub$q4 == 1, na.rm = TRUE), 
      " / 總數:", nrow(sub), 
      " = ", round(sum(sub$q4 == 1, na.rm = TRUE) / nrow(sub) * 100, 1), "%\n")
}
