# 進一步理解事業面的國籍與產業結構
library(tidyverse)

root <- if (basename(getwd()) == "scripts") dirname(getwd()) else getwd()

biz <- read_csv(file.path(root, "data/113年移工/事業面/data113.csv"),
                locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

# q3e 看起來是目前僱用的國籍（數量較大）
# q3c 看起來是新引進的國籍
# q3e: 1=印尼 2=馬來西亞 3=菲律賓 4=泰國 5=越南 6=? 7=? 8=?

cat("=== q3e 系列 (目前僱用的國籍) ===\n")
cat("q3e_1 (印尼?): ", sum(biz$q3e_1 == 1, na.rm=TRUE), "\n")
cat("q3e_2 (馬來西亞?): ", sum(biz$q3e_2 == 1, na.rm=TRUE), "\n")
cat("q3e_3 (菲律賓?): ", sum(biz$q3e_3 == 1, na.rm=TRUE), "\n")
cat("q3e_4 (泰國?): ", sum(biz$q3e_4 == 1, na.rm=TRUE), "\n")
cat("q3e_5 (越南?): ", sum(biz$q3e_5 == 1, na.rm=TRUE), "\n")
cat("q3e_6: ", sum(biz$q3e_6 == 1, na.rm=TRUE), "\n")
cat("q3e_7: ", sum(biz$q3e_7 == 1, na.rm=TRUE), "\n")
cat("q3e_8: ", sum(biz$q3e_8 == 1, na.rm=TRUE), "\n")

# 以 q3e 做產業×國籍交叉
cat("\n\n=== 產業×國籍交叉（使用 q3e 目前僱用）===\n")

# industry 編碼對照：
# 根據常見分類：1=食品, 17=金屬製品, 21=電子零組件 等
# 但我們最需要知道的是大分類

# 先看 q3e_4 (泰國) × industry
cat("\n--- q3e_4 (泰國) × industry ---\n")
thai_biz <- biz %>% filter(q3e_4 == 1)
cat("有泰國移工的事業數:", nrow(thai_biz), "\n")
print(table(thai_biz$industry))

# q3e_1 (印尼) × industry
cat("\n--- q3e_1 (印尼) × industry ---\n")
indo_biz <- biz %>% filter(q3e_1 == 1)
cat("有印尼移工的事業數:", nrow(indo_biz), "\n")
print(table(indo_biz$industry))

# q3e_6 (可能是其他國) × industry
cat("\n--- q3e_6 × industry ---\n")
nat6_biz <- biz %>% filter(q3e_6 == 1)
cat("有q3e_6移工的事業數:", nrow(nat6_biz), "\n")
print(table(nat6_biz$industry))

# 事業面的失聯(q4)交叉國籍
cat("\n\n=== q4(失聯) × q3e 國籍（加權）===\n")
for (i in 1:8) {
  col <- paste0("q3e_", i)
  sub <- biz %>% filter(.data[[col]] == 1)
  n_total <- nrow(sub)
  n_lost <- sum(sub$q4 == 1, na.rm = TRUE)
  # 加權
  w_total <- sum(sub$w3, na.rm = TRUE)
  w_lost <- sum(sub$w3[sub$q4 == 1], na.rm = TRUE)
  cat(col, ": 有失聯=", n_lost, "/", n_total,
      " (未加權", round(n_lost/n_total*100, 1), "%)",
      " 加權率=", round(w_lost/w_total*100, 1), "%\n")
}

# 看看 q4a 系列 - 失聯原因
cat("\n=== q4a 失聯原因 (可能欄位) ===\n")
for (i in 1:9) {
  col <- paste0("q4a_", i)
  if (col %in% names(biz)) {
    lost <- biz %>% filter(q4 == 1)
    cat(col, ":", sum(lost[[col]] == 1, na.rm=TRUE), "/", nrow(lost), "\n")
  }
}

# 確認 industry 碼對照
cat("\n=== Industry 總分布（帶權重 w3）===\n")
ind_summary <- biz %>%
  group_by(industry) %>%
  summarise(
    n = n(),
    w = sum(w3, na.rm=TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(w))
print(ind_summary, n=30)

# 看看事業面 q4b 系列 - 失聯移工國籍
cat("\n=== q4b (失聯移工的國籍) ===\n")
if ("q4b" %in% names(biz)) {
  print(table(biz$q4b, useNA="ifany"))
}

cat("\n=== q4bb 系列 (失聯移工國籍明細) ===\n")
for (i in 1:10) {
  col <- paste0("q4bb_", i)
  if (col %in% names(biz)) {
    cat(col, ":", sum(biz[[col]] == 1, na.rm=TRUE), "\n")
  }
}
