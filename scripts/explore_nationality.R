# 確認國籍編碼 - 透過交叉比對家庭面的已知國籍欄位
library(tidyverse)

root <- if (basename(getwd()) == "scripts") dirname(getwd()) else getwd()

# 家庭面 q8d: 1=印尼, 2=菲律賓, 3=泰國, 4=越南 (from main report)
home <- read_csv(file.path(root, "data/113年移工/家庭面/data113.csv"),
                 locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

cat("=== 家庭面 Q8D 國籍 (1=印尼 2=菲律賓 3=泰國 4=越南) ===\n")
cat("印尼(1):", sum(home$q8d == 1, na.rm=TRUE), "\n")
cat("菲律賓(2):", sum(home$q8d == 2, na.rm=TRUE), "\n")
cat("泰國(3):", sum(home$q8d == 3, na.rm=TRUE), "\n")
cat("越南(4):", sum(home$q8d == 4, na.rm=TRUE), "\n")

# 家庭面是全部家庭看護工，看泰國人有多少
cat("\n=== 泰國家庭看護工詳情 ===\n")
thai_home <- home %>% filter(q8d == 3)
cat("泰國家庭看護工樣本數:", nrow(thai_home), "\n")
cat("占家庭面總樣本:", round(nrow(thai_home)/nrow(home)*100, 2), "%\n")
cat("（加權）泰國看護工:", sum(thai_home$w, na.rm=TRUE), "\n")
cat("（加權）全部看護工:", sum(home$w, na.rm=TRUE), "\n")
cat("加權占比:", round(sum(thai_home$w, na.rm=TRUE)/sum(home$w, na.rm=TRUE)*100, 2), "%\n")

# 各國籍加權人數
cat("\n=== 各國籍家庭看護工加權人數 ===\n")
nat_summary <- home %>%
  mutate(國籍 = factor(q8d, levels=1:4, labels=c("印尼","菲律賓","泰國","越南"))) %>%
  group_by(國籍) %>%
  summarise(
    樣本數 = n(),
    加權人數 = sum(w, na.rm=TRUE),
    .groups = "drop"
  ) %>%
  mutate(加權占比 = round(加權人數/sum(加權人數)*100, 1))
print(nat_summary)

# 家庭面失聯 × 國籍
cat("\n=== 家庭面 失聯(q5=2) × 國籍 ===\n")
nat_lost <- home %>%
  mutate(國籍 = factor(q8d, levels=1:4, labels=c("印尼","菲律賓","泰國","越南"))) %>%
  group_by(國籍) %>%
  summarise(
    total_n = n(),
    lost_n = sum(q5 == 2, na.rm=TRUE),
    total_w = sum(w, na.rm=TRUE),
    lost_w = sum(w[q5 == 2], na.rm=TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    未加權失聯率 = round(lost_n/total_n*100, 1),
    加權失聯率 = round(lost_w/total_w*100, 1)
  )
print(nat_lost)

# 事業面：確認 q3e 國籍
# 根據問卷，事業面的國籍分類可能是：
# q3e_1=印尼 q3e_2=馬來西亞 q3e_3=菲律賓 q3e_4=泰國 q3e_5=越南 q3e_6=其他
# 但讓我們用 q4bb (失聯移工國籍) 來推斷

biz <- read_csv(file.path(root, "data/113年移工/事業面/data113.csv"),
                locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

cat("\n\n=== 事業面 q4bb 失聯移工國籍明細（有失聯經驗的事業）===\n")
lost_biz <- biz %>% filter(q4 == 1)
cat("有失聯經驗的事業數:", nrow(lost_biz), "\n")
cat("有進一步填答國籍的(q4b不是NA):", sum(!is.na(lost_biz$q4b), na.rm=TRUE), "\n")

# q4bb 系列
for (i in 1:10) {
  col <- paste0("q4bb_", i)
  if (col %in% names(biz)) {
    cat(col, ": ", sum(lost_biz[[col]] == 1, na.rm=TRUE), "\n")
  }
}

# 事業面：泰國移工的產業分布（以 q3e_4 或其他方式）
# 讓我們看一下事業面的 industry 編碼解釋
# 基於 113年問卷：industry 17 = 金屬製品 (最大宗)，21 = 電子零組件
# 大致分組：
# 製造業大類: 1-16, 17-20 (各種製造)
# 營造業: 21? 或者更高的數字
# 但讓我們從加權數來推測

cat("\n\n=== 事業面泰國(q3e_4=1)的 industry 分布 (加權) ===\n")
thai_biz <- biz %>% filter(q3e_4 == 1)
thai_ind <- thai_biz %>%
  group_by(industry) %>%
  summarise(n = n(), w = sum(w3, na.rm=TRUE), .groups = "drop") %>%
  mutate(pct = round(w/sum(w)*100, 1)) %>%
  arrange(desc(w))
print(thai_ind, n=30)

cat("\n=== 全部事業面 industry 分布 (加權) ===\n")
all_ind <- biz %>%
  group_by(industry) %>%
  summarise(n = n(), w = sum(w3, na.rm=TRUE), .groups = "drop") %>%
  mutate(pct = round(w/sum(w)*100, 1)) %>%
  arrange(desc(w))
print(all_ind, n=30)

# 看所有年份是否有事業面資料
cat("\n=== 各年度事業面資料 ===\n")
for (yr in c("109", "110", "111", "112", "113")) {
  path <- file.path(root, paste0("data/", yr, "年移工/事業面/data", yr, ".csv"))
  if (file.exists(path)) {
    d <- read_csv(path, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
    cat(yr, "年: ", nrow(d), "列, ", ncol(d), "欄\n")
  } else {
    cat(yr, "年: 找不到CSV\n")
  }
}
