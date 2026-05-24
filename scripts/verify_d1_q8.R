library(tidyverse)

d1 <- read_csv("data/103年針對移工問卷/data103_1.csv", show_col_types = FALSE)

cat("=== q8 table in d1 ===\n")
print(table(d1$q8, useNA="ifany"))

# Weighted percentages for those with q8 == 2 (has other deductions)
cat("\nPercentages within those who have deductions in d1 (q8 == 2):\n")
d1 %>%
  filter(q8 == 2) %>%
  summarise(
    total_w = sum(w),
    save_money = sum(w[q8_11 == 1]) / sum(w) * 100,
    deposit = sum(w[q8_12 == 2]) / sum(w) * 100,
    return_fee = sum(w[q8_13 == 3]) / sum(w) * 100,
    loan = sum(w[q8_14 == 4]) / sum(w) * 100,
    tax = sum(w[q8_15 == 5]) / sum(w) * 100,
    other = sum(w[q8_16 == 6]) / sum(w) * 100
  ) %>%
  print()
