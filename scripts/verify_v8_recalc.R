library(tidyverse)

d2 <- read_csv("data/103年針對移工問卷/data103_2.csv", show_col_types = FALSE)

cat("=== Percentages within those who have deductions (v8 == 2) ===\n")
d2 %>%
  filter(v8 == 2) %>%
  summarise(
    save_money = sum(w[v8_1 == 1]) / sum(w) * 100,
    deposit = sum(w[v8_2 == 2]) / sum(w) * 100,
    return_fee = sum(w[v8_3 == 3]) / sum(w) * 100,
    loan = sum(w[v8_4 == 4]) / sum(w) * 100,
    tax = sum(w[v8_5 == 5]) / sum(w) * 100,
    health = sum(w[v8_6 == 6]) / sum(w) * 100,
    other = sum(w[v8_7 == 7]) / sum(w) * 100
  ) %>%
  print()
