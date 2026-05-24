library(tidyverse)

d2 <- read_csv("data/103年針對移工問卷/data103_2.csv", show_col_types = FALSE)

# Check table 30 columns: v8, v8_1 to v8_7
# In our previous column listing for d2, we had:
# "v8", "v8_1", "v8_2", "v8_3", "v8_4", "v8_5", "v8_6", "v8_7", "v8_o"
cat("=== v8 table in d2 ===\n")
print(table(d2$v8, useNA="ifany"))

# Let's count how many are 1 in v8_1 to v8_7 for those with v8 == 2 (has other deductions)
has_other <- d2 %>% filter(v8 == 2)
cat("\nTotal who had other deductions (v8 == 2):", nrow(has_other), " (weighted:", sum(has_other$w), ")\n")

# Let's summarize the other deductions:
v8_summary <- d2 %>%
  summarise(
    no_agency = sum(w[v8 == 3]),
    no_deductions = sum(w[v8 == 1]),
    has_deductions = sum(w[v8 == 2]),
    
    # of those who have deductions, what are the percentages?
    save_money = sum(w[v8 == 2 & v8_1 == 1]),
    deposit = sum(w[v8 == 2 & v8_2 == 1]),
    return_fee = sum(w[v8 == 2 & v8_3 == 1]),
    loan = sum(w[v8 == 2 & v8_4 == 1]),
    tax = sum(w[v8 == 2 & v8_5 == 1]),
    health = sum(w[v8 == 2 & v8_6 == 1]),
    other = sum(w[v8 == 2 & v8_7 == 1])
  ) %>%
  mutate(across(everything(), ~ . / sum(d2$w) * 100)) %>%
  print()

cat("\nPercentages within those who have deductions:\n")
d2 %>%
  filter(v8 == 2) %>%
  summarise(
    save_money = sum(w[v8_1 == 1]) / sum(w) * 100,
    deposit = sum(w[v8_2 == 1]) / sum(w) * 100,
    return_fee = sum(w[v8_3 == 1]) / sum(w) * 100,
    loan = sum(w[v8_4 == 1]) / sum(w) * 100,
    tax = sum(w[v8_5 == 1]) / sum(w) * 100,
    health = sum(w[v8_6 == 1]) / sum(w) * 100,
    other = sum(w[v8_7 == 1]) / sum(w) * 100
  ) %>%
  print()
