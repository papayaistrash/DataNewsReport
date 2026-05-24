library(tidyverse)

d1 <- read_csv("data/103年針對移工問卷/data103_1.csv", show_col_types = FALSE)
d2 <- read_csv("data/103年針對移工問卷/data103_2.csv", show_col_types = FALSE)

# In d1 (business):
# q7_3 is whether have loan: 1=有, 2=沒有
# q7_3_1b is repayment method
cat("=== d1 repayment methods (weighted) ===\n")
d1 %>%
  filter(q7_3 == 1) %>% # has loan
  group_by(q7_3_1b) %>%
  summarise(
    count = n(),
    weighted_count = sum(w)
  ) %>%
  mutate(pct = weighted_count / sum(weighted_count) * 100) %>%
  print()

# In d2 (household):
# v7_3 is whether have loan: 1=有, 2=沒有
# v7_3_1b is repayment method
cat("\n=== d2 repayment methods (weighted) ===\n")
d2 %>%
  filter(v7_3 == 1) %>% # has loan
  group_by(v7_3_1b) %>%
  summarise(
    count = n(),
    weighted_count = sum(w)
  ) %>%
  mutate(pct = weighted_count / sum(weighted_count) * 100) %>%
  print()
