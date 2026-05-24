library(tidyverse)

d1 <- read_csv("data/103年針對移工問卷/data103_1.csv", show_col_types = FALSE)

# nation is: 1=印尼, 2=泰國, 3=菲律賓, 4=越南
# q7_1$ is home country total fee
# w is weight

cat("=== nation counts in d1 ===\n")
d1 %>%
  group_by(nation) %>%
  summarise(
    count = n(),
    weighted_count = sum(w)
  ) %>%
  print()

# Check q7_1$ categories
d1_clean <- d1 %>%
  mutate(
    fee_cat = case_when(
      `q7_1$` >= 999998 ~ "不知道/拒答",
      `q7_1$` == 0 ~ "沒有負擔費用",
      `q7_1$` < 30000 ~ "未滿30,000元",
      `q7_1$` >= 30000 & `q7_1$` < 50000 ~ "30,000～49,999元",
      `q7_1$` >= 50000 & `q7_1$` < 70000 ~ "50,000～69,999元",
      `q7_1$` >= 70000 & `q7_1$` < 90000 ~ "70,000～89,999元",
      `q7_1$` >= 90000 ~ "90,000元以上",
      TRUE ~ "其他"
    )
  )

cat("\n=== Weighted fee categories overall in d1 ===\n")
d1_clean %>%
  group_by(fee_cat) %>%
  summarise(
    count = n(),
    weighted_count = sum(w)
  ) %>%
  mutate(pct = weighted_count / sum(weighted_count) * 100) %>%
  print()
