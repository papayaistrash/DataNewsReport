library(tidyverse)

d2 <- read_csv("data/103年針對移工問卷/data103_2.csv", show_col_types = FALSE)

# s1 is nation: 1=印尼, 2=泰國, 3=菲律賓, 4=越南
# v7_1 is home country total fee
# v7_2 is whether know home agency fee: 1=不知道, 2=知道
# v7_2_1 is home agency fee amount
# v7_3 is whether have loan: 1=有, 2=沒有
# v7_3_1 is whether know loan amount: 1=不知道, 2= know
# v7_3_1a is loan amount
# v7_3_1b is repayment method: 1=自行辦理, 2=雇主協助辦理, 3=臺灣仲介由薪資中扣除, 4=其他
# w is weight

cat("=== v7_1 summary by nation ===\n")
d2 %>%
  group_by(s1) %>%
  summarise(
    n = n(),
    mean_w = sum(w),
    na_count = sum(is.na(v7_1)),
    max_val = max(v7_1, na.rm = TRUE),
    min_val = min(v7_1, na.rm = TRUE)
  ) %>%
  print()

# Check v7_1 categories
# Note: In 103 survey report:
# "沒有負擔費用": v7_1 == 0 ? or is there a specific code?
# "不知道/拒答": v7_1 in c(999998, 999999, 9999998, 9999999, etc.)?
# Let's write a categorizing logic:
d2_clean <- d2 %>%
  mutate(
    fee_cat = case_when(
      v7_1 >= 999998 ~ "不知道/拒答",
      v7_1 == 0 ~ "沒有負擔費用",
      v7_1 < 30000 ~ "未滿30,000元",
      v7_1 >= 30000 & v7_1 < 50000 ~ "30,000～49,999元",
      v7_1 >= 50000 & v7_1 < 70000 ~ "50,000～69,999元",
      v7_1 >= 70000 & v7_1 < 90000 ~ "70,000～89,999元",
      v7_1 >= 90000 ~ "90,000元以上",
      TRUE ~ "其他"
    )
  )

cat("\n=== Weighted fee categories overall ===\n")
d2_clean %>%
  group_by(fee_cat) %>%
  summarise(
    count = n(),
    weighted_count = sum(w)
  ) %>%
  mutate(pct = weighted_count / sum(weighted_count) * 100) %>%
  print()

cat("\n=== Weighted fee categories by nation ===\n")
d2_clean %>%
  group_by(s1, fee_cat) %>%
  summarise(
    weighted_count = sum(w),
    .groups = "drop"
  ) %>%
  group_by(s1) %>%
  mutate(pct = weighted_count / sum(weighted_count) * 100) %>%
  filter(s1 == 1) %>% # Let's print Indonesian caregivers to compare
  print()
