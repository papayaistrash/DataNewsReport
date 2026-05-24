library(tidyverse)

d1 <- read_csv("data/103年針對移工問卷/data103_1.csv", show_col_types = FALSE)
d2 <- read_csv("data/103年針對移工問卷/data103_2.csv", show_col_types = FALSE)

cat("=== d1 (Business) Recruitment Channels ===\n")
print(table(d1$q6, useNA="ifany"))

cat("\n=== d1 (Business) Home Country Total Fees (q7_1$) ===\n")
print(summary(d1$`q7_1$`))
print(head(d1 %>% select(nation, `q7_1$`, q7_2, `q7_21$`), 15))

cat("\n=== d2 (Household) Recruitment Channels ===\n")
print(table(d2$v6, useNA="ifany"))

cat("\n=== d2 (Household) Home Country Total Fees (v7_1) ===\n")
print(summary(d2$v7_1))
print(head(d2 %>% select(s1, v7_1, v7_2, v7_2_1, v7_3), 15))

cat("\n=== d1 Nations list ===\n")
print(table(d1$nation, useNA="ifany"))

cat("\n=== d2 Nations list (s1/s1.1) ===\n")
print(table(d2$s1, useNA="ifany"))
print(table(d2$`s1.1`, useNA="ifany"))
