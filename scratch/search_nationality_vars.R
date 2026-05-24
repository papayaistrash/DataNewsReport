library(tidyverse)

df_vars <- read_csv("scratch/biz_variables.csv")

cat("=== 尋找含有國籍關鍵字的變數 ===\n")
df_vars %>%
  filter(str_detect(label, "印尼|越南|泰國|菲律賓|國籍|越南|印|泰|菲|越")) %>%
  print(n = 100)

cat("\n=== 尋找含有人數或移工的變數 ===\n")
df_vars %>%
  filter(str_detect(label, "人數|移工")) %>%
  filter(!str_detect(variable, "^q3|^q4")) %>%
  print(n = 100)
