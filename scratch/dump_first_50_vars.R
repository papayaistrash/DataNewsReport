library(tidyverse)

df_vars <- read_csv("scratch/biz_variables.csv")
cat("=== 欄位數:", nrow(df_vars), "===\n")

# 列出第 1 到 50 個變數
df_vars %>%
  slice(1:50) %>%
  pwalk(function(variable, label) {
    cat(variable, " : ", label, "\n")
  })
