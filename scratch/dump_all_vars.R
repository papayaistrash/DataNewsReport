library(tidyverse)

df_vars <- read_csv("scratch/biz_variables.csv")
cat("=== 總欄位數:", nrow(df_vars), "===\n")

# 列出前 150 個變數
df_vars %>%
  slice(1:150) %>%
  pwalk(function(variable, label) {
    cat(variable, " : ", label, "\n")
  })
