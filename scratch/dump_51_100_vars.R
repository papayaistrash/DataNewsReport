library(tidyverse)

df_vars <- read_csv("scratch/biz_variables.csv")

# 列出第 51 到 100 個變數
df_vars %>%
  slice(51:100) %>%
  pwalk(function(variable, label) {
    cat(variable, " : ", label, "\n")
  })
