library(tidyverse)

df_vars <- read_csv("scratch/biz_variables.csv")

# 列出第 101 到 140 個變數
df_vars %>%
  slice(101:140) %>%
  pwalk(function(variable, label) {
    cat(variable, " : ", label, "\n")
  })
