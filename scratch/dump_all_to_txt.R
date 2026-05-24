library(tidyverse)

df_vars <- read_csv("scratch/biz_variables.csv")
write_lines(
  paste0(df_vars$variable, " : ", df_vars$label),
  "scratch/all_variables.txt"
)
cat("寫入所有變數名稱及標籤至 scratch/all_variables.txt\n")
