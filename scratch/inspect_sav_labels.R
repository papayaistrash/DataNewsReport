# 讀取 SPSS 檔以取得正確的變數標籤與值標籤
library(haven)
library(tidyverse)

root <- if (basename(getwd()) == "scripts") dirname(getwd()) else getwd()
sav_path <- file.path(root, "data/113年移工/事業面/data113.sav")

if (file.exists(sav_path)) {
  cat("讀取 SPSS 檔案中...\n")
  biz_sav <- read_sav(sav_path)
  
  # 取得所有變數標籤
  var_labels <- map_chr(biz_sav, ~ attr(.x, "label") %||% NA_character_)
  df_vars <- tibble(
    variable = names(biz_sav),
    label = var_labels
  )
  
  # 寫出變數標籤
  write_csv(df_vars, "scratch/biz_variables.csv")
  cat("已將變數標籤寫入 scratch/biz_variables.csv\n")
  
  # 檢查 industry 的值標籤
  cat("\n=== industry 的值標籤 ===\n")
  print(attr(biz_sav$industry, "labels"))
  
  # 檢查 q3e_1 至 q3e_8 的值標籤
  cat("\n=== q3e_1 的值標籤 ===\n")
  print(attr(biz_sav$q3e_1, "labels"))
  
  # 檢查 q4 的值標籤
  cat("\n=== q4 的值標籤 ===\n")
  print(attr(biz_sav$q4, "labels"))
  
  # 檢查所有以 q3c_ 或 q3e_ 開頭的變數
  cat("\n=== q3e / q3c 相關變數標籤 ===\n")
  df_vars %>% 
    filter(str_detect(variable, "^q3[ce]|^q4")) %>%
    print(n = 40)
    
} else {
  cat("找不到 SPSS 檔案:", sav_path, "\n")
}
