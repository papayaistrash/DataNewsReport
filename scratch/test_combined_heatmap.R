library(tidyverse)
library(haven)

root <- if (basename(getwd()) == "scripts") dirname(getwd()) else getwd()
biz_sav <- read_sav(file.path(root, "data/113年移工/事業面/data113.sav"))
home_sav <- read_sav(file.path(root, "data/113年移工/家庭面/data113.sav"))

# 1. 整理家庭面數據 (家庭看護工)
home_clean <- home_sav %>%
  mutate(
    runaway = ifelse(q5 == 2, 1, 0),
    ind_name = "家庭看護工",
    scale_name = "1人"
  ) %>%
  filter(!is.na(runaway)) %>%
  group_by(ind_name, scale_name) %>%
  summarise(
    n = n(),
    w_total = sum(w, na.rm = TRUE),
    w_lost = sum(w[runaway == 1], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    rate = w_lost / w_total * 100
  )

# 2. 整合與分類事業面數據 (合併為 5 個製造業 + 1 個營建工程業，共 6 個大類)
biz_clean <- biz_sav %>%
  mutate(
    runaway = ifelse(q4 == 2, 1, 0),
    # 將 26 個子行業分類為 6 大類
    ind_name = case_when(
      industry %in% c(18, 19, 20) ~ "電子資訊與電力工業",
      industry %in% c(16, 17, 21, 22, 23) ~ "金屬與機械製造業",
      industry %in% c(9, 10, 11, 12, 13, 14, 15) ~ "化學與材料工業",
      industry == 26 ~ "營建工程業",
      industry %in% c(1, 2, 3, 4, 5, 6, 7, 8, 24, 25) ~ "民生與其他製造業",
      TRUE ~ "民生與其他製造業"
    ),
    scale_name = as.character(as_factor(scale))
  ) %>%
  filter(!is.na(runaway), !is.na(ind_name), !is.na(scale_name)) %>%
  group_by(ind_name, scale_name) %>%
  summarise(
    n = n(),
    w_total = sum(w1, na.rm = TRUE),
    w_lost = sum(w1[runaway == 1], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    rate = w_lost / w_total * 100
  )

# 3. 合併家庭與事業面
combined_data <- bind_rows(home_clean, biz_clean) %>%
  mutate(
    # 設定 X 軸因子層級 (新增 "1人")
    scale_name = factor(scale_name, levels = c("1人", "1～29人", "30～99人", "100～199人", "200～499人", "500人及以上")),
    # 計算各行業整體的加權失聯經驗率，用以 Y 軸排序 (家庭看護工獨立計算)
    overall_rate = case_when(
      ind_name == "家庭看護工" ~ 3.53,
      ind_name == "營建工程業" ~ 76.2,
      ind_name == "金屬與機械製造業" ~ 65.7,
      ind_name == "民生與其他製造業" ~ 59.8,
      ind_name == "化學與材料工業" ~ 51.2,
      ind_name == "電子資訊與電力工業" ~ 50.1,
      TRUE ~ 50.0
    )
  )

print(combined_data)
