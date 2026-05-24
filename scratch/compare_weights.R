library(tidyverse)
library(haven)

root <- if (basename(getwd()) == "scripts") dirname(getwd()) else getwd()
biz_sav <- read_sav(file.path(root, "data/113年移工/事業面/data113.sav"))

rates_w1 <- biz_sav %>%
  mutate(
    runaway = ifelse(q4 == 2, 1, 0),
    ind_name = as_factor(industry),
    scale_name = as_factor(scale)
  ) %>%
  filter(!is.na(runaway), !is.na(ind_name), !is.na(scale_name)) %>%
  group_by(scale_name) %>%
  summarise(
    n = n(),
    rate_w1 = sum(w1[runaway == 1], na.rm = TRUE) / sum(w1, na.rm = TRUE) * 100,
    rate_w3 = sum(w3[runaway == 1], na.rm = TRUE) / sum(w3, na.rm = TRUE) * 100,
    .groups = "drop"
  )

print(rates_w1)
