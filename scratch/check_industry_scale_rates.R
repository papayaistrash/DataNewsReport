library(tidyverse)
library(haven)

root <- if (basename(getwd()) == "scripts") dirname(getwd()) else getwd()
biz_sav <- read_sav(file.path(root, "data/113年移工/事業面/data113.sav"))

# Calculate weighted runaway experience rate by industry and scale
cell_summary <- biz_sav %>%
  mutate(
    runaway = ifelse(q4 == 2, 1, 0),
    ind_name = as_factor(industry),
    scale_name = as_factor(scale)
  ) %>%
  filter(!is.na(runaway), !is.na(ind_name), !is.na(scale_name)) %>%
  group_by(ind_name, scale_name) %>%
  summarise(
    n = n(),
    w_total = sum(w3, na.rm = TRUE),
    w_lost = sum(w3[runaway == 1], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    rate = ifelse(w_total > 0, w_lost / w_total * 100, 0)
  )

print(cell_summary, n = 100)
