library(tidyverse)
library(haven)

root <- if (basename(getwd()) == "scripts") dirname(getwd()) else getwd()
home_sav <- read_sav(file.path(root, "data/113年移工/家庭面/data113.sav"))

cat("=== Home q5 labels ===\n")
print(attr(home_sav$q5, "labels"))
print(table(home_sav$q5, useNA = "ifany"))

cat("\n=== Home weights starting with w ===\n")
print(names(home_sav)[str_detect(names(home_sav), "^w")])

# Calculate weighted runaway experience rate for home caregivers
# Let's see what weights are in home_sav: typically "w" or similar.
cat("\n=== Home weights and mean ===\n")
print(summary(home_sav$w))

# Weighted runaway rate
home_rate <- home_sav %>%
  mutate(runaway = ifelse(q5 == 2, 1, 0)) %>%
  filter(!is.na(runaway)) %>%
  summarise(
    n = n(),
    w_total = sum(w, na.rm = TRUE),
    w_lost = sum(w[runaway == 1], na.rm = TRUE),
    rate = w_lost / w_total * 100
  )
print(home_rate)
