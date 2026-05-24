library(tidyverse)

d2 <- read_csv("data/103年針對移工問卷/data103_2.csv", show_col_types = FALSE)

# Let's inspect the columns starting with v8_
cat("=== summary of v8_ columns ===\n")
d2 %>% select(starts_with("v8_")) %>% summary() %>% print()

# Let's inspect the first 10 rows of v8_ columns
cat("\n=== head of v8_ columns ===\n")
d2 %>% select(starts_with("v8_")) %>% head(15) %>% print()
