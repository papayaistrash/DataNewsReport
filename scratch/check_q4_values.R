library(tidyverse)
library(haven)

root <- if (basename(getwd()) == "scripts") dirname(getwd()) else getwd()

# Read CSV
biz_csv <- read_csv(file.path(root, "data/113年移工/事業面/data113.csv"), show_col_types = FALSE)
cat("=== CSV q4 values ===\n")
print(table(biz_csv$q4, useNA = "ifany"))

# Read SAV
biz_sav <- read_sav(file.path(root, "data/113年移工/事業面/data113.sav"))
cat("\n=== SAV q4 values ===\n")
print(table(biz_sav$q4, useNA = "ifany"))
cat("\n=== SAV q4 labels ===\n")
print(attr(biz_sav$q4, "labels"))
