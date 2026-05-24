library(tidyverse)
library(haven)

root <- if (basename(getwd()) == "scripts") dirname(getwd()) else getwd()
biz_sav <- read_sav(file.path(root, "data/113年移工/事業面/data113.sav"))

cat("=== scale 的值與標籤 ===\n")
print(attr(biz_sav$scale, "labels"))
print(table(biz_sav$scale, useNA = "ifany"))
