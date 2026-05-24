library(haven)
library(tidyverse)

root <- if (basename(getwd()) == "scripts") dirname(getwd()) else getwd()
sav_path <- file.path(root, "data/113年移工/事業面/data113.sav")

if (file.exists(sav_path)) {
  biz_sav <- read_sav(sav_path)
  
  # Get all variables and labels
  for (name in names(biz_sav)) {
    lbl <- attr(biz_sav[[name]], "label")
    if (!is.null(lbl)) {
      if (str_detect(lbl, "印尼|越南|泰國|菲律賓|國籍|人數|外籍|失聯|行蹤不明")) {
        cat(name, ": ", lbl, "\n")
      }
    }
  }
}
