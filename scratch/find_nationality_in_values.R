library(haven)
library(tidyverse)

root <- if (basename(getwd()) == "scripts") dirname(getwd()) else getwd()
biz_sav <- read_sav(file.path(root, "data/113年移工/事業面/data113.sav"))

cat("=== 尋找值標籤中含有國籍名字的變數 ===\n")
found_val <- FALSE
for (name in names(biz_sav)) {
  lbls <- attr(biz_sav[[name]], "labels")
  if (!is.null(lbls)) {
    lbl_names <- names(lbls)
    if (any(str_detect(lbl_names, "印尼|越南|泰國|菲律賓|馬來西亞"))) {
      cat("變數:", name, "\n")
      cat("標籤:", attr(biz_sav[[name]], "label"), "\n")
      cat("值標籤:\n")
      print(lbls)
      cat("-----------------------------------\n")
      found_val <- TRUE
    }
  }
}

if (!found_val) {
  cat("沒有任何變數的值標籤包含印尼、越南、泰國、菲律賓等字眼。\n")
}
