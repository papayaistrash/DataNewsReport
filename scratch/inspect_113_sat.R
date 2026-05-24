library(haven)

base_dir <- "."
if (basename(getwd()) == "scratch") { base_dir <- ".." }

d113 <- read_dta(file.path(base_dir, "data/113年移工/家庭面/data113.dta"))

cat("========== 113年 滿意度題項 ==========\n")
for (col in grep("^q20", names(d113), value=TRUE)) {
  lbl <- attr(d113[[col]], "label")
  if (!is.null(lbl)) {
    cat(sprintf("  %s: %s\n", col, lbl))
  }
}
