library(haven)

base_dir <- "."
if (basename(getwd()) == "scratch") { base_dir <- ".." }

# Find salary variable for each year
for (yr in c("109", "110", "111", "112")) {
  cat(sprintf("\n========== %s年 薪資搜尋 ==========\n", yr))
  path <- file.path(base_dir, "data", paste0(yr, "年移工/家庭面/data", yr, ".dta"))
  d <- read_dta(path)
  
  for (col in names(d)) {
    lbl <- attr(d[[col]], "label")
    if (!is.null(lbl) && grepl("薪資|薪水|工資|給付|月薪", lbl, ignore.case=TRUE)) {
      vals <- as.numeric(d[[col]])
      if (max(vals, na.rm=TRUE) > 10000) {  # likely salary in NTD
        cat(sprintf("  ★ %s: %s\n", col, lbl))
        cat("    summary:", paste(round(summary(vals), 0), collapse=", "), "\n")
        cat("    non-NA:", sum(!is.na(vals)), "\n")
      }
    }
  }
  
  # Also check for leave frequency equivalent to 113's q13a
  for (col in names(d)) {
    lbl <- attr(d[[col]], "label")
    if (!is.null(lbl) && grepl("放假|休假.*幾|幾.*休|假日", lbl, ignore.case=TRUE)) {
      cat(sprintf("  休假頻率 %s: %s\n", col, lbl))
      print(table(d[[col]], useNA="ifany"))
    }
  }
}
