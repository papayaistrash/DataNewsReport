library(haven)

base_dir <- "."
if (basename(getwd()) == "scratch") { base_dir <- ".." }

# Check 110 and 111 for runaway/missing variable
for (yr in c("110", "111")) {
  cat(sprintf("\n========== %s年 失聯變數搜尋 ==========\n", yr))
  path <- file.path(base_dir, "data", paste0(yr, "年移工/家庭面/data", yr, ".dta"))
  d <- read_dta(path)
  
  for (col in names(d)) {
    lbl <- attr(d[[col]], "label")
    if (!is.null(lbl) && grepl("失聯|行蹤不明|逃跑", lbl, ignore.case=TRUE)) {
      cat(sprintf("  %s: %s\n", col, lbl))
      print(table(d[[col]], useNA="ifany"))
    }
  }
}

# Also check 109 satisfaction variable details
cat("\n========== 109年 滿意度 ==========\n")
d109 <- read_dta(file.path(base_dir, "data/109年移工/家庭面/data109.dta"))

# q20g?
if ("q20g" %in% names(d109)) {
  cat("q20g label:", attr(d109$q20g, "label"), "\n")
  print(table(d109$q20g, useNA="ifany"))
}

# check all q20 series
for (col in grep("^q20", names(d109), value=TRUE)) {
  lbl <- attr(d109[[col]], "label")
  cat(sprintf("  %s: %s\n", col, lbl))
}

# Check 109 t1 labels
cat("\n109 t1 labels:\n")
lbl109 <- attr(d109$t1, "labels")
if (!is.null(lbl109)) print(lbl109)
cat("t1 label:", attr(d109$t1, "label"), "\n")

# Also check 112 t1 labels
cat("\n========== 112年 t1 labels ==========\n")
d112 <- read_dta(file.path(base_dir, "data/112年移工/家庭面/data112.dta"))
lbl112 <- attr(d112$t1, "labels")
if (!is.null(lbl112)) print(lbl112)
cat("t1 label:", attr(d112$t1, "label"), "\n")

# 112 q19g
if ("q19g" %in% names(d112)) {
  cat("112 q19g label:", attr(d112$q19g, "label"), "\n")
  print(table(d112$q19g, useNA="ifany"))
}
