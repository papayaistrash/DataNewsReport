library(haven)

base_dir <- "."
if (basename(getwd()) == "scratch") { base_dir <- ".." }

# 109 salary
cat("========== 109年 薪資 ==========\n")
d109 <- read_dta(file.path(base_dir, "data/109年移工/家庭面/data109.dta"))
for (col in names(d109)) {
  lbl <- attr(d109[[col]], "label")
  if (!is.null(lbl) && grepl("薪資|薪水|工資|給付|月薪", lbl)) {
    vals <- as.numeric(d109[[col]])
    cat(sprintf("  %s: %s  | max=%s, non-NA=%d\n", col, lbl, max(vals, na.rm=TRUE), sum(!is.na(vals))))
  }
}

# 110 salary
cat("\n========== 110年 薪資 ==========\n")
d110 <- read_dta(file.path(base_dir, "data/110年移工/家庭面/data110.dta"))
for (col in names(d110)) {
  lbl <- attr(d110[[col]], "label")
  if (!is.null(lbl) && grepl("薪資|薪水|工資|給付|月薪", lbl)) {
    vals <- as.numeric(d110[[col]])
    cat(sprintf("  %s: %s  | max=%s, non-NA=%d\n", col, lbl, max(vals, na.rm=TRUE), sum(!is.na(vals))))
  }
}

# 110 leave frequency - q12a specific?
cat("\n========== 109/110 放假頻率 ==========\n")
for (col in names(d109)) {
  lbl <- attr(d109[[col]], "label")
  if (!is.null(lbl) && grepl("每月放假|放假.*次|假日.*放假", lbl)) {
    cat(sprintf("  109 %s: %s\n", col, lbl))
    print(table(d109[[col]], useNA="ifany"))
  }
}
for (col in names(d110)) {
  lbl <- attr(d110[[col]], "label")
  if (!is.null(lbl) && grepl("每月放假|放假.*次|假日.*放假|放假情形", lbl)) {
    cat(sprintf("  110 %s: %s\n", col, lbl))
    print(table(d110[[col]], useNA="ifany"))
  }
}

# Check q11 in 109 and 110
cat("\n========== 109 q11 ==========\n")
if ("q11" %in% names(d109)) {
  cat("q11:", attr(d109$q11, "label"), "\n")
  cat("summary:", paste(round(summary(as.numeric(d109$q11)), 0), collapse=", "), "\n")
}
cat("\n========== 110 q11 ==========\n")
if ("q11" %in% names(d110)) {
  cat("q11:", attr(d110$q11, "label"), "\n")
  cat("summary:", paste(round(summary(as.numeric(d110$q11)), 0), collapse=", "), "\n")
}

# 109 q15b, q15c for leave
cat("\n========== 109 q15 series ==========\n")
for (col in grep("^q15", names(d109), value=TRUE)) {
  cat(sprintf("  %s: %s\n", col, attr(d109[[col]], "label")))
}

# Check 109 n14 for salary
cat("\n========== 109 n14 series ==========\n")
for (col in grep("^n14", names(d109), value=TRUE)) {
  lbl <- attr(d109[[col]], "label")
  cat(sprintf("  %s: %s | max=%s\n", col, lbl, max(as.numeric(d109[[col]]), na.rm=TRUE)))
}
