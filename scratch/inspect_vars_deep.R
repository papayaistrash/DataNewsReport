library(haven)

base_dir <- "."
if (basename(getwd()) == "scratch") { base_dir <- ".." }

# Check 109 year for runaway and county variables
cat("========== 109年 深入檢查 ==========\n")
d109 <- read_dta(file.path(base_dir, "data/109年移工/家庭面/data109.dta"))

# Look for any column that might represent runaway/missing
# Check all column labels
for (col in names(d109)) {
  lbl <- attr(d109[[col]], "label")
  if (!is.null(lbl) && grepl("失聯|行蹤不明|逃跑|縣市|戶籍|地區|county", lbl, ignore.case=TRUE)) {
    cat(sprintf("  %s: %s\n", col, lbl))
    if (length(unique(d109[[col]])) <= 25) {
      print(table(d109[[col]], useNA="ifany"))
    }
  }
}

# Check 'area' or similar columns
cat("\nAll column names for 109:\n")
cat(paste(names(d109), collapse=", "), "\n")

cat("\n========== 112年 深入檢查 ==========\n")
d112 <- read_dta(file.path(base_dir, "data/112年移工/家庭面/data112.dta"))

# Look for county/city variable
for (col in names(d112)) {
  lbl <- attr(d112[[col]], "label")
  if (!is.null(lbl) && grepl("縣市|戶籍|地區|county|city|area|region", lbl, ignore.case=TRUE)) {
    cat(sprintf("  %s: %s\n", col, lbl))
    if (length(unique(d112[[col]])) <= 25) {
      print(table(d112[[col]], useNA="ifany"))
    }
  }
}

cat("\nAll column names for 112:\n")
cat(paste(names(d112), collapse=", "), "\n")

# Check 110 county variable details
cat("\n========== 110年 county 深入 ==========\n")
d110 <- read_dta(file.path(base_dir, "data/110年移工/家庭面/data110.dta"))
cat("county label:", attr(d110$county, "label"), "\n")
cat("county labels:\n")
lbl110 <- attr(d110$county, "labels")
if (!is.null(lbl110)) print(lbl110)
print(table(d110$county, useNA="ifany"))

# Check 110 q5 label (what does q5 mean in 110?)
cat("\n110 q5 label:", attr(d110$q5, "label"), "\n")
print(table(d110$q5, useNA="ifany"))

# Check 110 satisfaction q19g
cat("\n110 q19g label:", attr(d110$q19g, "label"), "\n")
print(table(d110$q19g, useNA="ifany"))

# Check 111 county
cat("\n========== 111年 county 深入 ==========\n")
d111 <- read_dta(file.path(base_dir, "data/111年移工/家庭面/data111.dta"))
cat("county label:", attr(d111$county, "label"), "\n")
lbl111 <- attr(d111$county, "labels")
if (!is.null(lbl111)) print(lbl111)
print(table(d111$county, useNA="ifany"))

cat("\n111 q5 label:", attr(d111$q5, "label"), "\n")
print(table(d111$q5, useNA="ifany"))
