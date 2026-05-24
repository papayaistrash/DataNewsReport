library(haven)
base_dir <- "."
if (basename(getwd()) == "scratch") { base_dir <- ".." }

d109 <- read_dta(file.path(base_dir, "data/109年移工/家庭面/data109.dta"))

# q9 series for 109
for (col in grep("^q9", names(d109), value=TRUE)) {
  lbl <- attr(d109[[col]], "label")
  vals <- as.numeric(d109[[col]])
  cat(sprintf("  %s: %s  | max=%s, mean=%s\n", col, lbl, max(vals, na.rm=TRUE), round(mean(vals, na.rm=TRUE),0)))
}

# The salary is q9_0_1 + q9_1_1 + ... 
cat("\n=== 109 薪資重建 ===\n")
salary_109 <- as.numeric(d109$q9_0_1) + as.numeric(d109$q9_1_1) + as.numeric(d109$q9_2_1) + as.numeric(d109$q9_3_1)
cat("Summary of total salary (q9_0_1 + q9_1_1 + q9_2_1 + q9_3_1):\n")
print(summary(salary_109))
