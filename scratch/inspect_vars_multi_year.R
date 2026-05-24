library(haven)

base_dir <- "."
if (basename(getwd()) == "scratch") { base_dir <- ".." }

years <- c("109", "110", "111", "112", "113")

for (yr in years) {
  path <- file.path(base_dir, "data", paste0(yr, "年移工/家庭面/data", yr, ".dta"))
  cat("\n============================\n")
  cat("Year:", yr, "\n")
  cat("Path:", path, "\n")
  
  d <- read_dta(path)
  cat("Rows:", nrow(d), "Cols:", ncol(d), "\n")
  
  # Check for city/county variable (j1 or similar)
  city_cols <- grep("^j[0-9]|city|county|area|region", names(d), value=TRUE, ignore.case=TRUE)
  cat("City-related cols:", paste(city_cols, collapse=", "), "\n")
  
  # Check for j1
  if ("j1" %in% names(d)) {
    cat("j1 values:\n")
    print(table(d$j1, useNA="ifany"))
    # Check labels
    cat("j1 labels:\n")
    lbl <- attr(d$j1, "labels")
    if (!is.null(lbl)) print(lbl)
    cat("j1 label attr:", attr(d$j1, "label"), "\n")
  }
  
  # Check for runaway/missing (q5 or similar)
  run_cols <- grep("^q5$|^q5[ab_]|runaway|missing", names(d), value=TRUE, ignore.case=TRUE)
  cat("Runaway cols:", paste(run_cols, collapse=", "), "\n")
  if ("q5" %in% names(d)) {
    cat("q5 values:\n")
    print(table(d$q5, useNA="ifany"))
    cat("q5 label:", attr(d$q5, "label"), "\n")
  }
  
  # Check for satisfaction (q20_7, q19g, q19 etc.)
  sat_cols <- grep("^q19|^q20|satisfaction", names(d), value=TRUE, ignore.case=TRUE)
  cat("Satisfaction cols:", paste(sat_cols, collapse=", "), "\n")
  
  # Check weight
  if ("w" %in% names(d)) {
    cat("Weight 'w' present, summary:\n")
    print(summary(as.numeric(d$w)))
  }
  
  cat("============================\n")
}
