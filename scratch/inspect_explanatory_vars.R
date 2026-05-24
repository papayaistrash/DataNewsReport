library(haven)

base_dir <- "."
if (basename(getwd()) == "scratch") { base_dir <- ".." }

# Check key explanatory variables across all 5 years
for (yr in c("109", "110", "111", "112", "113")) {
  cat(sprintf("\n========== %s年 解釋變數 ==========\n", yr))
  path <- file.path(base_dir, "data", paste0(yr, "年移工/家庭面/data", yr, ".dta"))
  d <- read_dta(path)
  
  # Nationality
  for (col in c("q8d", "country", "t2")) {
    if (col %in% names(d)) {
      cat(sprintf("  國籍 %s: %s\n", col, attr(d[[col]], "label")))
      lbl <- attr(d[[col]], "labels")
      if (!is.null(lbl)) cat("    labels:", paste(names(lbl), lbl, sep="=", collapse=", "), "\n")
      print(table(d[[col]], useNA="ifany"))
    }
  }
  
  # Age
  for (col in c("q8b", "age")) {
    if (col %in% names(d)) {
      cat(sprintf("  年齡 %s: %s\n", col, attr(d[[col]], "label")))
      lbl <- attr(d[[col]], "labels")
      if (!is.null(lbl)) cat("    labels:", paste(names(lbl), lbl, sep="=", collapse=", "), "\n")
      print(table(d[[col]], useNA="ifany"))
    }
  }
  
  # Salary
  for (col in c("nq10a", "q10", "q10a")) {
    if (col %in% names(d)) {
      cat(sprintf("  薪資 %s: %s\n", col, attr(d[[col]], "label")))
      cat("    summary:", paste(round(summary(as.numeric(d[[col]])), 0), collapse=", "), "\n")
    }
  }
  
  # Leave/holiday
  for (col in c("q13a", "q13", "n12")) {
    if (col %in% names(d)) {
      cat(sprintf("  休假 %s: %s\n", col, attr(d[[col]], "label")))
      lbl <- attr(d[[col]], "labels")
      if (!is.null(lbl)) cat("    labels:", paste(names(lbl), lbl, sep="=", collapse=", "), "\n")
      print(table(d[[col]], useNA="ifany"))
    }
  }
  
  # Gender of worker
  for (col in c("q8a", "gender")) {
    if (col %in% names(d)) {
      cat(sprintf("  性別 %s: %s\n", col, attr(d[[col]], "label")))
      print(table(d[[col]], useNA="ifany"))
    }
  }
  
  # Work experience / tenure
  for (col in c("q8c", "q8e")) {
    if (col %in% names(d)) {
      cat(sprintf("  %s: %s\n", col, attr(d[[col]], "label")))
      lbl <- attr(d[[col]], "labels")
      if (!is.null(lbl)) cat("    labels:", paste(names(lbl), lbl, sep="=", collapse=", "), "\n")
      print(table(d[[col]], useNA="ifany"))
    }
  }
  
  # Insurance
  for (col in c("q19", "q17")) {
    if (col %in% names(d)) {
      lbl_text <- attr(d[[col]], "label")
      if (!is.null(lbl_text) && grepl("保險|事故", lbl_text)) {
        cat(sprintf("  保險 %s: %s\n", col, lbl_text))
        print(table(d[[col]], useNA="ifany"))
      }
    }
  }
}
