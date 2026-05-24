library(tidyverse)

# Check if pdftools is installed
has_pdftools <- requireNamespace("pdftools", quietly = TRUE)
cat("has_pdftools:", has_pdftools, "\n")

# Let's read and summarize columns that might be fees in d1
d1_all <- read_csv("data/103年針對移工問卷/data103_1.csv", show_col_types = FALSE)
cat("\nd1 size:", dim(d1_all), "\n")

# Look at columns starting with q17, q18, q19
cat("\nSummary of q17 and q18 in d1:\n")
d1_all %>% select(any_of(c("q17", "q17_1", "q17_nt", "q18"))) %>% head(10) %>% print()
d1_all %>% select(any_of(c("q17", "q17_1", "q17_nt", "q18"))) %>% summary() %>% print()

# Let's read and summarize columns that might be fees in d2
d2_all <- read_csv("data/103年針對移工問卷/data103_2.csv", show_col_types = FALSE)
cat("\nd2 size:", dim(d2_all), "\n")

cat("\nSummary of v17, v18_1, v18_2 in d2:\n")
d2_all %>% select(any_of(c("v17", "v18_1", "v18_2", "v18_3", "v18_4"))) %>% head(10) %>% print()
d2_all %>% select(any_of(c("v17", "v18_1", "v18_2", "v18_3", "v18_4"))) %>% summary() %>% print()
