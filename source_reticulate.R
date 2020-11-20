library(tidyverse)
library(reticulate)

reticulate::use_condaenv("r-reticulate", required = TRUE)
# reticulate::use_condaenv("base (root)", required = TRUE)

source_python("PYTHON/try_src_reticulate.py")

res <- read_csv_file("PYTHON/data/tst_reticulate.csv")

res

in_out(list(A = 1:10, b = LETTERS[1:10]))
