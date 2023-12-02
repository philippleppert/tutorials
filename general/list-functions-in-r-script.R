library(tidyverse)

root <- "data-analysi/linear-regression-2/"

rmd_path <- paste0(root,"linear-regression-2.Rmd")

knitr::purl(rmd_path, output = "general/linear-regression-2.R")

r_path <- paste0(general, "linear-regression-2.R")

NCmisc::list.functions.in.file(r_path, alphabetic = FALSE)
