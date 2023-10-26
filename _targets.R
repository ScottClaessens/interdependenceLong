library(tidyverse)
library(targets)
library(tarchetypes)
source("R/functions_data.R")
options(tidyverse.quiet = TRUE)
tar_option_set(packages = c())
# workflow
list(
  # files
  tar_target(fileData, "data/usCohortCleanData_t1-t18.csv", format = "file"),
  # load data
  tar_target(d, loadData(fileData)),
  # print session info for reproducibility
  tar_target(sessionInfo, writeLines(capture.output(sessionInfo()), "sessionInfo.txt"))
)