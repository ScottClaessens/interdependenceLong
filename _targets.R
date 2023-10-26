library(tidyverse)
library(targets)
library(tarchetypes)
source("R/functions_data.R")
source("R/functions_plots.R")
options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("lavaan", "tidyverse"))
# workflow
list(
  # files
  tar_target(fileData, "data/usCohortCleanData_t1-t18.csv", format = "file"),
  # load data
  tar_target(d, loadData(fileData)),
  # plot missingness
  tar_target(plotMissing, plotMissingness(d)),
  # print session info for reproducibility
  tar_target(sessionInfo, writeLines(capture.output(sessionInfo()), "sessionInfo.txt"))
)
