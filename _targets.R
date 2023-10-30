library(tidyverse)
library(targets)
library(tarchetypes)
source("R/functions_data.R")
source("R/functions_plots.R")
source("R/functions_modelling.R")
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
  # plot average pfi over time
  tar_target(plotPFI, plotPerceivedInterdependence(d)),
  # plot average help given/received over time
  tar_target(plotHelp, plotHelping(d)),
  # fit random intercept cross-lagged panel model without controls
  tar_target(riclpm1.1, fitRICLPM1(d, constrained = TRUE)),
  tar_target(riclpm1.2, fitRICLPM1(d, constrained = FALSE)),
  # print session info for reproducibility
  tar_target(sessionInfo, writeLines(capture.output(sessionInfo()), "sessionInfo.txt"))
)
