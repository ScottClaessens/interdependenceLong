library(tidyverse)
library(targets)
library(tarchetypes)
source("R/functions_data.R")
source("R/functions_plots.R")
source("R/functions_modelling.R")
options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("lavaan", "papaja", "tidyverse"))
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
  # fit random intercept cross-lagged panel model with controls
  tar_target(riclpm2.1, fitRICLPM2(d, constrained = TRUE)),
  tar_target(riclpm2.2, fitRICLPM2(d, constrained = FALSE)),
  # plot unconstrained model parameters
  tar_target(plot1.2, plotUnconstrained(riclpm1.2, filename = "figures/riclpm1.2.pdf")),
  tar_target(plot2.2, plotUnconstrained(riclpm2.2, filename = "figures/riclpm2.2.pdf")),
  # knit report
  tar_render(report, "report.Rmd"),
  # print session info for reproducibility
  tar_target(sessionInfo, writeLines(capture.output(sessionInfo()), "sessionInfo.txt"))
)
