# custom functions

# plot missingness in key variables
plotMissingness <- function(d) {
  out <-
    d %>%
    # rename
    rename_with(function(x) paste0(x, ".1"), starts_with("b5_")) %>%
    # get data in long format
    pivot_longer(
      cols = everything(),
      names_pattern = "([^.]+).([^.]+)",
      names_to = c("item","wave")
    ) %>%
    # wave as numeric
    mutate(wave = as.numeric(wave)) %>%
    # group by items and waves
    group_by(item, wave) %>%
    # proportion of observed data
    summarise(propObs = mean(!is.na(value)), .groups = "drop") %>%
    # fill in the blanks
    complete(item, wave, fill = list(propObs = 0)) %>%
    # plot
    ggplot(aes(x = wave, y = item, fill = propObs)) +
    geom_tile() +
    scale_y_discrete(limits = rev) +
    scale_x_continuous(breaks = 1:18, limits = c(0.5, 18.5))
  # save
  ggsave(filename = "figures/missing.pdf", width = 7, height = 5)
  return(out)
}

# plot perceived fitness interdependence over time
plotPerceivedInterdependence <- function(d) {
  out <-
    d %>%
    # select pfi items
    dplyr::select(starts_with("PFI")) %>%
    # get data in long format
    pivot_longer(
      cols = everything(),
      names_pattern = "([^.]+).([^.]+)",
      names_to = c("item","wave")
    ) %>%
    # wave as numeric
    mutate(wave = as.numeric(wave)) %>%
    # averages and standard errors
    group_by(item, wave) %>%
    summarise(
      mean = mean(value, na.rm = TRUE),
      se = sd(value, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
      ) %>%
    # names for plot
    mutate(item = ifelse(item == "PFIaffectNeigh",
                         "PFI Affect",
                         "PFI Shared Fate")) %>%
    # plot
    ggplot(aes(x = wave, y = mean, ymin = mean - 2*se,
               ymax = mean + 2*se, colour = item)) +
    geom_pointrange(size = 0.2) +
    scale_y_continuous(name = "average rating", limits = c(1, 7), breaks = 1:7) +
    scale_x_continuous(breaks = 1:18) +
    theme(panel.grid = element_blank())
  # save
  ggsave(filename = "figures/pfi.pdf", width = 7, height = 4)
  return(out)
}

# plot help given / received over time
plotHelping <- function(d) {
  out <-
    d %>%
    # select pfi items
    dplyr::select(starts_with("Help")) %>%
    # get data in long format
    pivot_longer(
      cols = everything(),
      names_pattern = "([^.]+).([^.]+)",
      names_to = c("item","wave")
    ) %>%
    # wave as numeric
    mutate(wave = as.numeric(wave)) %>%
    # averages and standard errors
    group_by(item, wave) %>%
    summarise(
      mean = mean(value, na.rm = TRUE),
      se = sd(value, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    ) %>%
    # names for plot
    mutate(item = ifelse(item == "HelpGiven",
                         "Help Given",
                         "Help Received")) %>%
    # plot
    ggplot(aes(x = wave, y = mean, ymin = mean - 2*se,
               ymax = mean + 2*se, colour = item)) +
    geom_pointrange(size = 0.2) +
    scale_y_continuous(name = "average rating", limits = c(1, 5), breaks = 1:5) +
    scale_x_continuous(breaks = 1:18, limits = c(1, 18)) +
    theme(panel.grid = element_blank())
  # save
  ggsave(filename = "figures/help.pdf", width = 7, height = 4)
  return(out)
}
