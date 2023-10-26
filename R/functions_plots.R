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
    scale_x_continuous(breaks = 1:16, limits = c(0.5, 16.5))
  # save
  ggsave(filename = "figures/missing.pdf", width = 7, height = 5)
  return(out)
}
