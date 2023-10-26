# custom functions

# load and clean data
loadData <- function(fileData) {
  out <-
    # read in data from cleaned csv file
    suppressMessages(read_csv(file = fileData, show_col_types = FALSE)) %>%
    # select relevant variables
    dplyr::select(
      # PFI neighbourhood - affect
      starts_with("PFIaffectNeigh.") |
        # PFI neighbourhood - shared fate
        starts_with("PFIsharedfateNeigh.") |
        # other PFI items from waves 17 and 18, not composites
        (starts_with("PFINeigh") & !contains("Time") & ends_with("17")) |
        (starts_with("PFINeigh") & !contains("Time") & ends_with("18")) |
        # help given
        starts_with("HelpGiven.") |
        # help received
        starts_with("HelpReceived.") |
        # other help items from wave 18, not composites
        (starts_with("HelpGiven") & ends_with(".18")) |
        (starts_with("HelpReceived") & ends_with(".18")) |
        # personality
        starts_with("b5_") |
        # trait empathic concern
        starts_with("TraitEmpathicConcern.") |
        # trait empathic distress
        starts_with("TraitEmpathicdistress.") |
        # wealth
        starts_with("Wealth.")
      ) %>%
    # calculate additional composites for waves 17 and 18
    rowwise() %>%
    mutate(
      PFIaffectNeigh.17 = mean(c(PFINeigh1.17, PFINeigh2.17, (8 - PFINeigh5.17)), na.rm = TRUE),
      PFIaffectNeigh.18 = mean(c(PFINeigh1.18, PFINeigh2.18, (8 - PFINeigh5.18)), na.rm = TRUE),
      PFIsharedfateNeigh.17 = mean(c(PFINeigh3.17, PFINeigh4.17, PFINeigh6.17), na.rm = TRUE),
      PFIsharedfateNeigh.18 = mean(c(PFINeigh3.18, PFINeigh4.18, PFINeigh6.18), na.rm = TRUE),
      HelpGiven.18 = mean(c(HelpGiven1.18, HelpGiven2.18, HelpGiven3.18,
                            HelpGiven4.18, HelpGiven5.18, HelpGiven6.18), na.rm = TRUE),
      HelpReceived.18 = mean(c(HelpReceived1.18, HelpReceived2.18, HelpReceived3.18,
                               HelpReceived4.18, HelpReceived5.18, HelpReceived6.18), na.rm = TRUE)
    ) %>%
    # remove individual items
    dplyr::select(!c(PFINeigh1.17, PFINeigh2.17, PFINeigh3.17, PFINeigh4.17, 
                     PFINeigh5.17, PFINeigh6.17, PFINeigh1.18, PFINeigh2.18,
                     PFINeigh3.18, PFINeigh4.18, PFINeigh5.18, PFINeigh6.18,
                     HelpGiven1.18, HelpGiven2.18, HelpGiven3.18,
                     HelpGiven4.18, HelpGiven5.18, HelpGiven6.18,
                     HelpReceived1.18, HelpReceived2.18, HelpReceived3.18,
                     HelpReceived4.18, HelpReceived5.18, HelpReceived6.18))
  return(out)
}
