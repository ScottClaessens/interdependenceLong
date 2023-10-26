# custom functions

# load and clean data
loadData <- function(fileData) {
  # read in data from cleaned csv file
  suppressMessages(read_csv(file = fileData, show_col_types = FALSE)) %>%
    # select relevant variables
    dplyr::select(
      # PFI neighbourhood - affect
      starts_with("PFIaffectNeigh.") |
        # PFI neighbourhood - shared fate
        starts_with("PFIsharedfateNeigh.") |
        # help given
        starts_with("HelpGiven.") |
        # help received
        starts_with("HelpReceived.") |
        # personality
        starts_with("b5_") |
        # trait empathic concern
        starts_with("TraitEmpathicConcern.") |
        # trait empathic distress
        starts_with("TraitEmpathicdistress.") |
        # wealth
        starts_with("Wealth.")
      )
}