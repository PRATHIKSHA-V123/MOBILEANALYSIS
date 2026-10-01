# Run this once before launching the app for the first time.
needed <- c("shiny", "dplyr", "DT", "ggplot2", "scales", "tibble", "readr", "stringr")
to_install <- needed[!(needed %in% installed.packages()[, "Package"])]
if (length(to_install) > 0) install.packages(to_install)
