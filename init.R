
# init.R
#
# Example R code to install packages if not already installed
#
my_packages = c("ggplot2","tidyr","plyr","dplyr","readxl","shinycssloaders",
                "readr","cowplot","lubridate","scales","shinythemes","plotly",
                "gridExtra","stringr","ggpmisc","data.table","rlang","purrr",
                "shiny","DT","leaflet","sf","rmapshaper","shinyWidgets",
                "rsconnect","shinyjs","htmltools","htmlwidgets","leaflet.extras")
install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}
invisible(sapply(my_packages, install_if_missing))