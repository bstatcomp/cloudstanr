# debug stuff
library(httr)
library(keyring)
library(dplyr)

# install
roxygen2::roxygenise(clean=TRUE)
devtools::install()
devtools::document()
