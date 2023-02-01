## SETUP

#install
devtools::install_github("USAID-OHA-SI/Wavelength")

#load the package
library(Wavelength)
library(plyr)


tz <- Wavelength::pull_hierarchy(
  ouuid,
  datim_user(),
  datim_pwd(),
  baseurl = "https://final.datim.org/",
  folderpath_output = NULL
)

table(tz$level)
