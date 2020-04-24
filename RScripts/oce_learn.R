###############################################################################################
#
# oce_learn.r
# Erika Anderson
# 2020-04
#
# learn how to use OCE R package to read ctd files
# driving force is to pull the station and temperature at 10 m
# longterm goal is to wrangle data into high seas OCEAN_CTD format
#
#
###############################################################################################

library(oce) # oceanographic r package
library(here) # relative file paths

vignette("oce")

# # from vignette
# data(section)
# plot(section, which=c(1, 2, 3, 99))
# 
# s <- handleFlags(section, flags=list(c(1, 3:9)))
# ctd <- as.ctd(s[["salinity"]], s[["temperature"]], s[["pressure"]],
#               longitude=s[["longitude"]], latitude=s[["latitude"]])
# col <- ifelse(s[["longitude"]] > -30, "black", "gray")
# plotTS(ctd, col=col, eos="gsw")

###############################
# try load one data file
stn1 <- read.ctd(
  file = here("Input", "2015-15-processed", "Archive", "CTD", "2015-15-0002.ctd"),
  columns = c("Pressure", "pHSBENominal", "NumBins"),
  station = "Set 1",
  missingValue = FALSE,
  deploymentType = "profile")

###############################