######################################################################################
#
# CTD_Temp_Depth_Extract.R
# Original script:Yeongha Jung
# Modified: Erika Anderson
# Date: 2020-04
#
# find cruise, station, temperature at specific depths for all ctd casts
# save as csv file 
#
######################################################################################

# load libraries
library(tidyverse) # data manipulation
library(stringi) # string manipulation
library(here) # relative file paths
library(measurements) # convert coordinate data
library(leaflet) # mapping
library(leaflet.extras)
library(mapview) # to save leaflet as png

###################################
# directory with ctd files
dir <- here("Input", "2015-15-processed", "Archive", "CTD")

# get all ctd files in folder as vector
ctdFiles = list.files(dir, pattern = "ctd", full.names = TRUE)

# number of ctd files for loop
numFiles = length(ctdFiles)

# create empty list to hold data
mylist <- list()

# loop through all ctd files 
# load data and find mean temperature near 10 m
for (i in 1:numFiles) {
  
  i <- ctdFiles[[i]]
  
  # read entire file into R
  import <- readLines(i)
  
  # cruise
  cruise <- str_replace(import[[grep("MISSION", import)]], "    MISSION             : ", "")
  
  # station
  station <- str_replace(import[[grep("STATION", import)]], "    STATION             : ", "")
  
  # latitude
  lat <- stri_sub(str_replace(import[[grep("LATITUDE", import)]],
                              "    LATITUDE            :  ", ""), 1, 14)

  # longitude
  long <- stri_sub(str_replace(import[[grep("LONGITUDE", import)]],
                               "    LONGITUDE           : ", ""),1 , 15)
  
  # find the line with header end
  headerEnd <- grep("*END OF HEADER", import)
  
  # load data
  df <- read.table(i, skip = headerEnd, header = FALSE)
  
  # number of channels
  numChan <- as.integer(str_replace(import[[grep("NUMBER OF CHANNELS", import)]], 
                                    "    NUMBER OF CHANNELS  : ", ""))
  
  # channel info line
  chanStart <- grep("TABLE: CHANNELS", import) + 2
  
  # load channel names
  chandf <- read.table(i, skip = chanStart, header = FALSE, nrows = numChan)

  # assing channels to data columns
  colnames(df) <- chandf$V2
  
  # note that 10 decibars of pressure = 9.931 m depth sea water
  # subset to 
  pressure10 <- subset(df, Pressure > (9.931 - 1) & Pressure < (9.931 + 1))
  meantemp10 <- round(mean(pressure10$`Temperature:Primary`, na.rm = TRUE), 3)
  
  # make into tibble
  loopData <- tibble("Cruise" = cruise,
                      "Station" = station,
                      "Latitude" = lat,
                      "Longitude" = long,
                     "Temp10db" = meantemp10,
                     "File" = i,)

  mylist[[i]] <- loopData
}

# take out of list
df <- do.call(rbind.data.frame, mylist)

# convert lattitude and longitude to decimal degrees
df <- df %>%
  mutate(LatDeg = as.numeric(str_extract(Latitude, "^[0-9]{2}")),
         LatMinDec = as.numeric(str_extract(Latitude, "[ ]+ [0-9]+\\.[0-9]+"))/60,
         Latitude = LatDeg + LatMinDec,
         LongDeg = as.numeric(str_extract(Longitude, "^[0-9]{3}")),
         LongMinDec = as.numeric(str_extract(Longitude, "[ ]+ [0-9]+\\.[0-9]+"))/60,
         Longitude = (LongDeg + LongMinDec) * -1) %>%
  select(-LatDeg, -LatMinDec, -LongDeg, -LongMinDec)


# write as csv to review
write_csv(df, here("Output", "meanTemp10db.csv"), na = "")

# get max lat and long for survey
meanLat <- mean(df$Latitude, na.rm = TRUE)
meanLong <- mean(df$Longitude, na.rm = TRUE)

# graph results as qc
leaflet(df) %>% addTiles() %>%
  setView(lng = meanLong, lat = meanLat, zoom = 6) %>%
  addHeatmap(lng = ~Longitude, lat = ~Latitude, intensity = ~Temp10db, blur = 15, max = 20, radius = 12)

# need to export manually unless you install PhantomJS
# https://stackoverflow.com/questions/31336898/how-to-save-leaflet-in-r-map-as-png-or-jpg-file

       