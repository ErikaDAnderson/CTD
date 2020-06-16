######################################################################################
#
# CTD_Temp_Depth_Extract.R
# Original script:Yeongha Jung
# Modified: Erika Anderson
# Date: 2020-04
#
# find cruise, station, temperature at specific depths for ctd casts
# save as csv file 
#
######################################################################################

# load libraries
library(tidyverse) # data manipulation
library(stringi) # string manipulation
library(here) # relative file paths
library(seacarb) # pressure to depth at a given latitude
library(leaflet) # mapping

###################################
# directory with ctd files
dir <- here("Input", "2019-125-October_Survey_Sea_Crest", "Processed_CTD_Data", "Archive", "CTD")

# test directory for smaller dataset
#dir <- here("Input", "2015-15-processed", "Archive", "CTD")


# get all ctd files in folder as vector
ctdFiles = list.files(dir, pattern = "ctd", full.names = TRUE)
CTDFiles = list.files(dir, pattern =  "CTD", full.names = TRUE)
allCtdFiles <- c(ctdFiles, CTDFiles)

# number of ctd files for loop
numFiles = length(allCtdFiles)

# create empty list to hold data
mylist <- list()

# loop through all ctd files 
# load data and find mean temperature at ~ 10 meters
for (i in 1:numFiles) {
  
  thisfile <- allCtdFiles[[i]]
  
  # read entire file into R
  import <- readLines(thisfile)
  
  # cruise
  cruise <- str_replace(import[[grep("MISSION", import)]], "    MISSION             : ", "")
  
  # station or event number
  if (length(grep("STATION", import) > 0)) {
    if (str_replace(import[[grep("STATION", import)]], "    STATION             : ", "") != "NONE") {
    
    station <- str_replace(import[[grep("STATION", import)]], "    STATION             : ", "")
    
    # remove all space within stations
    station <- str_replace_all(station, fixed(" "), "")
    
  } else {
    station <- str_replace(import[[grep("EVENT NUMBER        :", import)]], "    EVENT NUMBER        : ", "")
    }
  } else {  
    station <- str_replace(import[[grep("EVENT NUMBER        :", import)]], "    EVENT NUMBER        : ", "") 
  }
  
  # latitude ( include colon to prevent issues with LATITUDE 2)
  lat <- stri_sub(str_replace(import[[grep("LATITUDE            :", import)]],
                              "    LATITUDE            :  ", ""), 1, 14)
  
  LatDeg <- as.numeric(str_extract(lat, "^[0-9]{2}"))
  LatMinDec <- as.numeric(str_extract(lat, "[ ]+ [0-9]+\\.[0-9]+"))/60
  thislatitude <- LatDeg + LatMinDec

  # longitude; include colon to omit LONGITUDE 2
  long <- stri_sub(str_replace(import[[grep("LONGITUDE           :", import)]],
                               "    LONGITUDE           : ", ""),1 , 15)
  
  # find the line with header end
  headerEnd <- grep("*END OF HEADER", import)
  
  # load data
  df <- read.table(thisfile, skip = headerEnd, header = FALSE)
  
  # number of channels
  numChan <- as.integer(str_replace(import[[grep("NUMBER OF CHANNELS", import)]], 
                                    "    NUMBER OF CHANNELS  : ", ""))
  
  # channel info line
  chanStart <- grep("TABLE: CHANNELS", import) + 2
  
  # load channel names
  chandf <- read.table(thisfile, skip = chanStart, header = FALSE, nrows = numChan)
  
  # make channels unique for column names
  chandf$V2 <- make.unique(as.character(chandf$V2))

  # assign channels to data columns
  colnames(df) <- chandf$V2
  
  # helper function to find mean temperature at different depths
  meanTemp_fn <- function(thisdf, thisdepth, thislatitude) {
    
    # find the name of the pressure column and rename to something that is standard 
    # survey 2005-17 has  colon in Presure column name
    thisdf <- thisdf %>%
      rename("Pressure" = colnames(select(thisdf, starts_with("Pressure"))))

    thispressurePlus <- d2p((thisdepth + 1), thislatitude) 
    thispressureMinus <- d2p((thisdepth - 1), thislatitude)

    thispressuredf <- thisdf %>%
      filter(Pressure > thispressureMinus) %>%
      filter(Pressure < thispressurePlus)
    
    # subset for temperature column 
    thistempdf <- thispressuredf %>%
      select(starts_with("Temperature")) %>%
     
      # replace -99 with NA
      mutate_all(., na_if, -99)
    
    # variable names all start with Temperature though
    meantemp <- round(mean(thistempdf[[1]], na.rm = TRUE), 4)
  }
  
  # account for situation when no temperature or pressure recorded in ctd file
  if (ncol(select(df, starts_with("Pressure"))) != 0 & ncol(select(df, starts_with("Temp"))) != 0) {
    
    # find temperature within + or - 1 m of 
    meantemp5 <- meanTemp_fn(df, 5, thislatitude)
    meantemp6 <- meanTemp_fn(df, 6, thislatitude)
    meantemp7 <- meanTemp_fn(df, 7, thislatitude)
    meantemp8 <- meanTemp_fn(df, 8, thislatitude)
    meantemp9 <- meanTemp_fn(df, 9, thislatitude)
    meantemp10 <- meanTemp_fn(df, 10, thislatitude)
    meantemp11 <- meanTemp_fn(df, 11, thislatitude)
    meantemp12 <- meanTemp_fn(df, 12, thislatitude)
    meantemp13 <- meanTemp_fn(df, 13, thislatitude)
    meantemp14 <- meanTemp_fn(df, 14, thislatitude)
    meantemp15 <- meanTemp_fn(df, 15, thislatitude)
    
  } else { 
    meantemp5 <- NA
    meantemp6 <- NA
    meantemp7 <- NA
    meantemp8 <- NA
    meantemp9 <- NA
    meantemp10 <- NA
    meantemp11 <- NA
    meantemp12 <- NA
    meantemp13 <- NA
    meantemp14 <- NA
    meantemp15 <- NA
  }

  # make into tibble
  loopData <- tibble("Cruise" = cruise,
                      "Station" = station,
                      "Latitude" = thislatitude,
                      "Longitude" = long,
                     "Temp5m" = meantemp5,
                     "Temp6m" = meantemp6,
                     "Temp7m" = meantemp7,
                     "Temp8m" = meantemp8,
                     "Temp9m" = meantemp9,
                     "Temp10m" = meantemp10,
                     "Temp11m" = meantemp11,
                     "Temp12m" = meantemp12,
                     "Temp13m" = meantemp13,
                     "Temp14m" = meantemp14,
                     "Temp15m" = meantemp15,
                     "File" = thisfile)

  mylist[[thisfile]] <- loopData
  
  # troubleshooting which file fails
  cat(paste0(i, "", thisfile, "\n"))
}

# take out of list
df_orig <- do.call(rbind.data.frame, mylist)

# convert longitude to decimal degrees
df <- df_orig %>%
  mutate(LongDeg = as.numeric(str_extract(Longitude, "^[0-9]{3}")),
         LongMinDec = as.numeric(str_extract(Longitude, "[ ]+ [0-9]+\\.[0-9]+"))/60,
         Longitude = (LongDeg + LongMinDec) * -1,
         
         # create station id
         Year = as.numeric(str_extract(Cruise, "^[0-9]{4}")),
         Cruise = if_else(Cruise == "2003-39", "2003-36", Cruise),
         Cruise_mod = str_replace_all(Cruise, fixed("-"), ""),
         
         # fix non standard cruise names
         Cruise_mod = case_when(
           Cruise_mod == "2018093" ~ "201893",
           Cruise_mod == "199816" ~ "9816",
           Cruise_mod == "199914" ~ "9914",
           Cruise_mod == "199817" ~ "9817",
           Cruise_mod == "200943" ~ "200941", # mislabelled in CTD file
           TRUE ~ Cruise_mod),
         
         #make station ID with some variation based on cruises
         Prefix = if_else(Year < 2016, "HS", "BCSI-"),
         TowNum = str_pad(str_extract(Station, "[0-9]+$"), 3, "left", pad = "0"),
         STATION_ID_NUM = str_c(Prefix, Cruise_mod, "-", TowNum, sep = ""),
         STATION_ID_CHR = str_c(Prefix, Cruise_mod, "-", Station, sep = ""),
         STATION_ID_CHR = if_else(Cruise == "2009-01", str_extract(STATION_ID_CHR, "(^[^-]+)(?:-[^-]+){1}"),
                                  STATION_ID_CHR)) %>%
  
    # fix hyphens within stations in ctd files
  separate(., STATION_ID_CHR, c("StationSurvey", "StationLetters", "StationNumbers"),
           sep = "-", remove = FALSE) %>%
  mutate(Station200114 = str_c(StationSurvey, "-", StationLetters, StationNumbers, sep = ""),
         STATION_ID_CHR = if_else(Cruise == "2001-14", Station200114, STATION_ID_CHR),
         STATION_ID = case_when(
           grepl("SET", toupper(STATION_ID_CHR)) ~ STATION_ID_NUM,
           Cruise == "1998-16" ~ STATION_ID_NUM,
           TRUE ~ STATION_ID_CHR),
         
         # assign NA where there was no temperature near depth
         Temp5m = if_else(is.nan(Temp5m), NA_real_, Temp5m),
         Temp6m = if_else(is.nan(Temp6m), NA_real_, Temp6m),
         Temp7m = if_else(is.nan(Temp7m), NA_real_, Temp7m),
         Temp8m = if_else(is.nan(Temp8m), NA_real_, Temp8m),
         Temp9m = if_else(is.nan(Temp9m), NA_real_, Temp9m),
         Temp10m = if_else(is.nan(Temp10m), NA_real_, Temp10m),
         Temp11m = if_else(is.nan(Temp11m), NA_real_, Temp11m),
         Temp12m = if_else(is.nan(Temp12m), NA_real_, Temp12m),
         Temp13m = if_else(is.nan(Temp13m), NA_real_, Temp13m),
         Temp14m = if_else(is.nan(Temp14m), NA_real_, Temp14m),
         Temp15m = if_else(is.nan(Temp15m), NA_real_, Temp15m)) %>%
  
  # simplify and order data
  select(STATION_ID, Year, Cruise, Station, 
         Latitude, Longitude, Temp5m, Temp6m, Temp7m, Temp8m, Temp9m, Temp10m,
         Temp11m, Temp12m, Temp13m, Temp14m, Temp15m, File) %>%
  arrange(Year, Cruise)

# import csv file of all stations to check joins
stations <- read_csv(here("Input", "EA_STATION_YEARS.csv"), 
                     col_types = cols(
                       Year = col_double(),
                       CRUISE = col_character(),
                       STATION_ID = col_character()
                        ))

# vector of surveys to omit from checking since already looked at
omitCruise <- c("2003-16", "1998-16",  "2015-56", 
                "2000-16", "2009-01", "1999-14", "2013-10",
                "2006-22", "2002-15", "2014-66", "2006-31",
                "2015-15", "2005-03", "2013-43")

# test which stations are not matching up
test <- df %>%
  anti_join(., stations, by = "STATION_ID") %>%
  filter(!(Cruise %in% omitCruise))
# # used this extra code while testing larger numbers of stations to find survey-wide issues
# %>%
#   group_by(Cruise) %>%
#   count() %>%
#   ungroup() %>%
#   arrange(-n)

# number of cruises
length(unique(df$Cruise))

# number of observations
nrow(df)

# write as csv to review
#write_csv(df, here("Output", "csvFiles", paste0(cruise, ".csv")), na = "")
write_csv(df, here("Output", str_c("CTD_DATA_2019125_", 
                   str_replace_all(Sys.Date(), "-", ""),
                   ".csv")), na = "")

###################################
# use to produce heat map to see locations and relative temperatures

# get max lat and long for survey
meanLat <- mean(df$Latitude, na.rm = TRUE)
meanLong <- mean(df$Longitude, na.rm = TRUE)

# graph results as qc
leaflet(df) %>% addTiles() %>%
  setView(lng = meanLong, lat = meanLat, zoom = 6) %>%
  addHeatmap(lng = ~Longitude, lat = ~Latitude, intensity = ~Temp10m, blur = 20, max = 20, radius = 12)

# need to export manually unless you install PhantomJS
# https://stackoverflow.com/questions/31336898/how-to-save-leaflet-in-r-map-as-png-or-jpg-file

###################################