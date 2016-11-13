#########################################
## Author: Daniel Berry                ##
## Description: Code to transform data ##
#########################################

library(sp)
library(magrittr)
library(stringr)
library(rgeos)

getwd()

mp.to.matrix <- function(mp_string) {
    matrix(as.numeric(unlist(lapply(mp_string %>% str_sub(17, -4) %>% str_split(', '), function(s) str_split(s, ' ')))), ncol = 2, byrow = TRUE)
}

compute.center <- function(coords) {c(mean(coords[,1]), mean(coords[,2]))}


blocks_raw <- read.csv('CensusBlockTIGER2010.csv', stringsAsFactors = FALSE)

centers <- do.call('rbind', lapply(blocks_raw$the_geom, function(s) compute.center(mp.to.matrix(s))))

blocks_raw$X_center <- centers[,1]
blocks_raw$Y_center <- centers[,2]

vacant_raw <- read.csv('311_Service_Requests_-_Vacant_and_Abandoned_Buildings_Reported_-_Map.csv',
                       stringsAsFactors = FALSE,
                       skip = 1)

names(vacant_raw) <- c('Type',
                       'ID',
                       'Date_Recieved',
                       'Lot_Location',
                       'Open',
                       'Dangerous',
                       'Entry',
                       'Vacant',
                       'Fire',
                       'Users',
                       'Address_Number',
                       'Address_Direction',
                       'Address_Street',
                       'Address_Suffic',
                       'Zip',
                       'X_Coordinate',
                       'Y_Coordinate',
                       'Latitude',
                       'Longitude',
                       'Location_string')


