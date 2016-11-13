#########################################
## Author: Daniel Berry                ##
## Description: Code to transform data ##
#########################################

library(sp)
library(magrittr)
library(stringr)
library(rgeos)
library(fuzzyjoin)
library(geosphere)

getwd()

mp.to.matrix <- function(mp_string) {
    matrix(as.numeric(unlist(lapply(mp_string %>% str_sub(17, -4) %>% str_split(', '), function(s) str_split(s, ' ')))), ncol = 2, byrow = TRUE)
}

compute.center <- function(coords) {c(mean(coords[,1]), mean(coords[,2]))}


blocks_raw <- read.csv('CensusBlockTIGER2010.csv', stringsAsFactors = FALSE)


centers <- do.call('rbind', lapply(blocks_raw$the_geom, function(s) compute.center(mp.to.matrix(s))))

blocks_raw$Longitude <- centers[,1]
blocks_raw$Latitude  <- centers[,2]

blocks_raw <- blocks_raw[apply(!is.na(blocks_raw[,c('Longitude', 'Latitude')]), 1, any),]

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

vacant_raw <- vacant_raw[apply(!is.na(vacant_raw[,c('Longitude', 'Latitude')]), 1, any),]

## tmp <- geo_full_join(blocks_raw[1:1,], vacant_raw[1:1,], by = c('Longitude', 'Latitude'), distance_col = 'dist') 

## system.time(dist_mat <- distm(blocks_raw[1:1000,c('Longitude','Latitude')], vacant_raw[1:1000,c('Longitude','Latitude')]))
t1 <- as.matrix(blocks_raw[, c('Longitude', 'Latitude')])
t2 <- as.matrix(vacant_raw[, c('Longitude', 'Latitude')])
system.time(dist_mat <- spDists(t1, t2, longlat = TRUE))

dist_mat <- dist_mat / 1609.344

counts <- rowSums(dist_mat <= 1)


## TODO:
## - Block level features: 
##   - Compute population within a threshold (probably 1 mile due to how long everything takes to run)
##   - Compute bus ridership within threshold
##   - Compute crimes within a certain threshold
## - Neighborhood level features (load in and join):
##   - Demographics
##     - Racial breakdown
##     - Poverty
##     - Income
##   - Public Health
##     - Cause of death? Diabetes?
##     - Public Health Indicators

