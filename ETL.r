#########################################
## Author: Daniel Berry                ##
## Description: Code to transform data ##
#########################################

library(sp)
library(magrittr)
library(stringr)
## library(rgeos)
## library(fuzzyjoin)
library(geosphere)

getwd()

#############################
## BLOCK LEVEL INFORMATION ##
#############################

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
## t1 <- as.matrix(blocks_raw[, c('Longitude', 'Latitude')])
## t2 <- as.matrix(vacant_raw[, c('Longitude', 'Latitude')])
## system.time(dist_mat <- spDists(t1, t2, longlat = TRUE))

## dist_mat <- dist_mat / 1609.344

## counts <- rowSums(dist_mat <= 1)

vacant_counts <- read.csv('counts.csv')
blocks_raw$vacant_counts <- vacant_counts$x


CTA_data <- read.csv('CTA_data.csv', stringsAsFactors = FALSE)

## CTA_locations <- do.call('rbind', lapply(CTA_data$location, function(s) {unlist(lapply(str_split(gsub('\\(|\\)', '', s), ', '), as.numeric))}))

## CTA_data$Latitude  <- CTA_locations[,1]
## CTA_data$Longitude <- CTA_locations[,2]

## t2 <- as.matrix(CTA_data[, c('Longitude', 'Latitude')])
## system.time(dist_mat2 <- spDists(t1, t2, longlat = TRUE))

## in_dist <- dist_mat2 <= 1
## CTA_counts <- apply(in_dist, 1, function(row) sum(CTA_data[which(row), 'boardings']))
## write.csv(CTA_counts, file = 'CTA_counts.csv')
## t <- apply(in_dist, 1, function(row) {sum(CTA_locations[which(row), 'boardings'])})

CTA_counts <- read.csv('CTA_counts.csv')

blocks_raw$CTA_counts <- CTA_counts$x

## groceries <- read.csv('food-deserts-master/data/Grocery_Stores_-_2011.csv', stringsAsFactors = FALSE)
## t3 <- as.matrix(groceries[, c('LONGITUDE', 'LATITUDE')])
store_counts <- read.csv('store_counts.csv')

blocks_raw$store_counts <- store_counts$x
## system.time(dist_mat3 <- spDists(t1, t3, longlat = TRUE))
## store_counts <- rowSums(dist_mat3 <= 1)

## write.csv(store_counts, 'store_counts.csv')


population <- read.csv('food-deserts-master/data/Population_by_2010_Census_Block.csv')

nrow(population)

blocks_raw$the_geom <- NULL

nrow(block_data <- merge(blocks_raw, population, by.x = 'TRACT_BLOC', by.y = 'CENSUS.BLOCK', all.x = TRUE))



library(ggplot2)

library(sp)
library(rgeos)
library(rgdal)
data.shape <- readOGR('./Neighborhoods_2012', layer = 'Neighborhoods_2012b')

data.shape_df <- fortify(data.shape)

sp_block_data <- block_data
coordinates(sp_block_data) <- ~ Longitude + Latitude
proj4string(sp_block_data) <- CRS("+proj=longlat")
## proj4string(sp_block_data) <- proj4string(data.shape)
sp_block_data <- spTransform(sp_block_data, proj4string(data.shape))
t <- over(sp_block_data, data.shape)

block_data$Neighborhood <- t$PRI_NEIGH
block_data$desert <- block_data$store_counts == 0

ggplot(block_data, aes(Longitude, Latitude, color = desert)) + geom_point(alpha = .1)

public_health <- read.csv('Public_Health_Statistics-_Selected_public_health_indicators_by_Chicago_community_area.csv')

socioeconomic <- read.csv('Census_Data_-_Selected_socioeconomic_indicators_in_Chicago__2008___2012.csv')




## TODO:
## - Block level features: 
##   - Compute population within a threshold (probably 1 mile due to how long everything takes to run)
##   - Compute bus ridership within threshold
##   - Compute crimes within a certain threshold
##   - TODO: RECOMPUTE Grocery counts based on size
## - Neighborhood level features (load in and join):
##   - Demographics
##     - Racial breakdown
##     - Poverty
##     - Income
##   - Public Health
##     - Cause of death? Diabetes?
##     - Public Health Indicators

