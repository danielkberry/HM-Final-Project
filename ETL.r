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
t1 <- as.matrix(blocks_raw[, c('Longitude', 'Latitude')])
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

groceries <- read.csv('food-deserts-master/data/Grocery_Stores_-_2011.csv', stringsAsFactors = FALSE)
## drop liquor stores
groceries <- groceries[grep('liquor', tolower(groceries$STORE.NAME), invert = TRUE),]
t3 <- as.matrix(groceries[groceries$SQUARE.FEET >= 10000, c('LONGITUDE', 'LATITUDE')])

## buffer <- .5 + .5*as.numeric()
## buffers <- do.call('rbind', lapply(1:nrow(blocks_raw), function(tmp) buffer))

## store_counts <- read.csv('store_counts.csv')

## blocks_raw$store_counts <- store_counts$x
system.time(dist_mat3 <- spDists(t1, t3, longlat = TRUE))

dist_mat3_mi <- dist_mat3 * 0.621371

store_counts_new <- rowSums(dist_mat3_mi <= 1)

nearest_supermarket <- apply(dist_mat3_mi, 1, min)

## write.csv(store_counts_new, 'store_counts.csv')
blocks_raw$store_counts <- store_counts_new
blocks_raw$nearest_supermarket <- nearest_supermarket

population <- read.csv('food-deserts-master/data/Population_by_2010_Census_Block.csv')

nrow(population)

blocks_raw$the_geom <- NULL

nrow(block_data <- merge(blocks_raw, population, by.x = 'TRACT_BLOC', by.y = 'CENSUS.BLOCK', all.x = TRUE))

##############################
## NEIGHBORHOOD INFORMATION ##
##############################

library(data.table)

crime <- fread('rows.csv')

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

## fix missed point in polygon

missing <- which(is.na(block_data$Neighborhood))
t1 <- as.matrix(block_data[which(!is.na(block_data$Neighborhood)),c('Longitude','Latitude')])
for (miss_ID in missing) {
    d1 <- as.matrix(block_data[miss_ID,c('Longitude', 'Latitude')])
    dist_mat <- spDists(d1,t1)
    block_data$Neighborhood[miss_ID] <- block_data[!is.na(block_data$Neighborhood),'Neighborhood'][which.min(dist_mat)]
}

save(block_data, file = 'block_data')
write.csv(block_data, file = 'block_data.csv')


## ggplot(block_data, aes(Longitude, Latitude, color = desert)) + geom_point(alpha = .1)
## ggplot(block_data, aes(Longitude, Latitude, color = nearest_supermarket)) + geom_point(alpha = .1)
## ggplot(block_data, aes(Longitude, Latitude, color = nearest_supermarket)) + stat_density_2d(aes(fill = ..level..), geom="polygon", n = 1000)

public_health <- read.csv('Public_Health_Statistics-_Selected_public_health_indicators_by_Chicago_community_area.csv', stringsAsFactors = FALSE)

socioeconomic <- read.csv('Census_Data_-_Selected_socioeconomic_indicators_in_Chicago__2008___2012.csv', stringsAsFactors = FALSE)

race <- read.csv('race.csv', stringsAsFactors = FALSE)

## NHW: Non-hispanic white
## NHB: non-hispanic black
## NHAM: american indian/alaskan native, non hispanic
## NHAS: asian, not hispanic
## NHOTHER: other single race, not hispanic


block_data$Neighborhood <- as.character(block_data$Neighborhood)

## Set up table to match subneighborhoods
block_replacements <- list(c('Andersonville', 'Edgewater'),
                     c('Wrigleyville', 'Edgewater'),
                     c('Boystown', 'Edgewater'),
                     c('Sheffield & DePaul', 'Lincoln Park'),
                     c('Bucktown', 'Logan Square'),
                     c('Old Town', 'Near North Side'),
                     c('Gold Coast', 'Near North Side'),
                     c('River North', 'Near North Side'),
                     c('Rush & Division','Near North Side'),
                     c('Streeterville', 'Near North Side'),
                     c('Magnificent Mile', 'Near North Side'),
                     c('Sauganash,Forest Glen', 'Forest Glen'),
                     c("Montclare"           , 'Montclaire' ),
                     c("Wicker Park"         , 'West Town'),
                     c("East Village"        , 'West Town'),
                     c("Ukrainian Village"   , 'West Town'),
                     c("Galewood"            , 'Austin'),
                     c("West Loop"           , 'Near West Side'),
                     c("United Center"       , 'Near West Side'),
                     c("Greektown"           , 'Near West Side'),
                     c("Little Italy, UIC"   , 'Near West Side'),
                     c("Little Village"      , 'South Lawndale'),
                     c("Millenium Park"      , 'Loop'),
                     c("Grant Park"          , 'Loop'),
                     c("Museum Campus"       , 'Loop'),
                     c("Printers Row"        , 'Loop'),
                     c("Jackson Park"        , 'Woodlawn'),
                     c("Grand Crossing"      , 'Greater Grand Crossing'),
                     c("Mckinley Park"       , 'McKinley Park'),
                     c("Chinatown"  , 'Near South Side')
                     )

for (tpl in block_replacements) {
    old <- tpl[1]; new <- tpl[2];
    block_data$Neighborhood[block_data$Neighborhood == old] <- new
}

## join west garfield park and east garfield park 

public_health$Gonorrhea.in.Males <- as.numeric(public_health$Gonorrhea.in.Males)

tmp <- colMeans(public_health[public_health$Community.Area.Name %in% c('East Garfield Park', 'West Garfield Park'), !(names(public_health) %in% c('Community.Area', 'Community.Area.Name'))])

public_health[88,'Community.Area.Name'] <- 'Garfield Park'
for (var in names(tmp)) {public_health[88,var] <- tmp[var]}



tmp <- colMeans(socioeconomic[socioeconomic$COMMUNITY.AREA.NAME %in% c('East Garfield Park', 'West Garfield Park'), !(names(socioeconomic) %in% c('Community.Area.Number', 'COMMUNITY.AREA.NAME'))])

socioeconomic[78,'COMMUNITY.AREA.NAME'] <- 'Garfield Park'
for (var in names(tmp)) {socioeconomic[78,var] <- tmp[var]}

socioeconomic$COMMUNITY.AREA.NAME[socioeconomic$COMMUNITY.AREA.NAME == 'Humboldt park'] <- 'Humboldt Park'
socioeconomic$COMMUNITY.AREA.NAME[socioeconomic$COMMUNITY.AREA.NAME == 'Washington Height'] <- 'Washington Heights'

race$X[race$X == 'Montclare'] <- 'Montclaire'

to_rep <- setdiff(names(race), c('X', 'Community.Area'))
for (var in to_rep) {race[,var] <- as.numeric(gsub(',','', race[,var]))}

tmp <- colMeans(race[race$X %in% c('East Garfield Park', 'West Garfield Park'), c("NHW","NHB", "NHAM", "NHAS", "NHOTHER", "HISP", "Multiple.Race..", "TOTAL")])
race[78,'X'] <- 'Garfield Park'
for (var in names(tmp)) {race[78,var] <- tmp[var]}



## Standardize names:
public_health$Neighborhood <- public_health$Community.Area.Name
public_health$Community.Area.Name <- NULL

socioeconomic$Neighborhood <- socioeconomic$COMMUNITY.AREA.NAME
socioeconomic$COMMUNITY.AREA.NAME <- NULL

race$Neighborhood <- race$X
race$X <- NULL



for (var in setdiff(to_rep, 'TOTAL')) {race[,paste0(var,'_p')] <- race[,var] / race[,'TOTAL']}

all_data <- merge(block_data, public_health, by = 'Neighborhood', all.x = TRUE)
all_data <- merge(all_data, socioeconomic, by = 'Neighborhood', all.x = TRUE)
all_data <- merge(all_data, race, by = 'Neighborhood', all.x = TRUE)



write.csv(all_data, file = 'all_data.csv')
save(all_data, file = 'all_data')



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
