##############################
## Code to make some plots  ##
##############################

load('all_data')

## Plot location of missing blocks
library(ggplot2)

pct_missing <- lapply(all_data, function(var) mean(is.na(var)))

plot_data <- all_data
plot_data$missing <- is.na(plot_data$Neighborhood)
ggplot(plot_data, aes(Longitude,Latitude, color = Neighborhood)) + geom_point(alpha = .1)

library(ggmap)
ggmap('chicago', extent = 'normal')

## base map

base_map <- get_map(location = 'chicago')
ggmap(base_map, extent = 'normal')

library(sp)
library(rgeos)
library(rgdal)
library(maptools)
data.shape <- readOGR('./Neighborhoods_2012', layer = 'Neighborhoods_2012b')

data.shape@data$id <- rownames(data.shape@data)
nbhd_points <- fortify(data.shape, region = 'id')
nbhd_df <- merge(nbhd_points, data.shape@data, by = 'id')

ggplot(nbhd_df, aes(long, lat, group = id)) + geom_path()

# add to data a new column termed "id" composed of the rownames of data
dataProjected@data$id <- rownames(dataProjected@data)
 
# create a data.frame from our spatial object
watershedPoints <- fortify(dataProjected, region = "id")
 
# merge the "fortified" data with the data from our spatial object
watershedDF <- merge(watershedPoints, dataProjected@data, by = "id")

## sp_block_data <- block_data
## coordinates(sp_block_data) <- ~ Longitude + Latitude
## proj4string(sp_block_data) <- CRS("+proj=longlat")
## ## proj4string(sp_block_data) <- proj4string(data.shape)
## sp_block_data <- spTransform(sp_block_data, proj4string(data.shape))

## sp_block_data_df <- fortify(sp_block_data)

ggmap(base_map, extent = 'normal') + geom_point(aes(Longitude, Latitude, color = missing), data = plot_data, alpha = .01)

t <- project(as.matrix(nbhd_df[,c('long', 'lat')]),'+proj=merc +lat_0=36.66666666666666+lon_0=-88.33333333333333', inv = TRUE)

head(t <- project(as.matrix(nbhd_df[,c('long', 'lat')]),'+proj=merc +lat_0=36.66666666666666+lon_0=-88.33333333333333', inv = TRUE))

head(t <- project(as.matrix(nbhd_df[,c('long', 'lat')]),'+proj=merc +lat_0=36.66666666666666 +lon_0=-88.33333333333333 +k=0.9999749999999999 +x_0=300000 +y_0=0', inv = TRUE))
