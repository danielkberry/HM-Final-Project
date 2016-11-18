##############################
## Code to make some plots  ##
##############################

load('all_data')

## Plot location of missing blocks
library(ggplot2)

pct_missing <- lapply(all_data, function(var) sum(is.na(var)))

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
data.shape@data$id <- rownames(data.shape@data)
 
# create a data.frame from our spatial object
watershedPoints <- fortify(data.shape, region = "id")
 
# merge the "fortified" data with the data from our spatial object
watershedDF <- merge(nbhd_df, data.shape@data, by = "id")




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


######################
## Univariate Plots ##
######################

all_data$desert <- as.numeric(all_data$desert)





summary(glm(desert ~ NHB_p, family = 'binomial', data = all_data))

ggplot(all_data, aes(x = NHB_p, y = desert)) +
    geom_point() + stat_smooth(method = 'glm', method.args = list(family = 'binomial'))


summary(glm(desert ~ vacant_counts, family = 'binomial', data = all_data))

ggplot(all_data, aes(x = vacant_counts, y = desert)) +
    geom_point() + stat_smooth(method = 'glm', method.args = list(family = 'binomial'))

exclude <- c('Neighborhood', 'TRACT_BLOC','STATEFP10', 'COUNTYFP10', 'TRACTCE10', 'BLOCKCE10', 'GEOID10', 'NAME10', 'Longitude', 'Latitude', 'Community.Area.y', 'nearest_supermarket', 'desert', 'Community.Area.x', 'store_counts')
potential_covariates <- setdiff(names(all_data), exclude)

for (covar in potential_covariates) {
    print(covar)
    covar_plot <- ggplot(all_data, aes_string(x = covar, y = 'desert')) +
        geom_point() + stat_smooth(method = 'glm', method.args = list(family = 'binomial')) +
        theme_bw()
    ggsave(paste0('Plot of ',covar, '.png'), covar_plot)
}

##########
## MAPS ##
##########

all_data$desert_logical <- all_data$desert == 1
ggplot(all_data, aes(x = Longitude, y = Latitude, color = desert_logical)) + geom_point(alpha = .1) + theme_bw()
