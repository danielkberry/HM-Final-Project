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

ggmap(base_map, extent = 'normal') +
    geom_point(aes(Longitude, Latitude, color = missing), data = plot_data, alpha = .01) + theme_bw()

t <- project(as.matrix(nbhd_df[,c('long', 'lat')]),'+proj=merc +lat_0=36.66666666666666+lon_0=-88.33333333333333', inv = TRUE)

head(t <- project(as.matrix(nbhd_df[,c('long', 'lat')]),'+proj=merc +lat_0=36.66666666666666+lon_0=-88.33333333333333', inv = TRUE))

head(t <- project(as.matrix(all_data[,c('Longitude', 'Latitude')]),'+proj=tmerc +lat_0=36.66666666666666 +lon_0=-88.33333333333333 +k=0.9999749999999999 +x_0=300000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0'))


all_data$Longitude_t <- t[,1]
all_data$Latitude_t <- t[,2]
######################
## Univariate Plots ##
######################

all_data$desert <- as.numeric(all_data$desert)

plot_data <- subset(all_data, TOTAL.POPULATION > 0)



summary(glm(desert ~ NHB_p, family = 'binomial', data = all_data))

all_data$rider_density <- all_data$CTA_counts / all_data$TOTAL

ggplot(all_data, aes(x = NHB_p, y = desert)) +
    geom_point() + stat_smooth(method = 'glm', method.args = list(family = 'binomial'))

ggplot(all_data, aes(x = rider_density, y = desert)) +
    geom_point() + stat_smooth(method = 'glm', method.args = list(family = 'binomial'))

ggplot(plot_data, aes(x = Gonorrhea.in.Females, y = desert)) +
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

deserts_plot <- ggplot(all_data, aes(x = Longitude_t, y = Latitude_t, color = desert_logical)) +
    geom_point(alpha = .1) +
    theme_bw() +
    scale_color_manual(values = c('grey', 'black')) +
    geom_path(data = nbhd_df, aes(long, lat, group = id, color = NULL)) +
    labs(x = NULL, y = NULL, title = 'Food Desert Locations in Chicago') +
    guides(color = guide_legend(title = 'Food Desert Status')) +
    theme(axis.ticks.x = element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y=element_blank())

ggsave('deserts_plot.png', deserts_plot)


pct_black_plot <- ggplot(all_data, aes(x = Longitude_t, y = Latitude_t, color = NHB_p)) +
    geom_point(alpha = .1) +
    theme_bw() +
    geom_path(data = nbhd_df, aes(long, lat, group = id, color = NULL)) +
    geom_path(data = nbhd_df, aes(long, lat, group = id, color = NULL)) +
    labs(x = NULL, y = NULL, title = 'Racial Segregation: % Black') +
    guides(color = guide_legend(title = 'Percent Black')) +
    theme(axis.ticks.x = element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y=element_blank())
ggsave('pct_black_plot.png', pct_black_plot)

pct_white_plot <- ggplot(all_data, aes(x = Longitude_t, y = Latitude_t, color = NHW_p)) +
    geom_point(alpha = .1) +
    theme_bw() +
    geom_path(data = nbhd_df, aes(long, lat, group = id, color = NULL)) +
    geom_path(data = nbhd_df, aes(long, lat, group = id, color = NULL)) +
    labs(x = NULL, y = NULL, title = 'Racial Segregation: % White') +
    guides(color = guide_legend(title = 'Percent White')) +
    theme(axis.ticks.x = element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y=element_blank())

ggsave('pct_white_plot.png', pct_white_plot)


crime_plot <- ggplot(all_data, aes(x = Longitude_t, y = Latitude_t, color = crime)) +
    geom_point(alpha = .1) +
    theme_bw() +
    geom_path(data = nbhd_df, aes(long, lat, group = id, color = NULL)) +
    geom_path(data = nbhd_df, aes(long, lat, group = id, color = NULL)) +
    labs(x = NULL, y = NULL, title = 'Total Crime Within 1 Mile') +
    guides(color = guide_legend(title = 'Total Crime')) +
    theme(axis.ticks.x = element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y=element_blank())

ggsave('crime_plot.png', crime_plot)
crime_plot

income_plot <- ggplot(all_data, aes(x = Longitude_t, y = Latitude_t, color = PER.CAPITA.INCOME)) +
    geom_point(alpha = .1) +
    theme_bw() +
    geom_path(data = nbhd_df, aes(long, lat, group = id, color = NULL)) +
    geom_path(data = nbhd_df, aes(long, lat, group = id, color = NULL)) +
    labs(x = NULL, y = NULL, title = 'Per Capita Income') +
    guides(color = guide_legend(title = 'Income')) +
    theme(axis.ticks.x = element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y=element_blank())

ggsave('income_plot.png', income_plot)
income_plot

vacant_plot <- ggplot(all_data, aes(x = Longitude_t, y = Latitude_t, color = vacant_counts)) +
    geom_point(alpha = .1) +
    theme_bw() +
    geom_path(data = nbhd_df, aes(long, lat, group = id, color = NULL)) +
    geom_path(data = nbhd_df, aes(long, lat, group = id, color = NULL)) +
    labs(x = NULL, y = NULL, title = 'Vacant Buildings within 1 Mile') +
    guides(color = guide_legend(title = 'Vacancy')) +
    theme(axis.ticks.x = element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y=element_blank())

ggsave('vacant_plot.png', vacant_plot)
vacant_plot

pct_white_plot
