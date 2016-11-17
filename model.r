########################
## Modeling code file ##
########################

## load packages
library(lme4)

## load data
load('all_data')
all_data$desert <- as.numeric(all_data$desert)

model_data <- subset(all_data, TOTAL.POPULATION > 0)


######################
## Complete pooling ##
######################

cp <- glm(desert ~ CTA_counts + vacant_counts, family = 'binomial', data = model_data)
summary(cp)
################
## No pooling ##
################

np <- glm(desert ~ CTA_counts + vacant_counts + Neighborhood, family = 'binomial', data = model_data)
summary(np)

#####################
## Partial pooling ##
#####################

pp <- glmer(desert ~ CTA_counts + vacant_counts + (1 | Neighborhood), data = model_data, family = 'binomial')
summary(pp)

##################
## Hierarchical ##
##################

mlm <- glmer(desert ~ CTA_counts + vacant_counts + Diabetes.related + (1 | Neighborhood),
             data = model_data,
             family = 'binomial')
summary(mlm)

mlm_2 <- glmer(desert ~ CTA_counts + vacant_counts + Diabetes.related + Below.Poverty.Level + (1 | Neighborhood),
             data = model_data,
             family = 'binomial')
summary(mlm_2)


## rescale variables
exclude <- c('Neighborhood', 'TRACT_BLOC','STATEFP10', 'COUNTYFP10', 'TRACTCE10', 'BLOCKCE10', 'GEOID10', 'NAME10', 'Longitude', 'Latitude', 'Community.Area.y', 'nearest_supermarket', 'Community.Area.x', 'store_counts', 'desert')
potential_covariates <- setdiff(names(all_data), exclude)
model_data_scale <- model_data
for (var in potential_covariates) {model_data_scale[var] <- as.numeric(scale(model_data[var]))}

mlm_c <- glmer(desert ~ CTA_counts + vacant_counts + Diabetes.related + (1 | Neighborhood),
             data = model_data_scale,
             family = 'binomial')
summary(mlm_c)

mlm_c_2 <- glmer(desert ~ CTA_counts + vacant_counts + Diabetes.related + Below.Poverty.Level + (1 | Neighborhood),
             data = model_data_scale,
             family = 'binomial')
summary(mlm_c_2)

mlm_c_3 <- glmer(desert ~ CTA_counts + vacant_counts + Diabetes.related + Below.Poverty.Level + NHB_p + PER.CAPITA.INCOME + (1 | Neighborhood),
             data = model_data_scale,
             family = 'binomial')
summary(mlm_c_3)

mlm_c_4 <- glmer(desert ~ CTA_counts + vacant_counts + Diabetes.related + Below.Poverty.Level + NHB_p + PER.CAPITA.INCOME + HISP_p + (1 | Neighborhood),
             data = model_data_scale,
             family = 'binomial')
summary(mlm_c_4)

mlm_c_5 <- glmer(desert ~ CTA_counts + vacant_counts + Diabetes.related + Below.Poverty.Level + NHB_p + PER.CAPITA.INCOME + HISP_p + TOTAL.POPULATION + (1 | Neighborhood),
             data = model_data_scale,
             family = 'binomial')
summary(mlm_c_5)

mlm_c_5 <- glmer(desert ~ CTA_counts + vacant_counts + Diabetes.related + Below.Poverty.Level + NHW_p + NHB_p + HISP_p + PER.CAPITA.INCOME  + TOTAL.POPULATION + (1 | Neighborhood),
             data = model_data_scale,
             family = 'binomial')
summary(mlm_c_5)

cor(model_data[, c('Diabetes.related', 'NHB_p', 'NHW_p', 'HISP_p')])
