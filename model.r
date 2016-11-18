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

## mlm <- glmer(desert ~ CTA_counts + vacant_counts + Diabetes.related + (1 | Neighborhood),
##              data = model_data,
##              family = 'binomial')
## summary(mlm)

## mlm_2 <- glmer(desert ~ CTA_counts + vacant_counts + Diabetes.related + Below.Poverty.Level + (1 | Neighborhood),
##              data = model_data,
##              family = 'binomial')
## summary(mlm_2)


## rescale variables
exclude <- c('Neighborhood', 'TRACT_BLOC','STATEFP10', 'COUNTYFP10', 'TRACTCE10', 'BLOCKCE10', 'GEOID10', 'NAME10', 'Longitude', 'Latitude', 'Community.Area.y', 'nearest_supermarket', 'Community.Area.x', 'store_counts', 'desert')
potential_covariates <- setdiff(names(all_data), exclude)
model_data_scale <- model_data
for (var in potential_covariates) {model_data_scale[var] <- as.numeric(scale(model_data[var]))}

## mlm_c <- glmer(desert ~ CTA_counts + vacant_counts + Diabetes.related + (1 | Neighborhood),
##              data = model_data_scale,
##              family = 'binomial')
## summary(mlm_c)

## mlm_c_2 <- glmer(desert ~ CTA_counts + vacant_counts + Diabetes.related + Below.Poverty.Level + (1 | Neighborhood),
##              data = model_data_scale,
##              family = 'binomial')
## summary(mlm_c_2)

## mlm_c_3 <- glmer(desert ~ CTA_counts + vacant_counts + Diabetes.related + Below.Poverty.Level + NHB_p + PER.CAPITA.INCOME + (1 | Neighborhood),
##              data = model_data_scale,
##              family = 'binomial')
## summary(mlm_c_3)

## mlm_c_4 <- glmer(desert ~ CTA_counts + vacant_counts + Diabetes.related + Below.Poverty.Level + NHB_p + PER.CAPITA.INCOME + HISP_p + (1 | Neighborhood),
##              data = model_data_scale,
##              family = 'binomial')
## summary(mlm_c_4)

## mlm_c_5 <- glmer(desert ~ CTA_counts + vacant_counts + Diabetes.related + Below.Poverty.Level + NHB_p + PER.CAPITA.INCOME + HISP_p + TOTAL.POPULATION + (1 | Neighborhood),
##              data = model_data_scale,
##              family = 'binomial')
## summary(mlm_c_5)

## mlm_c_6 <- glmer(desert ~ CTA_counts + vacant_counts + Dependency + NHB_p  + TOTAL.POPULATION + (1 | Neighborhood),
##              data = model_data_scale,
##              family = 'binomial')
## summary(mlm_c_6)

## mlm_c_7 <- glmer(desert ~ CTA_counts + vacant_counts + Dependency  + HISP_p  + TOTAL.POPULATION + (1 | Neighborhood),
##              data = model_data_scale,
##              family = 'binomial')
## summary(mlm_c_7)

## mlm_c_8 <- glmer(desert ~ CTA_counts + vacant_counts + Dependency + Cancer..All.Sites. + HISP_p + TOTAL.POPULATION + (1 | Neighborhood),
##              data = model_data_scale,
##              family = 'binomial')
## summary(mlm_c_8)

## mlm_c_9 <- glmer(desert ~ CTA_counts + vacant_counts + Dependency + Cancer..All.Sites. + NHB_p +  TOTAL.POPULATION + (1 | Neighborhood),
##              data = model_data_scale,
##              family = 'binomial')
## summary(mlm_c_9)

## mlm_c_10 <- glmer(desert ~ CTA_counts + vacant_counts + Dependency + Cancer..All.Sites. + Diabetes.related + TOTAL.POPULATION + (1 | Neighborhood),
##              data = model_data_scale,
##              family = 'binomial')
## summary(mlm_c_10)

cor(model_data[, c('Diabetes.related', 'NHB_p', 'NHW_p', 'HISP_p', 'PER.CAPITA.INCOME')])

search_covariates <- setdiff(potential_covariates, c('vacant_counts', 'CTA_counts', 'Community.Area.Number'))

best_model <- glmer(desert ~ vacant_counts + CTA_counts + (1 | Neighborhood),
                    data = model_data_scale,
                    family = 'binomial')

in_vars <- c()
out_vars <- search_covariates

library(parallel)
library(doMC)

registerDoMC(detectCores() - 3)

old_aic <- AIC(best_model)

fit_model <- function(i) {
    print(paste('fitting:',i))
    form <- as.formula(paste('desert ~ vacant_counts + CTA_counts +',
                             paste(c(in_vars, out_vars[i]), collapse = '+'),
                             '+(1|Neighborhood)'))
    model <- glmer(form, data = model_data_scale, family = 'binomial')
    return(c(i, AIC(model)))
}

while(TRUE) {
    search_results <- foreach(i=1:length(out_vars), .combine = 'rbind') %dopar% fit_model(i)
    min_aic <- which.min(search_results[,2])
    if (min_aic < old_aic) {
        print(paste('ADDING:',out_vars[min_aic]))
        in_vars <- c(in_vars, out_vars[min_aic])
        out_vars <- setdiff(out_vars, out_vars[min_aic])
        print(search_results[min_aic,])
    }
    else {
        break
    }
}
print(paste('Final model:', in_vars, collapse = ', '))

model <- glmer(desert ~ CTA_counts + vacant_counts + Gonorrhea.in.Females + Cancer..All.Sites. + TOTAL.POPULATION + NHAS +
                   Dependency +Childhood.Lead.Poisoning + Prenatal.Care.Beginning.in.First.Trimester + Gonorrhea.in.Males
               + NHAM_p + Multiple.Race.. + Stroke..Cerebrovascular.Disease. + Firearm.related + Tuberculosis + NHW_p +
               Teen.Birth.Rate + No.High.School.Diploma + Lung.Cancer +
               (1|Neighborhood),
               data = model_data_scale,
               family = 'binomial')
summary(model)
