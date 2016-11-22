
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

cp <- glm(desert ~ CTA_counts + vacant_counts + crime, family = 'binomial', data = model_data)
summary(cp)
################
## No pooling ##
################

np <- glm(desert ~ CTA_counts + vacant_counts + crime + Neighborhood, family = 'binomial', data = model_data)
summary(np)

#####################
## Partial pooling ##
#####################

pp <- glmer(desert ~ CTA_counts + vacant_counts + crime + (1 | Neighborhood), data = model_data, family = 'binomial')
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

library(mice)

vars <- c(                         
"vacant_counts"                               ,
"CTA_counts"                                  ,
"store_counts"                                ,
"nearest_supermarket"                         ,
"TOTAL.POPULATION"                            ,
"desert"                                      ,
"Community.Area.x"                            ,
"Birth.Rate"                                  ,
"General.Fertility.Rate"                      ,
"Low.Birth.Weight"                            ,
"Prenatal.Care.Beginning.in.First.Trimester"  ,
"Preterm.Births"                              ,
"Teen.Birth.Rate"                             ,
"Assault..Homicide."                          ,
"Breast.cancer.in.females"                    ,
"Cancer..All.Sites."                          ,
"Colorectal.Cancer"                           ,
"Diabetes.related"                            ,
"Firearm.related"                             ,
"Infant.Mortality.Rate"                       ,
"Lung.Cancer"                                 ,
"Prostate.Cancer.in.Males"                    ,
"Stroke..Cerebrovascular.Disease."            ,
"Childhood.Blood.Lead.Level.Screening"        ,
"Childhood.Lead.Poisoning"                    ,
"Gonorrhea.in.Females"                        ,
"Gonorrhea.in.Males"                          ,
"Tuberculosis"                                ,
"Below.Poverty.Level"                         ,
"Crowded.Housing"                             ,
"Dependency"                                  ,
"No.High.School.Diploma"                      ,
"Per.Capita.Income"                           ,
"Unemployment"                                ,
"Community.Area.Number"                       ,
"PERCENT.OF.HOUSING.CROWDED"                  ,
"PERCENT.HOUSEHOLDS.BELOW.POVERTY"            ,
"PERCENT.AGED.16..UNEMPLOYED"                 ,
"PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA",
"PERCENT.AGED.UNDER.18.OR.OVER.64"            ,
"PER.CAPITA.INCOME"                           ,
"HARDSHIP.INDEX"                              ,
"Community.Area.y"                            ,
"NHW"                                         ,
"NHB"                                         ,
"NHAM"                                        ,
"NHAS"                                        ,
"NHOTHER"                                     ,
"HISP"                                        ,
"Multiple.Race.."                             ,
"TOTAL"                                       ,
"NHW_p"                                       ,
"NHB_p"                                       ,
"NHAM_p"                                      ,
"NHAS_p"                                      ,
"NHOTHER_p"                                   ,
"HISP_p"                                      ,
"Multiple.Race.._p")

tmp_data <- mice(model_data_scale[, vars])

complete_datas <- lapply(1:5, function(i) complete(tmp_data, i))
complete_datas2 <- lapply(complete_datas, function(dat) {dat$Neighborhood <- model_data_scale$Neighborhood; dat})

models <- lapply(complete_datas2, function(dat)  glmer(desert ~ CTA_counts + vacant_counts + (1 | Neighborhood), data = dat, family = 'binomial'))

fits <- with(tmp_data, glmer(desert ~ CTA_counts + vacant_counts + (1 | Neighborhood), family = 'binomial'))
pooled <- pool(fits)

tmp_data <- mice(model_data_scale[,!(names(model_data_scale) %in% c('Neighborhood', 'Community.Area.x', 'Community.Area.y'))])

cp <- glm(desert ~ CTA_counts + vacant_counts + crime, family = 'binomial', data = model_data_scale)
summary(cp)
################
## No pooling ##
################

np <- glm(desert ~ CTA_counts + vacant_counts + crime + Neighborhood, family = 'binomial', data = model_data_scale)
summary(np)

#####################
## Partial pooling ##
#####################

pp <- glmer(desert ~ CTA_counts + vacant_counts + crime + (1 | Neighborhood), data = model_data_scale, family = 'binomial', verbose = TRUE)
summary(pp)

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

form <- as.formula(paste('desert ~ vacant_counts + CTA_counts +',
                         paste(setdiff(search_covariates,
                                       c('NHW', 'NHW_p', 'Multiple.Race..', 'Multiple.Race.._p', "PERCENT.AGED.UNDER.18.OR.OVER.64",
                                         "PERCENT.HOUSEHOLDS.BELOW.POVERTY")), collapse = '+'),
                         '+(1|Neighborhood)'))

complete_model <- glmer(form, data = model_data_scale, family = 'binomial', verbose = TRUE)


fit_model <- function(i) {
    print(paste('fitting:',i))
    form <- as.formula(paste('desert ~ vacant_counts + CTA_counts +',
                             paste(c(in_vars, out_vars[i]), collapse = '+'),
                             '+(1|Neighborhood)'))
    model <- glmer(form, data = model_data_scale, family = 'binomial')
    return(c(i, AIC(model)))
}

model <- glmer(desert ~ CTA_counts + vacant_counts + Gonorrhea.in.Females + Cancer..All.Sites. + TOTAL.POPULATION + NHAS +
                   Dependency +Childhood.Lead.Poisoning + Prenatal.Care.Beginning.in.First.Trimester + Gonorrhea.in.Males
               + NHAM_p + Multiple.Race.. + Stroke..Cerebrovascular.Disease. + Firearm.related + Tuberculosis + NHW_p +
               Teen.Birth.Rate + No.High.School.Diploma + Lung.Cancer +
               (1|Neighborhood),
               data = model_data_scale,
               family = 'binomial',
               verbose = TRUE)

model2 <- glmer(desert ~ CTA_counts +
                   vacant_counts +
                   Gonorrhea.in.Females +
                   Cancer..All.Sites. +
                   TOTAL.POPULATION +
                   NHAS +
                   Dependency +
                   Childhood.Lead.Poisoning +
                   Prenatal.Care.Beginning.in.First.Trimester +
                   Gonorrhea.in.Males +
                   NHAM_p +
                   Multiple.Race.. +
                   Stroke..Cerebrovascular.Disease. +
                   Firearm.related +
                   Tuberculosis +
                   NHW_p +
                   Teen.Birth.Rate +
                   No.High.School.Diploma +
                   Lung.Cancer +
                   Colorectal.Cancer +
                   Low.Birth.Weight +
                   Preterm.Births +
                   Assault..Homicide. +
                   Breast.cancer.in.females + 
                   (1|Neighborhood),
               data = model_data_scale,
               family = 'binomial',
               verbose = TRUE,
               control = glmerControl(calc.derivs = FALSE))



model3 <- glmer(desert ~ CTA_counts +
                   vacant_counts +
                   Gonorrhea.in.Females +
                   TOTAL.POPULATION +
                   NHAS +
                   Dependency +
                   Childhood.Lead.Poisoning +
                   Prenatal.Care.Beginning.in.First.Trimester +
                   Gonorrhea.in.Males +
                   NHAM_p +
                   Multiple.Race.. +
                   Stroke..Cerebrovascular.Disease. +
                   Tuberculosis +
                   Teen.Birth.Rate +
                   No.High.School.Diploma +
                   Lung.Cancer +
                   Colorectal.Cancer +
                   (1|Neighborhood),
               data = model_data_scale,
               family = 'binomial',
               verbose = TRUE,
               control = glmerControl(calc.derivs = FALSE), optCtrl=list(maxfun=5000))

model4 <- glmer(desert ~ CTA_counts +
                    vacant_counts +
                    crime +
                    Cancer..All.Sites. +
                    TOTAL.POPULATION +
                    NHAS +
                    Dependency +
                    Childhood.Lead.Poisoning +
                    Prenatal.Care.Beginning.in.First.Trimester +
                    NHAM_p +
                    Multiple.Race.. +
                    Stroke..Cerebrovascular.Disease. +
                    Tuberculosis +
                    Teen.Birth.Rate +
                    No.High.School.Diploma +
                    Lung.Cancer +
                    Colorectal.Cancer +
                    (1|Neighborhood),
                data = model_data_scale,
                family = 'binomial',
                verbose = TRUE,
                control = glmerControl(calc.derivs = FALSE, optCtrl=list(maxfun=5000)))

model5 <- glmer(desert ~ . -
                    Neighborhood -
                    TRACT_BLOC -
                    STATEFP10 -
                    COUNTYFP10 -
                    TRACTCE10 -
                    BLOCKCE10 -
                    GEOID10 -
                    NAME10 -
                    Latitude -
                    Longitude -
                    Birth.Rate - 
                    Community.Area.Number -
                    Childhood.Blood.Lead.Level.Screening -
                    Childhood.Lead.Poisoning - 
                    Community.Area.x -
                    Community.Area.y -
                    Gonorrhea.in.Females -
                    Gonorrhea.in.Males -
                    Prostate.Cancer.in.Males -
                    Breast.cancer.in.females -
                    Colorectal.Cancer - 
                    TOTAL -
                    Multiple.Race.. -
                    PERCENT.HOUSEHOLDS.BELOW.POVERTY -
                    PERCENT.OF.HOUSING.CROWDED -
                    PERCENT.AGED.16..UNEMPLOYED -
                    PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA - 
                    PER.CAPITA.INCOME -
                    PERCENT.AGED.UNDER.18.OR.OVER.64 -
                    HARDSHIP.INDEX - 
                    nearest_supermarket -
                    Below.Poverty.Level -
                    Prenatal.Care.Beginning.in.First.Trimester -
                    Preterm.Births -
                    Low.Birth.Weight -
                    Crowded.Housing -
                    General.Fertility.Rate -
                    Infant.Mortality.Rate - 
                    NHW -
                    NHW_p -
                    NHOTHER -
                    NHOTHER_p - 
                    Multiple.Race.._p +
                    (1|Neighborhood),
                data = model_data_scale,
                family = 'binomial',
                verbose = 2,
                control = glmerControl(calc.derivs = FALSE, optCtrl = list(maxfun = 5000)))

summary(model6 <- glmer(desert ~ CTA_counts + crime + vacant_counts +
                            Cancer..All.Sites. +
                            Diabetes.related +
                            Dependency +
                            TOTAL.POPULATION +
                            (1 | Neighborhood),
                        data = model_data_scale,
                        family = 'binomial',
                        verbose = 2,
                        control = glmerControl(calc.derivs = FALSE, optCtrl = list(maxfun = 1000))))

## Gonorrhea in females
## Cancer all sites
## Total population
## NHAS
## Dependency
## Childhood lead poisoning
## Prenatal care beginning in first trimester
## Gonorrhea in males
## NHAM_p
## Multiple.Race..
## Stroke..Cerebrovascular.Disease
## Firearm.related
## Tuberculosis
## NHW_p
## Teen birth rate
## No.high.school.diploma
## Lung.cancer

summary(model7 <- glmer(desert ~ CTA_counts + crime + vacant_counts +
                            Cancer..All.Sites. +
                            TOTAL.POPULATION +
                            Prenatal.Care.Beginning.in.First.Trimester +
                            NHAM_p +
                            Multiple.Race.. +
                            Stroke..Cerebrovascular.Disease. +
                            Tuberculosis +
                            NHW_p +
                            Teen.Birth.Rate +
                            No.High.School.Diploma +
                            Lung.Cancer + 
                            (1 | Neighborhood),
                        data = model_data_scale,
                        family = 'binomial',
                        verbose = 2,
                        control = glmerControl(calc.derivs = FALSE, optCtrl = list(maxfun = 2000))
                        ))

summary(pp <- glmer(desert ~ CTA_counts + vacant_counts + crime + (1 | Neighborhood), data = model_data_scale, family = 'binomial', verbose = 2))

summary(cp <- glm(desert ~ CTA_counts + vacant_counts + crime, data = model_data_scale, family = 'binomial'))

t_data <- complete_datas2[[1]]

model4 <- glmer(desert ~ CTA_counts +
                   vacant_counts +
                   TOTAL.POPULATION +
                   NHAS +
                   Dependency +
                   Childhood.Lead.Poisoning +
                   Prenatal.Care.Beginning.in.First.Trimester +
                   NHAM_p +
                   Multiple.Race.. +
                   Stroke..Cerebrovascular.Disease. +
                   Tuberculosis +
                   Teen.Birth.Rate +
                   No.High.School.Diploma +
                   Lung.Cancer +
                   Colorectal.Cancer +
                   (1|Neighborhood),
               data = t_data,
               family = 'binomial',
               verbose = TRUE,
               control = glmerControl(calc.derivs = FALSE, optCtrl=list(maxfun=5000)))

cp_mses <- c(); np_mses <- c(); pp_mses <- c(); mlm_mses <- c();

for (i in 1:10) {
    cv_ind <- sample.split(model_data_scale$Neighborhood, SplitRatio = .8)
    train <- model_data_scale[cv_ind,]
    test <- model_data_scale[!cv_ind,]

    print(paste('TRAINING: ', i))
    
    cp <- glm(desert ~ CTA_counts + vacant_counts + crime, family = 'binomial', data = train)
    print(paste('AIC cp:', AIC(cp)))
    
    np <- glm(desert ~ CTA_counts + vacant_counts + crime + Neighborhood, family = 'binomial', data = train)
    print(paste('AIC np:', AIC(np)))
    
    pp <- glmer(desert ~ CTA_counts + vacant_counts + crime + (1 | Neighborhood), data = train, family = 'binomial')
    print(paste('AIC pp:', AIC(pp)))
    
    ## mlm <- glmer(desert ~ CTA_counts +
    ##                 vacant_counts +
    ##                 crime +
    ##                 TOTAL.POPULATION +
    ##                 NHAS +
    ##                 Dependency +
    ##                 Childhood.Lead.Poisoning +
    ##                 Prenatal.Care.Beginning.in.First.Trimester +
    ##                 NHAM_p +
    ##                 Multiple.Race.. +
    ##                 Stroke..Cerebrovascular.Disease. +
    ##                 Tuberculosis +
    ##                 Teen.Birth.Rate +
    ##                 No.High.School.Diploma +
    ##                 Lung.Cancer +
    ##                 Colorectal.Cancer +
    ##                 (1|Neighborhood),
    ##             data = train,
    ##             family = 'binomial',
    ##             control = glmerControl(calc.derivs = FALSE, optCtrl=list(maxfun=1000)))

   mlm <- glmer(desert ~ CTA_counts + crime + vacant_counts +
                            Cancer..All.Sites. +
                            Diabetes.related +
                            Dependency +
                            TOTAL.POPULATION +
                            (1 | Neighborhood),
                        data = train,
                        family = 'binomial',
                        control = glmerControl(calc.derivs = FALSE, optCtrl = list(maxfun = 1000)))

    
    print(paste('AIC mlm:', AIC(mlm)))
    
    print(paste('EVALUATING: ', i))

    cp_pred <- predict(cp, test, allow.new.levels = TRUE, type = 'response')

    print(cp_mse <- mean((cp_pred - test$desert)^2))
    cp_mses <- c(cp_mses, cp_mse)


    np_pred <- predict(np, test, allow.new.levels = TRUE, type = 'response')
    print(np_mse <- mean((np_pred - test$desert)^2))
    np_mses <- c(np_mses, np_mse)

    pp_pred <- predict(pp, test, allow.new.levels = TRUE, type = 'response')
    print(pp_mse <- mean((pp_pred - test$desert)^2))
    pp_mses <- c(pp_mses, pp_mse)

    mlm_pred <- predict(mlm, test, allow.new.levels = TRUE, type = 'response')
    print(mlm_mse <- mean((mlm_pred - test$desert)^2, na.rm = TRUE))
    mlm_mses <- c(mlm_mses, mlm_mse)
    
}


summary(model)

save(result, file = 'result')

in_vars <- c("Gonorrhea.in.Females" , 'Cancer..All.Sites.' , 'TOTAL.POPULATION' , 'NHAS' ,
                   'Dependency' ,'Childhood.Lead.Poisoning' , 'Prenatal.Care.Beginning.in.First.Trimester' , 'Gonorrhea.in.Males'
               , 'NHAM_p' , 'Multiple.Race..' , 'Stroke..Cerebrovascular.Disease.' , 'Firearm.related' , 'Tuberculosis' , 'NHW_p' ,
             'Teen.Birth.Rate' , 'No.High.School.Diploma' , 'Lung.Cancer', 'Colorectal.Cancer', 'HISP_p', 'Below.Poverty.Level', 'Breast.cancer.in.females', 'NHAM')
out_vars <- setdiff(search_covariates, in_vars)


old_aic <- AIC(model)


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



## model <- glmer(desert ~ CTA_counts + vacant_counts + Gonorrhea.in.Females + Cancer..All.Sites. + TOTAL.POPULATION + NHAS +
##                    Dependency +Childhood.Lead.Poisoning + Prenatal.Care.Beginning.in.First.Trimester + Gonorrhea.in.Males
##                + NHAM_p + Multiple.Race.. + Stroke..Cerebrovascular.Disease. + Firearm.related + Tuberculosis + NHW_p +
##                Teen.Birth.Rate + No.High.School.Diploma + Lung.Cancer +
##                (1|Neighborhood),
##                data = model_data_scale,
##                family = 'binomial')

## summary(model)

## in_vars <- c("Gonorrhea.in.Females" , 'Cancer..All.Sites.' , 'TOTAL.POPULATION' , 'NHAS' ,
##                    'Dependency' ,'Childhood.Lead.Poisoning' , 'Prenatal.Care.Beginning.in.First.Trimester' , 'Gonorrhea.in.Males'
##                , 'NHAM_p' , 'Multiple.Race..' , 'Stroke..Cerebrovascular.Disease.' , 'Firearm.related' , 'Tuberculosis' , 'NHW_p' ,
##              'Teen.Birth.Rate' , 'No.High.School.Diploma' , 'Lung.Cancer')
## out_vars <- setdiff(search_covariates, in_vars)


## old_aic <- AIC(model)

## search_results <- foreach(i=1:length(out_vars), .combine = 'rbind') %dopar% fit_model(i)
## min_aic <- which.min(search_results[,2])
## if (min_aic < old_aic) {
##     print(paste('ADDING:',out_vars[min_aic]))
##     in_vars <- c(in_vars, out_vars[min_aic])
##     out_vars <- setdiff(out_vars, out_vars[min_aic])
##     print(search_results[min_aic,])
## }
## else {
##     break
## }


cp <- glm(desert ~ CTA_counts + vacant_counts + crime, family = 'binomial', data = model_data_scale)
print(paste('AIC cp:', AIC(cp)))

np <- glm(desert ~ CTA_counts + vacant_counts + crime + Neighborhood, family = 'binomial', data = model_data_scale)
print(paste('AIC np:', AIC(np)))

pp <- glmer(desert ~ CTA_counts + vacant_counts + crime + (1 | Neighborhood), data = model_data_scale, family = 'binomial')
print(paste('AIC pp:', AIC(pp)))

## mlm <- glmer(desert ~ CTA_counts +
##                 vacant_counts +
##                 crime +
##                 TOTAL.POPULATION +
##                 NHAS +
##                 Dependency +
##                 Childhood.Lead.Poisoning +
##                 Prenatal.Care.Beginning.in.First.Trimester +
##                 NHAM_p +
##                 Multiple.Race.. +
##                 Stroke..Cerebrovascular.Disease. +
##                 Tuberculosis +
##                 Teen.Birth.Rate +
##                 No.High.School.Diploma +
##                 Lung.Cancer +
##                 Colorectal.Cancer +
##                 (1|Neighborhood),
##             data = model_data_scale,
##             family = 'binomial',
##             control = glmerControl(calc.derivs = FALSE, optCtrl=list(maxfun=1000)))

mlm <- glmer(desert ~ CTA_counts + crime + vacant_counts +
                 Cancer..All.Sites. +
                 Diabetes.related +
                 Dependency +
                 TOTAL.POPULATION +
                 (1 | Neighborhood),
             data = model_data_scale,
             family = 'binomial',
             control = glmerControl(calc.derivs = FALSE, optCtrl = list(maxfun = 1000)))


print(paste('AIC mlm:', AIC(mlm)))

summary(glm(desert ~ Birth.Rate + 
            General.Fertility.Rate                      + 
            Low.Birth.Weight                            + 
            Prenatal.Care.Beginning.in.First.Trimester  + 
            Preterm.Births                              + 
            Teen.Birth.Rate                             + 
            Assault..Homicide.                          + 
            Breast.cancer.in.females                    + 
            Cancer..All.Sites.                          + 
            Colorectal.Cancer                           + 
            Diabetes.related                            + 
            Firearm.related                             + 
            Infant.Mortality.Rate                       + 
            Lung.Cancer                                 + 
            Prostate.Cancer.in.Males                    + 
            Stroke..Cerebrovascular.Disease.            + 
            Tuberculosis                                + 
            Below.Poverty.Level                         + 
            Crowded.Housing                             + 
            Dependency                                  + 
            No.High.School.Diploma                      + 
            Per.Capita.Income                           + 
            Unemployment,
            data = model_data_scale,
            family = 'binomial'))
