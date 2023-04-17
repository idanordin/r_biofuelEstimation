#######################################
# Run econometric model for biofuel
#######################################

#removing old information
rm(list=ls())

# list of relevant libraries for GIS plotting (found from spatial analysis course) 
packGIS<-c("rgdal", "classInt", "GISTools", "maps", "raster", "foreign", "shape", "ggmap","readxl","rgeos")

# installing relevant libraries from list 
for (i in packGIS){
  install.packages(pkgs=i, dependencies=TRUE)
}
# List pacakges used for estimation and data analysis
packEst <- c('readr',"car","MASS","corrplot", "car","dplyr","tidyr","StatMatch","ecodist","reshape2","Metrics","caret","lmtest","leaps",'sandwich','stats')

# installing relevant libraries from list 
for (i in packEst){
  install.packages(pkgs=i, dependencies=TRUE)
}
install.packages('readr')
# check for updates
update.packages()

# calling libraries 
lapply(packGIS, require, character.only=T)
lapply(packEst, require, character.only=T)

###########################################################################
### setting working directory relevant paths
###########################################################################


## define data, result and graph path
data_path<-"C:/R_estimation/pIVestim/data"
result_path<-"C:/R_estimation/pIVestim/results"
graphs_path<-"C:/R_estimation/pIVestim/graphs"


#set default path as data path
setwd(data_path)

###########################################################################
### read in data
###########################################################################
# Chose data to use
data_file <- "regrdata_march2023"

# name data (biofuel dataframe)

bf.df <- 0

# Read in from simulation experiments and name columns
bf.df <-  read_excel(paste(data_file, ".xls", sep =""), sheet = 'regr_dataR')

names(bf.df)[1]<-paste("source")
names(bf.df)[2]<-paste("scenario")
names(bf.df)[3]<-paste("region")

summary(bf.df)

# make NA to 0 - not so good?
#bf.df[is.na(bf.df)] <- 0

# assign an ID number
bf.df$ID <- seq.int(nrow(bf.df))

# Make subset with only BF and CAPRI normalized data, to extract only covariates
#covariates.df <-  read_excel(paste("regrdata_march2023.xls"), sheet = 'regr_data')
#covariates.dfnames(covariates.df)[1]<-paste("simulation")
#names(covariates.df)[2]<-paste("region")

#covariates.df <- bf.df[,!names(bf.df) %in% c("CAPRI", "normalization")]

# binding the first two columns as it is 
# and stacking the columns of different sources /(BF and capri normalized)
#stack.covariates.df <- cbind(covariates.df[1:2], stack(covariates.df[3:4]))
#print(stack.covariates.df)

#can't put zero values to NA as this migh give false estimations, but think somethings doesn't work when we have NA...
#maybe need to remove NA, but fix it in step before importing data
#stack.covariates.df[is.na(covariates.df)] <- 0

## Spread covariates into each column
#spread.covariates.df <- spread(stack.covariates.df,variables, values)

###########################################################################
### Analyse data /data summary
###########################################################################
summary(bf.df)
str(bf.df)
head(bf.df)

### Analyse variables for estimation



# only sources with same scaling of variables
bf.df.var <- bf.df[bf.df$source %in% c('BM','CAPRI_norm'),]

# remove NA for land share, should be zero if so
bf.df.var $landShare[is.na(bf.df.var $landShare)] <- 0

bf.df.var$region <- as.numeric(bf.df.var$region)
# make subset of lan
bf_lan.df.var <- subset(bf.df.var, region <= 100)
bf_lan.df.var.south<-subset(bf_lan.df.var,region <20)
bf_lan.df.var.north <- subset(bf_lan.df.var,region >19)

# Make subset of varaibles only
bf.df.var <- subset(bf.df.var, select = -c(scenario,region,borderDummy,ID))

# Make subset of varaibles only for lan
bf_lan.df.var <- subset(bf_lan.df.var, select = -c(source,scenario,region,borderDummy,ID))

summary(bf_lan.df.var)



#calculate correlations
corr.bf_lan.df.var<- cor(bf_lan.df.var, method = c("spearman"))
round(corr.bf_lan.df.var, 2)

#plot color scaled correlation matrix
corrplot(corr.bf_lan.df.var)

### Compute VIF data for each independent variable
# specify model with all variables
mod.all<-lm(landShare~yield+agriDens+leyDens+leyElas+GHGintensity+feedstockCost+carbonPrice, data=bf_lan.df.var)
summary(mod.all)
mod.all.carbonPrice<-lm(landShare~carbonPrice*(yield+agriDens+leyDens+leyElas+GHGintensity+feedstockCost+carbonPrice), data=bf_lan.df.var)
summary(mod.all.carbonPrice)

# for a linear model each variable one time
car::vif(mod.all)

# Vif for a model with interaction terms - hence "predictor"
car::vif(mod.all.carbonPrice, type="predictor")

### Compare estimation data and proejtetion data
# make boxplots to compare data
boxplot(bf_lan.df.var, col = rainbow(ncol(bf_lan.df.var)), main = "Customized boxplot in base R")

### make boxplots of covariates from different sources
#########################################################
#boxplot(bf.df$yield ~ bf.df$ind, main = "Customized boxplot in base R", outline=FALSE)
#boxplot(spread.covariates.df2$yield ~ spread.covariates.df2$ind, main = "Customized boxplot in base R", outline=FALSE)

# 4 figures arranged in 2 rows and 2 columns

par(mfrow=c(3,2))
boxplot(bf.df.var$agriDens ~ bf.df.var$source, main = "agridens", outline=FALSE)
boxplot(bf.df.var$feedstockCost ~ bf.df.var$source, main = "feedstock cost", outline=FALSE)
boxplot(bf.df.var$GHGintensity ~ bf.df.var$source, main = "GHGintensity", outline=FALSE)
boxplot(bf.df.var$leyDens ~ bf.df.var$source, main = "leyDens", outline=FALSE)
boxplot(bf.df.var$leyElas ~ bf.df.var$source, main = "leyElas", outline=FALSE)
boxplot(bf.df.var$yield ~  bf.df.var$source, main = "yield", outline=FALSE)


### Trying to make distance matrix for covariates - does not work 
#spread.covariates.df2.BF <-  subset(spread.covariates.df2, ind=='BM')
#spread.covariates.df2.BF
#myvars <- names(spread.covariates.df2.BF) %in% c('region', "ind",'landShare','borderDummy','carbonPrice')
#spread.covariates.df2.BF <- spread.covariates.df2.BF[!myvars]
#spread.covariates.df2.BF <- spread.covariates.df2.BF[rowSums(is.na(spread.covariates.df2.BF)) != ncol(spread.covariates.df2.BF), ]
#spread.covariates.df2.BF <- spread.covariates.df2.BF[complete.cases(spread.covariates.df2.BF), ] 


#spread.covariates.df2.CAPRI_norm <-  subset(spread.covariates.df2, ind=='CAPRI_norm')
#myvars2 <- names(spread.covariates.df2.CAPRI_norm) %in% c('region', "ind",'landShare','borderDummy','carbonPrice')
#spread.covariates.df2.CAPRI_norm <- spread.covariates.df2.CAPRI_norm[!myvars2]
#spread.covariates.df2.CAPRI_norm <- spread.covariates.df2.CAPRI_norm[rowSums(is.na(spread.covariates.df2.CAPRI_norm)) != ncol(spread.covariates.df2.CAPRI_norm), ]
#spread.covariates.df2.CAPRI_norm
#spread.covariates.df2.CAPRI_norm <- spread.covariates.df2.CAPRI_norm[complete.cases(spread.covariates.df2.CAPRI_norm), ] 

# summary statistics for  simulation experiments
#sum.spread.covariates.df2.CAPRI_norm <-  summary(spread.covariates.df2.CAPRI_norm)

#write.table(sum.spread.covariates.df2.CAPRI_norm, file = "summary.csv", sep = ",", col.names = NA,
#            qmethod = "double")

# Gower distance https://search.r-project.org/CRAN/refmans/StatMatch/html/gower.dist.html
#gower.dist(data.x=spread.covariates.df2.BF, data.y=spread.covariates.df2.CAPRI_norm, rngs=NULL, KR.corr=TRUE, var.weights = NULL, robcb=NULL)

#xdistance(spread.covariates.df2.BF,spread.covariates.df2.CAPRI_norm, method = "euclidean")

## specify Mahalanobis distance. here the columns in the resulting matrix are for each recipent region (CCAPRI)
# M. distance is the normalized distnace beween two datapoints
bf_lan.df.var.BF <- bf_lan.df.var %>% drop_na()
bf_lan.df.var.BF <- subset(bf_lan.df.var.BF, select = -c(carbonPrice))

bf.df.var.CAPRI_norm <- bf.df.var[bf.df.var$source %in% c('CAPRI_norm'),]
bf.df.var.CAPRI_norm <- subset(bf.df.var.CAPRI_norm, select = -c(source,carbonPrice))
bf.df.var.CAPRI_norm <- bf.df.var.CAPRI_norm %>% drop_na()
md2a <- mahalanobis.dist(data.x=bf_lan.df.var.BF, data.y=bf.df.var.CAPRI_norm)
dim(md2a)
md2a
## specify an average of xx of the observations in BM
# Stack data - funkar inte!

#md2a$ID <- seq.int(nrow(md2a))
#stack.md2a <- stack(md2a, drop= TRUE,stringsAsFactors = TRUE)
#colnames(md2a) <- rep(paste( NUMBER, "col", sep =""), length.out=ncol(md2a))

# uConvert to array 
#se data as array/matrix,seem to be possible to stack with Melt function
melt.md2a <- melt(
  md2a,
  varnames = names(dimnames(data)),

  na.rm = FALSE,
  as.is = FALSE,
  value.name = "value"
)

# aggregate to for each variable 1 - the BF observations
agg.melt.md2a <- aggregate(melt.md2a[, 'value'], list(melt.md2a$Var1), mean)
agg.melt.md2a

#define quantiles of interest
q = c(.5, 1.0)

#calculate quantiles by grouping variable
quantileMean.melt.md2a <- melt.md2a %>%
  group_by(Var2) %>%
  summarize(quant50 = quantile(value, probs = q[1]), 
            quant100 = quantile(value, probs = q[2]))

summary(quantileMean.melt.md2a)
# vageuly, above 3 is bad. need some refernce on this

# summary Mahalobian distances 
sum.quantileMean.melt.md2a <- summary(quantileMean.melt.md2a)
sum.quantileMean.melt.md2a
quantileMean.melt.md2a$quant50

### Specify models
###########################################################################

#Specify some models
mod1.ave<-lm(landShare~1, data=bf_lan.df.var)
summary(mod1.ave)
mod.all<-lm(landShare~yield+agriDens+leyDens+leyElas+GHGintensity+feedstockCost+carbonPrice, data=bf_lan.df.var)
summary(mod.all)
mod.allInteractCP<-lm(landShare~ (yield+agriDens+leyDens+leyElas+GHGintensity+feedstockCost+carbonPrice)*carbonPrice, data=bf_lan.df.var)
summary(mod.allInteractCP)


## this thing chose model with high VIF
#from http://r-statistics.co/Model-Selection-in-R.html
lmMod <- lm(landShare ~ . , data = bf_lan.df.var)
selectedMod <- step(lmMod)
summary(selectedMod)
all_vifs <- car::vif(selectedMod)
print(all_vifs)

signif_all <- names(all_vifs)

# Remove vars with VIF> 4 and re-build model until none of VIFs don't exceed 4.
while(any(all_vifs > 4)){
  var_with_max_vif <- names(which(all_vifs == max(all_vifs)))  # get the var with max vif
  signif_all <- signif_all[!(signif_all) %in% var_with_max_vif]  # remove
  myForm <- as.formula(paste("landShare ~ ", paste (signif_all, collapse=" + "), sep=""))  # new formula
  selectedMod <- lm(myForm, data=bf_lan.df.var)  # re-build model with new formula
  all_vifs <- car::vif(selectedMod)
}
summary(selectedMod)
car::vif(selectedMod)


## Stepwise model selction interaction terms
#from https://jackauty.com/stepwise-multiple-regression/


#Check dependent variable, ideally it's normally distributed. Transform if need
hist(bf_lan.df.var$landShare, breaks=100)
hist(sqrt(bf_lan.df.var$landShare), breaks=100)

#Set up full model
full.model<-lm(landShare ~ yield*agriDens*leyDens*leyElas*GHGintensity*feedstockCost*carbonPrice, data = bf_lan.df.var)
summary(full.model)

full.model.C<-lm(landShare ~ carbonPrice*(yield+agriDens+leyDens+leyElas+GHGintensity+feedstockCost), data = bf_lan.df.var)
summary(full.model.C)

#Perform stepwise regression in both directions
step.model<-stepAIC(full.model, direction="both", trace = 1)

step.model.C<-stepAIC(full.model.C, direction="both", trace = 1)
summary(step.model)
summary(step.model.C)


#Make it so you can see four graphs in one plot area
par(mfrow=c(2,2)) 

#Plot your assumption graphs
plot(step.model.C)

#BoxCox transformation - can ,make better results
full.model.untransformed<-lm(landShare ~ yield*agriDens*leyDens*GHGintensity*feedstockCost*carbonPrice, data = bf_lan.df.var)
step.model.untransformed<-stepAIC(full.model, direction="both", trace = F)

boxcox<-boxcox(step.model.untransformed,lambda = seq(-5, 5, 1/1000),plotit = TRUE )
Selected.Power<-boxcox$x[boxcox$y==max(boxcox$y)]
Selected.Power

#make trnasformed varaible
bf_lan.df.var$landShare.boxcox.transformed<-bf_lan.df.var$landShare^Selected.Power
#Running again with BoxCox transformed variable
#Set up full model
full.model<-lm(landShare.boxcox.transformed ~ carbonPrice*(yield+agriDens+leyDens+GHGintensity+feedstockCost), data = bf.df.lan2)

#Perform stepwise regression in both directions
step.model<-stepAIC(full.model, direction="both", trace = F)

summary(step.model)


#Checking assumptions 
par(mfrow=c(2,2)) 
plot(step.model)

###########################################################################
### Make cross validation with model variables, sees which model fits best
###########################################################################
# http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/
# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(landShare ~ carbonPrice*(yield+agriDens+leyDens+leyElas+GHGintensity+feedstockCost), data = bf_lan.df.var,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 5:13),
                    trControl = train.control
)
step.model$results
summary(step.model$finalModel)
# show coeffcinets for best models
coef(step.model$finalModel, 7)
coef(step.model$finalModel, 8)
coef(step.model$finalModel, 9)

summary(step.model)

####################
##fractional logit regression
####################

#https://m-clark.github.io/posts/2019-08-20-fractional-regression/
# additive
model_glm = glm(
  landShare ~ yield+agriDens+leyDens+leyElas+GHGintensity+feedstockCost+carbonPrice, 
  data = bf_lan.df.var,
  family = binomial
)
# should be ok it's not integer, as in the warning

summary(model_glm)
# Make robust stdard errors
se_glm_robust = coeftest(model_glm, vcov = vcovHC(model_glm, type="HC"))
summary(se_glm_robust)
se_glm_robust


# with interaction term
model_glm_interact = glm(
  landShare ~ carbonPrice*(yield+agriDens+leyDens+leyElas+GHGintensity+feedstockCost+carbonPrice), 
  data = bf_lan.df.var,
  family = binomial
)
# should be ok it's not integer, as in the warning

summary(model_glm_interact)
# Make robust standard errors
se_glm_interact_robust = coeftest(model_glm_interact, vcov = vcovHC(model_glm_interact, type="HC"))
summary(se_glm_interact_robust)
se_glm_interact_robust


# Interpreting logit https://www.r-bloggers.com/2015/09/how-to-perform-a-logistic-regression-in-r/
anova(model_glm_interact, test="Chisq")
####################
#Make it so you can see four graphs in one plot area
par(mfrow=c(2,2)) 

#Plot your assumption graphs
plot(model_glm_interact)

###########################################################################
### Robustness check
###########################################################################
###########################################################################
### Make prediction on base data
# Fiting the linear model

# Predicts the future values
outcome <- 0
outcome$actual <- bf_lan.df.var$landShare
outcome <-as.data.frame(outcome)
outcome$predicted.lmC <- predict(mod.allInteractCP, newdata = bf_lan.df.var)
outcome
### RMSE
rmse(outcome$actual, outcome$predicted.lmC)


# By setting the parameter type='response', R will output probabilities in the form of P(y=1|X).
outcome$predicted.fracC <- predict(model_glm_interact, newdata = bf_lan.df.var, type= "response")
outcome
### RMSE
rmse(outcome$actual, outcome$predicted.fracC)


#### Leave one out cross validation
# https://www.statology.org/leave-one-out-cross-validation-in-r/
#specify the cross-validation method
ctrlLOOCV <- trainControl(method = "LOOCV")

#fit a regression model and use LOOCV to evaluate performance
full.model.C.LOOCV <- train(landShare ~ carbonPrice*(yield+agriDens+leyDens+leyElas+GHGintensity+feedstockCost), data = bf_lan.df.var, method = "lm", trControl = ctrlLOOCV)

#view summary of LOOCV               
print(C.LOOCV)

#fit a regression model and use LOOCV to evaluate performance - for fractional logit used here
model_glm_interact.LOOCV <- train(landShare ~ carbonPrice*(yield+agriDens+leyDens+leyElas+GHGintensity+feedstockCost+carbonPrice),method='glm', data = bf_lan.df.var, trControl = ctrlLOOCV)
model_glm.LOOCV <- train(landShare ~ (yield+agriDens+leyDens+leyElas+GHGintensity+feedstockCost),method='glm', family=binomial,data = bf_lan.df.var, trControl = ctrlLOOCV)
#view summary of LOOCV               
summary(model_glm_interact)
summary(model_glm_interact.LOOCV)

##################################################################
##heteroskedasticity 

#Execute Breusch-Pagan Test

bptest(full.model)

#trasnform variable 
#make trnasformed varaible
bf.df.lan2$landShare.sqr<-bf.df.lan2$landShare^2
#Set up full model
full.model.C.sqr<-lm(landShare.sqr ~ carbonPrice*(yield+agriDens+leyDens+GHGintensity+feedstockCost), data = bf.df.lan2)
bptest(full.model.C.sqr)
#make trnasformed varaible
bf.df.lan2$landShare.sqrt<-sqrt(bf.df.lan2$landShare)
#Set up full model
full.model.C.sqrt<-lm(landShare.sqrt ~ carbonPrice*(yield+agriDens+leyDens+GHGintensity+feedstockCost), data = bf.df.lan2)
bptest(full.model.C.sqrt)

#make trnasformed varaible
bf.df.lan2$landShare.log<-log(bf.df.lan2$landShare)
#Set up full model
full.model.C.log<-lm(landShare.log ~ carbonPrice*(yield+agriDens+leyDens+GHGintensity+feedstockCost), data = bf.df.lan2)
bptest(full.model.C.log)

BCdist <- BoxCoxTrans(bf.df.lan2$landShare)
bf.df.lan2.BCdist <- cbind(bf.df.lan2, landShare.BCdist = predict(BCdist, bf.df.lan2$landShare))
bf.df.lan2.BCdist$landShare.BCdist
full.model.C.BCdist<-lm(landShare.BCdist ~ carbonPrice*(yield+agriDens+leyDens+GHGintensity+feedstockCost), data = bf.df.lan2.BCdist)
bptest(full.model.C.BCdist)


plot(full.model.C.BCdist)
###########################################################################
### Validity on aggregation
###########################################################################


#### Cross validation
# R program to implement https://www.geeksforgeeks.org/cross-validation-in-r-programming/
# K-fold cross-validation

# setting seed to generate a
# reproducible random sampling
set.seed(125)

# defining training control
# as cross-validation and
# value of K equal to 10
ctrlKfold <- trainControl(method = "cv",
                              number = 8)

# training the model by assigning sales column
# as target variable and rest other column
# as independent variable
full.model.C.Kfold <- train(landShare ~ carbonPrice*(yield+agriDens+leyDens+leyElas+GHGintensity+feedstockCost), data = bf_lan.df.var,
               method = "lm",
               trControl = ctrlKfold)
model_glm.Kfold <- train(landShare ~ (yield+agriDens+leyDens+leyElas+GHGintensity+feedstockCost), data = bf_lan.df.var,
                            method = "glm",
                            trControl = ctrlKfold)

# printing model performance metrics
# along with other details
print(full.model.C.Kfold)
print(model_glm.Kfold)

##############################################################
## split data in two set - one to train and one to test

# We have a subset of north and one of south

summary(bf_lan.df.var.south)
summary(bf_lan.df.var.north)



# Build the model for south 
model.south <- lm(landShare ~ carbonPrice*(yield+agriDens+leyDens+leyElas+GHGintensity+feedstockCost), data = bf_lan.df.var.south
                  )

# Make predictions and compute the R2, RMSE and MAE
predictions <- model.south %>% predict(bf_lan.df.var.north)
data.frame( R2 = R2(predictions, bf_lan.df.var.north$landShare),
            RMSE = RMSE(predictions, bf_lan.df.var.north$landShare),
            MAE = MAE(predictions, bf_lan.df.var.north$landShare))
predictions
bf_lan.df.var.north$landShare
bf_lan.df.var.north

# Build the model . now try with north
model.north <- lm(landShare ~ carbonPrice*(yield+agriDens+leyDens+leyElas+GHGintensity+feedstockCost), data = bf_lan.df.var.north
)
# Make predictions and compute the R2, RMSE and MAE
predictions2 <- model.north %>% predict(bf_lan.df.var.south)
tests.lm <-data.frame( R2 = R2(predictions2, bf_lan.df.var.south$landShare),
            RMSE = RMSE(predictions2, bf_lan.df.var.south$landShare),
            MAE = MAE(predictions2, bf_lan.df.var.south$landShare))
tests.lm
                      predictions2
                      bf_lan.df.var.south$landShare
                      bf_lan.df.var.south
# prediction error rate
  RMSE(predictions2, bf_lan.df.var.south$landShare)/mean(bf_lan.df.var.south$landShare)
  
## Now with glm
######
model_glm.south = glm(
    landShare ~ yield+agriDens+leyDens+leyElas+GHGintensity+feedstockCost+carbonPrice, 
    data = bf_lan.df.var.south,
    family = binomial
  )
# Make predictions and compute the R2, RMSE and MAE
predictions.glm <- model_glm.south %>% predict(bf_lan.df.var.north, type= "response")
tests.glm <-data.frame( R2 = R2(predictions.glm, bf_lan.df.var.north$landShare),
            RMSE = RMSE(predictions.glm, bf_lan.df.var.north$landShare),
            MAE = MAE(predictions.glm, bf_lan.df.var.north$landShare))
tests.glm
predictions.glm


###########################################################################
### Validty on a regional level
###########################################################################

###########################################################################
### Export results
###########################################################################
glm.sum<-summary(model_glm_interact)
glm.df<-as.data.frame(model_glm_interact)
write.csv(glm.sum,  "glm_res.csv")
