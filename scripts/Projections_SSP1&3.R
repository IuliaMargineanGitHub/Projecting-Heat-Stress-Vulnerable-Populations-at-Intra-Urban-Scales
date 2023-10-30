##
## PROJECTIONS SSP3
##


setwd("<PATH/TO/WD>")
rm(list = ls())


##
## PACKAGES --------------------------------------------------------------------
##


library(BMS)
library(tidyverse)



# import data for BMA input

isced02 <-read.csv("isced02.csv", sep=";", header=TRUE) 
isced3 <-read.csv("isced3.csv", sep=";", header=TRUE) 
isced68 <-read.csv("isced68.csv", sep=";", header=TRUE) 

##
## BMA SHORT RUN  for figuring out model constrains 
##

bma_isced02=bms(isced02, burn=100, iter=50000, g="BRIC", mprior="uniform", mcmc="bd", nmodel=100)
bma_isced3=bms(isced3, burn=100, iter=50000, g="BRIC", mprior="uniform", mcmc="bd", nmodel=100)
bma_isced68=bms(isced68, burn=1000, iter=50000, g="BRIC", mprior="uniform", mcmc="bd", nmodel=100)


## Run image() by edu category

image(bma_isced02)
image(bma_isced3)
image(bma_isced68)

# defining model constrains: select the models for predictions
# the models included in each age-sex group are listed in tables 4.a., 4.b. and 4.c. in Supplementary material 1

models_isced02 <- c()
models_isced3 <- c()
models_isced68 <- c()


##
## BMA -------------------------------------------------------------------------
##

bma_isced02=bms(isced02, burn=10000, iter=5000000, g="BRIC", mprior="uniform", mcmc="enumerate", nmodel=100)
bma_isced3=bms(isced3, burn=10000, iter=5000000, g="BRIC", mprior="uniform", mcmc="enumerate", nmodel=100)
bma_isced68=bms(isced68, burn=10000, iter=5000000, g="BRIC", mprior="uniform", mcmc="enumerate", nmodel=100)

##
## BASELINE DATA ---------------------------------------------------------------
## 

#Baseline 2012
baseline2012_02 <- read.csv("baseline2012_02.csv", sep=";", header=TRUE) 

baseline2012_3 <- read.csv("baseline2012_3.csv", sep=";", header=TRUE) 

baseline2012_68 <- read.csv("baseline2012_68.csv", sep=";", header=TRUE) 


# Baseline 2020
baseline2020_02 <- read.csv("baseline2020_02.csv", sep=";", header=TRUE) 

baseline2020_3 <- read.csv("baseline2020_3.csv", sep=";", header=TRUE) 

baseline2020_68 <- read.csv("baseline2020_68.csv", sep=";", header=TRUE) 


##
## PROJECTIONS 2020 ------------------------------------------------------------
##

# Predict change
predict_isced02_2020<- as.data.frame(predict(bma_isced02, newdata = baseline2012_02))
predict_isced3_2020<- as.data.frame(predict(bma_isced3, newdata = baseline2012_3))
predict_isced68_2020<- as.data.frame(predict(bma_isced68, newdata = baseline2012_68)) 

predict_change_2020 <- cbind(predict_isced02_2020, predict_isced3_2020, predict_isced68_2020)

# Final data 
isced02_2020 <-  isced02$sh_25.64_isced02_2012 + predict_isced02_2020$`predict(bma_isced02, newdata = baseline2012_02)`
isced3_2020 <-  isced3$sh_25.64_isced3_2012 + predict_isced3_2020$`predict(bma_isced3, newdata = baseline2012_3)`
isced68_2020 <-  isced68$sh_25.64_isced68_2012 + predict_isced68_2020$`predict(bma_isced68, newdata = baseline2012_68)`

data_2020 <- as.data.frame(cbind(isced02_2020, isced3_2020, isced68_2020))
data_2020 <- data_2020 %>% 
  mutate_at(vars(isced02_2020, isced3_2020, isced68_2020), ~ifelse(. < 0, 0.0000000001, .))

num_changed_2020 <- sum(data_2020==0.0000000001)
write.csv(data_2020,"<PATH/TO/WD>/data_2020.csv", row.names = FALSE)


##
## PROJECTIONS 2030 ------------------------------------------------------------
##

# Predict change
predict_isced02_2030<- as.data.frame(predict(bma_isced02, newdata = baseline2020_02, topmodels = models_isced02)) 
predict_isced3_2030<- as.data.frame(predict(bma_isced3, newdata = baseline2020_3, topmodels = models_isced3)) 
predict_isced68_2030<- as.data.frame(predict(bma_isced68, newdata = baseline2020_68, topmodels = models_isced68)) 

predict_change_2030 <- cbind(predict_isced02_2030, predict_isced3_2030, predict_isced68_2030)

# Final data 

data_2020 <-read.csv("data_2020.csv", sep=";", header=TRUE) 

isced02_2030 <-  data_2020$isced02_2020 + predict_isced02_2030$`predict(bma_isced02, newdata = baseline2020_02, topmodels = models_isced02)`
isced3_2030 <-  data_2020$isced3_2020 + predict_isced3_2030$`predict(bma_isced3, newdata = baseline2020_3, topmodels = models_isced3)`
isced68_2030 <-  data_2020$isced68_2020 + predict_isced68_2030$`predict(bma_isced68, newdata = baseline2020_68, topmodels = models_isced68)`

data_2030 <- as.data.frame(cbind(isced02_2030, isced3_2030, isced68_2030)) 
data_2030 <- data_2030 %>% 
  mutate_at(vars(isced02_2030, isced3_2030, isced68_2030), ~ifelse(. < 0, 0.0000000001, .))

num_changed_2030 <- sum(data_2030==0.0000000001)

write.csv(data_2030,"<PATH/TO/WD>/data_2030.csv", row.names = FALSE)


##
## PROJECTIONS 2040 ------------------------------------------------------------
##

baseline2030_02 <- read.csv("baseline2030_02.csv", sep=";", header=TRUE) 

baseline2030_3 <- read.csv("baseline2030_3.csv", sep=";", header=TRUE) 

baseline2030_68 <- read.csv("baseline2030_68.csv", sep=";", header=TRUE)


predict_isced02_2040<- as.data.frame(predict(bma_isced02, newdata = baseline2030_02, topmodels = models_isced02)) 
predict_isced3_2040<- as.data.frame(predict(bma_isced3, newdata = baseline2030_3, topmodels = models_isced3)) 
predict_isced68_2040<- as.data.frame(predict(bma_isced68, newdata = baseline2030_68, topmodels = models_isced68)) 

predict_change_2040 <- cbind(predict_isced02_2040, predict_isced3_2040, predict_isced68_2040) 

# Final data 

data_2030 <-read.csv("data_2030.csv", sep=";", header=TRUE) 

isced02_2040 <-  data_2030$isced02_2030 + predict_isced02_2040$`predict(bma_isced02, newdata = baseline2030_02, topmodels = models_isced02)`
isced3_2040 <-  data_2030$isced3_2030 + predict_isced3_2040$`predict(bma_isced3, newdata = baseline2030_3, topmodels = models_isced3)`
isced68_2040 <-  data_2030$isced68_2030 + predict_isced68_2040$`predict(bma_isced68, newdata = baseline2030_68, topmodels = models_isced68)`

data_2040 <- as.data.frame(cbind(isced02_2040, isced3_2040, isced68_2040)) 
data_2040 <- data_2040 %>% 
  mutate_at(vars(isced02_2040, isced3_2040, isced68_2040), ~ifelse(. < 0, 0.0000000001, .))

num_changed_2040 <- sum(data_2040==0.0000000001)


write.csv(data_2040,"<PATH/TO/WD>/data_2040.csv", row.names = FALSE)


##
## PROJECTIONS 2050 ------------------------------------------------------------
##

baseline2040_02 <- read.csv("baseline2040_02.csv", sep=";", header=TRUE) 

baseline2040_3 <- read.csv("baseline2040_3.csv", sep=";", header=TRUE) 

baseline2040_68 <- read.csv("baseline2040_68.csv", sep=";", header=TRUE) 
 

predict_isced02_2050<- as.data.frame(predict(bma_isced02, newdata = baseline2040_02, topmodels = models_isced02)) 
predict_isced3_2050<- as.data.frame(predict(bma_isced3, newdata = baseline2040_3, topmodels = models_isced3)) 
predict_isced68_2050<- as.data.frame(predict(bma_isced68, newdata = baseline2040_68, topmodels = models_isced68)) 

predict_change_2050 <- cbind(predict_isced02_2050, predict_isced3_2050, predict_isced68_2050) 

# Final data 

data_2040 <-read.csv("data_2040.csv", sep=";", header=TRUE)

isced02_2050 <-  data_2040$isced02_2040 + predict_isced02_2050$`predict(bma_isced02, newdata = baseline2040_02, topmodels = models_isced02)`
isced3_2050 <-  data_2040$isced3_2040 + predict_isced3_2050$`predict(bma_isced3, newdata = baseline2040_3, topmodels = models_isced3)`
isced68_2050 <-  data_2040$isced68_2040 + predict_isced68_2050$`predict(bma_isced68, newdata = baseline2040_68, topmodels = models_isced68)`

data_2050 <- as.data.frame(cbind(isced02_2050, isced3_2050, isced68_2050))
data_2050 <- data_2050 %>% 
  mutate_at(vars(isced02_2050, isced3_2050, isced68_2050), ~ifelse(. < 0, 0.0000000001, .))

num_changed_2050 <- sum(data_2050==0.0000000001)


write.csv(data_2050,"<PATH/TO/WD>/data_2050.csv", row.names = FALSE)
