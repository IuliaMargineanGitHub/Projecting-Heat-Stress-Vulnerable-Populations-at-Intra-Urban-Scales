##
## PROJECTIONS SSP2
##


setwd("<PATH/TO/WD>")
rm(list = ls())

##
## PACKAGES --------------------------------------------------------------------
##


library(BMS)
library(tidyverse)
library(ggplot2)
library(ggpubr)


# import data for BMA input

isced02 <-read.csv("isced02.csv", sep=";", header=TRUE) 
isced3 <-read.csv("isced3.csv", sep=";", header=TRUE) 
isced68 <-read.csv("isced68.csv", sep=";", header=TRUE) 



##
## MODEL CONSTRAINS ------------------------------------------------------------ 
##

bma_isced02=bms(isced02, burn=100, iter=50000, g="BRIC", mprior="uniform", mcmc="bd", nmodel=100)
bma_isced3=bms(isced3, burn=100, iter=50000, g="BRIC", mprior="uniform", mcmc="bd", nmodel=100)
bma_isced68=bms(isced68, burn=1000, iter=50000, g="BRIC", mprior="uniform", mcmc="bd", nmodel=100)


## Run image() by edu category
image(bma_isced02)
image(bma_isced3)
image(bma_isced68)


# defining model constrains: select the models for predictions
# the models included in each age-sex group are listed in table 4.a. in Supplementary material 1
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
## PROJECTIONS 2020 ------------------------------------------------------------
##


## import baseline data

baseline2012 <- read.csv("baseline2012.csv", sep=";", header=TRUE) 

baseline2020 <- read.csv("baseline2020.csv", sep=";", header=TRUE) 

predict_isced02_2020<- as.data.frame(predict(bma_isced02, newdata = baseline2012)) 
predict_isced3_2020<- as.data.frame(predict(bma_isced3, newdata = baseline2012)) 
predict_isced68_2020<- as.data.frame(predict(bma_isced68, newdata = baseline2012)) 

data_2020 <- cbind(predict_isced02_2020, predict_isced3_2020, predict_isced68_2020) 
write.csv(data_2020,"<PATH/TO/WD>/data_2020.csv", row.names = FALSE)

# rename columns in csv and reimport
data_2020 <-read.csv("data_2020.csv", sep=";", header=TRUE) 

# replace negative numbers with 0.0000000001
data_2020 <- data_2020 %>% 
  mutate_at(vars(isced02_2020, isced3_2020, isced68_2020), ~ifelse(. < 0, 0.0000000001, .))
num_changed_2020 <- sum(data_2020==0.0000000001)

# save the final data
write.csv(data_2020,"<PATH/TO/WD>/data_2020.csv", row.names = FALSE)


##
## Validation 2020 DATA --------------------------------------------------------
##


# make 2020.csv containing raw and projected 2020 shares for isced02, isced3 and isced68
# import data 
csv <- read.table("2020.csv", sep=";", header=TRUE)


# Basic scatter plot - run this for all education groups
ggplot(csv, aes(x=sh_65.84_isced68_2020, y=isced68_2020)) + 
  geom_point(size=5, shape=1, color="darkgrey") +
  geom_smooth(method=lm,  linetype="solid", color="darkblue", se=FALSE) +
  labs(title="isced68 2020", x="raw data", y = "projected") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 22)) +
  stat_regline_equation(aes(label = ..rr.label..))



##
## PROJECTIONS 2030 ------------------------------------------------------------
##


predict_isced02_2030<- as.data.frame(predict(bma_isced02, newdata = baseline2020, topmodels = models_isced02)) 
predict_isced3_2030<- as.data.frame(predict(bma_isced3, newdata = baseline2020, topmodels = models_isced3)) 
predict_isced68_2030<- as.data.frame(predict(bma_isced68, newdata = baseline2020, topmodels = models_isced68)) 

data_2030 <- cbind(predict_isced02_2030, predict_isced3_2030, predict_isced68_2030) 
write.csv(data_2030,"<PATH/TO/WD>/data_2030.csv", row.names = FALSE)

data_2030 <-read.csv("data_2030.csv", sep=";", header=TRUE) 
data_2030 <- data_2030 %>% 
  mutate_at(vars(isced02_2030, isced3_2030, isced68_2030), ~ifelse(. < 0, 0.0000000001, .))

num_changed_2030 <- sum(data_2030==0.0000000001)

write.csv(data_2030,"<PATH/TO/WD>/data_2030.csv", row.names = FALSE)


##
## PROJECTIONS 2040 ------------------------------------------------------------
##

baseline2030 <- read.csv("baseline2030.csv", sep=";", header=TRUE) 


predict_isced02_2040<- as.data.frame(predict(bma_isced02, newdata = baseline2030, topmodels = models_isced02)) 
predict_isced3_2040<- as.data.frame(predict(bma_isced3, newdata = baseline2030, topmodels = models_isced3)) 
predict_isced68_2040<- as.data.frame(predict(bma_isced68, newdata = baseline2030, topmodels = models_isced68)) 

data_2040 <- cbind(predict_isced02_2040, predict_isced3_2040, predict_isced68_2040) 
write.csv(data_2040,"<PATH/TO/WD>/data_2040.csv", row.names = FALSE)

data_2040 <-read.csv("data_2040.csv", sep=";", header=TRUE) 
data_2040 <- data_2040 %>% 
  mutate_at(vars(isced02_2040, isced3_2040, isced68_2040), ~ifelse(. < 0, 0.0000000001, .))

num_changed_2040 <- sum(data_2040==0.0000000001)

write.csv(data_2040,"<PATH/TO/WD>/data_2040.csv", row.names = FALSE)

##
## PROJECTIONS 2050 ------------------------------------------------------------
##

baseline2040 <- read.csv("baseline2040.csv", sep=";", header=TRUE) 


predict_isced02_2050<- as.data.frame(predict(bma_isced02, newdata = baseline2040, topmodels = models_isced02)) 
predict_isced3_2050<- as.data.frame(predict(bma_isced3, newdata = baseline2040, topmodels = models_isced3)) 
predict_isced68_2050<- as.data.frame(predict(bma_isced68, newdata = baseline2040, topmodels = models_isced68)) 

data_2050 <- cbind(predict_isced02_2050, predict_isced3_2050, predict_isced68_2050) 
write.csv(data_2050,"<PATH/TO/WD>/data_2050.csv", row.names = FALSE)

data_2050 <- read.csv("data_2050.csv", sep=";", header=TRUE)
data_2050 <- data_2050 %>% 
  mutate_at(vars(isced02_2050, isced3_2050, isced68_2050), ~ifelse(. < 0, 0.0000000001, .))

num_changed_2050 <- sum(data_2050==0.0000000001)
write.csv(data_2050,"<PATH/TO/WD>/data_2050.csv", row.names = FALSE)


