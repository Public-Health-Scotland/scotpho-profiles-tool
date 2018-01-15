#Code to create ScotPHO's Shiny profile platform
#In this script include packages, datasets and anyting that will be used both by UI and server

#TODO:
#see server syntax

############################.
##Packages ----
############################.
library(readr)
library(dplyr) 

###############################################.
## Data ----
###############################################.   

optdata<- read_csv("./data/locality_OPTdata.csv") %>% 
  rename(measure = rate) %>% 
  mutate_at(.funs=funs(round(.,2)), .vars=vars(numerator, measure, lowci, upci)) %>% 
  setNames(tolower(names(.))) #variables to lower case
  
optdata$interpret <- ifelse(
  optdata$indicator %in% c('Healthy birth weight','Babies exclusively breastfed at 6-8 weeks'),
           'Highgood', 'Lowgood')

#Scaling measures (0 to 1) in groups by year, area type and indicator. 
#Does not work well for Scotland totals.
optdata <- optdata %>% group_by(indicator, year, areatype) %>% 
  mutate(measure_sc = ifelse(interpret=="Highgood", as.vector(rescale(measure, to=c(1,0))), 
                             as.vector(rescale(measure, to=c(0,1)))))

#Creating variables for topic/profile filters
optdata$topic1 <- "All"
optdata$topic2 <- ifelse(optdata$indicator %in% c("Child obesity in primary 1", "Healthy birth weight", 
                                                  "Babies exclusively breastfed at 6-8 weeks"), 
                         "Children", "Cancer")

saveRDS(optdata, "./data/optdata_test.rds")
optdata <- readRDS("./data/optdata_test.rds")

##END
