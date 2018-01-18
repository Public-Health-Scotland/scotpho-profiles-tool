#Code to create ScotPHO's Shiny profile platform
#This script includes the data manipulation necessary to produce data in the way
#the Shiny app needs.

#TODO:
#see server syntax

############################.
##Packages ----
############################.
library(readr)
library(dplyr) 
library(scales)
library(haven) #for SPPS file reading

###############################################.
## Lookups ---- 
###############################################.
# Lookup with geographical information.
geo_lookup <-  read_spss('/conf/phip/Projects/Profiles/Locality profiles/Lookup files/code_dictionary.sav') %>% 
  mutate_all(factor) # converting variables into factors

#Indicator information lookup table 
ind_lookup<- read_csv("./data/indicator_lookup.csv") %>% 
  setNames(tolower(names(.))) %>% #variables to lower case
  select(c(ind_id, indicator, interpret)) %>% #at the moment don't need most things
  mutate_all(factor) # converting variables into factors

###############################################.
## New locality data ----
###############################################.   
#Data that Vicky created with new macros.
under75_chd<- read_csv("./data/under75_CHDdeaths_dz11_locality_OPTdata.csv")
hosp_chd<- read_csv("./data/CHD_hosp_dz11_locality_OPTdata.csv")
deaths_1544<- read_csv("./data/deaths_15to44_dz11_locality_OPTdata.csv")
deaths_all<- read_csv("./data/deaths_allages_dz11_locality_OPTdata.csv")

#Joining together
optdata <- rbind(under75_chd, hosp_chd, deaths_1544, deaths_all) %>% 
  setNames(tolower(names(.))) %>% #variables to lower case
  select(-c(uni_id)) %>% #taking out variables
  #Creating geography type variable
  mutate(areatype = ifelse(substr(code, 1, 3) == "S00", "Scotland", 
                           ifelse(substr(code, 1, 3) == "S08", "Health board", 
                                  ifelse(substr(code, 1, 3) == "S12", "Council area", 
                                         ifelse(substr(code, 1, 3) == "S99", "Partnership", 
                                                ifelse(substr(code, 1, 3) == "S98", "Locality",
                                                       ifelse(substr(code, 1, 3) == "S02", "Intermediate zone", "Error"))))))) %>% 
  mutate_if(is.character, factor) %>% #converting characters into factors
  mutate(ind_id = as.factor(ind_id)) %>% 
  rename(measure = rate)

#Merging with indicator and geography information
optdata <- merge(x=optdata, y=ind_lookup, by="ind_id", all.x = TRUE) 
optdata <- merge(x=optdata, y=geo_lookup, by="code", all.x = TRUE) 


# Scaling measures (0 to 1) in groups by year, area type and indicator. 
#Does not work well for Scotland totals. TRUE?
optdata <- optdata %>% group_by(ind_id, year, areatype) %>% 
  mutate(measure_sc = ifelse(interpret=="H", as.vector(rescale(measure, to=c(1,0))), 
                             as.vector(rescale(measure, to=c(0,1))))) 

# Not sure if it will be needed.
# *to avoid repetition of names between HB and CA.
# if areaname= "Highland" and areatype="Local Authority" areaname="Highland - LA".
# if areaname= "Fife" and areatype="Local Authority" areaname="Fife - LA".
# if areaname= "Clackmannanshire" and areatype="Local Authority" areaname="Clackmannanshire - LA".

#Creating variables for topic/profile filters. 
#This probably should be added to the indicator lookup.
optdata <- optdata %>% 
  mutate(topic1 = "All") %>%
  mutate(topic2 = ifelse(ind_id %in% c("20105", "20303"), "CHD", "Deaths")) %>% 
  droplevels() #to get rid of factor levels not present in data set.

saveRDS(optdata, "./data/optdata_test.rds")

###############################################.
## Old locality data ----
###############################################.   

optdata_old<- read_csv("./data/locality_OPTdata.csv") %>% 
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
optdata_old <- readRDS("./data/optdata_test.rds")

##END
