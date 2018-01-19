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

#very fiddly way of identifying and fixing duplicate names
#First creating object with problematic names
conflictive_names <- data.frame(table(geo_lookup$areaname, geo_lookup$code))  %>% 
  subset(Freq != 0) %>% 
  count(Var1,Var1) %>% 
  subset(n >1) %>% 
  droplevels()

#Then merging with original geography lookup
geo_lookup <- merge(x=geo_lookup, y=conflictive_names, by.x="areaname", by.y="Var1", all.x = TRUE) 

#Need to fill the NA's so the recoding behaves correctly
geo_lookup[is.na(geo_lookup)] <- 0

# Now that they have been identified, we need to rename them
geo_lookup <- geo_lookup %>% 
  mutate(areaname = ifelse(substr(code, 1, 3) == "S12" & n>=2, 
                           paste(areaname, " - Council area"), 
                    ifelse(substr(code, 1, 3) == "S08" & n>=2, 
                                  paste(areaname, " - Health board"), 
                    ifelse(substr(code, 1, 3) == "S98" & n>=2, 
                                  paste(areaname, " - Partnership"),   
                    ifelse(substr(code, 1, 3) == "S99" & n>=2, 
                                  paste(areaname, " - Locality"),  
                    ifelse(substr(code, 1, 3) == "S02" & n>=2, 
                                  paste(areaname, " - Intermediate zone"),
                           paste(areaname)))))))

###There are a number of IZ's with the same name, recoding.
geo_lookup <- geo_lookup %>% 
  mutate(areaname = ifelse(code == "S02001938", "Woodside - Intermediate zone Glasgow City",
                    ifelse(code == "S02001267", "Woodside - Intermediate zone Abeerdeen City",
                    ifelse(code == "S02002233", "Western Edge - Intermediate zone Perth and Kinross",
                    ifelse(code == "S02001475", "Western Edge - Intermediate zone Dundee City",
                    ifelse(code == "S02001620", "Tollcross - Intermediate zone City of Edinburgh",
                    ifelse(code == "S02001911", "Tollcross - Intermediate zone Glasgow City",
                    ifelse(code == "S02001671", "Muirhouse - Intermediate zone City of Edinburgh",
                    ifelse(code == "S02002137", "Muirhouse - Intermediate zone North Lanarkshire",
                    ifelse(code == "S02002358", "Law - Intermediate zone South Lanarkshire",
                    ifelse(code == "S02001469", "Law - Intermediate zone Dundee City",
                    ifelse(code == "S02002490", "Ladywell - Intermediate zone West Lothian",
                    ifelse(code == "S02002156", "Ladywell - Intermediate zone North Lanarkshire",
                    ifelse(code == "S02001528", "Hillhead - Intermediate zone East Dunbartonshire",
                    ifelse(code == "S02001953", "Hillhead - Intermediate zone Glasgow City",
                    ifelse(code == "S02001249", "City Centre West - Intermediate zone Aberdeen City",
                    ifelse(code == "S02001933", "City Centre West - Intermediate zone Glasgow City",
                    ifelse(code == "S02001250", "City Centre East - Intermediate zone Aberdeen City",
                    ifelse(code == "S02001932", "City Centre East - Intermediate zone Glasgow City",
                    ifelse(code == "S02001448", "City Centre - Intermediate zone Dundee City",
                    ifelse(code == "S02002449", "City Centre - Intermediate zone Stirling",
                    ifelse(code == "S02001307", "Blackburn - Intermediate zone Aberdeenshire",
                    ifelse(code == "S02002496", "Blackburn - Intermediate zone West Lothian",
                    ifelse(code == "S02001534", "IZ01 - Intermediate zone East Lothian",
                    ifelse(code == "S02002460", "IZ01 - Intermediate zone West Dunbartonshire",
                    ifelse(code == "S02001535", "IZ02 - Intermediate zone East Lothian",
                    ifelse(code == "S02002461", "IZ02 - Intermediate zone West Dunbartonshire",
                    ifelse(code == "S02001536", "IZ03 - Intermediate zone East Lothian",
                    ifelse(code == "S02002462", "IZ03 - Intermediate zone West Dunbartonshire",
                    ifelse(code == "S02001537", "IZ04 - Intermediate zone East Lothian",
                    ifelse(code == "S02002463", "IZ04 - Intermediate zone West Dunbartonshire",
                    ifelse(code == "S02001538", "IZ05 - Intermediate zone East Lothian",
                    ifelse(code == "S02002464", "IZ05 - Intermediate zone West Dunbartonshire",
                    ifelse(code == "S02001539", "IZ06 - Intermediate zone East Lothian",
                    ifelse(code == "S02002465", "IZ06 - Intermediate zone West Dunbartonshire",
                    ifelse(code == "S02001540", "IZ07 - Intermediate zone East Lothian",
                    ifelse(code == "S02002466", "IZ07 - Intermediate zone West Dunbartonshire",
                    ifelse(code == "S02001541", "IZ08 - Intermediate zone East Lothian",
                    ifelse(code == "S02002467", "IZ08 - Intermediate zone West Dunbartonshire",
                    ifelse(code == "S02001542", "IZ09 - Intermediate zone East Lothian",
                    ifelse(code == "S02002468", "IZ09 - Intermediate zone West Dunbartonshire",
                    ifelse(code == "S02001543", "IZ10 - Intermediate zone East Lothian",
                    ifelse(code == "S02002469", "IZ10 - Intermediate zone West Dunbartonshire",
                    ifelse(code == "S02001544", "IZ11 - Intermediate zone East Lothian",
                    ifelse(code == "S02002470", "IZ11 - Intermediate zone West Dunbartonshire",
                    ifelse(code == "S02001545", "IZ12 - Intermediate zone East Lothian",
                    ifelse(code == "S02002471", "IZ12 - Intermediate zone West Dunbartonshire",
                    ifelse(code == "S02001546", "IZ13 - Intermediate zone East Lothian",
                    ifelse(code == "S02002472", "IZ13 - Intermediate zone West Dunbartonshire",
                    ifelse(code == "S02001547", "IZ14 - Intermediate zone East Lothian",
                    ifelse(code == "S02002473", "IZ14 - Intermediate zone West Dunbartonshire",
                    ifelse(code == "S02001548", "IZ15 - Intermediate zone East Lothian",
                    ifelse(code == "S02002474", "IZ15 - Intermediate zone West Dunbartonshire",
                    ifelse(code == "S02001549", "IZ16 - Intermediate zone East Lothian",
                    ifelse(code == "S02002475", "IZ16 - Intermediate zone West Dunbartonshire",
                    ifelse(code == "S02001550", "IZ17 - Intermediate zone East Lothian",
                    ifelse(code == "S02002476", "IZ17 - Intermediate zone West Dunbartonshire",
                    ifelse(code == "S02001551", "IZ18 - Intermediate zone East Lothian",
                    ifelse(code == "S02002477", "IZ18 - Intermediate zone West Dunbartonshire",
                           paste(areaname) #no argument
                       )))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

######
#Indicator information lookup table 
ind_lookup<- read_csv("./data/indicator_lookup.csv") %>% 
  setNames(tolower(names(.))) %>% #variables to lower case
  select(c(ind_id, indicator, interpret, supression, supress_less_than, type_id, type_definition)) %>% #at the moment don't need most things
  mutate_if(is.character, factor) # converting variables into factors

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
  rename(measure = rate)

#Merging with indicator and geography information
optdata <- merge(x=optdata, y=ind_lookup, by="ind_id", all.x = TRUE) 
optdata <- merge(x=optdata, y=geo_lookup, by="code", all.x = TRUE) 

#Apply supressions. NEEDS TO CHECK THAT IT WORKS FINE ONCE WE HAVE A REAL CASE
# If indicator is presented as standardised rate and suppression required then suppress numerator where count is less than specified value.
# standardised rates do not require suppression of rates or CI.
optdata$numerator[supression=="Y" & substr(optdata$type_id,1,2)=='sr' 
                  & optdata$numerator<optdata$supress_less_than] <- NA
# If indicator is presented as crude rate or percentage and suppression required then suppress numerator where count is less than specified value.
# crude rate and percentages DO require suppression of rates and CI as well as numerator.
optdata$numerator[supression=="Y" & (substr(optdata$type_id,1,2)=='cr' | (substr(optdata$type_id,1,1))=='%') 
                   & optdata$numerator<optdata$supress_less_than] <- NA
optdata$measure[supression=="Y" & (substr(optdata$type_id,1,2)=='cr' | (substr(optdata$type_id,1,1))=='%') 
                  & optdata$numerator<optdata$supress_less_than] <- NA
optdata$lowci[supression=="Y" & (substr(optdata$type_id,1,2)=='cr' | (substr(optdata$type_id,1,1))=='%') 
                  & optdata$numerator<optdata$supress_less_than] <- NA
optdata$upci[supression=="Y" & (substr(optdata$type_id,1,2)=='cr' | (substr(optdata$type_id,1,1))=='%') 
                  & optdata$numerator<optdata$supress_less_than] <- NA


# Scaling measures (0 to 1) in groups by year, area type and indicator. 
#Does not work well for Scotland totals. TRUE?
optdata <- optdata %>% group_by(ind_id, year, areatype) %>% 
  mutate(measure_sc = ifelse(interpret=="H", as.vector(rescale(measure, to=c(1,0))), 
                             as.vector(rescale(measure, to=c(0,1))))) 


#Creating variables for topic/profile filters. 
#This probably should be added to the indicator lookup.
optdata <- optdata %>% 
  mutate(topic1 = "All") %>%
  mutate(topic2 = ifelse(ind_id %in% c("20105", "20303"), "CHD", "Deaths")) %>% 
  select(-c(supression, supress_less_than, type_id, n)) %>%  #taking out some variables
  #rounding variables
  mutate(numerator = round(numerator, 1), measure = round(measure, 1),
         lowci = round(lowci, 1), upci = round(upci, 1)) %>% 
  droplevels() #to get rid of factor levels not present in data set.

saveRDS(optdata, "./data/optdata_test.rds")
optdata <- readRDS("./data/optdata_test.rds") 

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
