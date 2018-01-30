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
#library(rmapshaper) #for reducing size of shapefiles -not needed for app,just for modifying shp

###############################################.
## Lookups ---- 
###############################################.
# Lookup with geographical information.
geo_lookup <-  read_spss('/conf/phip/Projects/Profiles/Locality profiles/Lookup files/code_dictionary.sav') %>% 
  mutate_all(factor) %>% # converting variables into factors
  #Creating geography type variable
  mutate(areatype = ifelse(substr(code, 1, 3) == "S00", "Scotland", 
                           ifelse(substr(code, 1, 3) == "S08", "Health board", 
                                  ifelse(substr(code, 1, 3) == "S12", "Council area", 
                                         ifelse(substr(code, 1, 3) == "S99", "HSC Locality", 
                                                ifelse(substr(code, 1, 3) == "S98", "HSC Partnership",
                                                       ifelse(substr(code, 1, 3) == "S02", "Intermediate zone", "Error"))))))) 

#Bringing parent geography information
geo_parents <- read_spss('/conf/phip/Projects/Profiles/Locality profiles/Lookup files/DataZone11_All_Geographies_Lookup.sav') %>% 
  setNames(tolower(names(.))) %>% #variables to lower case
  select(c(intzone2011, hscp_locality, partnership)) %>% 
  distinct() # eliminating duplicates

#Creating parent geography for IZ level.
geo_par_iz <- geo_parents %>% 
  select(c(intzone2011, partnership)) %>% 
  rename(code = intzone2011, parent_area = partnership) %>% 
  distinct() # eliminating duplicates

#Creating parent geography for locality level.
geo_par_loc <- geo_parents %>% 
  select(c(hscp_locality, partnership)) %>% 
  rename(code = hscp_locality, parent_area = partnership) %>% 
  distinct() # eliminating duplicates

#Merging together
geo_parents <- rbind(geo_par_iz, geo_par_loc)

geo_lookup <- merge(x=geo_lookup, y=geo_parents, by="code", all.x = TRUE) 
  
# An evil IZ “S02001984” is assigned to “S98004227” (Highland) and “S98004767” (Moray). 
#I am going to assign it to Highland for now, as most datazones fall into its partnership
#If parent geography for IZ's is changed to CA or HB, remember checking for similar issues.
geo_lookup <- geo_lookup %>% 
  filter(!(code == 'S02001984' & parent_area == 'S98004767'))

#Replacing parent_area NA for CA, HB, Scotland and partnership with area type,
# as parent_area is only going to be used for locality and IZ.
geo_lookup$parent_area[is.na(geo_lookup$parent_area)] <- as.character(geo_lookup$areatype[is.na(geo_lookup$parent_area)]) 

#This part is to avoid areas having the same names, not used at the moment
#very fiddly way of identifying and fixing duplicate names
#First creating object with problematic names
# conflictive_names <- data.frame(table(geo_lookup$areaname, geo_lookup$code))  %>% 
#   subset(Freq != 0) %>% 
#   count(Var1,Var1) %>% 
#   subset(n >1) %>% 
#   droplevels()

#Then merging with original geography lookup
# geo_lookup <- merge(x=geo_lookup, y=conflictive_names, by.x="areaname", by.y="Var1", all.x = TRUE) 
# 
# #Need to fill the NA's so the recoding behaves correctly
# geo_lookup[is.na(geo_lookup)] <- 0
# 
# # Now that they have been identified, we need to rename them
# geo_lookup <- geo_lookup %>% 
#   mutate(areaname = ifelse(substr(code, 1, 3) == "S12" & n>=2, 
#                            paste(areaname, " - Council area"), 
#                     ifelse(substr(code, 1, 3) == "S08" & n>=2, 
#                                   paste(areaname, " - Health board"), 
#                     ifelse(substr(code, 1, 3) == "S98" & n>=2, 
#                                   paste(areaname, " - Partnership"),   
#                     ifelse(substr(code, 1, 3) == "S99" & n>=2, 
#                                   paste(areaname, " - Locality"),  
#                     ifelse(substr(code, 1, 3) == "S02" & n>=2, 
#                                   paste(areaname, " - Intermediate zone"),
#                            paste(areaname)))))))

###There are a number of IZ's with the same name, recoding.
geo_lookup <- geo_lookup %>% 
  mutate(areaname = ifelse(code == "S02001938", "Woodside - Glasgow City",
                    ifelse(code == "S02001267", "Woodside - Abeerdeen City",
                    ifelse(code == "S02002233", "Western Edge - Perth and Kinross",
                    ifelse(code == "S02001475", "Western Edge - Dundee City",
                    ifelse(code == "S02001620", "Tollcross - City of Edinburgh",
                    ifelse(code == "S02001911", "Tollcross - Glasgow City",
                    ifelse(code == "S02001671", "Muirhouse - City of Edinburgh",
                    ifelse(code == "S02002137", "Muirhouse - North Lanarkshire",
                    ifelse(code == "S02002358", "Law - South Lanarkshire",
                    ifelse(code == "S02001469", "Law - Dundee City",
                    ifelse(code == "S02002490", "Ladywell - West Lothian",
                    ifelse(code == "S02002156", "Ladywell - North Lanarkshire",
                    ifelse(code == "S02001528", "Hillhead - East Dunbartonshire",
                    ifelse(code == "S02001953", "Hillhead - Glasgow City",
                    ifelse(code == "S02001249", "City Centre West - Aberdeen City",
                    ifelse(code == "S02001933", "City Centre West - Glasgow City",
                    ifelse(code == "S02001250", "City Centre East - Aberdeen City",
                    ifelse(code == "S02001932", "City Centre East - Glasgow City",
                    ifelse(code == "S02001448", "City Centre - Dundee City",
                    ifelse(code == "S02002449", "City Centre - Stirling",
                    ifelse(code == "S02001307", "Blackburn - Aberdeenshire",
                    ifelse(code == "S02002496", "Blackburn - West Lothian",
                    ifelse(code == "S02001534", "IZ01 - East Lothian",
                    ifelse(code == "S02002460", "IZ01 - West Dunbartonshire",
                    ifelse(code == "S02001535", "IZ02 - East Lothian",
                    ifelse(code == "S02002461", "IZ02 - West Dunbartonshire",
                    ifelse(code == "S02001536", "IZ03 - East Lothian",
                    ifelse(code == "S02002462", "IZ03 - West Dunbartonshire",
                    ifelse(code == "S02001537", "IZ04 - East Lothian",
                    ifelse(code == "S02002463", "IZ04 - West Dunbartonshire",
                    ifelse(code == "S02001538", "IZ05 - East Lothian",
                    ifelse(code == "S02002464", "IZ05 - West Dunbartonshire",
                    ifelse(code == "S02001539", "IZ06 - East Lothian",
                    ifelse(code == "S02002465", "IZ06 - West Dunbartonshire",
                    ifelse(code == "S02001540", "IZ07 - East Lothian",
                    ifelse(code == "S02002466", "IZ07 - West Dunbartonshire",
                    ifelse(code == "S02001541", "IZ08 - East Lothian",
                    ifelse(code == "S02002467", "IZ08 - West Dunbartonshire",
                    ifelse(code == "S02001542", "IZ09 - East Lothian",
                    ifelse(code == "S02002468", "IZ09 - West Dunbartonshire",
                    ifelse(code == "S02001543", "IZ10 - East Lothian",
                    ifelse(code == "S02002469", "IZ10 - West Dunbartonshire",
                    ifelse(code == "S02001544", "IZ11 - East Lothian",
                    ifelse(code == "S02002470", "IZ11 - West Dunbartonshire",
                    ifelse(code == "S02001545", "IZ12 - East Lothian",
                    ifelse(code == "S02002471", "IZ12 - West Dunbartonshire",
                    ifelse(code == "S02001546", "IZ13 - East Lothian",
                    ifelse(code == "S02002472", "IZ13 - West Dunbartonshire",
                    ifelse(code == "S02001547", "IZ14 - East Lothian",
                    ifelse(code == "S02002473", "IZ14 - West Dunbartonshire",
                    ifelse(code == "S02001548", "IZ15 - East Lothian",
                    ifelse(code == "S02002474", "IZ15 - West Dunbartonshire",
                    ifelse(code == "S02001549", "IZ16 - East Lothian",
                    ifelse(code == "S02002475", "IZ16 - West Dunbartonshire",
                    ifelse(code == "S02001550", "IZ17 - East Lothian",
                    ifelse(code == "S02002476", "IZ17 - West Dunbartonshire",
                    ifelse(code == "S02001551", "IZ18 - East Lothian",
                    ifelse(code == "S02002477", "IZ18 - West Dunbartonshire",
                           paste(areaname) #no argument
                       )))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

geo_lookup$parent_area <- as.factor(geo_lookup$parent_area )
geo_lookup$areatype <- as.factor(geo_lookup$areatype )

saveRDS(geo_lookup, "./data/geo_lookup.rds")
geo_lookup <- readRDS("./data/geo_lookup.rds") 


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
  mutate_if(is.character, factor) %>% #converting characters into factors
  rename(measure = rate)

#Merging with indicator and geography information
optdata <- merge(x=optdata, y=ind_lookup, by="ind_id", all.x = TRUE) 
optdata <- merge(x=optdata, y=geo_lookup, by="code", all.x = TRUE) 

#Apply supressions. NEEDS TO CHECK THAT IT WORKS FINE ONCE WE HAVE A REAL CASE
# If indicator is presented as standardised rate and suppression required then suppress numerator where count is less than specified value.
# standardised rates do not require suppression of rates or CI.
optdata$numerator[optdata$supression=="Y" & substr(optdata$type_id,1,2)=='sr' 
                  & optdata$numerator<optdata$supress_less_than] <- NA
# If indicator is presented as crude rate or percentage and suppression required then suppress numerator where count is less than specified value.
# crude rate and percentages DO require suppression of rates and CI as well as numerator.
optdata$numerator[optdata$supression=="Y" & (substr(optdata$type_id,1,2)=='cr' | (substr(optdata$type_id,1,1))=='%') 
                   & optdata$numerator<optdata$supress_less_than] <- NA
optdata$measure[optdata$supression=="Y" & (substr(optdata$type_id,1,2)=='cr' | (substr(optdata$type_id,1,1))=='%') 
                  & optdata$numerator<optdata$supress_less_than] <- NA
optdata$lowci[optdata$supression=="Y" & (substr(optdata$type_id,1,2)=='cr' | (substr(optdata$type_id,1,1))=='%') 
                  & optdata$numerator<optdata$supress_less_than] <- NA
optdata$upci[optdata$supression=="Y" & (substr(optdata$type_id,1,2)=='cr' | (substr(optdata$type_id,1,1))=='%') 
                  & optdata$numerator<optdata$supress_less_than] <- NA


# Scaling measures (0 to 1) in groups by year, area type and indicator. 
#Does not work well for Scotland totals. TRUE?
optdata <- optdata %>% group_by(ind_id, year, areatype) %>% 
  mutate(measure_sc = ifelse(interpret=="H", as.vector(rescale(measure, to=c(1,0))), 
                             as.vector(rescale(measure, to=c(0,1))))) 


#Creating variables for topic/profile filters. 
#This probably should be added to the indicator lookup.
optdata <- optdata %>% 
  mutate(topic1 = as.factor("All")) %>%
  mutate(topic2 = as.factor(ifelse(ind_id %in% c("20105", "20303"), "CHD", "Deaths"))) %>% 
  select(-c(supression, supress_less_than, type_id, areaname, parent_area, areatype)) %>%  #taking out some variables
  #rounding variables
  mutate(numerator = round(numerator, 1), measure = round(measure, 1),
         lowci = round(lowci, 1), upci = round(upci, 1)) %>% 
  droplevels() #to get rid of factor levels not present in data set.

optdata <- as.data.frame(optdata)
optdata$ind_id <- as.factor(optdata$ind_id )

#To be able to upload it with no confidentiality issues to the tool
optdata <- optdata %>% 
  subset(!(indicator == "Patients hospitalised with coronary heart disease" &
             year == 2015))

saveRDS(optdata, "./data/optdata_test.rds")
optdata <- readRDS("./data/optdata_test.rds") 

###############################################.
## Map shapefiles ----
###############################################.   
#Reading file with council shapefiles
#making it small 29mb to 2.5. Sometimes it fails, due to lack of memory (use memory.limits and close things).
CA_bound_orig<-readOGR("./shapefiles","CA_2011_EoR_Scotland") %>% 
  rmapshaper::ms_simplify(keep=0.0025)

object.size(CA_bound_orig)

#Transforming coordinate system to the one leaflet needs
CA_bound_orig <- spTransform(CA_bound_orig,  CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))

#Saving the simplified shapefile to avoid the calculations.
writeOGR(CA_bound_orig, dsn="./shapefiles", "CA_simpl", driver="ESRI Shapefile", overwrite_layer=TRUE)

##########################.
###Health board
#making it small 29mb to 2.5. Sometimes it fails, due to lack of memory (use memory.limits and close things).
HB_bound_orig<-readOGR("./shapefiles","SG_NHS_HealthBoards_2014") %>% 
  rmapshaper::ms_simplify(keep=0.0025)

object.size(HB_bound_orig)

#Transforming coordinate system to the one leaflet needs
HB_bound_orig <- spTransform(HB_bound_orig,  CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))

#Saving the simplified shapefile to avoid the calculations.
writeOGR(HB_bound_orig, dsn="./shapefiles", "HB_simpl", driver="ESRI Shapefile", overwrite_layer=TRUE)

##END
