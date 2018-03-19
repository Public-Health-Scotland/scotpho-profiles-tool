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
library(rmapshaper) #for reducing size of shapefiles
library (rgdal) #for reading shapefiles
###############################################.
## Lookups ---- 
###############################################.


# Lookup with all geography codes information.
geo_lookup<- read_csv("./data/Geo Data for Shiny.csv")%>%
  setNames(tolower(names(.))) %>% #variables to lower case
  rename(code = geography_code)%>%
  rename(areaname = geography_name)%>%
  mutate_all(factor) %>% # converting variables into factors
  #Creating geography type variable
  mutate(areatype = ifelse(substr(code, 1, 3) == "S00", "Scotland", 
                           ifelse(substr(code, 1, 3) == "S08", "Health board", 
                                  ifelse(substr(code, 1, 3) == "S12", "Council area", 
                                         ifelse(substr(code, 1, 3) == "S99", "HSC Locality", 
                                                ifelse(substr(code, 1, 3) == "S98", "HSC Partnership",
                                                       ifelse(substr(code, 1, 3) == "S02", "Intermediate zone", "Error"))))))) 


#Bringing parent geography information
geo_parents <- read_spss('./data/IZtoPartnership_parent_lookup.sav') %>% 
  setNames(tolower(names(.))) %>% #variables to lower case
  select(c(intzone2011, hscp_locality, hscp_partnership)) %>% 
  distinct() # eliminating duplicates

#Creating parent geography for IZ level.
geo_par_iz <- geo_parents %>% 
  select(c(intzone2011, hscp_partnership)) %>% 
  rename(code = intzone2011, parent_code = hscp_partnership) %>% 
  distinct() # eliminating duplicates

#Creating parent geography for locality level.
geo_par_loc <- geo_parents %>% 
  select(c(hscp_locality, hscp_partnership)) %>% 
  rename(code = hscp_locality, parent_code = hscp_partnership) %>% 
  distinct() # eliminating duplicates

#bringing area names for parent geographies
geo_partnership <- geo_parents %>% 
  select(c(hscp_partnership)) %>% 
  rename(code = hscp_partnership) %>% 
  distinct() # eliminating duplicates

geo_partnership <- merge(x=geo_partnership, y=geo_lookup, by="code", all.x = TRUE) 
geo_partnership <- geo_partnership %>% select(-c(areatype)) %>% 
  rename(parent_code = code, parent_area = areaname)

#Merging together
geo_parents <- rbind(geo_par_iz, geo_par_loc)
geo_parents <- merge(x=geo_parents, y=geo_partnership, by="parent_code", all.x = TRUE) 

geo_lookup <- merge(x=geo_lookup, y=geo_parents, by="code", all.x = TRUE) 

##No IZ seem to be assigned to more than one partnership in this file.
  
geo_lookup <- geo_lookup %>% 
  mutate(parent_area2 = geo_lookup$areaname[geo_lookup$parent_area == geo_lookup$code])
#Replacing parent_area NA for CA, HB, Scotland and partnership with area type,
# as parent_area is only going to be used for locality and IZ.
geo_lookup$parent_area[is.na(geo_lookup$parent_area)] <- as.character(geo_lookup$areatype[is.na(geo_lookup$parent_area)]) 

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
                           paste(areaname) #no argument
                    ))))))))))))))))))))))) %>% 
  mutate(areaname = ifelse(code == "S02001534", "IZ01 - East Lothian",
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
                    )))))))))))))))))))))))))))))))))))))

geo_lookup <- geo_lookup %>%  mutate_if(is.character, factor) %>% #transforming into factors
  select(-c(parent_code)) 

geo_lookup <- as.data.frame(geo_lookup)
saveRDS(geo_lookup, "./data/geo_lookup.rds")
geo_lookup <- readRDS("./data/geo_lookup.rds") 

######
#Indicator information lookup table 

ind_lookup<- read_csv("./data/indicator_lookup_modified.csv") %>% 
  setNames(tolower(names(.))) %>% #variables to lower case
  select(c(ind_id, indicator, interpret, supression, supress_less_than, 
           type_id, type_definition, domain1, domain2, domain3)) %>% #at the moment don't need most things
  mutate_if(is.character, factor) # converting variables into factors

###############################################.
## Locality data ----
###############################################.   
optdata <- read_csv("./data/All Data for Shiny.csv")

optdata<- optdata %>%
  setNames(tolower(names(.)))%>% #names to lower case
  rename(ind_id = indicator_id, code = geography_code) %>% 
  mutate_if(is.character,factor) #converting characters into factors

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
                             ifelse(interpret=="L", as.vector(rescale(measure, to=c(0,1))),
                                    0)))  %>%
  ungroup()


#Creating variables for topic/profile filters. 
#This probably should be added to the indicator lookup - most indicators assigned to death topic for now.
optdata <- optdata %>% 
  select(-c(supression, supress_less_than, type_id)) %>%  #taking out some variables
  #rounding variables
  mutate(numerator = round(numerator, 1), measure = round(measure, 1),
         lowci = round(lowci, 1), upci = round(upci, 1)) %>% 
  droplevels() #to get rid of factor levels not present in data set.

#Making the numerator the measure for pop all ages, so it plots correctly
optdata$measure <- ifelse(optdata$indicator == 'Mid-year population estimate - all ages',
                          optdata$numerator, optdata$measure)

optdata <- as.data.frame(optdata)
optdata$ind_id <- as.factor(optdata$ind_id )

saveRDS(optdata, "./data/optdata.rds")
optdata <- readRDS("./data/optdata.rds") 

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

#Saving as rds as it is much faster to read
CA_bound<-readOGR("./shapefiles","CA_simpl")
saveRDS(CA_bound, "./shapefiles/CA_boundary.rds")

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

#Saving as rds as it is much faster to read
HB_bound<-readOGR("./shapefiles","HB_simpl") 
saveRDS(HB_bound, "./shapefiles/HB_boundary.rds")

###############################################.
## Deprivation data ----
###############################################.  
deaths_all_depr <- read_csv("./data/deaths_allages_deprivation_OPTdata.csv")
deaths_all_depr_rii <- read_csv("./data/deaths_allages_deprivation_rii_OPTdata.csv") %>% 
  select(-ind_id)
deaths_all_depr_sii <- read_csv("./data/deaths_allages_deprivation_sii_OPTdata.csv")

deaths_all_depr_merged <- merge(deaths_all_depr, deaths_all_depr_rii, 
                                by = c("code", "year", "def_period", "trend_axis"),
                                all.x = TRUE)

deaths_all_depr_merged <- merge(deaths_all_depr_merged, deaths_all_depr_sii, 
                               by = c("code", "year", "def_period", "trend_axis"),
                               all.x = TRUE)


deaths_15to44_depr <- read_csv("./data/deaths_15to44_deprivation_OPTdata.csv")
deaths_15to44_depr_rii <- read_csv("./data/deaths_15to44_deprivation_rii_OPTdata.csv") %>% 
  select(-ind_id)
deaths_15to44_depr_sii <- read_csv("./data/deaths_15to44_deprivation_sii_OPTdata.csv")

deaths_15to44_depr_merged <- merge(deaths_15to44_depr, deaths_15to44_depr_rii, 
                                by = c("code", "year", "def_period", "trend_axis"),
                                all.x = TRUE)

deaths_15to44_depr_merged <- merge(deaths_15to44_depr_merged, deaths_15to44_depr_sii, 
                                by = c("code", "year", "def_period", "trend_axis"),
                                all.x = TRUE)

#It has the same ind_id as the the other indicator.
deaths_15to44_depr_merged$ind_id <- 10000

deprivation_data <- rbind(deaths_15to44_depr_merged, deaths_all_depr_merged)

deprivation_data <- deprivation_data %>% 
  mutate(quintile = ifelse(substr(code, 4, 4)=="0", "Total",
                           ifelse(substr(code, 4, 4)=="1", "1 - Most deprived",
                                  ifelse(substr(code, 4, 4)=="2", "2",
                                         ifelse(substr(code, 4, 4)=="3", "3",
                                                ifelse(substr(code, 4, 4)=="4", "4",
                                                       ifelse(substr(code, 4, 4)=="5", "5 - Least deprived", "Error"))))))) %>% 
  mutate(code = paste(substr(code, 1, 3), "0", substr(code, 5, 9), sep = ""))
table(deprivation_data$quintile)


#Merging with lookup, at the moment lookup does not have this.
#deprivation_data <- merge(x=deprivation_data, y=ind_lookup, by="ind_id", all.x = TRUE) 
#Patch in the meantime.
deprivation_data <- deprivation_data %>% 
  mutate(indicator = ifelse(ind_id == "10000", "All-cause mortality among the 15-44 year olds",
                            "Deaths all ages")) %>% 
  mutate(type_definition = "Age-sex standardized rate per 100,000") %>% 
  mutate(numerator = round(numerator, 1), rate = round(rate, 1),
         lowci = round(lowci, 1), upci = round(upci, 1), rii  = round(rii, 1), 
         slope_coef = round(slope_coef, 1), lowci_slope = round(lowci_slope, 1),
         upci_slope = round(upci_slope, 1), lowci_rii = round(lowci_rii, 1), upci_rii = round(upci_rii, 1))

saveRDS(deprivation_data, "./data/deprivation_OPT.rds")
deprivation_data <- readRDS("./data/deprivation_OPT.rds")




##END
