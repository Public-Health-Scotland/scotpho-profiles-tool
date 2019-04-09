#Code to create ScotPHO's Shiny profile platform
#This script includes the data manipulation necessary to produce data in the way
#the Shiny app needs.

############################.
##Filepaths ----
############################.
server_desktop <- "server"
if (server_desktop == "server") {
  lookups <- "/PHI_conf/ScotPHO/Profiles/Data/Lookups/"
  basefiles <- "/conf/phip/Projects/Profiles/Data/Scotland Localities/"
  shapefiles <- "/PHI_conf/ScotPHO/Profiles/Data/Shapefiles/"
  shiny_files <- "/PHI_conf/ScotPHO/Profiles/Data/Shiny Data/"
  
} else if (server_desktop == "desktop") {
  lookups <- "//stats/ScotPHO/Profiles/Data/Lookups/"
  basefiles <- "//stats/phip/Projects/Profiles/Data/Scotland Localities/"
  shapefiles <- "//stats/ScotPHO/Profiles/Data/Shapefiles/"
  shiny_files <- "//stats/ScotPHO/Profiles/Data/Shiny Data/"
  
}

############################.
##Packages ----
############################.
library(readr)
library(dplyr) 
library(scales)
library(haven) #for SPPS file reading
library(data.table) #new process
library (rgdal) #for reading shapefiles
library(rgeos) #for reducing size of shapefiles
library(rmapshaper) #for reducing size of shapefiles

###############################################.
## Lookups ---- 
###############################################.
# Lookup with all geography codes information.
geo_lookup <- readRDS(paste0(lookups, "Geography/codedictionary.rds")) %>% 
  mutate_all(factor) %>% # converting variables into factors
  #Creating geography type variable
  mutate(areatype = case_when(substr(code, 1, 3) == "S00" ~ "Scotland", 
                              substr(code, 1, 3) == "S08" ~ "Health board",
                              substr(code, 1, 3) == "S12" ~ "Council area",
                              substr(code, 1, 3) == "S11" ~ "Alcohol & drug partnership",
                              substr(code, 1, 3) == "S99" ~ "HSC locality",
                              substr(code, 1, 3) == "S37" ~ "HSC partnership",
                              substr(code, 1, 3) == "S02" ~ "Intermediate zone"),
         #TEMPORARY FIX. dealing with change in ca, hb and hscp codes
         code = recode(code, "S12000015"='S12000047', "S12000024"='S12000048', 
                       "S08000018"='S08000029', "S08000027"= 'S08000030', 
                       "S37000014"='S37000032', "S37000023"='S37000033'),
         #Changing ands for & to reduce issues with long labels and " - " for "-"
         areaname = gsub(" and ", " & ", areaname),
         areaname = gsub(" - ", "-", areaname))

#Bringing parent geography information
geo_parents <- readRDS(paste0(lookups, "Geography/IZtoPartnership_parent_lookup.rds")) %>% 
  #TEMPORARY FIX. dealing with change in ca, hb and hscp codes
  mutate(hscp_partnership = recode(hscp_partnership, "S37000014"='S37000032', 
                                   "S37000023"='S37000033'))

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

geo_partnership <- left_join(x=geo_partnership, y=geo_lookup, by=c("code"))
geo_partnership <- geo_partnership %>% select(-c(areatype)) %>% 
  rename(parent_code = code, parent_area = areaname)

#Merging together
geo_parents <- rbind(geo_par_iz, geo_par_loc)
geo_parents <- left_join(x=geo_parents, y=geo_partnership, by=c("parent_code")) 

geo_lookup <- left_join(x=geo_lookup, y=geo_parents, by="code", all.x = TRUE) 

##No IZ seem to be assigned to more than one partnership in this file.

###There are a number of IZ's with the same name, recoding.
geo_lookup <- geo_lookup %>% 
  mutate(areaname = case_when(
    code == "S02001938" ~ "Woodside-Glasgow City",
    code == "S02001938" ~ "Woodside-Glasgow City",
    code == "S02001267" ~ "Woodside-Abeerdeen City",
    code == "S02002233" ~ "Western Edge-Perth & Kinross",
    code == "S02001475" ~ "Western Edge-Dundee City",
    code == "S02001620" ~ "Tollcross-City of Edinburgh",
    code == "S02001911" ~ "Tollcross-Glasgow City",
    code == "S02001671" ~ "Muirhouse-City of Edinburgh",
    code == "S02002137" ~ "Muirhouse-North Lanarkshire",
    code == "S02002358" ~ "Law-South Lanarkshire",
    code == "S02001469" ~ "Law-Dundee City",
    code == "S02002490" ~ "Ladywell-West Lothian",
    code == "S02002156" ~ "Ladywell-North Lanarkshire",
    code == "S02001528" ~ "Hillhead-East Dunbartonshire",
    code == "S02001953" ~ "Hillhead-Glasgow City",
    code == "S02001249" ~ "City Centre West-Aberdeen City",
    code == "S02001933" ~ "City Centre West-Glasgow City",
    code == "S02001250" ~ "City Centre East-Aberdeen City",
    code == "S02001932" ~ "City Centre East-Glasgow City",
    code == "S02001448" ~ "City Centre-Dundee City",
    code == "S02002449" ~ "City Centre-Stirling",
    code == "S02001307" ~ "Blackburn-Aberdeenshire",
    code == "S02002496" ~ "Blackburn-West Lothian",
    code == "S02001534" ~ "IZ01-East Lothian",
    code == "S02002460" ~ "IZ01-West Dunbartonshire",
    code == "S02001535" ~ "IZ02-East Lothian",
    code == "S02002461" ~ "IZ02-West Dunbartonshire",
    code == "S02001536" ~ "IZ03-East Lothian",
    code == "S02002462" ~ "IZ03-West Dunbartonshire",
    code == "S02001537" ~ "IZ04-East Lothian",
    code == "S02002463" ~ "IZ04-West Dunbartonshire",
    code == "S02001538" ~ "IZ05-East Lothian",
    code == "S02002464" ~ "IZ05-West Dunbartonshire",
    code == "S02001539" ~ "IZ06-East Lothian",
    code == "S02002465" ~ "IZ06-West Dunbartonshire",
    code == "S02001540" ~ "IZ07-East Lothian",
    code == "S02002466" ~ "IZ07-West Dunbartonshire",
    code == "S02001541" ~ "IZ08-East Lothian",
    code == "S02002467" ~ "IZ08-West Dunbartonshire",
    code == "S02001542" ~ "IZ09-East Lothian",
    code == "S02002468" ~ "IZ09-West Dunbartonshire",
    code == "S02001543" ~ "IZ10-East Lothian",
    code == "S02002469" ~ "IZ10-West Dunbartonshire",
    code == "S02001544" ~ "IZ11-East Lothian",
    code == "S02002470" ~ "IZ11-West Dunbartonshire",
    code == "S02001545" ~ "IZ12-East Lothian",
    code == "S02002471" ~ "IZ12-West Dunbartonshire",
    code == "S02001546" ~ "IZ13-East Lothian",
    code == "S02002472" ~ "IZ13-West Dunbartonshire",
    code == "S02001547" ~ "IZ14-East Lothian",
    code == "S02002473" ~ "IZ14-West Dunbartonshire",
    code == "S02001548" ~ "IZ15-East Lothian",
    code == "S02002474" ~ "IZ15-West Dunbartonshire",
    code == "S02001549" ~ "IZ16-East Lothian",
    code == "S02002475" ~ "IZ16-West Dunbartonshire",
    code == "S02001550" ~ "IZ17-East Lothian",
    code == "S02002476" ~ "IZ17-West Dunbartonshire",
    code == "S02001551" ~ "IZ18-East Lothian",
    code == "S02002477" ~ "IZ18-West Dunbartonshire",
    TRUE  ~  paste(areaname))) #Last line for the rest of cases

geo_lookup <- geo_lookup %>% 
  #Creating variable that includeas area name and type for trend plotting
  mutate(areaname_full = paste(areaname, "-", areatype)) %>% 
  mutate_if(is.character, factor) %>% #transforming into factors
  #select(-c(parent_code)) %>% 
#Reducing length of the area type descriptor
  mutate(areaname_full = ifelse(areaname == "Scotland", "Scotland",
                                paste(areaname_full)),
         areaname_full = gsub("Health board", "HB", areaname_full), 
         areaname_full = gsub("Council area", "CA", areaname_full), 
         areaname_full =gsub("Alcohol & drug partnership", "ADP", areaname_full), 
         areaname_full =gsub("HSC partnership", "HSCP", areaname_full), 
         areaname_full =gsub("HSC locality", "HSCL", areaname_full), 
         areaname_full =gsub("Intermediate zone", "IZ", areaname_full))

saveRDS(geo_lookup, "data/geo_lookup.rds")
geo_lookup <- readRDS("data/geo_lookup.rds") 

######
#Indicator information lookup table 
#Many variables might not be needed
ind_lookup<- read_csv(paste0(lookups, "indicator_lookup.csv")) %>% 
  setNames(tolower(names(.))) %>% #variables to lower case
  select(c(ind_id, indicator, interpret, supression, supress_less_than, 
           type_id, type_definition, domain1, domain2, domain3, 
           profile_domain1, profile_domain2)) %>% 
  mutate_if(is.character, factor) # converting variables into factors

###############################################.
## Indicator data ----
###############################################.   
optdata <- read_csv(paste0(basefiles, "All Data for Shiny.csv"),
                    col_types = cols(NUMERATOR = col_number())) %>%
  setNames(tolower(names(.)))%>% #names to lower case
  rename(ind_id = indicator_id, code = geography_code) %>% 
  mutate_if(is.character,factor) #converting characters into factors

# These indicators are not in the old tool, so they are added now
part_measure <- read_csv(paste0(shiny_files, "ParticipationMeasure.csv")) %>%
  mutate(update_date = "01/11/2018") %>%
  rename(measure = rate) %>%
  mutate_if(is.character,factor) #converting characters into factors

sechand_smok <- read_csv(paste0(shiny_files, "SecondhandSmoke_shiny.csv")) %>%
  mutate(update_date = "19/12/2018") %>%
  rename(measure = rate) %>%
  mutate_if(is.character,factor) #converting characters into factors

child_lowinc <- read_csv(paste0(shiny_files, "ChildrenLowIncome_shiny.csv")) %>%
  mutate(update_date = "10/03/2019") %>%
  rename(measure = rate) %>%
  mutate_if(is.character,factor) #converting characters into factors

dental_p1 <- read_csv(paste0(shiny_files, "child_dental_p1_shiny.csv")) %>%
  mutate(update_date = "10/01/2019") %>%
  rename(measure = rate) %>%
  mutate_if(is.character,factor) #converting characters into factors

dental_p7 <- read_csv(paste0(shiny_files, "child_dental_p7_shiny.csv")) %>%
  mutate(update_date = "10/01/2019") %>%
  rename(measure = rate) %>%
  mutate_if(is.character,factor) #converting characters into factors

weight_p1 <- read_csv(paste0(shiny_files, "child_healthyweight_shiny.csv")) %>%
  mutate(update_date = "10/01/2019") %>%
  rename(measure = rate) %>%
  mutate_if(is.character,factor) #converting characters into factors

alcohol_stays_d11 <- read_csv(paste0(shiny_files,"alcohol_stays_dz11.csv")) %>%
  mutate(update_date = "08/02/2019") %>%
  rename(measure = rate) %>%
  mutate_if(is.character,factor) #converting characters into factors

alcohol_stays_adp <- read_csv(paste0(shiny_files,"alcohol_stays_ADP.csv")) %>%
  filter(substr(code,1,3)=="S11") %>% #selecting only adp level
  mutate(update_date = "08/02/2019", ind_id = 20203) %>%
  rename(measure = rate) %>%
  mutate_if(is.character,factor) #converting characters into factors

alcohol_stays <- rbind(alcohol_stays_d11, alcohol_stays_adp)

alc_deaths_adp <- read_csv(paste0(shiny_files,"alcohol_deaths_ADP_AL.csv")) %>%
  filter(substr(code,1,3) == "S11") %>% #other geographies already in main file
  mutate(ind_id = 20204) %>% # so it has the same ind number as the H&W one
  mutate(update_date = "22/09/2017") %>%
  rename(measure = rate) %>%
  mutate_if(is.character,factor) #converting characters into factors

optdata <- rbind(optdata, part_measure, sechand_smok, child_lowinc,
                 alcohol_stays, alc_deaths_adp, dental_p1, dental_p7, weight_p1) %>% 
#TEMPORARY FIX. dealing with change in ca, hb and hscp codes
  mutate(code = recode(code, "S12000015"='S12000047', "S12000024"='S12000048', 
                       "S08000018"='S08000029', "S08000027"= 'S08000030', 
                       "S37000014"='S37000032', "S37000023"='S37000033'))

#Dealing with lack of update_date for HSCP and HSC locality
update_table <- optdata %>% select(c(ind_id, update_date)) %>% distinct() %>% 
  subset(!is.na(update_date)) %>% 
  mutate(update_date = as.Date(update_date,"%m/%d/%Y")) %>% 
  group_by(ind_id) %>% top_n(1, update_date) %>% 
  ungroup() %>% unique()

optdata <- optdata %>% select(-update_date)

optdata <- left_join(x=optdata, y=update_table, by=c("ind_id"))

#Merging with indicator and geography information
optdata <- left_join(x=optdata, y=ind_lookup, by=c("ind_id"))
optdata <- left_join(x=optdata, y=geo_lookup, by=c("code")) 

#Apply supressions. 
# If indicator is presented as standardised rate and suppression required 
# then suppress numerator where count is less than specified value.
# standardised rates do not require suppression of rates or CI.
# If indicator is presented as crude rate or percentage and suppression required 
# then suppress numerator where count is less than specified value.
# crude rate and percentages DO require suppression of rates and CI as well as numerator.
optdata <- optdata %>% 
  mutate(numerator = case_when(#std rate case
    supression=="Y" & substr(type_id,1,2)=='sr' & numerator<supress_less_than ~ NA_real_,
    # crude rate and percentage cases
    supression =="Y" & (substr(type_id,1,2)=='cr' | (substr(type_id,1,1))=='%') & 
     numerator<supress_less_than ~ NA_real_, TRUE ~ numerator ), #if not keep numerator
    measure = case_when(# crude rate and percentage cases
      supression =="Y" & (substr(type_id,1,2)=='cr' | (substr(type_id,1,1))=='%') & 
        numerator<supress_less_than ~ NA_real_, TRUE ~ measure ),
    lowci =case_when(# crude rate and percentage cases
      supression =="Y" & (substr(type_id,1,2)=='cr' | (substr(type_id,1,1))=='%') & 
        numerator<supress_less_than ~ NA_real_, TRUE ~ lowci ),
    upci =case_when(# crude rate and percentage cases
      supression =="Y" & (substr(type_id,1,2)=='cr' | (substr(type_id,1,1))=='%') & 
        numerator<supress_less_than ~ NA_real_, TRUE ~ upci )
    )

# Scaling measures (0 to 1) in groups by year, area type and indicator. 
#Does not work well for Scotland totals. TRUE?
optdata <- optdata %>% group_by(ind_id, year, areatype) %>% 
  mutate(measure_sc = case_when(interpret=="H"~ as.vector(rescale(measure, to=c(1,0))), 
                                interpret=="L" ~ as.vector(rescale(measure, to=c(0,1))),
                                TRUE ~ 0))  %>% ungroup()

#Creating variables for topic/profile filters. 
#This probably should be added to the indicator lookup - most indicators assigned to death topic for now.
optdata <- optdata %>% 
  select(-c(supression, supress_less_than, type_id)) %>%  #taking out some variables
  #rounding variables
  mutate(numerator = round(numerator, 1), measure = round(measure, 1),
         lowci = round(lowci, 1), upci = round(upci, 1)) %>% 
  droplevels() %>%  #to get rid of factor levels not present in data set.
  #Making the numerator the measure for a few indicators, so it plots correctly
  mutate(measure = ifelse(indicator %in% c('Mid-year population estimate - all ages',
                                           'S2 pupils - SALSUS', 'S4 pupils - SALSUS',
                                           "Quit attempts"), numerator, measure),
         ind_id = as.factor(ind_id))

###TEMPORARY FIXES HOPEFULLY
#Dealing with lack of data for certain years and hb for Healthy weight at P1.
#Excluding those under 5 as most are lack of data and also avoids excess of variation from non-representative years
optdata <- optdata %>% 
  subset(!(ind_id == "21106" & (numerator<5 |
            (((code %in% c('S37000001', 'S37000002', "S12000033", "S12000034", 'S12000020', 
                         'S37000019', 'S08000020') | 
                parent_area %in% c("Aberdeen City", 'Aberdeenshire', 'Moray')) 
              & year <2009) |
            ((code %in% c('S12000035', 'S37000004', 'S12000017', 'S37000016', "S12000027", 'S37000026', 
                         'S12000040', 'S37000030', 'S08000022', 'S08000026')  |
                parent_area %in% c("Argyll & Bute", 'Shetland Islands', 'West Lothian', 'Highland'))
              & year %in% c('2007') ) |
            ((code %in% c('S12000039', 'S37000029', 'S12000011', 'S37000011', 'S12000046', 'S37000015') |
                parent_area %in% c("East Renfrewshire", 'Glasgow City', 'West Dunbartonshire')) 
          & year %in% c('2007', "2008", "2010")) |
            ((code %in% c('S12000045', 'S37000009') | parent_area %in% c("East Dunbartonshire")
              ) & year %in% c('2007', "2008", "2010", "2016")) |
            ((code %in% c('S12000018', 'S37000017') | parent_area %in% c("Inverclyde")
              ) & year %in% c('2007', "2008", "2009", "2010")) |
            ((code %in% c('S12000023', 'S37000022', 'S08000025') | 
                parent_area %in% c("Orkney Islands") ) & year %in% c('2007', "2008", "2009")))
          )#numerator plus code conditions
        ) #negation
      )#subset

saveRDS(optdata, "data/optdata.rds")
optdata <- readRDS("data/optdata.rds") 

###############################################.
## Profile lookup ----
###############################################.   
#Creating a file with a column for profile and another one for domain
profile_lookup <- data.frame(profile_domain = c(paste(unique(optdata$profile_domain1)),
                                                paste(unique(optdata$profile_domain2)))) %>% 
  mutate(profile = substr(profile_domain, 1, 3),
         domain = substr(profile_domain, 5, nchar(as.vector(profile_domain)))) %>% 
  select(-profile_domain)

saveRDS(profile_lookup, "data/profile_lookup.rds")
profile_lookup <- readRDS("data/profile_lookup.rds") 

###############################################.
## Technical document ----
###############################################.
#This syntax updates the Technical Document table based on an online Google Drive version of the table
#Run every time you want to refresh the data in the local copy to represent what's in the online copy
definition_table <-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTzrwAG7IFBjLvxuxUO0vJ7mn2AgilWVA1ZJQ9oVaLOSG4mgkquMKWga8MY5g2OFkFn-3awM_GYaHjL/pub?gid=94312583&single=true&output=csv") %>% 
  as.data.frame() %>% mutate(indicator_number = as.factor(indicator_number))

saveRDS(definition_table,"data/techdoc.rds") #for opt
techdoc <- readRDS("data/techdoc.rds") 
#backup copy in case issues with google drive
write_csv(definition_table,"/PHI_conf/ScotPHO/Profiles/Shiny Tool/techdoc_backup.csv") 

###############################################.
## Shapefiles ----
###############################################.   
#Reading file with council shapefiles
#making it small 29mb to 2.5. Sometimes it fails, due to lack of memory (use memory.limits and close things).
ca_bound_orig<-readOGR(shapefiles, "CA_2011_EoR_Scotland") %>% 
  rmapshaper::ms_simplify(keep=0.0025)

object.size(ca_bound_orig)

#Transforming coordinate system to the one leaflet needs
ca_bound_orig <- spTransform(ca_bound_orig,  CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))

#Saving the simplified shapefile to avoid the calculations.
writeOGR(ca_bound_orig, dsn=shapefiles, "CA_simpl", driver="ESRI Shapefile", overwrite_layer=TRUE)

#Saving as rds as it is much faster to read
ca_bound<-readOGR(shapefiles, "CA_simpl") %>% 
  setNames(tolower(names(.))) #variables to lower case
names(ca_bound@data)[names(ca_bound@data)=="gss_cod"] <- "code"
names(ca_bound@data)[names(ca_bound@data)=="name"] <- "area_name"

#TEMPORARY FIX. dealing with change in ca, hb and hscp codes
ca_bound$code <- recode(as.character(ca_bound$code), 
                          "S12000015"='S12000047', "S12000024"='S12000048')

saveRDS(ca_bound, "data/CA_boundary.rds")

##########################.
###Health board
#making it small 29mb to 2.5. Sometimes it fails, due to lack of memory (use memory.limits and close things).
hb_bound_orig<-readOGR(shapefiles,"SG_NHS_HealthBoards_2014") %>% 
  rmapshaper::ms_simplify(keep=0.0025)

object.size(hb_bound_orig)

#Transforming coordinate system to the one leaflet needs
hb_bound_orig <- spTransform(hb_bound_orig,  CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))

#Saving the simplified shapefile to avoid the calculations.
writeOGR(hb_bound_orig, dsn=shapefiles, "HB_simpl", driver="ESRI Shapefile", overwrite_layer=TRUE)

#Saving as rds as it is much faster to read
hb_bound<-readOGR(shapefiles,"HB_simpl") %>% 
  setNames(tolower(names(.))) #variables to lower case
names(hb_bound@data)[names(hb_bound@data)=="hbcode"] <- "code"
names(hb_bound@data)[names(hb_bound@data)=="hbname"] <- "area_name"

#TEMPORARY FIX. dealing with change in ca, hb and hscp codes
hb_bound$code <- recode(as.character(hb_bound$code), 
                          "S08000018"='S08000029', "S08000027"= 'S08000030')

saveRDS(hb_bound, "data/HB_boundary.rds")

##########################.
###HSC Partnership
#making it small 29mb to 2.5. Sometimes it fails, due to lack of memory (use memory.limits and close things).
hscp_bound_orig <- readOGR(shapefiles,"SG_NHS_IntegrationAuthority_2018") %>% 
  rmapshaper::ms_simplify(keep=0.0025) %>% 
  setNames(tolower(names(.))) #variables to lower case

object.size(hscp_bound_orig)

#Substituing codes to old ones. New ones still not in use.
hscp_bound_orig@data$hiacode <- as.factor(case_when(
  hscp_bound_orig@data$hiacode == "S37000032"~ "S37000014", 
  hscp_bound_orig@data$hiacode == "S37000033" ~ "S37000023",
  TRUE ~ paste0(hscp_bound_orig@data$hiacode)))

#Changing the projection to WSG84, the ones leaflet needs.
proj4string(hscp_bound_orig) #Checking projection
hscp_bound_orig <- spTransform(hscp_bound_orig, CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))

#Saving the simplified shapefile.
writeOGR(hscp_bound_orig, dsn=shapefiles, "HSCP_simpl", 
         driver="ESRI Shapefile", overwrite_layer=TRUE, verbose=TRUE,
         morphToESRI=TRUE)

hscp_bound <- readOGR(shapefiles,"HSCP_simpl")
names(hscp_bound@data)[names(hscp_bound@data)=="hiacode"] <- "code"
names(hscp_bound@data)[names(hscp_bound@data)=="hianame"] <- "area_name"

#TEMPORARY FIX. dealing with change in ca, hb and hscp codes
hscp_bound$code <- recode(as.character(hscp_bound$code), 
                          "S37000014"='S37000032', "S37000023"='S37000033')

saveRDS(hscp_bound, "data/HSCP_boundary.rds")
hscp_bound <- readRDS("data/HSCP_boundary.rds")

##########################.
###Intermediate zone
# It comes from Improvement Service
iz_bound_orig <- readRDS(paste0(shapefiles, "IZshapes.rds")) %>% #IZ 
  setNames(tolower(names(.))) #variables to lower case
names(iz_bound_orig@data)[names(iz_bound_orig@data)=="interzone"] <- "code"
names(iz_bound_orig@data)[names(iz_bound_orig@data)=="name"] <- "area_name"
iz_bound_orig$council <- gsub(" and ", " & ", iz_bound_orig$council)
iz_bound_orig$council <- gsub("Edinburgh", "City of Edinburgh", iz_bound_orig$council)
iz_bound_orig$council <- gsub("Eilean Siar", "Na h-Eileanan Siar", iz_bound_orig$council)

saveRDS(iz_bound_orig, "data/IZ_boundary.rds")
iz_bound <- readRDS("data/IZ_boundary.rds")

###############################################.
## New process data ----
###############################################.
#NOT READY
#Finds all the csvs in that folder reads them and combine them.
files <-  list.files(path = shiny_files, pattern = "*.csv", full.names = TRUE)
optdata <- as.data.frame(do.call(rbind, lapply(files, fread)))
# optdata2 <- as.data.frame(do.call(rbind, lapply(files, read_csv)))

# ldf <- lapply(files, read_csv)
# system.time(as.data.frame(do.call(rbind, lapply(files, fread))))
# system.time(as.data.frame(do.call(rbind, lapply(files, read_csv))))

optdata<- optdata %>%
  setNames(tolower(names(.)))%>% #names to lower case
  rename(ind_id = indicator_id, code = geography_code) %>% 
  mutate_if(is.character,factor) #converting characters into factors

#Merging with indicator and geography information
optdata <- left_join(x=optdata, y=ind_lookup, by="ind_id") 
optdata <- left_join(x=optdata, y=geo_lookup, by="code") 

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
  mutate(measure_sc = case_when(interpret=="H" ~ as.vector(rescale(measure, to=c(1,0))), 
                                interpret=="L", as.vector(rescale(measure, to=c(0,1))),
                                TRUE ~ 0))  %>%
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


##END
