#Code to create ScotPHO's Shiny profile platform
#This script includes the data manipulation necessary to produce data in the way
#the Shiny app needs.

############################.
##Filepaths ----
############################.
if (sessionInfo()$platform == "x86_64-redhat-linux-gnu (64-bit)") {
  lookups <- "/PHI_conf/ScotPHO/Profiles/Data/Lookups/"
  basefiles <- "/conf/phip/Projects/Profiles/Data/Scotland Localities/"
  shapefiles <- "/PHI_conf/ScotPHO/Profiles/Data/Shapefiles/"
  shiny_files <- "/PHI_conf/ScotPHO/Profiles/Data/Shiny Data/"
  
} else  {
  lookups <- "//stats/ScotPHO/Profiles/Data/Lookups/"
  basefiles <- "//stats/phip/Projects/Profiles/Data/Scotland Localities/"
  shapefiles <- "//stats/ScotPHO/Profiles/Data/Shapefiles/"
  shiny_files <- "//stats/ScotPHO/Profiles/Data/Shiny Data/"
}

############################.
##Packages ----
############################.
library(dplyr) 
library(tidyr)
library(readr)
library(scales) #rescaling variables
library(haven) #for SPPS file reading
library(data.table) #reading data
library(zoo) # dealing with dates
library(lubridate) #for automated list of dates in welcome modal
library(janitor) #cleaning names
library (rgdal) #for reading shapefiles
library(rgeos) #for reducing size of shapefiles
library(rmapshaper) #for reducing size of shapefiles

###############################################.
## Technical document ----
###############################################.
#This code updates the Technical Document table based on an online Google Drive version of the table
#Run every time you want to refresh the data in the local copy to represent what's in the online copy
#This file is where indicator names and definitions are stored and are loaded into the shiny tool.
definition_table <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTzrwAG7IFBjLvxuxUO0vJ7mn2AgilWVA1ZJQ9oVaLOSG4mgkquMKWga8MY5g2OFkFn-3awM_GYaHjL/pub?gid=94312583&single=true&output=csv") %>%
  as.data.frame() %>% mutate(indicator_number = as.factor(indicator_number)) 

#automating dates
new_date <- fast_strptime(paste("01",definition_table$last_updated,sep="-"),"%d-%b-%Y")

definition_table <- definition_table %>% 
  mutate(days_since_update=day(as.period(new_date %--% today(), unit="days")))

saveRDS(definition_table,"data/techdoc.rds") #for opt
techdoc <- readRDS("data/techdoc.rds")
#backup copy in case issues with google drive
write_csv(definition_table,"/PHI_conf/ScotPHO/Profiles/Shiny Tool/techdoc_backup.csv")

###############################################.
## Lookups ---- 
###############################################.
# Lookup with all geography codes information. This file is created in the lookup repo
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

#Bringing parent geography information and formatting in one column with no duplicates
geo_parents <- readRDS(paste0(lookups, "Geography/IZtoPartnership_parent_lookup.rds")) %>% 
  #TEMPORARY FIX. dealing with change in ca, hb and hscp codes
  mutate(hscp_partnership = recode(hscp_partnership, "S37000014"='S37000032', 
                                   "S37000023"='S37000033')) %>% 
  gather(geotype, code, c(intzone2011, hscp_locality)) %>% distinct() %>% 
  select(-geotype) %>% rename(parent_code = hscp_partnership)

# Merging to geo_lookup to obtain parent area name
geo_parents <- left_join(x=geo_parents, y=geo_lookup, by=c("parent_code" = "code")) %>% 
  select(-c(areatype)) %>% rename(parent_area = areaname)

#Merging parents to geo_lookup
geo_lookup <- left_join(x=geo_lookup, y=geo_parents, by="code", all.x = TRUE) 

##No IZ seem to be assigned to more than one partnership in this file.

###There are a number of IZ's with the same name, recoding.
geo_lookup <- geo_lookup %>% 
  mutate(areaname = case_when(
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
    # THIS ONE IS A MISSPELLING - TEMPORARY UNTIL LOOKups fixed
    code == "S02002007" ~ "Lochalsh",
    TRUE  ~  paste(areaname))) #Last line for the rest of cases

geo_lookup <- geo_lookup %>% 
  #Creating variable that includeas area name and type for trend plotting
  mutate(areaname_full = paste(areaname, "-", areatype)) %>% 
  mutate_if(is.character, factor) %>% #transforming into factors
  select(-c(parent_code)) %>% 
#Reducing length of the area type descriptor
  mutate(areaname_full = ifelse(areaname == "Scotland", "Scotland",
                                paste(areaname_full)),
         areaname_full = gsub("Health board", "HB", areaname_full), 
         areaname_full = gsub("Council area", "CA", areaname_full), 
         areaname_full = gsub("Alcohol & drug partnership", "ADP", areaname_full), 
         areaname_full = gsub("HSC partnership", "HSCP", areaname_full), 
         areaname_full = gsub("HSC locality", "HSCL", areaname_full), 
         areaname_full = gsub("Intermediate zone", "IZ", areaname_full))

saveRDS(geo_lookup, "data/geo_lookup.rds")
geo_lookup <- readRDS("data/geo_lookup.rds") 

###############################################.
## Indicator lookup table 
ind_lookup<- read_csv(paste0(lookups, "indicator_lookup.csv")) %>% 
  setNames(tolower(names(.))) %>% #variables to lower case
  select(c(ind_id, indicator, interpret, supression, supress_less_than, 
           type_id, type_definition, profile_domain1, profile_domain2, 
           # This ones can be deleted once release 2.0 comes out
           domain1, domain2, domain3)) %>% 
  mutate_if(is.character, factor) %>%  # converting variables into factors
  mutate(ind_id = as.factor(ind_id))

###############################################.
## Indicator data ----
###############################################.   
# Brings data prepared by spss and then delete all the one already present in shiny files
# Eventually the spss data will be deleted.

#Start creating a backup of the old file 
optdata <- readRDS("data/optdata.rds")
saveRDS(optdata, "/PHI_conf/ScotPHO/Profiles/Data/shiny_tool_backup_data.rds")

#Finds all the csv files in the shiny folder
files <-  list.files(path = shiny_files, pattern = "*.csv", full.names = TRUE)
# To check dates of update of each file and who did it
View(file.info(files,  extra_cols = TRUE))

# reads the data and combines it, variables to lower case and variable with filename
optdata <- do.call(rbind, lapply(files, function(x){ 
  fread(x)[,file_name:= x] %>% clean_names() })) %>% 
  mutate(file_name = gsub("/PHI_conf/ScotPHO/Profiles/Data/Shiny Data//", "", file_name)) %>% 
  rename(measure = rate) %>% 
  mutate_at(c("numerator", "measure", "lowci", "upci"), as.numeric) %>% 
  mutate(ind_id = as.integer(ind_id))

# to check if there is more then one file for the same indicator. This should be empty
optdata %>% select(ind_id, file_name) %>% unique %>% group_by(ind_id) %>% 
  add_tally() %>% filter(n >1) %>% View()

# Bringing data created by SPSS code extracting from database
data_spss <- read_csv(paste0(basefiles, "All Data for Shiny.csv"),
                      col_types = cols(NUMERATOR = col_number())) %>%
  setNames(tolower(names(.)))%>% #names to lower case
  rename(ind_id = indicator_id, code = geography_code) %>% 
  select(-update_date) %>% 
  # excluding indicators already present in shiny folder data files
  filter(!(ind_id %in% unique(optdata$ind_id))) %>% droplevels() 

# Merging together spss and shiny data folder datasets
optdata <- bind_rows(optdata, data_spss) %>% 
  mutate_if(is.character, factor) %>%  #converting characters into factors
  #TEMPORARY FIX. dealing with change in ca, hb and hscp codes
  mutate(code = recode(code, "S12000015"='S12000047', "S12000024"='S12000048',
                       "S08000018"='S08000029', "S08000027"= 'S08000030',
                       "S37000014"='S37000032', "S37000023"='S37000033'))

# Adding update date for all indicators based on technical document
update_table <- techdoc %>% rename(ind_id = indicator_number, update_date = last_updated) %>% 
  select(ind_id, update_date) %>% 
  mutate(update_date = as.yearmon(update_date, "%b-%Y"),
         ind_id = as.integer(ind_id))

optdata <- left_join(x=optdata, y=update_table, by=c("ind_id"))

# Removing some indicators from IZ level to reduce risk of secondary disclosure.
# until we are sure that statistical disclosure signed off.
# deaths from suicide - 20403
# alcohol-related deaths - 20204
# psychiatric hospital adm - 20402
# cancer registrations - 20301
# Rx for drugs for depression - 20401
# teenage pregnancies - 21001
# mothers smoking during pregnancy - 21002
# drug-related hospital stays - 20205.
optdata <- optdata %>% filter(!(ind_id %in% c("20205", "20403", "20204", "20402",
                                              "20301", "20401", "21001", "21002") & 
                                  substr(code, 1, 3) == "S02"))

#Merging with indicator and geography information
optdata <- left_join(x=optdata, y=ind_lookup, by="ind_id") 
optdata <- left_join(x=optdata, y=geo_lookup, by="code") 

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
optdata <- optdata %>% group_by(ind_id, year, areatype) %>%
  mutate(measure_sc = case_when(interpret=="H"~ as.vector(scales::rescale(measure, to=c(1,0))),
                                interpret=="L" ~ as.vector(scales::rescale(measure, to=c(0,1))),
                                TRUE ~ 0))  %>% ungroup()

# Tidying up the format
optdata <- optdata %>% #taking out some variables
  # we will be able to exclude ind_id here once release2.0 comes out
  select(-c(supression, supress_less_than, type_id, file_name)) %>%  
  #rounding variables
  mutate(numerator = round(numerator, 2), measure = round(measure, 2),
         lowci = round(lowci, 2), upci = round(upci, 2)) %>%
  droplevels() %>%  #to get rid of factor levels not present in data set.
  #Making the numerator the measure for a few indicators, so it plots correctly
  mutate(measure = ifelse(indicator %in% c('Mid-year population estimate - all ages',
                                           'S2 pupils - SALSUS', 'S4 pupils - SALSUS',
                                           "Quit attempts"), numerator, measure))

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

##END
