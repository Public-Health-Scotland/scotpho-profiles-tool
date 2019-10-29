#Code to create ScotPHO's Shiny profile platform
#This script includes the data manipulation necessary to produce data in the way
#the Shiny app needs.

############################.
##Filepaths ----
############################.
if (sessionInfo()$platform %in% c("x86_64-redhat-linux-gnu (64-bit)", "x86_64-pc-linux-gnu (64-bit)")) {  
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
library(gsheet) #for reading google sheets
library(rgdal) #for reading shapefiles
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
         #Changing ands for & to reduce issues with long labels and " - " for "-"
         areaname = gsub(" and ", " & ", areaname),
         areaname = gsub(" - ", "-", areaname))

#Bringing parent geography information and formatting in one column with no duplicates
geo_parents <- readRDS(paste0(lookups, "Geography/IZtoPartnership_parent_lookup.rds")) %>% 
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
#Can't use read_csv as it's not the first tab of the spreadsheet.
ind_lookup <- gsheet2tbl("docs.google.com/spreadsheets/d/1JOr1_MSnKdQfg4o8qEiqX-EKDsbXUjejnAV4NzbSg78#gid=2036303524") %>% 
  setNames(tolower(names(.))) %>% #variables to lower case
  mutate(ind_id =as.numeric(ind_id)) %>% 
  mutate_if(is.character, factor)  # converting variables into factors

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
  #Dealing with changes in ca, hb and hscp codes. Transforms old code versions into 2019 ones
  mutate(code = recode(code, "S12000015"='S12000047', "S12000024"='S12000048', 
              "S12000046"='S12000049', "S12000044"='S12000050',
              "S08000018"='S08000029', "S08000027"= 'S08000030',
              "S08000021"='S08000031', "S08000023"= 'S08000032', 
              "S37000014"='S37000032', "S37000023"='S37000033',
              "S37000015"='S37000034', "S37000021"='S37000035'))

# Adding update date for all indicators based on technical document
update_table <- techdoc %>% rename(ind_id = indicator_number, update_date = last_updated) %>% 
  select(ind_id, update_date) %>% filter(ind_id != "no_id") %>% 
  mutate(update_date = as.yearmon(update_date, "%b-%Y"),
         ind_id = as.integer(paste0(ind_id)))

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
  select(-c(supression, supress_less_than, type_id, file_name, label_ineq)) %>%  
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
## Inequalities data ----
###############################################.
###############################################.
## Preparing Andy's indicators data
.# This functions reads Andy's data and formats it the way we need to be joined
# with the rest of the data
prepare_andyp_data <- function(filename, indic_id) {
  overall_data <- read_csv(paste0("data/", filename, ".csv")) %>% 
    mutate(code = paste0(substr(code, 1, 3), "0", substr(code, 5, 9)),
           quintile = recode(as.character(quintile), "0" = "Total"),
           quint_type = case_when(substr(code, 1, 3) == "S08" ~ "hb_quin",
                                  substr(code, 1, 3) == "S12" ~ "ca_quin",
                                  substr(code, 1, 3) == "S00" ~ "sc_quin"),
           ind_id=indic_id) %>%
    rename(measure = rate) %>% 
    select (-code2)
  
}

andyp_data <- rbind( # merging together all indicators
  prepare_andyp_data("04_prev_hosp_sii_rii_opt", 4),
  prepare_andyp_data("07_hc_amenable_mort_3-year aggregate_sii_rii_opt", 7),
  prepare_andyp_data("08_prem_mort_3-year aggregate_sii_rii_opt", 8)
)

###############################################.
## Rest of the data
#Finds all the rds for inequalities in the data folder reads them and combine them.
files <-  list.files(path = shiny_files, pattern = "*_ineq.rds", full.names = TRUE)
View(gsub(paste0(shiny_files, "/"), "", files))
data_depr <- do.call(rbind, lapply(files, readRDS)) %>% 
  rename(measure = rate) 

# Merging with Andy's data and then formatting
data_depr <- bind_rows(data_depr, andyp_data) %>% 
  mutate_if(is.character,factor) %>% #converting characters into factors
  mutate_at(c("numerator", "measure", "lowci", "upci", "rii", "upci_rii",
              "lowci_rii", "sii", "lowci_sii", "upci_sii", "par", "abs_range", 
              "rel_range", "rii_int", "lowci_rii_int", "upci_rii_int"),
            round, 1) %>% 
  #Dealing with changes in ca, hb and hscp codes. Transforms old code versions into 2019 ones
  mutate(code = recode(code, "S12000015"='S12000047', "S12000024"='S12000048', 
                       "S12000046"='S12000049', "S12000044"='S12000050',
                       "S08000018"='S08000029', "S08000027"= 'S08000030',
                       "S08000021"='S08000031', "S08000023"= 'S08000032', 
                       "S37000014"='S37000032', "S37000023"='S37000033',
                       "S37000015"='S37000034', "S37000021"='S37000035'))

#Merging with indicator and geography information
data_depr <- left_join(x=data_depr, y=ind_lookup, by="ind_id") 
data_depr <- left_join(x=data_depr, y=geo_lookup, by="code") %>% 
  select(-profile_domain1, -profile_domain2, -areaname_full, -parent_area) %>% 
  mutate(quintile = recode(quintile, "1" = "1 - most deprived", 
                           "5" = "5 - least deprived")) %>% 
  droplevels()

#Apply supressions. 
# If indicator is presented as standardised rate and suppression required then suppress numerator where count is less than specified value.
# standardised rates do not require suppression of rates or CI.
# If indicator is presented as crude rate or percentage and suppression required then suppress numerator where count is less than specified value.
# crude rate and percentages DO require suppression of rates and CI as well as numerator.
data_depr <- data_depr %>% 
  mutate(numerator = case_when(supression == "Y" & numerator < supress_less_than ~ NA_real_,
                               TRUE  ~ numerator),
         measure = case_when(supression == "Y" & (substr(type_id,1,2)=='cr' | (substr(type_id,1,1))=='%') &
                               numerator<supress_less_than ~ NA_real_, TRUE  ~ measure),
         lowci = case_when(supression == "Y" & (substr(type_id,1,2)=='cr' | (substr(type_id,1,1))=='%') &
                             numerator<supress_less_than ~ NA_real_, TRUE  ~ lowci),
         upci = case_when(supression == "Y" & (substr(type_id,1,2)=='cr' | (substr(type_id,1,1))=='%') &
                            numerator<supress_less_than ~ NA_real_, TRUE  ~ upci)) %>% 
  select(-supression, -supress_less_than, -type_id)

#selecting out indicators where higher is better as app doesn't work with them
data_depr <- data_depr %>% 
  filter(!(indicator %in% c("Child dental health in primary 1", 
                            "Child dental health in primary 7",
                            "Healthy birth weight",
                            "Bowel screening uptake",
                            "Single adult dwellings",
                            "Immunisation uptake at 24 months - 5 in 1",
                            "Immunisation uptake at 24 months - MMR",
                            "Teenage pregnancies"))) %>% droplevels()

# Temporary until we decide to add new indicators
data_depr <- data_depr %>% 
  filter(!(indicator %in% c("Alcohol-related mortality", 
                            "Alcohol-related hospital stays",
                            "New cancer registrations",
                            "Drug-related hospital stays",
                            "Population prescribed drugs for anxiety/depression/psychosis",
                            "Deaths from suicide",
                            "People living in 15% most 'access deprived' areas",
                            "Women smoking during pregnancy"))) %>% droplevels()

saveRDS(data_depr, "data/deprivation_data.rds")

###############################################.
## Shapefiles ----
###############################################.
#Reading file with council shapefiles: https://www.spatialdata.gov.scot/geonetwork/srv/eng/catalog.search;jsessionid=4B4BEB1B1E52BCA9D3C1FD531CC199F8#/metadata/1cd57ea6-8d6e-412b-a9dd-d1c89a80ad62
#making it small 29mb to 2.5.
ca_bound_orig <-readOGR(shapefiles, "pub_las") %>%
  rmapshaper::ms_simplify(keep=0.0025)  %>% 
  setNames(tolower(names(.))) #variables to lower case

object.size(ca_bound_orig)

#Transforming coordinate system to the one leaflet needs
ca_bound_orig <- spTransform(ca_bound_orig,  CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))

#Saving the simplified shapefile to avoid the calculations.
writeOGR(ca_bound_orig, dsn=shapefiles, "CA_simpl_2019", driver="ESRI Shapefile", overwrite_layer=TRUE)

#Saving as rds as it is much faster to read
names(ca_bound_orig@data)[names(ca_bound_orig@data)=="local_auth"] <- "area_name"

saveRDS(ca_bound_orig, paste0(shapefiles, "CA_boundary.rds"))
saveRDS(ca_bound_orig, "data/CA_boundary.rds")

##########################.
###Health board
#making it small 29mb to 2.5. Sometimes it fails, due to lack of memory (use memory.limits and close things).
hb_bound_orig<-readOGR(shapefiles,"SG_NHS_HealthBoards_2019") %>% 
  rmapshaper::ms_simplify(keep=0.0025) %>% 
  setNames(tolower(names(.))) #variables to lower case

object.size(hb_bound_orig)

#Transforming coordinate system to the one leaflet needs
hb_bound_orig <- spTransform(hb_bound_orig,  CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))

#Saving the simplified shapefile to avoid the calculations.
writeOGR(hb_bound_orig, dsn=shapefiles, "HB_simpl_2019", driver="ESRI Shapefile", overwrite_layer=TRUE)

#Saving as rds as it is much faster to read
names(hb_bound_orig@data)[names(hb_bound_orig@data)=="hbcode"] <- "code"
names(hb_bound_orig@data)[names(hb_bound_orig@data)=="hbname"] <- "area_name"

saveRDS(hb_bound_orig, paste0(shapefiles, "HB_boundary.rds"))
saveRDS(hb_bound_orig, "data/HB_boundary.rds")

##########################.
###HSC Partnership
#making it small 29mb to 2.5. Sometimes it fails, due to lack of memory (use memory.limits and close things).
hscp_bound_orig <- readOGR(shapefiles,"SG_NHS_IntegrationAuthority_2019") %>% 
  rmapshaper::ms_simplify(keep=0.0025) %>% 
  setNames(tolower(names(.))) #variables to lower case

object.size(hscp_bound_orig)

#Changing the projection to WSG84, the ones leaflet needs.
proj4string(hscp_bound_orig) #Checking projection
hscp_bound_orig <- spTransform(hscp_bound_orig, CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))

#Saving the simplified shapefile.
writeOGR(hscp_bound_orig, dsn=shapefiles, "HSCP_simpl_2019", 
         driver="ESRI Shapefile", overwrite_layer=TRUE, verbose=TRUE,
         morphToESRI=TRUE)

names(hscp_bound_orig@data)[names(hscp_bound_orig@data)=="hiacode"] <- "code"
names(hscp_bound_orig@data)[names(hscp_bound_orig@data)=="hianame"] <- "area_name"

saveRDS(hscp_bound_orig, paste0(shapefiles, "HSCP_boundary.rds"))
saveRDS(hscp_bound_orig, "data/HSCP_boundary.rds")
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

saveRDS(iz_bound_orig, paste0(shapefiles, "IZ_boundary.rds"))
saveRDS(iz_bound_orig, "data/IZ_boundary.rds")
iz_bound <- readRDS("data/IZ_boundary.rds")

##END
