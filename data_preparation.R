#Code to create ScotPHO's Shiny profile platform
#This script includes the data manipulation necessary to produce data in the way
#the Shiny app needs.

############################.
##Filepaths ----
############################.
if (sessionInfo()$platform %in% c("x86_64-redhat-linux-gnu (64-bit)", "x86_64-pc-linux-gnu (64-bit)")) {
  lookups <- "/PHI_conf/ScotPHO/Profiles/Data/Lookups/"
  shapefiles <- "/PHI_conf/ScotPHO/Profiles/Data/Shapefiles/"
  shiny_files <- "/PHI_conf/ScotPHO/Profiles/Data/Shiny Data/"
} else  {
  lookups <- "//stats/ScotPHO/Profiles/Data/Lookups/"
  shapefiles <- "//stats/ScotPHO/Profiles/Data/Shapefiles/"
  shiny_files <- "//stats/ScotPHO/Profiles/Data/Shiny Data/"
}

############################.
##Packages ----
############################.
library(dplyr)
library(tidyr)
library(readr)
library(magrittr)
library(scales) #rescaling variables
library(haven) #for SPPS file reading
library(data.table) #reading data
library(zoo) # dealing with dates
library(lubridate) #for automated list of dates in welcome modal
library(janitor) #cleaning names
library(gsheet) #for reading google sheets
library(rgdal) #for reading shapefiles

###############################################.
## Functions ----
###############################################.

# If indicator is presented as standardised rate and suppression required
# then suppress numerator where count is less than specified value.
# standardised rates do not require suppression of rates or CI.
# If indicator is presented as crude rate or percentage and suppression required
# then suppress numerator where count is less than specified value.
# crude rate and percentages DO require suppression of rates and CI as well as numerator.
apply_supressions <- function(dataset) {
  dataset %<>%
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
}

# This functions reads Andy's HSC inequality data and formats it the way we need to be joined
# with the rest of the deprivation data
prepare_andyp_data <- function(filename, indic_id) {
  read_csv(paste0(shiny_files, "Inequalities HSC Data/", filename, ".csv")) %>%
    mutate(code = paste0(substr(code, 1, 3), "0", substr(code, 5, 9)),
           quintile = recode(as.character(quintile), "0" = "Total"),
           quint_type = case_when(substr(code, 1, 3) == "S08" ~ "hb_quin",
                                  substr(code, 1, 3) == "S12" ~ "ca_quin",
                                  substr(code, 1, 3) == "S00" ~ "sc_quin"),
           ind_id=indic_id) %>%
    rename(measure = rate) %>%
    select (-code2)

}

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

definition_table %<>%
  mutate(days_since_update=day(as.period(new_date %--% today(), unit="days"))) %>%
  #filtering out non-active indicators
  filter(active == "A")

saveRDS(definition_table,"shiny_app/data/techdoc.rds") #for opt
techdoc <- readRDS("shiny_app/data/techdoc.rds")
#backup copy in case issues with google drive
write_csv(definition_table, paste0("/PHI_conf/ScotPHO/Profiles/Data/Backups/techdoc_backup_", today() ,".csv"))

###############################################.
## Lookups ----
###############################################.
# Lookup with all geography codes information. This file is created in the lookup repo
geo_lookup <- readRDS(paste0(lookups, "Geography/opt_geo_lookup.rds"))
saveRDS(geo_lookup, "shiny_app/data/geo_lookup.rds")

###############################################.
## Indicator lookup table
#Can't use read_csv as it's not the first tab of the spreadsheet.
# For some reason, it's important that the raw tab is alphabetically sorted for this to work properly
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
optdata <- readRDS("shiny_app/data/optdata.rds")
saveRDS(optdata, paste0("/PHI_conf/ScotPHO/Profiles/Data/Backups/shiny_tool_backup_data_", today() ,".rds"))

#Finds all the csv files in the shiny folder
files <-  list.files(path = shiny_files, pattern = "*.csv", full.names = TRUE)
# taking out spss/old opt data
files <- files[files != "/PHI_conf/ScotPHO/Profiles/Data/Shiny Data//All Data for Shiny.csv"]
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
data_spss <- read_csv(paste0(shiny_files, "All Data for Shiny.csv"),
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
optdata %<>% filter(!(ind_id %in% c("20205", "20403", "20204", "20402",
                                              "20301", "20401", "21001", "21002") &
                                  substr(code, 1, 3) == "S02"))

# TEMPORARY - take out localities data for children low income as it's not updated yet
optdata %<>% filter(!(ind_id == "20705" & substr(code, 1, 3) == "S99"))

#Merging with indicator and geography information
optdata <- left_join(x=optdata, y=ind_lookup, by="ind_id")
# if for some reason some indicators haven't matched with the lookup this will show them
View(optdata %>% filter(is.na(indicator)))

optdata <- left_join(x=optdata, y=geo_lookup, by="code")

#Apply supressions.
optdata %<>% apply_supressions()

# Scaling measures (0 to 1) in groups by year, area type and indicator.
optdata %<>% group_by(ind_id, year, areatype) %>%
  mutate(measure_sc = case_when(interpret=="H"~ as.vector(scales::rescale(measure, to=c(1,0))),
                                interpret=="L" ~ as.vector(scales::rescale(measure, to=c(0,1))),
                                TRUE ~ 0))  %>% ungroup()

# Tidying up the format
optdata %<>% #taking out some variables
  # we will be able to exclude ind_id here once release2.0 comes out
  select(-c(supression, supress_less_than, type_id, file_name, label_ineq)) %>%
  #rounding variables
  mutate(numerator = round(numerator, 2), measure = round(measure, 2),
         lowci = round(lowci, 2), upci = round(upci, 2)) %>%
  droplevels() %>%  #to get rid of factor levels not present in data set.
  #Making the numerator the measure for a few indicators, so it plots correctly
  mutate(measure = ifelse(indicator %in% c('Mid-year population estimate - all ages',
                                           'S2 pupils who smoke (SALSUS)', 'S4 pupils who smoke (SALSUS)',
                                           "Smoking quit attempts"), numerator, measure))

saveRDS(optdata, "shiny_app/data/optdata.rds")
optdata <- readRDS("shiny_app/data/optdata.rds")

###############################################.
## Profile lookup ----
###############################################.
#Creating a file with a column for profile and another one for domain
profile_lookup <- data.frame(profile_domain = c(paste(unique(optdata$profile_domain1)),
                                                paste(unique(optdata$profile_domain2)))) %>%
  mutate(profile = substr(profile_domain, 1, 3),
         domain = substr(profile_domain, 5, nchar(as.vector(profile_domain)))) %>%
  select(-profile_domain)

saveRDS(profile_lookup, "shiny_app/data/profile_lookup.rds")
profile_lookup <- readRDS("shiny_app/data/profile_lookup.rds")

###############################################.
## Inequalities data ----
###############################################.
data_depr <- readRDS("shiny_app/data/deprivation_data.rds") #deprivation/inequalities dataset

saveRDS(data_depr, paste0("/PHI_conf/ScotPHO/Profiles/Data/Backups/deprivation_data_", today() ,".rds"))

###############################################.
## Preparing Andy's indicators data

andyp_data <- rbind( # merging together all indicators
  prepare_andyp_data("01_pc_access_sii_rii_opt", 1),
  prepare_andyp_data("04_prev_hosp_sii_rii_opt", 4),
  prepare_andyp_data("05_rep_hosp_sii_rii_opt", 5),
  prepare_andyp_data("06_dying_hosp_sii_rii_opt", 6),
  prepare_andyp_data("07_hc_amenable_mort_3-year aggregate_sii_rii_opt", 7),
  prepare_andyp_data("08_prem_mort_3-year aggregate_sii_rii_opt", 8)) %>%
  # patients by gp is all scotland simd
  mutate(quint_type = case_when(ind_id ==1  ~ "sc_quin",
                                TRUE ~ quint_type))

###############################################.
## Rest of the data
#Finds all the rds for inequalities in the data folder reads them and combine them.
files_depr <-  list.files(path = shiny_files, pattern = "*_ineq.rds", full.names = TRUE)
View(gsub(paste0(shiny_files, "/"), "", files_depr))
data_depr <- do.call(rbind, lapply(files_depr, readRDS)) %>%
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

data_depr <- data_depr %>% apply_supressions() #Apply supressions.

#selecting out indicators where higher is better as app doesn't work with them
data_depr <- data_depr %>%
  filter(!(indicator %in% c("Child dental health in primary 1",
                            "Child dental health in primary 7",
                            "Healthy birth weight",
                            "Bowel screening uptake",
                            "Single adult dwellings",
                            "Immunisation uptake at 24 months - 6 in 1",
                            "Immunisation uptake at 24 months - MMR",
                            "Teenage pregnancies"))) %>% droplevels()

# Temporary until we decide to add new indicators
data_depr <- data_depr %>%
  filter(!(indicator %in% c("Deaths from suicide"))) %>% droplevels()

saveRDS(data_depr, "shiny_app/data/deprivation_data.rds")

###############################################.
## Shapefiles ----
###############################################.
##########################.
### Council area
ca_bound_orig <- readRDS(paste0(shapefiles, "CA_boundary.rds"))
saveRDS(ca_bound_orig, "shiny_app/data/CA_boundary.rds")
##########################.
###Health board
hb_bound_orig <- readRDS(paste0(shapefiles, "HB_boundary.rds"))
saveRDS(hb_bound_orig, "shiny_app/data/HB_boundary.rds")
##########################.
###HSC Partnership
hscp_bound_orig <- readRDS(paste0(shapefiles, "HSCP_boundary.rds"))
saveRDS(hscp_bound_orig, "shiny_app/data/HSCP_boundary.rds")
###############################################.
# HSC locality
locality_shp <- readRDS(paste0(shapefiles, "HSC_locality_boundary.rds"))
saveRDS(locality_shp, "shiny_app/data/HSC_locality_boundary.rds")
##########################.
###Intermediate zone
iz_bound <- readRDS(paste0(shapefiles, "IZ_boundary.rds"))
saveRDS(iz_bound_orig, "shiny_app/data/IZ_boundary.rds")

##END
