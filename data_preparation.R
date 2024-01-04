# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This script prepares the data files required for the ScotPHO Profiles Tool
# 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 1. Set up --------------------------------------------------------------------

## Dependencies  -----

library(dplyr) # data wrangling
library(openxlsx) # for reading in technical document / converting excel dates
library(readr) # for reading csv files
library(data.table) # for quickly combining multiple files
library(scales) # for re-scaling measures
library(arrow) # for writing parquet files
library(janitor) # for cleaning column names
library(assertthat) # for data validation tests
library(purrr) # for copying multiple files at once

source("Data preparation/Data prep functions.R")



## File-paths -----

data_folder <- "/PHI_conf/ScotPHO/Profiles/Data/"
lookups <- paste0(data_folder, "Lookups/")
shape_files <- paste0(data_folder, "Shapefiles/")
shiny_files <- paste0(data_folder, "Shiny Data")
backups <- paste0(data_folder, "Backups/")




## Look-ups -----

geography_lookup <- readRDS(
  file = paste0(lookups, "Geography/opt_geo_lookup.rds")
)


technical_doc <- read.xlsx(
  xlsxFile = paste0(lookups, "Technical_Document_MM.xlsx"), 
  sheet = "Raw", 
  sep = " ") |>
  clean_names() |>
  filter(active == "A")





# 2. Prepare technical Document  -----------------------------------------------

## Clean date columns -----
technical_doc <- technical_doc |>
  mutate(
    last_updated_date = suppressWarnings(convertToDate(last_updated)), 
    days_since_update = difftime(Sys.Date(), last_updated_date),
    across(
      contains("update"), 
      ~ suppressWarnings(strftime(convertToDate(.), "%b-%Y"))
    )
  )



## Save file -----
write_parquet(technical_doc, "shiny_app/data/techdoc") # version for shiny app
write_parquet(technical_doc, paste0(backups, "techdoc-", Sys.Date())) # version for backups folder



## Select columns to be joined to main indicator dataset -----
technical_doc <- technical_doc |>
  select(
    ind_id, indicator, type_id, interpret, supression,
    supress_less_than, type_definition, profile_domain1,
    profile_domain2, profile_domain3, label_inequality
  )





# 3. Create main indicator dataset ---------------------------------------------


## 3.1 Read in and combine data --------

### Create backup of existing data from local repo -----
if (file.exists("shiny_app/data/optdata")) {
  
  file.copy(
    "shiny_app/data/optdata", 
    paste0(backups, "main_dataset_", Sys.Date()), 
    overwrite = TRUE
  )
  
} 


### Find all separate indicator data files in the shiny data folder -----
indicator_data_files <- list.files(
  path = shiny_files, 
  pattern = "*_shiny.csv", 
  full.names = TRUE
)



### Combine separate files into one dataset  -----
main_dataset <- combine_files(indicator_data_files)



### Read in older data ----
# note: most indicators will have their own separate data file. 
# However, some indicators which haven't been updated in years sit in a file called 'All Data for Shiny.csv' 
# from when there was a different process for creating indicator data
old_data <- read_csv(paste0(shiny_files, "/All Data for Shiny.csv")) |>
  filter(!(ind_id %in% unique(main_dataset$ind_id)))



### Combine new and old data ----
main_dataset <- bind_rows(main_dataset, old_data)


### Tests ----
# note: if tests pass, 'TRUE' will print in console
# otherwise error message will appear with details of the issue
TEST_no_duplicate_ids(main_dataset)

TEST_no_missing_indicators(main_dataset)




## 3.2 Add technical information  ---------

### Attach metadata from the technical doc ----
main_dataset <- left_join(x = main_dataset, y = technical_doc, by = "ind_id")


TEST_no_missing_metadata(main_dataset)



## 3.2. Add geography information --------
main_dataset <- main_dataset |> 
  # temporarily removing this indicator at HSC locality level as still uses old codes
  filter(!(indicator == "Children in low income families" & 
             substr(code, 1, 3) == "S99")
  ) |>
  replace_old_geography_codes(col_name = "code") |>
  left_join(geography_lookup, by = "code")


TEST_no_missing_geography_info(main_dataset)



## 3.3  Apply suppression where required ---------
main_dataset <- main_dataset |>
  apply_suppressions()



## 3.4 Format data ---------

# convert some cols to numeric and round digits
main_dataset <- main_dataset |>
  mutate(
    across(
      c("numerator", "measure", "upci", "lowci"),
      ~ round(as.numeric(.), digits = 2)
    )
  )


# create new variable ?
main_dataset <- main_dataset |> 
  mutate(
    measure_sc = case_when(
      interpret == "H" ~ as.vector(rescale(measure, to = c(1, 0))),
      interpret == "L" ~ as.vector(rescale(measure, to = c(0, 1))),
      TRUE ~ 0
    )
  )



# some indicators have years missing from their dataset (e.g. if no data was collected that year due to covid)
# To ensure that the data in the trend tab doesn't drop to 0 for those years, we create data for those missing years
# and populate the measure with 'NA' instead - this creates a gap in the trend chart, instead of an incorrect drop to 0
# Note: This might make more sense to eventually add to the indicator production functions instead?
main_dataset <- main_dataset |>
  rbind(
    # child dental health pri 1
    create_gap_year(
      indicator_id = 21005, 
      gap_year = 2020, 
      base_year = 2019, 
      gap_trend_axis = "2020/21"
    ), 
    
    # Population within 500 metres of a derelict site
    create_gap_year(
      indicator_id = 20901, 
      gap_year = 2020, 
      base_year = 2019, 
      gap_trend_axis = "2020"
    ) 
  )



# Temporarily remove some indicators from IZ level to reduce risk of secondary disclosure
# until we are sure that statistical disclosure signed off
main_dataset <- main_dataset |>
  filter(
    !(ind_id %in% c(
      "20205", # drug-related hospital stays
      "20403", # deaths from suicide
      "20204", # alcohol-related deaths
      "20402", # psychiatric hospital admissions
      "20301", # cancer registrations
      "20401", # teenage pregnancies
      "21001", # Population prescribed drugs for anxiety/depression/psychosis
      "21002" # mothers smoking during pregnancy
    ) & substr(code, 1, 3) == "S02")
  )


# 3.5 Save final file -------------

# double check suppression was applied
TEST_suppression_applied(main_dataset) 

# if test is passed, remove columns no longer required
main_dataset <- main_dataset |>
  select(-c(supression, supress_less_than, type_id, file_name))

# save file to local repo
write_parquet(main_dataset, "shiny_app/data/optdata")






# 4. Create deprivation dataset ------------------------------------------------

## Create backup of existing data from your local repo ----
if (file.exists("shiny_app/data/deprivation_data")) {
  
  file.copy(
    "shiny_app/data/deprivation_data", 
    paste0(backups, "deprivation_data_", Sys.Date()), 
    overwrite = TRUE
  )
  
} 



## combine files ----
files_depr <- list.files(
  path = shiny_files, 
  pattern = "*_ineq.rds", 
  full.names = TRUE
)


data_depr <- combine_files(files_depr)


## prepare older data ----
hsc_ineq_files <- list.files(
  path = paste0(shiny_files, "/Inequalities HSC Data/reformatted"), 
  pattern = "*.rds", 
  full.names = TRUE
)

hsc_ineq_data <- combine_files(hsc_ineq_files)


# combine old and new data ----
data_depr <- bind_rows(data_depr, hsc_ineq_data)


# attach metadata ----
data_depr <- left_join(x = data_depr, y = technical_doc, by = "ind_id")


# attach geography info ----
data_depr <- data_depr |>
  replace_old_geography_codes(col_name = "code") |>
  left_join(geography_lookup, by = "code")


TEST_no_missing_ineq_indicators(data_depr)
TEST_no_missing_geography_info(data_depr)


# apply suppression function ----
data_depr <- data_depr |>
  apply_suppressions()


# formatting numbers ----
data_depr <- data_depr |>
  mutate(quintile = recode(quintile,
                           "1" = "1 - most deprived",
                           "5" = "5 - least deprived"
  )) |>
  mutate_at(
    c(
      "numerator", "measure", "lowci", "upci", "rii", "upci_rii",
      "lowci_rii", "sii", "lowci_sii", "upci_sii", "par", "abs_range",
      "rel_range", "rii_int", "lowci_rii_int", "upci_rii_int"
    ),
    round, 1
  )




data_depr <- data_depr |>
  group_by(ind_id, year, quint_type, code) |>
  # label if par, sii or rii  positive or negative (helps with health inequality dynamic summary text)
  mutate(
    across(c(sii, rii, par),
           ~ case_when(. > 0 ~ "positive", . < 0 ~ "negative", TRUE ~ "zero"),
           .names = "{.col}_gradient"
    )
  ) |>
  mutate(
    qmax = quintile[which.max(measure)], # which quintile contains highest rate/value
    qmin = quintile[which.min(measure)] # which quintile contains lowest rate/value
  ) |>
  ungroup()


# TEMPORARY - remove some indicators
data_depr <- data_depr |>
  filter(!(indicator %in% c(
    "Healthy birth weight", # check healthy weight as seems like reverse of what i'd expect
    "People living in 15% most 'access deprived' areas" # does this indicator make sense in inequalities module - can't quite get my head around what this means
  )
  )
  )



# make sure there's no deprivation indicators where latest year doesn't go up to the same year covered by the main dataset
TEST_inequalities_trends(data_depr)


# save file for shiny app
TEST_suppression_applied(data_depr) # double checking suppression function wasn't skipped
write_parquet(main_dataset, "shiny_app/data/deprivation_data")





# 5. Shapefiles  --------------------------------------------------------------

# Copy geography lookups to your local repository
# Note: this step is only really necessary if you are running this script for the first time
# OR if there have been updates to the geography lookups
map_lgl(c("CA_boundary.rds", 
          "HB_boundary.rds", 
          "HSCP_boundary.rds",
          "HSC_locality_boundary.rds",
          "IZ_boundary.rds"), ~ {
            
            file.copy(
              paste0(shape_files, .x), # old file path
              paste0("shiny_app/data/", .x),  # new destination
              overwrite = TRUE
            )
          })
