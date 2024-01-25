######################################################################################################################################.
# 1. Functions called during running of script that prepares data files included in the ScotPHO online profiles tool shiny app ----
######################################################################################################################################.

## Suppression function ------
# Purpose: applies statistical disclosure control (SDC) to any indicators where it is required (see technical document)
# Note: only some indicators require SDC 
# and they each have different thresholds for what needs suppressed (i.e. figures less than 5, less then 10 etc.)

apply_suppressions <- function(dataset) {
  
  dataset <- dataset |>
    # suppress numerator if required (for crude rates, %'s and standardised rates)
    mutate(numerator = case_when(supression == "Y" & 
                                   type_id %in% c('cr', '%', 'sr') 
                                 & numerator < supress_less_than 
                                 ~ NA_real_, TRUE ~ numerator),
           # additionally, suppress measure, upper ci and lower ci if it's a crude rate or % (to avoid backwards calculating)     
           across(.cols = c('measure', 'upci', 'lowci'),
                  ~ case_when(supression == "Y" & 
                                type_id %in% c('cr', '%') & 
                                numerator < supress_less_than ~ NA_real_, TRUE ~ .x))
    )
}


## Gap year function ----
# Purpose: Generates 'NA' data for indicators where there are years missing from their data file
# e.g. if a data collection was paused due to the pandemic, or that years data is deemed to be of poor data quality
# It ensures that when users are looking at trends in the profiles tool, the missing years still appear on the x-axis
# and instead of dropping to 0, a gap will appear in the trend chart to signify missing data

create_gap_year <- function(indicator_id, # ind_id
                            gap_year, # the missing year
                            base_year, # year to take a copy of to create data for missing year
                            gap_trend_axis) { # axis label for missing year
  
  # take a copy of a years data for that indicator
  base_year_data <- main_dataset |> 
    filter(ind_id == indicator_id & year == base_year)
  
  if(!(gap_year %in% unique(base_year_data$year))){
    
    # generate data for the missing year
    gap_year_data <- base_year_data |>
      mutate(year = gap_year, # overwrite the year column with missing year
             across(c("def_period", "trend_axis"), ~ gsub(unique(trend_axis),gap_trend_axis,.)), # overwrite the axis label with missing years axis label
             across(c("numerator","measure","lowci","upci"), ~ NA)) # populate numeric columns with NA
    
    return(gap_year_data) 
    
  }  else {
    
    warning(paste0("ABORT!!: ", gap_year," is already contained in the data for indicator ",indicator_id))
  }
  
}


## Re-coding geography codes function ----
# Purpose: replace any old/inactive geography codes with their new one
# Note: sometimes geography codes are made 'inactive' and replaced with a different one
# any changes are usually published here: https://www.opendata.nhs.scot/dataset/geography-codes-and-labels
# can be linked with boundary changes where postcodes/IZ change organistion affiliation and new standard geography codes are released.

replace_old_geography_codes <- function(data, col_name) {
  data |> 
    mutate(!!col_name := recode(!!sym(col_name), 
                                # Council area code changes
                                "S12000015" = 'S12000047', # Fife
                                "S12000024" = 'S12000048', # Perth and Kinross
                                "S12000046" = 'S12000049', # Glasgow City
                                "S12000044" = 'S12000050', # North Lanarkshire
                                # Health board code changes
                                "S08000018" = 'S08000029', # NHS Fife
                                "S08000027" = 'S08000030', # NHS Tayside
                                "S08000021" ='S08000031', # NHS Greater Glasgow & Clyde
                                "S08000023" = 'S08000032', # NHS Lanarkshire
                                # HSCP code changes
                                "S37000014" ='S37000032', # Fife
                                "S37000023" ='S37000033', # Perth and Kinross
                                "S37000015" ='S37000034', # Glasgow City
                                "S37000021" ='S37000035')) # North Lanarkshire))
}


## Combine data files ----
# Purpose: read in and combine individual indicator datasets to create one dataset
# note: will stop process and throw an error if there is a dataset that has missing columns

combine_files <- function(file_list) {
  
  # Determine the file type in list of files passed to function
  file_type <- if (grepl("\\.csv$", file_list[1])) 'csv' else 'rds'
  
  
  # Define expected columns 
  expected_cols <- if (file_type == 'csv') {
    
    # opt data mandatory columns 
    c("code", "ind_id", "year", "numerator", "rate", 
      "upci", "lowci", "def_period", "trend_axis")
  } else {
    
    # inequalities data mandatory columns 
    c("year",  "code",  "quintile",  "quint_type", 
      "numerator", "denominator", "rate", "lowci",
      "upci", "sii" , "lowci_sii" , "upci_sii" , "rii", 
      "lowci_rii", "upci_rii", "rii_int", "lowci_rii_int", 
      "upci_rii_int", "par", "abs_range", "rel_range", "ind_id"   
    )
  }
  
  check_columns <- function(data, expected_cols, file_name) {
    
    missing_cols <- setdiff(expected_cols, 
                            tolower(names(data)))
    
    if (length(missing_cols) > 0) {
      stop("Missing columns in file: ", file_name, ": ", 
           paste(missing_cols, collapse = ", "), "\n")
    }
  }
  
  
  combined_data <- rbindlist(lapply(file_list, function(x) {
    
    # read in datafile
    data <- if (file_type == 'rds') readRDS(x) else fread(x)
    
    # check columns 
    check_columns(data, expected_cols, x)
    
    # add column that includes filename
    data$file_name <- basename(x)
    #rename column
    colnames(data)[colnames(data) == 'rate'] <- 'measure'
    
    # clean column names 
    clean_names(data)
    
  }), use.names = TRUE)
  
  return(combined_data)
}


# create gepgraphy path --------
create_geography_path_column <- function(dataset) {
  dataset <- dataset %>%
    mutate(
      path = paste(
        areatype,
        case_when(
          areatype %in% c("Intermediate zone", "HSC locality") ~ parent_area,
          TRUE ~ areaname
        ),
        case_when(
          areatype %in% c("Intermediate zone", "HSC locality") ~ areaname,
          TRUE ~ NA_character_
        ),
        sep = "/"
      ),
      path = sub("/NA$", "", path)
    )
  
  return(dataset)
}



# Create geography nodes -------
# creates geography lists to be used in geography filter for the data table tab of the profiles tool
# this function is lifted from the documentation for the jsTreeR package (which is used to create this filter)
# see examples here: https://www.rdocumentation.org/packages/jsTreeR/versions/1.1.0/topics/jstree-shiny 
makeNodes <- function(leaves){
  dfs <- lapply(strsplit(leaves, "/"), function(s){
    item <-
      Reduce(function(a,b) paste0(a,"/",b), s[-1], s[1], accumulate = TRUE)
    data.frame(
      item = item,
      parent = c("root", item[-length(item)]),
      stringsAsFactors = FALSE
    )
  })
  dat <- dfs[[1]]
  for(i in 2:length(dfs)){
    dat <- merge(dat, dfs[[i]], all = TRUE)
  }
  f <- function(parent){
    i <- match(parent, dat$item)
    item <- dat$item[i]
    children <- dat$item[dat$parent==item]
    label <- tail(strsplit(item, "/")[[1]], 1)
    if(length(children)){
      list(
        text = label,
        children = lapply(children, f),
        icon =FALSE,
        state = list(selected = FALSE, opened = FALSE )
      )
    }else{
      list(text = label, type = "child",icon = FALSE,
           state = list(selected = FALSE,opened = FALSE ))
    }
  }
  lapply(dat$item[dat$parent == "root"], f)
}


######################################################################################################################################.
# 2. Data validation tests on the data file prepared for shiny app   ----
######################################################################################################################################.

## Test 1: Ensure no 2 files contain the same unique indicator ID
TEST_no_duplicate_ids <- function(data) {
  
  data <- data |> 
    distinct(ind_id, file_name) |>
    group_by(ind_id) |>
    add_tally() |>
    filter(n > 1)
  
  assert_that(nrow(data) == 0, 
              msg = paste0("The same indicator ID was found in more than 1 data file.", 
                           "\nThis could be because the previous file for this indicator was named slightly differently, and therefore wasn't overwritten OR because the wrong indicator ID has accidentally been used for a particular indicator.\n 
                Check the following files in the shiny folder: \n \n", 
                           paste(data$file_name, collapse = "\n")))
}


## Test 2: Ensure there are no indicator data files missing 
TEST_no_missing_indicators <- function(data) {
  
  data <- anti_join(technical_doc, data, by = c("ind_id"))
  
  assert_that(nrow(data) == 0, 
              msg = paste0("\nThe number of indicators in the dataset created DOES NOT match the number of indicators listed as 'Active' in the technical document. \n",
                           "This could be because there are indicators incorrectly listed as 'Active' in the technical document OR because there are data files missing from the shiny folder\n",
                           "Check the following indicator(s): \n",
                           paste(paste0("• ", data$indicator_name), collapse = "\n"))
  )
  
}


## Test 3: Ensure there are no indicators with missing metadata
TEST_no_missing_metadata <- function(data) {
  
  data <- data |>
    filter(is.na(indicator_name))
  
  assert_that(nrow(data) == 0, 
              msg = paste0("\n Metdata was not successfully joined for the following indicator(s) \n",
                           paste(paste0("• ", data$indicator_name), collapse = "\n"))
  )
  
}


## Test 4: Ensure there are no indicators with missing geography info
TEST_no_missing_geography_info <- function(data) {
  
  data <- data |>
    filter(is.na(areaname_full))
  
  assert_that(nrow(data) == 0, 
              msg = paste0("\n The following geography code(s) were not found in the geography lookup, either because they are invalid OR because they are 'old' codes no longer in use\n",
                           paste(paste0("• ", unique(data$code)), collapse = "\n")
              )
  )
  
}


## Test 5: Ensure suppression has been applied
TEST_suppression_applied <- function(data) {
  
  data <- data |>
    filter(supression == "Y") |>
    subset(numerator < supress_less_than)
  
  assert_that(nrow(data) == 0, 
              msg = paste0("SUPPRESSION NOT APPLIED! Please run the suppression function before saving this data.\n",
                           "The following indicators still contain numbers that shouldn't be there:\n",
                           paste(paste0("• ", data$indicators), collapse = "\n"))
  )
  
}


## Test 6: Ensure there are no inequalities indicators missing
TEST_no_missing_ineq_indicators <- function(data) {
  
  # find indicators from tech doc with an inequalities label
  depr_indicators <- technical_doc |>
    filter(!is.na(label_inequality))
  
  # check what indicators are in deprivation dataset
  data <- data |>
    select(indicator_name, ind_id) |>
    unique()
  
  # check which are missing
  data <- anti_join(depr_indicators, data, by = c("ind_id"))
  
  assert_that(nrow(data) == 0, 
              msg = paste0(
                "\nThe number of indicators DOES NOT match the number of indicators in the technical doc with an inequalities label assigned to them. This could be because they should not be produced at this level OR because there are data files missing from the shiny folder\n",
                "Check the following indicator(s): \n",
                paste(paste0("• ", data$indicator), collapse = "\n"))
  )
  
  
}

# Test 7: Ensure deprivation files go up to the correct year
TEST_inequalities_trends <- function(data) {
  
  # get list of main indicators and they're max year
  opt_indicators <- main_dataset |> 
    group_by(ind_id) |>
    summarise(year = max(year),
              ind_id = first(ind_id),
              indicator = first(indicator)) |>
    ungroup()
  
  
  # get list of inequalities indicators and they're max year
  depr_indicators <- data |> 
    group_by(ind_id) |>
    summarise(year = max(year),
              ind_id = first(ind_id),
              indicator = first(indicator_name)) |>
    ungroup() 
  
  # get indicators where max years don't match
  data <- left_join(depr_indicators, opt_indicators, by = c("ind_id")) |>
    filter(year.x != year.y)
  
  
  assert_that(nrow(data) == 0, 
              msg = paste0("The max year in the inequalities dataset is not the same as the max year in the main dataset for the indicators listed below.This could be because the deprivation function wasn't updated/run when refreshing these indicators, 
                           or because the new inequalities file wasn't moved across to the shiny folder \n", 
                           paste("max year for ", paste0(data$indicator.x, " should be: ", data$year.y, " but is ", data$year.x), collapse = "\n"))
  )
  
}



