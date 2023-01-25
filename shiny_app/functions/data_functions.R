###############################################################################
#
# Functions used for dataframes
#
###############################################################################


# 1. formatting data downloads
# select and rename columns to ensure downloaded data is userfriendly
format_csv <- function(reactive_dataset, extra_vars = NULL ) {
  
  techdoc <- techdoc %>%
    select(indicator_name, data_source)
  left_join(reactive_dataset, techdoc, by = c("indicator" = "indicator_name")) %>%
    select(c("indicator", 
             "area_name" = "areaname", 
             "area_code" = "code", 
             "area_type" = "areatype", 
             "year", 
             "period" = "def_period", 
             "numerator", 
             "measure",
             "lower_confidence_interval" = "lowci", 
             "upper_confidence_interval" = "upci", 
             extra_vars, 
             "definition" = "type_definition", 
             "data_source"))
  
}