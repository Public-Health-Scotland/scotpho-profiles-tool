#Code to create ScotPHO's Shiny profile platform
#In this script include packages, functions, datasets and anyting that will be used both by UI and server

# TODO:
#Data preparation: 
#   Lookups need to be checked/refined: indicator measures, domains etc.
#   Indicator and geographical info out of main file and use merge/lookups?
#   Include deprivation indicators in lookup
#   Format better the map shapefiles (lower case variable names)
#   Deprivation needs PAR 
#   Add denominator?
#   Take out Plotly toolbox and produce plots with a button?
#----------------.
#General:
#   Fix bookmarking - check if server solution in .io ?enableBookmarking
#   Include reporting functionality
#   Create user guide
#   Host in the website and link from here: shapefiles, lookups, etc
#----------------.
#Barcode chart:
#   Make reactive and incorporate
#----------------.
#Overview:  
#   Long labels of indicators are an issue  https://github.com/plotly/plotly.js/issues/296#issuecomment-371780321
#   Add functionaly to instead of comparing against an area, comparing against a baseline/past year
#----------------.
#Time trend: 
#   Adding numerator/rate tick box?
#   Add functionality to be able to add IZ/localities fromm different partnerships
#   To fix long label issue move to have legend as a different chart? 
#   Or inside same chart try to move legend below chart
#----------------.
#Rank chart
#   Issue with long label names, take them out? put them in the side? in the bars?
#   Vertical bars instead?
#   Indicator list reactive to geography?
#----------------.
#Table:
#   Add deprivation data to table (maybe with switch or just merging everything)
#   Change placeholder text in filters (require javascript)
#   Move from DT filter to Shiny ones
#   Include year variable
#   Include filter to select all IZ/locs for a specific partnerhsip
#----------------.
#Map:
#   Avoid redrawing of map using leafletProxy
#   Review what to include in tooltip
#   Add intermediate zones to map, or is it going to be too big?
#   How to save map as png? Move away from Leaflet? Will likely be faster (sp ggmap)
#   For IZ's and localities maybe something like this: https://isresearchnc.shinyapps.io/CMaps/
#----------------.
#Deprivation
#   Do we want CI's? Error bars? polygon areas for time trends?
#   Include PAR information and charts (trend and bar)
#   Add slope of SII to bar chart
#   Maybe improve a bit the tooltip for RII and SII?
#   instead of time period dropdown do slider
#----------------.

############################.
##Packages ----
############################.
library(shiny)
library(shinythemes) # layouts for shiny
library(tidyverse) # data manipulation, ggplot
library (DT) # for data tables
library(leaflet) #javascript maps
library(plotly) #interactive graphs

###############################################.
## Functions ----
###############################################.  
# Selects the variables of interest and renames them so csv downloads are 
# more user friendly
format_csv <- function(reactive_dataset) {
  reactive_dataset %>% 
    select(c(indicator, areaname, areatype, def_period, numerator, measure, 
             lowci, upci, type_definition)) %>% 
    rename(lower_confidence_interval=lowci, upper_confidence_interval=upci, 
           period = def_period, definition = type_definition)
}

###############################################.
## Data ----
###############################################.    
optdata <- readRDS("./data/optdata.rds") 
deprivation <- readRDS("./data/deprivation_OPT.rds")

geo_lookup <- readRDS("./data/geo_lookup.rds") #geography lookup

#Map data
ca_bound<-readRDS("./data/CA_boundary.rds") #Reading file with council shapefiles
hb_bound<-readRDS("./data/HB_boundary.rds") #Reading file with health board shapefiles simplified

###############################################.
## Names ----
###############################################.   
#Geographies names
area_list <- sort(geo_lookup$areaname)
comparator_list <- sort(geo_lookup$areaname[geo_lookup$areatype %in% 
                                    c("Health board", "Council area", "Scotland")]) 
hb_name <- sort(geo_lookup$areaname[geo_lookup$areatype=="Health board"]) 
la_name <- sort(geo_lookup$areaname[geo_lookup$areatype=="Council area"]) 
intzone_name <- sort(geo_lookup$areaname[geo_lookup$areatype=="Intermediate zone"]) 
partnership_name <- sort(geo_lookup$areaname[geo_lookup$areatype=="HSC Partnership"]) 
locality_name <- sort(geo_lookup$areaname[geo_lookup$areatype=="HSC Locality"]) 

#Area type names
areatype_list <- c("Scotland", "Health board", "Council area", "HSC Partnership", 
                   "HSC Locality", "Intermediate zone")
areatype_noscot_list <- c("Health board", "Council area", "HSC Partnership", 
                          "HSC Locality", "Intermediate zone")
areatype_depr_list <- c("Scotland", "Health board", "Council area") #for deprivation tab

#Indicator names
indicator_list <- sort(unique(optdata$indicator))
indicator_map_list <- sort(unique(optdata$indicator[optdata$interpret != 'O']))
ind_depr_list <- unique(deprivation$indicator)

#Topic names
# topic_list <- sort(c('Alcohol', 'Behaviours',  'Cancer', 'Children and young people - Achieving ', 
#                 'Children and young people - Active', 'Children and young people - Healthy', 
#                 'Children and young people - Included', 'Children and young people - Nurtured', 
#                 'Children and young people - Responsible', 'Children and young people - Safe', 
#                 'Community safety', 'Crime', 'Deprivation', 'Drugs', 'Economy', 
#                 'Education', 'Environment', 'Ill Health & Injury', 
#                 'Immunisations and Screening', 'Life Expectancy & Mortality', 
#                 'Mental health', 'Population', 'Smoking', 'Social Care & Housing'))

topic_list <- sort(c('Behaviours', 'Children and young people - Healthy', 
                     'Children and young people - Included', 'Children and young people - Nurtured', 
                     'Children and young people - Responsible', 'Children and young people - Safe', 
                     'Community safety', 'Crime', 'Deprivation', 'Economy', 
                     'Education', 'Environment', 'Ill Health & Injury', 
                     'Immunisations and Screening', 'Life Expectancy & Mortality', 
                     'Mental health', 'Population', 'Social Care & Housing'))

###############################################.
## Palettes ----
###############################################.   
#Palette for SIMD.
pal_simd_bar <- c('#abd9e9', '#74add1', '#4575b4', '#313695', '#022031')
pal_simd_trend <- c('#abd9e9', '#74add1', '#4575b4', '#313695', '#022031', '#FF0000')

#Palette for map
pal_map <- c('#2c7bb6','#abd9e9', '#ffffbf','#fdae61','#d7191c')

##########.
#######
#Beta version warning/feedback
beta_box <- div(style =  "background-color: #ffffcc; padding: 5px; border: 1px solid #000000; margin: 5px;",
    p(tags$b("Beta version:"), "this tool is under development. The current version 
      is available on the main", tags$a(href="https://scotpho.nhsnss.scot.nhs.uk/scotpho/homeAction.do", 
                                        "ScotPHO website",  class="externallink"), 
      ". We would welcome ", tags$a(href="mailto:ScotPHO@nhs.net", tags$b("any feedback"), 
                                    class="externallink"), " you have on this tool."))

# Identify which geographies have data for each indicator
# indic <- unique(optdata$indicator[!is.na(optdata$measure)])
# indic_geog <- tapply(optdata$code[!is.na(optdata$measure)], optdata$indicator[!is.na(optdata$measure)], unique)

##END
