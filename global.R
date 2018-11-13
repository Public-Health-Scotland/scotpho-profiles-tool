#Code to create ScotPHO's Shiny profile platform
#In this script include packages, functions, datasets and anyting that will be used both by UI and server

# TODO:
#Data preparation: 
#   Indicator and geographical info out of main file and use merge/lookups?
#   Add denominator?
#----------------.
#General:
#   Fix bookmarking - check if server solution in .io ?enableBookmarking
#   Include reporting functionality
#   Create user guide
#   Host in the website and link from here: shapefiles, lookups, etc
#----------------.
#Barcode:
#   Name of the tab
#   Blank chart
#   Legend cut in live tool, why not left aligned or centered?
#   Add compare against time period feature?
#   Space between buttons
#   Labels text bigger than other charts, pretty big in general.
#   Help image bigger than modal window
#   All the ones with “no significance can be calculated” excluded (e.g. Active travel to work), maybe that is not completely desired
#----------------.
#Heatmap:  
#   name of the tab
#   Long labels of indicators are an issue  https://github.com/plotly/plotly.js/issues/296#issuecomment-371780321
#----------------.
#Time trend: 
#   Adding numerator/rate tick box?
#   Improve filtering, dynamic show/hide geolevels depending on indicator
#   Add functionality to be able to add IZ/localities from different partnerships
#   To fix long label issue move to have legend as a different chart? 
#   Or inside same chart try to move legend below chart
#----------------.
#Rank chart
#   Issue with long label names, take them out? put them in the side? in the bars?
#   Vertical bars instead?
#----------------.
#Table:
#   Add deprivation data to table (maybe with switch or just merging everything)
#----------------.
#Map:
#   Avoid redrawing of map using leafletProxy
#   Review what to include in tooltip
#   Show significance against comparator in palette 9? (as in rank?)
#   Explore pop up graphics when clicking an area (mapview package)
#   Add intermediate zones to map - issues with size of shp and with simplification
#   For IZ's maybe something like this: https://isresearchnc.shinyapps.io/CMaps/
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
library(shinyBS) #modals
library(shinythemes) # layouts for shiny
library(dplyr) # data manipulation
library(ggplot2) #data visualization
library (DT) # for data tables
library(leaflet) #javascript maps
library(plotly) #interactive graphs
library(shinyWidgets) # for extra widgets
library(tidyr) #for string maniupulations in ring plot
library(shinyjs)
library(shinydashboard) #for valuebox on techdoc tab
library(sp)
library(lubridate) #for automated list of dates in welcome modal
library(shinycssloaders) #for loading icons

###############################################.
## Functions ----
###############################################.  
# Selects the variables of interest and renames them so csv downloads are 
# more user friendly
format_csv <- function(reactive_dataset) {
  reactive_dataset %>% 
    select(c(indicator, areaname, code, areatype, year, def_period, numerator, measure, 
             lowci, upci, type_definition)) %>% 
    rename(lower_confidence_interval=lowci, upper_confidence_interval=upci, 
           period = def_period, definition = type_definition, area_code=code, area_name=areaname,area_type=areatype)
}

format_definitions_csv <- function(reactive_dataset) {
  reactive_dataset %>% 
    select(c(indicator_name, indicator_number, profile, domain, indicator_definition, inclusion_rationale, data_source,
             diagnostic_code_position, numerator, denominator, measure, disclosure_control, rounding, age_group, sex, year_type,
             trends_from, aggregation, update_frequency, available_geographies, confidence_interval_method, notes_caveats, 
             related_publications, supporting_information, last_updated, next_update
    )) 
}

#Download button for charts, just changing the icon
savechart_button <- function(outputId, label = "Save chart", class=NULL){
  tags$a(id = outputId, class = paste("btn btn-default shiny-download-link", class), 
         href = "", target = "_blank", download = NA, icon("image"), label)
}

#Function to wrap titles, so they show completely when saving plot in ggplot
title_wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

#Function to create plot when no data available
plot_nodata <- function() {
  text_na <- list(x = 5, y = 5, text = "No data available" ,
                  xref = "x", yref = "y",  showarrow = FALSE)
  
  plot_ly() %>%
    layout(annotations = text_na,
           #empty layout
           yaxis = list(showline = FALSE, showticklabels = FALSE, showgrid = FALSE, fixedrange=TRUE),
           xaxis = list(showline = FALSE, showticklabels = FALSE, showgrid = FALSE, fixedrange=TRUE),
           font = list(family = '"Helvetica Neue", Helvetica, Arial, sans-serif')) %>% 
    config( displayModeBar = FALSE) # taking out plotly logo and collaborate button
}

###############################################.
## Data ----
###############################################.    
optdata <- readRDS("./data/optdata.rds") #main dataset
techdoc <- readRDS("./data/techdoc.rds") #technical documents data including definitions
geo_lookup <- readRDS("./data/geo_lookup.rds") #geography lookup
profile_lookup <- readRDS("./data/profile_lookup.rds") #profile lookup

#Map-shapefile data
ca_bound<-readRDS("./data/CA_boundary.rds") #Council area 
hb_bound<-readRDS("./data/HB_boundary.rds") #Health board
hscp_bound <- readRDS("./data/HSCP_boundary.rds") #HSC Partnerships
iz_bound <- readRDS("./data/IZ_boundary.rds") #Intermediate zone

###############################################.
## Names ----
###############################################.   
#Geographies names
area_list <- sort(geo_lookup$areaname)
comparator_list <- sort(geo_lookup$areaname[geo_lookup$areatype %in% 
                                    c("Health board", "Council area", "Scotland")]) 
code_list <- unique(optdata$code)
parent_geo_list <- c("Show all", sort(as.character((unique(optdata$parent_area))[-1])))
parent_iz_list <- geo_lookup %>% filter(areatype=="Intermediate zone") %>% select(areaname,parent_area)
parent_hscl_list <- geo_lookup %>% filter(areatype=="HSC locality") %>% select(areaname,parent_area)
hb_name <- sort(geo_lookup$areaname[geo_lookup$areatype=="Health board"]) 
la_name <- sort(geo_lookup$areaname[geo_lookup$areatype=="Council area"]) 
adp_name <- sort(geo_lookup$areaname[geo_lookup$areatype=="Alcohol & drug partnership"]) 
intzone_name <- sort(geo_lookup$areaname[geo_lookup$areatype=="Intermediate zone"]) 
partnership_name <- sort(geo_lookup$areaname[geo_lookup$areatype=="HSC partnership"]) 
locality_name <- sort(geo_lookup$areaname[geo_lookup$areatype=="HSC locality"]) 
adp_name <- sort(geo_lookup$areaname[geo_lookup$areatype=="Alcohol & drug partnership"])

#year of indicators
min_year <- min(optdata$year)
max_year <- max(optdata$year)

#Area type names
areatype_list <- c("Alcohol & drug partnership", "Council area", "Health board",  
                   "HSC locality", "HSC partnership",  "Intermediate zone", "Scotland")
areatype_noscot_list <- c("Alcohol & drug partnership", "Council area", "Health board",  
                          "HSC locality", "HSC partnership",  "Intermediate zone")

#Indicator names
indicator_list <- sort(unique(optdata$indicator))
indicator_map_list <- sort(unique(optdata$indicator[optdata$interpret != 'O']))

#Profile names
topic_list_filter <- (as.factor(c("Show all",unique(sort(c(as.character(optdata$domain1), 
                                                           as.character(optdata$domain2),
                                                           as.character(optdata$domain3)))))))
topic_list <- topic_list_filter[-1]

profile_list <- setNames(c('HWB','CYP','ALC','DRG','MEN', "TOB", "POP"),
                         c('Health & wellbeing','Children & young people','Alcohol',
                           'Drugs','Mental Health', "Tobacco control", "Population"))

profile_list_filter <-c(setNames("Show all", "Show all"), sort(profile_list))

#Geography types available for each indicator
areatype_profile <- list(
  'Health & wellbeing' = c("Council area", "Health board", "HSC locality", 
                           "HSC partnership",  "Intermediate zone", "Scotland"),
  'Children & young people' = c("Council area", "Health board", "HSC locality", 
                                "HSC partnership",  "Intermediate zone", "Scotland"),
  'Alcohol' = c("Alcohol & drug partnership", "Health board", "Scotland"),
  'Drugs' = c("Alcohol & drug partnership", "Health board", "Scotland"),
  'Mental Health' = c("Council area", "Scotland"),
  "Tobacco control" = c("Council area", "Health board", "Scotland"), 
  "Population" = c("Alcohol & drug partnership", "Council area", "Health board",  
                   "HSC locality", "HSC partnership",  "Intermediate zone", "Scotland") 
)

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
beta_box <- div(#style =  "background-color: #ffffcc; padding: 5px; border: 1px solid #000000; margin: 5px;",
  class="alert alert-warning", style = "margin-bottom: 0",
  HTML('<a href="#" class="close" data-dismiss="alert" aria-label="close">&times;</a>'),
    p(tags$b("Beta version:"), "welcome to our new profile tool. The old version 
      is available ", tags$a(href="https://scotpho.nhsnss.scot.nhs.uk/scotpho/homeAction.do", 
                                        "here",  class="externallink"), 
      ". We would welcome ", tags$a(href="mailto:ScotPHO@nhs.net", tags$b("any feedback"), 
                                    class="externallink"), " you have on this tool.")) 

#automating dates
new_date <- fast_strptime(paste("01",techdoc$last_updated,sep="-"),"%d-%b-%Y")
#time_between <- as.period(new_date %--% today(), unit="days")
techdoc <- mutate(techdoc,days_since_update=day(as.period(new_date %--% today(), unit="days")))

# Identify which geographies have data for each indicator
# indic <- unique(optdata$indicator[!is.na(optdata$measure)])
# indic_geog <- tapply(optdata$areatype[!is.na(optdata$measure)], optdata$indicator[!is.na(optdata$measure)], unique)

##END
