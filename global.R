# Code to create ScotPHO's Shiny profile platform
# In this script include packages, functions, datasets and anyting that will be 
# used both by UI and server

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
library(webshot) #to download plotly charts
# As well as webshot phantomjs is needed l to download Plotly charts
# https://github.com/rstudio/shinyapps-package-dependencies/pull/180
if (is.null(suppressMessages(webshot:::find_phantom()))) {
  webshot::install_phantomjs()
  }

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

#Function to create plot when no data available for ggplot visuals
plot_nodata_gg <- function() {
  ggplot()+
    xlab("No data available")+
    scale_x_discrete(position = "top")+
    theme(panel.background = element_blank(),
          axis.title.x=element_text(size=20, colour ='#555555'))
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
  'Alcohol' = c("Alcohol & drug partnership", "Council area", "Health board", "Scotland"),
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
#Cookie warning
cookie_box <- div(class="alert alert-info", style = "margin-bottom: 0",
      "This website places cookies on your device to help us improve our service 
      to you. To find out more, see our ",
      tags$a(href='https://www.scotpho.org.uk/about-us/scotpho-website-policies-and-statements/privacy-and-cookies',
  " Privacy and Cookies"), "statement.",
      HTML('<a href="#" class="close" data-dismiss="alert" aria-label="close">&check;</a>'))

#automating dates
new_date <- fast_strptime(paste("01",techdoc$last_updated,sep="-"),"%d-%b-%Y")
#time_between <- as.period(new_date %--% today(), unit="days")
techdoc <- mutate(techdoc,days_since_update=day(as.period(new_date %--% today(), unit="days")))

# Identify which geographies have data for each indicator
# indic <- unique(optdata$indicator[!is.na(optdata$measure)])
# indic_geog <- tapply(optdata$areatype[!is.na(optdata$measure)], optdata$indicator[!is.na(optdata$measure)], unique)

##END
