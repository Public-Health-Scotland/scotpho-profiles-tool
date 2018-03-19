#Code to create ScotPHO's Shiny profile platform
#In this script include packages, datasets and anyting that will be used both by UI and server

#TODO:
#see server syntax

############################.
##Packages ----
############################.
library(shiny)
library(shinythemes) # layouts for shiny
library(tidyverse) # data manipulation, ggplot
library (DT) # for data tables
library(stringr) # dealing with string variables
library(mgcv) #modelling
library(leaflet) #javascript maps
library(reshape2) #for dygraph data modification
library(plotly) #interactive graphs
library(data.table) #for quick reading of csv files - Does not work with the actual version

###############################################.
## Data ----
###############################################.    
optdata <- readRDS("./data/optdata.rds") 
deprivation <- readRDS("./data/deprivation_OPT.rds")

geo_lookup <- readRDS("./data/geo_lookup.rds") #geography lookup

#Map data
ca_bound<-readRDS("./shapefiles/CA_boundary.rds") #Reading file with council shapefiles
hb_bound<-readRDS("./shapefiles/HB_boundary.rds") #Reading file with health board shapefiles simplified

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
beta_box <- div(style =  "background-color: #ffffcc; padding: 5px; border: 1px solid #000000; margin-bottom: 15px;",
    p(tags$b("Beta version:"), "this tool is under development. The current version 
      is available on the main", tags$a(href="https://scotpho.nhsnss.scot.nhs.uk/scotpho/homeAction.do", "ScotPHO website",  target="_blank"), 
      ". We would welcome ", tags$a(href="mailto:ScotPHO@nhs.net", tags$b("any feedback")), " you have on this tool."))

# Identify which geographies have data for each indicator
# indic <- unique(optdata$indicator[!is.na(optdata$measure)])
# indic_geog <- tapply(optdata$code[!is.na(optdata$measure)], optdata$indicator[!is.na(optdata$measure)], unique)

##END
