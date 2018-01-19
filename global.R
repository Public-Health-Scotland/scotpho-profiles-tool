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
library(dygraphs) #time trend graphs
library(reshape2) #for dygraph data modification
library (rgdal) #for reading shapefiles
library(ggiraph) #more interactive graphs -testing
library(scales) #for scaling variables for spine chart
library(plotly) #interactive graphs
#library(rmapshaper) #for reducing size of shapefiles -not needed for app,just for modifying shp
library(data.table) #for quick reading of csv files - Does not work with the actual version
#library(gridExtra) #for combining graph and table (spine chart)

#library(shinydashboard) #testing it: layout for shiny
#library(metricsgraphics) #interactive graphics -testing
#library(highcharter) #more interactive graphs -testing, problem with license
#library(rbokeh) #more interactive graphs -testing
#library(taucharts) #more interactive graphs -testing Cannot use due to R version


###############################################.
## Data ----
###############################################.    
optdata <- readRDS("./data/optdata_test.rds") 

#Geographies names
hb_name <- unique(optdata$areaname[optdata$areatype=="Health board"]) %>% droplevels()
la_name <- unique(optdata$areaname[optdata$areatype=="Council area"]) %>% droplevels()
intzone_name <- unique(optdata$areaname[optdata$areatype=="Intermediate zone"]) %>% droplevels()
partnership_name <- unique(optdata$areaname[optdata$areatype=="Partnership"]) %>% droplevels()
locality_name <- unique(optdata$areaname[optdata$areatype=="Locality"]) %>% droplevels()


indicator_list <- unique(optdata$indicator)
area_list <- unique(optdata$areaname)
topic_list <- c(unique(optdata$topic1), unique(optdata$topic2))
areatype_list <- c("Scotland", "Health board", "Council area", "Partnership", 
                   "Locality", "Intermediate zone")
areatype_noscot_list <- c("Health board", "Council area", "Partnership", 
                         "Locality", "Intermediate zone")

##########.
#Map data
CA_bound<-readOGR("./shapefiles","CA_simpl") #Reading file with council shapefiles
HB_bound<-readOGR("./shapefiles","HB_simpl") #Reading file with health board shapefiles simplified


# Identify which geographies have data for each indicator
indic <- unique(optdata$indicator[!is.na(optdata$measure)])
indic_geog <- tapply(optdata$areaname[!is.na(optdata$measure)], optdata$indicator[!is.na(optdata$measure)], unique)
indic_geog_type <- tapply(optdata$areatype[!is.na(optdata$measure)], optdata$indicator[!is.na(optdata$measure)], unique)


##END
