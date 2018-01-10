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
#library(rmapshaper) #for reducing size of shapefiles -not needed for app,just for modifying shp
library(data.table) #for quick reading of csv files - Does not work with the actual version
#library(gridExtra) #for combining graph and table (spine chart)

#library(shinydashboard) #testing it: layout for shiny
#library(metricsgraphics) #interactive graphics -testing
#library(highcharter) #more interactive graphs -testing, problem with license
#library(rbokeh) #more interactive graphs -testing
#library(taucharts) #more interactive graphs -testing Cannot use due to R version
#library(plotly) #interactive graphs

###############################################.
## Data ----
###############################################.    
optdata<- read.csv("./data/all_OPTdata.csv", na.strings=c(""," ","NA")) %>%
  subset(year>2013 & areatype != "Intermediate Zone") %>%  # reducing size dataset for testing purposes
  mutate_at(.funs=funs(round(.,2)), .cols=vars(numerator, measure, lowci, upci)) 


#Scaling measures (0 to 1) in groups by year, area type and indicator. 
#Does not work well for Scotland totals.
optdata <- optdata %>% group_by(indicator, year, areatype) %>% 
  mutate(measure_sc = ifelse(interpret=="Highgood", as.vector(rescale(measure, to=c(1,0))), 
                             as.vector(rescale(measure, to=c(0,1)))))

#Creating variables for topic/profile filters
optdata$topic1 <- "All"
optdata$topic2 <- as.character(optdata$topic2)
optdata$topic3 <- as.character(optdata$topic3)

#Geographies names
hb_name <- unique(optdata$areaname[optdata$areatype=="Health Board"])
la_name <- unique(optdata$areaname[optdata$areatype=="Local Authority"])
intzone01 <- unique(optdata$areaname[optdata$areatype=="Intermediate Zone"])

##########.
#Map data
CA_bound<-readOGR("./shapefiles","CA_simpl") #Reading file with council shapefiles
HB_bound<-readOGR("./shapefiles","HB_simpl") #Reading file with health board shapefiles simplified


# Identify which geographies have data for each indicator
indic <- unique(optdata$indicator[!is.na(optdata$measure)])
indic_geog <- tapply(optdata$areaname[!is.na(optdata$measure)], optdata$indicator[!is.na(optdata$measure)], unique)
indic_geog_type <- tapply(optdata$areatype[!is.na(optdata$measure)], optdata$indicator[!is.na(optdata$measure)], unique)


##END
