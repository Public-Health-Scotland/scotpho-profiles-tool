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
optdata <- readRDS("./data/optdata_test.rds") 
deprivation <- readRDS("./data/deprivation_OPT.rds")

geo_lookup <- readRDS("./data/geo_lookup.rds") #geography lookup

#Geographies names
hb_name <- unique(geo_lookup$areaname[geo_lookup$areatype=="Health board"]) %>% droplevels()
la_name <- unique(geo_lookup$areaname[geo_lookup$areatype=="Council area"]) %>% droplevels()
intzone_name <- unique(geo_lookup$areaname[geo_lookup$areatype=="Intermediate zone"]) %>% droplevels()
partnership_name <- unique(geo_lookup$areaname[geo_lookup$areatype=="HSC Partnership"]) %>% droplevels()
locality_name <- unique(geo_lookup$areaname[geo_lookup$areatype=="HSC Locality"]) %>% droplevels()

#Lists of names used in dropdowns
indicator_list <- unique(optdata$indicator)
ind_depr_list <- unique(deprivation$indicator)

area_list <- unique(geo_lookup$areaname)
topic_list <- c(unique(as.character(optdata$topic1)), unique(as.character(optdata$topic2)))
areatype_list <- c("Scotland", "Health board", "Council area", "HSC Partnership", 
                   "HSC Locality", "Intermediate zone")
areatype_noscot_list <- c("Health board", "Council area", "HSC Partnership", 
                         "HSC Locality", "Intermediate zone")

areatype_depr_list <- c("Scotland", "Health board", "Council area") #for deprivation tab


#Palette for time trend (still experimenting).
trend_pal <-  c('#2166ac','#4393c3', '#92c5de', '#d1e5f0', '#053061',
                '#8c510a', '#bf812d', '#dfc27d', '#f6e8c3', '#543005')

trend_pal2 <-  c('#a6cee3','#1f78b4', '#b2df8a', '#33a02c')

#Palette for SIMD.
pal_simd_bar <- c( '#e0f3f8', '#abd9e9', '#74add1', '#4575b4', '#313695')
pal_simd_trend <- c( '#e0f3f8', '#abd9e9', '#74add1', '#4575b4', '#313695', '#FF0000')


pal_map <- c('#2c7bb6','#abd9e9', '#ffffbf','#fdae61','#d7191c')
##########.
#Map data
CA_bound<-readRDS("./shapefiles/CA_boundary.rds") #Reading file with council shapefiles
HB_bound<-readRDS("./shapefiles/HB_boundary.rds") #Reading file with health board shapefiles simplified

# Identify which geographies have data for each indicator
indic <- unique(optdata$indicator[!is.na(optdata$measure)])
indic_geog <- tapply(optdata$code[!is.na(optdata$measure)], optdata$indicator[!is.na(optdata$measure)], unique)
#It seems I was not using it
#indic_geog_type <- tapply(optdata$areatype[!is.na(optdata$measure)], optdata$indicator[!is.na(optdata$measure)], unique)


##END
