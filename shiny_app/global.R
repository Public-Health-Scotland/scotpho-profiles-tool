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
library(tibble) # rownames to column in techdoc
library(shinyBS) #modals
library(shinyjs)
library(shinydashboard) #for valuebox on techdoc tab
library(sp)
library(lubridate) #for automated list of dates in welcome modal
library(shinycssloaders) #for loading icons, see line below
# it uses github version devtools::install_github("andrewsali/shinycssloaders")
# This is to avoid issues with loading symbols behind charts and perhaps with bouncing of app
library(rmarkdown)
library(flextable) #for tech document table
library(webshot) #to download plotly charts
library(rintrojs) # for help intros
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
format_csv <- function(reactive_dataset, extra_vars = NULL ) {
  
  techdoc <- techdoc %>%
    select(indicator_name, data_source)
 
  left_join(reactive_dataset, techdoc, by = c("indicator" = "indicator_name")) %>%
    select_at(c("indicator", "areaname", "code", "areatype", "year", "def_period", "numerator", "measure",
                "lowci", "upci", extra_vars, "type_definition", "data_source")) %>%
    rename(lower_confidence_interval=lowci, upper_confidence_interval=upci,
           period = def_period, definition = type_definition, area_code=code, area_name=areaname,area_type=areatype)
}

format_definitions_csv <- function(reactive_dataset) {
  reactive_dataset %>% 
    select(c(profile, domain,indicator_name, indicator_number, indicator_definition, inclusion_rationale, data_source,
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
plot_nodata <- function(height_plot = 450) {
  text_na <- list(x = 5, y = 5, text = "No data available" , size = 20,
                  xref = "x", yref = "y",  showarrow = FALSE)
  
  plot_ly(height = height_plot) %>%
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


# UI for heatmap and snapshot plots
sum_ui <- function(title, plot_name) {
  tagList(
    h5(title, style="color: black; text-align: center; font-weight: bold;"),
    div(align = "center", withSpinner(plotlyOutput(plot_name, height = "auto")))
  ) }

# Indicator definition boxes for indicator definition tab
ind_def_box <- function(label, text_output) {
  div(class="definitionbox",
      p(paste(label), style="font-weight:bold; font-size: 16px; color: #2FA4E7;"),
      h5(style = "color: black", textOutput(text_output)))
}

#Creating big boxes for main tabs in the landing page (see ui for formatting css)
lp_main_box <- function(title_box, image_name, button_name, description) {
  div(class="landing-page-box",
      div(title_box, class = "landing-page-box-title"),
      div(description, class = "landing-page-box-description"),
      div(class = "landing-page-icon", style= paste0("background-image: url(", image_name, ".png);
          background-size: auto 80%; background-position: center; background-repeat: no-repeat; ")),
      actionButton(button_name, NULL, class="landing-page-button")
      )
}


#Creating small boxes for further information in the landing page (see ui for formatting css)
lp_about_box <- function(title_box, image_name, button_name, description) {

  div(class="landing-page-box-about",
      div(title_box, class = "landing-page-box-title"),
      div(class = "landing-page-about-icon", style= paste0("background-image: url(", image_name, ".png);
          background-size: auto 80%; background-position: center; background-repeat: no-repeat; ")),
          (actionButton(button_name, NULL,
                   class="landing-page-button",
                   icon = icon("arrow-circle-right", "icon-lp"),title=description)))
}

###############################################.
## Data ----
###############################################.    
optdata <- readRDS("data/optdata.rds") #main dataset
depr_data <- readRDS("data/deprivation_data.rds") #deprivation/inequalities dataset
techdoc <- readRDS("data/techdoc.rds") #technical documents data including definitions

geo_lookup <- readRDS("data/geo_lookup.rds") #geography lookup
profile_lookup <- readRDS("data/profile_lookup.rds") #profile lookup

#Map-shapefile data
ca_bound<-readRDS("data/CA_boundary.rds") #Council area 
hb_bound<-readRDS("data/HB_boundary.rds") #Health board
hscp_bound <- readRDS("data/HSCP_boundary.rds") #HSC Partnerships
hscloc_bound <- readRDS("data/HSC_locality_boundary.rds") #HSC localities
iz_bound <- readRDS("data/IZ_boundary.rds") #Intermediate zone

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
areatype_depr_list <- c("Scotland", "Health board", "Council area") #for deprivation tab

#Indicator names
indicator_list <- sort(unique(optdata$indicator))
indicator_map_list <- sort(unique(optdata$indicator[optdata$interpret != 'O']))
indicators_updated <- techdoc %>% filter(days_since_update<60) %>% pull(indicator_name)
ind_depr_list <- sort(unique(depr_data$indicator)) #list of indicators
# Hsc deprivation indicators
ind_hsc_list <- c("Preventable emergency hospitalisation for a chronic condition",
                  "Repeat emergency hospitalisation in the same year",
                  "Mortality amenable to health care",                            
                  "All-cause premature mortality",
                  "Dying in hospital", "Mortality amenable to health care")

#Profile names
topic_list_filter <- as.factor(c("Show all",unique(sort(c(
  substr(optdata$profile_domain1, 5, nchar(as.vector(optdata$profile_domain1))), 
  substr(optdata$profile_domain2, 5, nchar(as.vector(optdata$profile_domain2)))))))) 

depr_measure_types <- c("Trend", "Gap", "Risk") #list of measure types

topic_list <- topic_list_filter[-1] #taking out show all from list

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

profile_areatype <- list(
  "Scotland" = setNames(c('HWB','CYP','ALC','DRG','MEN', "TOB", "POP"),
                        c('Health & wellbeing','Children & young people','Alcohol',
                          'Drugs','Mental Health', "Tobacco control", "Population")),
  "Health board" = setNames(c('HWB','CYP','ALC','DRG', "TOB", "POP"),
                            c('Health & wellbeing','Children & young people','Alcohol',
                              'Drugs', "Tobacco control", "Population")),
  "Council area" = setNames(c('HWB','CYP','ALC','MEN', "TOB", "POP"),
                            c('Health & wellbeing','Children & young people','Alcohol',
                              'Mental Health', "Tobacco control", "Population")),
  "HSC partnership" = setNames(c('HWB','CYP', "POP"),
                               c('Health & wellbeing','Children & young people', "Population")),
  "HSC locality" = setNames(c('HWB','CYP', "POP"),
                            c('Health & wellbeing','Children & young people', "Population")),
  "Intermediate zone" = setNames(c('HWB','CYP', "POP"),
                                 c('Health & wellbeing','Children & young people', "Population")),
  "Alcohol & drug partnership" = setNames(c('ALC','DRG', "POP"),
                                          c('Alcohol','Drugs', "Population"))
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

###############################################.
## Plot parameters ----
###############################################.

#Common parameters for plots
xaxis_plots <- list(title = FALSE, tickfont = list(size=14), titlefont = list(size=14), 
                    showline = TRUE, tickangle = 270, fixedrange=TRUE)

yaxis_plots <- list(title = FALSE, rangemode="tozero", fixedrange=TRUE, size = 4, 
                    tickfont = list(size=14), titlefont = list(size=14)) 

font_plots <- list(family = '"Helvetica Neue", Helvetica, Arial, sans-serif')

# Identify which geographies have data for each indicator
# indic <- unique(optdata$indicator[!is.na(optdata$measure)])
# indic_geog <- tapply(optdata$areatype[!is.na(optdata$measure)], optdata$indicator[!is.na(optdata$measure)], unique)

##END
