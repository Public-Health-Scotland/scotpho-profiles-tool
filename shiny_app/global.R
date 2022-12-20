# Code to create ScotPHO's Shiny profile platform
# In this script include packages, functions, datasets and anyting that will be 
# used both by UI and server

############################.
##Packages ----
############################.
library(cicerone) # for guided tours
library(shiny)
library(shinyBS) #modals
library(shinythemes) # layouts for shiny
library(dplyr) # data manipulation
library(ggplot2) #data visualization
library(leaflet) #javascript maps
library(plotly) #interactive graphs
library(shinyWidgets) # for extra widgets
library(tibble) # rownames to column in techdoc
library(shinyjs)
library(shinydashboard) #for valuebox on techdoc tab
library(sp)
library(lubridate) #for automated list of dates in welcome modal
library(shinycssloaders) #for loading icons, see line below
# it uses github version devtools::install_github("andrewsali/shinycssloaders")
# This is to avoid issues with loading symbols behind charts and perhaps with bouncing of app
library(rmarkdown)
library(webshot) #to download plotly charts
library(rintrojs) # for help intros
library(tidyr) # for pivoting
library(stringr)
library(purrr)
library(reactable)
library(htmltools)

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
savechart_button <- function(outputId, label = "Save chart", class=NULL, disabled=FALSE){

  if (disabled == TRUE){

    # Message to display when disabled button is clicked
    disabled_msg = list(p("A software update has disabled the save chart functionality. We are working on a replacement."),
                        p("In the meantime, you can:"),
                        tags$ul(
                          tags$li("Download the data with the Download data button and build new charts in tools like Excel"),
                          tags$li("Take a screenshot of the chart area using ",
                                          tags$a(href="https://support.microsoft.com/en-us/windows/open-snipping-tool-and-take-a-screenshot-a35ac9ff-4a58-24c9-3253-f12bac9f9d44",
                                          "Snipping Tool"),
                                          " or ",
                                          tags$a(href="https://blogs.windows.com/windowsexperience/2019/04/08/windows-10-tip-snip-sketch/",
                                                 "Snip and Sketch."),
                                          "At least one of these tools is usually installed on recent versions of Windows."
                          )))

    # Create button without link
    disabled_button = tags$p(id = outputId, class = paste("btn btn-default shiny-download-link", class, "down_disabled"),
                             icon("image"), label)

    # Define popup message box
    disabled_popup = bsModal(paste0(outputId, "-disabled-modal"), "Save Chart Disabled", outputId, disabled_msg, size="small")

    # need to explicitly return both ui elements otherwise only the last will be returned
    return(tagList(disabled_button, disabled_popup))


  } else {
    tags$a(id = outputId, class = paste("btn btn-default shiny-download-link", class),
           href = "", target = "_blank", download = NA, icon("image"), label)
 }


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
ind_depr_list <- sort(unique(depr_data$indicator)) #list of indicators


# indicators that contain gap years
gap_indicators_ids = c(21005,20901)
indicators_with_gap_years = optdata %>% filter(ind_id %in% gap_indicators_ids)%>% select(indicator) %>% unique()


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
  'Drugs' = c("Alcohol & drug partnership", "Council area", "Health board", "Scotland"),
  'Mental Health' = c("Council area", "Scotland"),
  "Tobacco control" = c("Council area", "Health board", "Scotland"), 
  "Population" = c("Alcohol & drug partnership", "Council area", "Health board",  
                   "HSC locality", "HSC partnership",  "Intermediate zone", "Scotland") 
)

profile_areatype <- list(
  "Scotland" = setNames(c('HWB','CYP','ALC','DRG','MEN', "TOB", "POP"),
                        c('Health & wellbeing','Children & young people','Alcohol',
                          'Drugs','Mental Health', "Tobacco control", "Population")),
  "Health board" = setNames(c('HWB','CYP','ALC','DRG', 'MEN', "TOB", "POP"),
                            c('Health & wellbeing','Children & young people','Alcohol',
                              'Drugs', 'Mental Health', "Tobacco control", "Population")),
  "Council area" = setNames(c('HWB','CYP','ALC', "DRG",'MEN', "TOB", "POP"),
                            c('Health & wellbeing','Children & young people','Alcohol', "Drugs",
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
cookie_box <- div(class="alert alert-info", style = "margin-bottom: 0; background-color: white; color:black",
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


#formatting techdoc data to be used in indicator definitions tab
ind_dat <- techdoc %>%
  mutate(across(contains("Profile_Domain"),  ~ sub("\\-.*", "", .))) %>%
  rename(Profile_short1 = Profile_Domain1,
         Profile_short2 = Profile_Domain2)%>%
  mutate(profile_short = ifelse(is.na(Profile_short2), Profile_short1, paste0(Profile_short1, ",", Profile_short2)))%>%
  bind_rows(mutate(., profile_short = "Show all")) %>%
  select(-c("active", "interpretation", "team_updating", "indicator_author", "analyst_notes", "days_since_update","source_last_updated", "source_next_update", "scotpho_profiles", "Profile_short1", "Profile_short2")) %>%
  mutate(across(everything(), ~replace_na(.,"N/A"))) %>% 
  mutate(next_update_column = ifelse(next_update == "TBC", NA, paste("01-", next_update, sep = "")))

ind_dat$next_update_column <- format(
  as.Date(ind_dat$next_update_column, "%d-%b-%Y") , "%Y-%m-%d")


# Theme for reactables in tool
table_theme <- function() {
  search_icon <- function(fill = "none") {
    # Icon from https://boxicons.com
    svg <- sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24"><path fill="%s" d="M10 18c1.85 0 3.54-.64 4.9-1.69l4.4 4.4 1.4-1.42-4.39-4.4A8 8 0 102 10a8 8 0 008 8.01zm0-14a6 6 0 11-.01 12.01A6 6 0 0110 4z"/></svg>', fill)
    sprintf("url('data:image/svg+xml;charset=utf-8,%s')", URLencode(svg))
  }
  reactableTheme(
  backgroundColor = 'white',
  borderWidth = '1px',
  borderColor = 'lightgrey',
  headerStyle = list(backgroundColor = "#ececec"),
  searchInputStyle = list(
    borderColor = '#cccccc',
    paddingLeft = "3.5rem",
    width = "100%",
    backgroundSize = "2rem",
    backgroundPosition = "left 1rem center",
    backgroundRepeat = "no-repeat",
    backgroundImage = search_icon("black")))
  
}

# Table of indicators updated in the last 60 days
indicators_updated <- techdoc %>% filter(days_since_update<60) %>% 
  select(indicator_name, last_updated)

#updates modal to appear when click on 'latest updates' button on homepage
updates_modal <- modalDialog(
  reactable(indicators_updated,
  columns = list(
    indicator_name = colDef(show = T, name = "Indicator"),
    last_updated= colDef(show = T, name = "Last updated")
  )),
  size = "l", align= "left",
  easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
)




# define the step-by-step guided tour of the tool for homepage
guide <- Cicerone$
  new(allow_close = FALSE)$
  
  step(
    "explore",
    "Exploring indicators in the tool",
    "A helpful starting point is exploring what types of indicators are included and at what geography level. 
    Click this button to start exploring what is available. When you find an indicator you are interested in, there are further buttons to quickly explore the indicator in various ways. 
    There are more guided-tours on each tab to help you find what you are looking for."
  ) $
  step("tab-buttons",
       "View indicators in multiple ways",
       "Alternatively, you can navigate directly to tabs using the boxes below to start viewing data. Use the buttons highlighted (or the navigation bar at the top) to go to different tabs, depending on how you want to view the data.")$
  step("ind-updates-section",
       "Check when indicators are updated",
       "Use the buttons higlighted to see when an indicator is due to be updated and what has been updated recently"
       )$
  step("whats-new-section", 
       "New indicators",
       "We are continously reviewing and adding to our suite of indicators. Use the buttons highlighted to see what's been added recently.")$
  step("further-links-section",
       "Further ScotPHO Profiles links",
       "Other useful links relating to the ScotPHO Collaboration and the profiles can be found below")



