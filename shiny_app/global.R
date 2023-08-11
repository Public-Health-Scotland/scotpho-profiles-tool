###############################################################################
#
# Global script ---- 
#
###############################################################################

# contains :- 

# 1. required packages
# 2. required datafiles
# 3. lists for dashboard filters
# 4. common chart themes
# 5. extra UI components that are not reactive (cookie box/guided tours/updates modal)
# 6. sourcing functions created for app (see functions folder) 



# 1. required packages ----------------------------------------------------------
library(cicerone) # for guided tours
library(shiny) # shiny functions
library(shinydashboard) # more shiny functions
library(shinyBS) # modals
library(dplyr) # data manipulation
library(ggplot2) # data visualization
library(highcharter) # data visualisation
library(plotly) # interactive graphs
library(BH)
library(htmlwidgets)
library(leaflet) #javascript maps
library(shinyWidgets) # for extra widgets
library(shinyjs) # for using javascript
library(rintrojs) # for introbox in summary tab
library(lubridate) # for automated list of dates in welcome modal
library(shinycssloaders) # for loading icons
library(webshot) # to download plotly charts
library(tidyr) # for pivoting
library(stringr) # for working with strings
library(reactable) # interactive tables
library(htmltools) # for using html tags
library(purrr) # for applying function to lists
library(data.table) # for working with large datasets
library(jsonlite)
library(rmarkdown)
library(knitr)



# As well as webshot, phantomjs is needed to download Plotly charts
# https://github.com/rstudio/shinyapps-package-dependencies/pull/180
if (is.null(suppressMessages(webshot:::find_phantom()))) {
  webshot::install_phantomjs()
}



# 2. required datafiles ------------------------------------------------------------

# main datasets 
optdata <- readRDS("data/optdata.rds") # main dataset with indicator data
depr_data <- readRDS("data/deprivation_data.rds") # deprivation/inequalities dataset
techdoc <- readRDS("data/techdoc.rds") # technical doc including indicator definitions


# lookups 
geo_lookup <- readRDS("data/geo_lookup.rds") # geography lookup
profile_lookup <- readRDS("data/profile_lookup.rds") # profiles lookup


# shapefiles (for map) 
ca_bound<-readRDS("data/CA_boundary.rds") # Council area 
hb_bound<-readRDS("data/HB_boundary.rds") # Health board
hscp_bound <- readRDS("data/HSCP_boundary.rds") # HSC Partnerships
hscloc_bound <- readRDS("data/HSC_locality_boundary.rds") # HSC localities
iz_bound <- readRDS("data/IZ_boundary.rds") # Intermediate zone


#formatting techdoc data to be used in indicator definitions tab
ind_dat <- techdoc %>%
  mutate(across(contains("Profile_Domain"),  ~ sub("\\-.*", "", .))) %>%
  rename(Profile_short1 = Profile_Domain1,
         Profile_short2 = Profile_Domain2)%>%
  mutate(profile_short = ifelse(is.na(Profile_short2), Profile_short1, paste0(Profile_short1, ",", Profile_short2)))%>%
  bind_rows(mutate(., profile_short = "Show all")) %>%
  select(-c("active", "interpretation", "COVID impact", "indicator_author", "analyst_notes", "days_since_update","source_last_updated", "source_next_update", "scotpho_profiles", "Profile_short1", "Profile_short2")) %>%
  mutate(across(profile:profile_short, ~replace_na(.,"N/A"))) %>%
  mutate(next_update_column = ifelse(next_update == "TBC", NA, paste("01-", next_update, sep = "")))


ind_dat$next_update_column <- format(
  as.Date(ind_dat$next_update_column, "%d-%b-%Y") , "%Y-%m-%d")


# indicators updated in the last 60 days
indicators_updated <- techdoc %>% 
  filter(days_since_update<60) %>% 
  select(indicator_name, last_updated)


#3. lists for filter dropdowns ------------------------------------------------------

# geographies -----

# for rank tab
comparator_list <- sort(geo_lookup$areaname[geo_lookup$areatype %in%
                                              c("Health board", "Council area", "Scotland")])

# for data tab
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


# for summary tab
areatype_list <- c("Alcohol & drug partnership", 
                   "Council area", 
                   "Health board",  
                   "HSC locality", 
                   "HSC partnership",  
                   "Intermediate zone")



topic_list_filter <- as.factor(c("Show all",unique(sort(c(
  substr(optdata$profile_domain1, 5, nchar(as.vector(optdata$profile_domain1))), 
  substr(optdata$profile_domain2, 5, nchar(as.vector(optdata$profile_domain2)))))))) 

topic_list <- topic_list_filter[-1] #taking out show all from list

# for inequalities tab
areatype_depr_list <- c("Scotland", 
                        "Health board", 
                        "Council area")

# for data tab
min_year <- min(optdata$year)
max_year <- max(optdata$year)


# indicators names -----

ind_depr_list <- sort(unique(depr_data$indicator))# for inequalities tab
indicator_list <- sort(unique(optdata$indicator)) # for all other tabs


# indicators that contain gap years due to COVID-19
gap_indicators_ids <- c(21005,
                        20901)

indicators_with_gap_years <- optdata %>% 
  filter(ind_id %in% gap_indicators_ids) %>% 
  select(indicator) %>% unique() # come back to this - might be able to remove?


# Hsc deprivation indicators
ind_hsc_list <- c("Preventable emergency hospitalisation for a chronic condition",
                  "Repeat emergency hospitalisation in the same year",
                  "Mortality amenable to health care",                            
                  "All-cause premature mortality",
                  "Mortality amenable to health care")

# scotpho profile names -----
profile_list <- setNames(c('HWB',
                           'CYP',
                           'ALC',
                           'DRG',
                           'MEN', 
                           'TOB', 
                           'POP'),
                         c('Health & wellbeing',
                           'Children & young people',
                           'Alcohol',
                           'Drugs',
                           'Mental Health', 
                           'Tobacco control', 
                           'Population'))

profile_list_filter <-c(setNames("Show all", "Show all"), sort(profile_list))
  
#  measure options for inequalities tab -----
depr_measure_options <- c("Patterns of inequality",
                          "Inequality gap", 
                          "Potential for improvement",
                          "About these options")

# 4. chart themes  ----------------------------------------------------------------

# colour palettes for SIMD.
pal_simd_bar <- c('#abd9e9', 
                  '#74add1', 
                  '#4575b4', 
                  '#313695', 
                  '#022031')


pal_simd_trend <- c('#abd9e9', 
                    '#74add1', 
                    '#4575b4', 
                    '#313695', 
                    '#022031', 
                    '#FF0000')


# colour palette for map
pal_map <- c('#2c7bb6',
             '#abd9e9', 
             '#ffffbf',
             '#fdae61',
             '#d7191c')


# common parameters for plots
xaxis_plots <- list(title = FALSE, 
                    tickfont = list(size=14), 
                    titlefont = list(size=14), 
                    showline = TRUE, 
                    tickangle = 270, 
                    fixedrange=TRUE)


yaxis_plots <- list(title = FALSE, 
                    rangemode="tozero", 
                    fixedrange=TRUE, 
                    size = 4, 
                    tickfont = list(size=14), 
                    titlefont = list(size=14)) 


font_plots <- list(family = '"Helvetica Neue", 
                              Helvetica, 
                              Arial, 
                              sans-serif')



# 5. extra UI components  ----------------------------------------------------------

# define the step-by-step guided tour of the tool for homepage
# guide <- Cicerone$
#   new(allow_close = FALSE)$
# 
#   step(
#     "explore",
#     "Exploring indicators in the tool",
#     "A helpful starting point is exploring what types of indicators are included and at what geography level.
#     Click this button to start exploring what is available. When you find an indicator you are interested in,
#     there are further buttons to quickly explore the indicator in various ways.
#     There are more guided-tours on each tab to help you find what you are looking for."
#   ) $
# 
#   step("tab-buttons",
#        "View indicators in multiple ways",
#        "Alternatively, you can navigate directly to tabs using the boxes below to start viewing data. Use the
#        buttons highlighted (or the navigation bar at the top) to go to different tabs, depending on how you want
#        to view the data.")$
# 
#   step("ind-updates-section",
#        "Check when indicators are updated",
#        "Use the buttons higlighted to see when an indicator is due to be updated and what has been updated recently"
#        )$
# 
#   step("whats-new-section",
#        "New indicators",
#        "We are continously reviewing and adding to our suite of indicators. Use the buttons highlighted to see what's
#        been added recently.")$
# 
#   step("further-links-section",
#        "Further ScotPHO Profiles links",
#        "Other useful links relating to the ScotPHO Collaboration and the profiles can be found below")
# 



# cookie box to appear along the top of dashboard
cookie_box <-
  div(
    class = "alert alert-info",
    style = "margin-bottom: 0; background-color: white; color:black",
    "This website places cookies on your device to help us improve our service
    to you. To find out more, see our ",
    tags$a(href = 'https://www.scotpho.org.uk/about-us/scotpho-website-policies-and-statements/privacy-and-cookies',
           " Privacy and Cookies"),
    "statement.",
    HTML(
      '<a href="#" class="close" data-dismiss="alert" aria-label="close">&check;</a>'
    )
  )


# updates modal to appear when user clicks 'latest updates' button on homepage
updates_modal <- modalDialog(
  reactable(indicators_updated,
            columns = list(
              indicator_name = colDef(show = T, name = "Indicator"),
              last_updated= colDef(show = T, name = "Last updated")
            )),
  size = "l", align= "left",
  easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
)




# 6. sourcing functions created for app (see functions folder) -------------------------------
list.files("functions") %>% 
  map(~ source(paste0("functions/", .)))






##END