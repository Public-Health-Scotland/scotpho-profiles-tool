###############################################.
## Summary - common objects ----
###############################################.


#####################.
# Reactive controls
## Remember the selected samples
# creates reactive values to remember user selection of the profile
# so it only changes when the user changes it on purpose
prof_chosen <- reactiveValues(value_profile = "HWB")
observeEvent(input$profile_summary, 
             isolate({ prof_chosen$value_profile <- input$profile_summary})
)

# Reactive controls for profile depending on area selected
output$profile_ui_summary <- renderUI({
  
  profiles <- profile_areatype[input$geotype_summary]
  selectInput("profile_summary", label = NULL, choices = profiles, 
              prof_chosen$value_profile)
  
})



# Reactive controls for heatmap:area name depending on areatype selected
output$geoname_ui_summary <- renderUI({
  
  areas_summary <- if (input$geotype_summary %in% c("Health board", "Council area", 
                                                    "HSC partnership", "Scotland", "Alcohol & drug partnership"))
  {
    sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_summary])
  } else {
    sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_summary
                             & geo_lookup$parent_area == input$loc_iz_summary])
  }
  
  selectizeInput("geoname_summary", label = NULL,  
                 choices = areas_summary, selected = "")
  
})


# geography to compare with / years to compare with depending on what data is available or
output$comp_ui_summary <- renderUI({
  
    if (input$comp_summary == 1) {
      selectInput("geocomp_summary", "Select a comparison area", choices = comparator_list,
                  selectize=TRUE, selected = "Scotland")
    } else if (input$comp_summary == 2) {
      
      years <- min(optdata$year):max(optdata$year) 
      
      selectizeInput("yearcomp_summary", "Baseline year", choices = years)
    }

  
})

#####################.
# Reactive data
summary_data <- reactive({
  #data for the chosen area. Filtering based on user input values.
  summary_chosen_area <- optdata %>%
    subset(areaname == input$geoname_summary &
             areatype == input$geotype_summary &
             !(indicator %in% c("Mid-year population estimate - all ages", "Quit attempts")) &
             (substr(profile_domain1, 1, 3) == input$profile_summary |
                substr(profile_domain2, 1, 3) == input$profile_summary)) %>%
    droplevels()

#Select comparator based on years available for area selected.
if (input$comp_summary == 1){ #if area comparison selected
  summary_chosencomp <- optdata %>%
    subset(areaname == input$geocomp_summary &
             indicator != "Mid-year population estimate - all ages" &
             areatype %in% c("Health board", "Council area", "Scotland") &
             (substr(profile_domain1, 1, 3) == input$profile_summary |
                substr(profile_domain2, 1, 3) == input$profile_summary)) %>%
    select(c(year, indicator, measure)) %>%
    rename(comp_m=measure)
} else if (input$comp_summary == 2) { #if time comparison selected
  summary_chosencomp <- summary_chosen_area %>%
    subset(year == input$yearcomp_summary) %>%
    select(c(indicator, measure)) %>%
    rename(comp_m=measure)
}

#Merging comparator and chosen area
if (input$comp_summary == 1){
  sum_data <- left_join(x = summary_chosen_area, y = summary_chosencomp,
                        by=c("indicator", "year")) %>% droplevels()
} else if (input$comp_summary == 2) {
  sum_data <- left_join(x = summary_chosen_area, y = summary_chosencomp,
                        by=c("indicator")) %>% droplevels()
}

sum_data <- sum_data %>%
  #Creating a palette of colours based on statistical significance
  mutate(color = case_when(lowci <= comp_m & upci >= comp_m
                           & interpret %in% c("H", "L") ~'gray',
                           lowci > comp_m & interpret == "H" ~ 'blue',
                           lowci > comp_m & interpret == "L" ~ 'red',
                           upci < comp_m & interpret == "L" ~ 'blue',
                           upci < comp_m & interpret == "H" ~ 'red',
                           interpret == "O" ~ 'white',
                           TRUE ~ 'white'),
         #identifies correct domain name for title
         domain = as.factor(case_when(
           substr(profile_domain1,1,3)==input$profile_summary ~
             substr(profile_domain1, 5, nchar(as.vector(profile_domain1))),
           TRUE ~ substr(profile_domain2, 5, nchar(as.vector(profile_domain2))))))

final <- sum_data %>%
  mutate(profile = input$profile_summary) %>%
  group_by(indicator) %>% 
  slice(which.max(year))


if(input$comp_summary == 1) { 
  final <- final %>% 
  mutate(comparator = input$geocomp_summary)
} else if(input$comp_summary == 2) {
  final <- final %>% 
    mutate(comparator = input$yearcomp_summary)
}
  

  


final <- final %>%
select(domain, 
       indicator, 
       #areaname, 
       def_period, 
       type_definition,
       #comparator, 
       color,
       comp_m, 
       measure)




})








output$summary_ui_plots <- renderReactable({
  
  reactable(
    summary_data(),
    groupBy = "domain",
    defaultExpanded = T,
    columns = list(
      color = colDef(show = F),
      indicator = colDef(html = T,
                         minWidth = 150,
                         # display measure type under each indicator name
                         cell = function(value, index) {
                           type_definition <- summary_data()$type_definition[index]
                           div(
                             div(style = list(fontWeight = "Medium"), value),
                             div(style = list(fontSize = "1.0rem"), type_definition)
                           )
                         }),
      type_definition = colDef(show = F),
      comp_m = colDef(na = "–", minWidth = 40), 
      # colour formatting of results
      measure = colDef(
        minWidth = 40,
        style = JS("function(rowInfo) {
      const value = rowInfo.values['color']
      let color
      if (value == 'red') {
        color = '#FFA500'
      } else if (value == 'blue') {
        color = '#0000FF'
      }
       else if (value == 'gray') {
        color = '#777'
      }
        else {
        color = '#FFFFFF'
      }
      return { backgroundColor: color, fontWeight: 'bold' }
    }")
      )
    ))
  })


