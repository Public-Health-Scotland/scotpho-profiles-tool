#Code for technical document tab

#################################################.
##  Technical Doc Page ----
#################################################.
#### Techdoc for summary of indicators.
## Reactive filter of available geography types based on which profile is selected.
output$tecdoc_geographies <- renderUI ({
  
  if (input$profile_picked != "Show all"){
    geo_selection <- sort(unique(c(as.character(optdata$areatype[grep(input$profile_picked,optdata$profile_domain1)]),
                                   as.character(optdata$areatype[grep(input$profile_picked,optdata$profile_domain2)] ))))
  } else {geo_selection <- areatype_list }
  
  
  div(title="Filter table selecting only indicators available for a specific geography type", 
      selectizeInput("techdoc_geotype", 
                     label = "Step 3. Select a geography type to see indicators available at that level (optional)",
                     width = "100%", choices = c("Show all", geo_selection), 
                     selected = "Show all", multiple=TRUE, 
                     options = list(placeholder = "Select....", maxItems = 1))) 
}) 

output$profile_picked_ui <- renderUI({
  
  if (input$techdoc_selection == "List of available indicators") {
    label_filter <- "Step 2. Select a profile to see indicators included on it (optional)"
    div_title <- "Filter table selecting only indicators available for a specific profile"
  } else if (input$techdoc_selection == "Detailed information about single indicator") {
    label_filter <- "Step 3a. Filter indicator list selecting a single profile (optional)"
    div_title <- "Filter indicator list from step 2 selecting only indicators from a specific profile"
  }
  
  div(title= div_title, 
      selectizeInput("profile_picked", label = label_filter,
                     width = "100%",choices = profile_list_filter, 
                     selected = "Show all", multiple=FALSE))
})

###############################################.
# Reactive dataset filtered for flextable - four possible combinations of data
techdoc_indicator_data <- reactive({  
  if (input$profile_picked != "Show all"){ # if a single profile selected
    if(input$techdoc_geotype != "Show all"){ #further filter if user selects a geography type
      techdoc %>%
        subset(grepl(input$techdoc_geotype,available_geographies)) %>%
        subset(grepl(names(profile_list[unname(profile_list) == input$profile_picked]),profile))} 
    else { # dataset if user wants a single profile but all geography types 
      techdoc %>%
        subset(grepl(names(profile_list[unname(profile_list) == input$profile_picked]),profile))}}
  else if (input$profile_picked == "Show all"){ #subset applied if user selects all profiles
    if(input$techdoc_geotype == "Show all"){  # user selects all geography types
      techdoc}
    else { # user selects a single geography type
      techdoc %>%
        subset(grepl(input$techdoc_geotype,available_geographies))}}
})

## Function to manipulate filtered data - easier for data download if manipulations done after filters
formatted_techdoc_data <- function(){
  if (input$profile_picked != "Show all"){
    techdoc_indicator_data() %>%
      mutate(prof_start=regexpr((names(profile_list[unname(profile_list) == input$profile_picked])), domain), #find start position of profile name in domain column
             prof_name_text=substr(domain,prof_start, nchar(domain)),  #generate column that starts with filtered profile
             findcomma=regexpr(",",prof_name_text), #find position of comma (where domain description ends
             findhyp=regexpr("-",prof_name_text), #find position of hyphen (where domain description starts)
             domain1= case_when(findcomma<0 ~ substr(prof_name_text,findhyp+1,nchar(prof_name_text)),
                                findcomma>0 ~ substr(prof_name_text,findhyp+1,findcomma-1),
                                TRUE ~ "")) %>% # extract domain string linked to seletec profile
      mutate (profilename=input$profile_picked) %>%  #sort on profile name since some indicators in multiple profiles
      arrange(profilename, domain1, indicator_name) %>%
      rownames_to_column(var="ind_index")} 
  else{
    techdoc_indicator_data()}}

## Function to construct flextable displaying techdoc info
plot_techdoc <- function(){
  
  if (input$profile_picked != "Show all"){ # table for a single profile selection
    formatted_techdoc_data() %>%
      select(domain1, ind_index,indicator_name, indicator_definition,available_geographies,aggregation) %>%
      flextable() %>%
      add_header_lines(paste0((names(profile_list[unname(profile_list) == input$profile_picked]))," profile")) %>%
      set_header_labels (domain1="Domain",ind_index= "",indicator_name="Indicator",indicator_definition="Indicator Definition",
                         available_geographies="Available geographies", aggregation="Level of aggregation") %>%
      theme_box() %>%
      merge_v(j = ~ domain1) %>%
      align_text_col(align = "left") %>%
      color(i = 1, color = "white", part = "header") %>% # format text colour of header to identify profile 
      bg(i=1,bg="#007ba7",part="header") %>%  # format background colour of header to identify profile
      fontsize(size = 14, part = "all") %>% 
      autofit() %>%
      htmltools_value()
  } else { #table all indicators (ie "show all") profiles selected - additional column for profile(s)
    formatted_techdoc_data() %>%
      arrange(profile, domain) %>%
      rownames_to_column(var="ind_index") %>%
      select (profile, domain,ind_index,indicator_name, indicator_definition,available_geographies, aggregation) %>%
      flextable() %>%
      set_header_labels (profile="Profile(s)",domain="Domain(s)",ind_index="",indicator_name="Indicator",indicator_definition="Indicator Definition",
                         available_geographies="Available geographies", aggregation="Level of aggregation") %>%
      theme_box() %>%
      merge_v(j = ~ profile) %>%
      merge_v(j = ~ domain) %>%
      align_text_col(align = "left") %>%
      fontsize(size = 14, part = "all") %>% 
      autofit() %>%
      htmltools_value()}
}

## RenderUI for which version flextable to display on techdoc page
#render techincal info depending on whether selected to see summary of 
# available indictors or single indicator definition
output$techdoc_display <- renderUI({  
  # Preparing a brief explanation for each visualisation 
  if (input$techdoc_selection == "List of available indicators") {
    plot_techdoc()
  } else if (input$techdoc_selection == "Detailed information about single indicator")
    p("loading..")}  #shows 'loading' as there can be a small delay while switching back between views
)

## Function to format data for csv according to whether showing single profile or all profiles
techdoc_csv <- function() {
  if (input$profile_picked != "Show all"){
    formatted_techdoc_data() %>%
      rename(profile_selection=profilename, all_profiles=profile, domain_selection=domain1) %>%  
      select(c(profile_selection, domain_selection,ind_index,indicator_name, indicator_number, indicator_definition, all_profiles, domain,inclusion_rationale, data_source,
               diagnostic_code_position, numerator, denominator, measure, disclosure_control, rounding, age_group, sex, year_type,
               trends_from, aggregation, update_frequency, available_geographies, confidence_interval_method, notes_caveats, 
               related_publications, supporting_information, last_updated, next_update))}
  else { #table all indicators (ie "show all") profiles selected - additional column for profile(s)
    formatted_techdoc_data() %>%
      arrange(indicator_name) %>%
      select(c(indicator_name, indicator_number, indicator_definition,profile, domain, inclusion_rationale, data_source,
               diagnostic_code_position, numerator, denominator, measure, disclosure_control, rounding, age_group, sex, year_type,
               trends_from, aggregation, update_frequency, available_geographies, confidence_interval_method, notes_caveats, 
               related_publications, supporting_information, last_updated, next_update))}
}

## Download techdoc data from conditional panel (flextable)
output$download_techdoc1_csv <- downloadHandler(
  filename ="indicator_definitions.csv",
  content = function(file) {
    write.csv(techdoc_csv(),
              file, row.names=FALSE) } 
)

#################################################.  
#### Techdoc for individual indicator.

## Reactive filters for technical details document - filter for indicator dependent on profile/topic selected
output$indicator_choices <- renderUI ({
  
  if (input$profile_picked != "Show all"){
    indic_selection <- sort(unique(c(as.character(optdata$indicator[grep(input$profile_picked,optdata$profile_domain1)]),
                                     as.character(optdata$indicator[grep(input$profile_picked,optdata$profile_domain2)] ))))
  } else if (input$topic_defined != "Show all"){
    indic_selection <- sort(unique(
      optdata$indicator[substr(optdata$profile_domain1, 5, nchar(as.vector(optdata$profile_domain1)))
                        == input$topic_defined |
                          substr(optdata$profile_domain2, 5, nchar(as.vector(optdata$profile_domain2)))
                        == input$topic_defined]))
  } else {indic_selection <- indicator_list}
  
  selectizeInput("indicator_selection", 
                 label = shiny::HTML("<p>Step 2. Select an indicator for detailed technical information <br/> <span style='font-weight: 400'>(hit backspace and start typing to search for an indicator)</span></p>"),
                 width = "510px", choices = indic_selection, 
                 selected = character(0), multiple=TRUE, 
                 options = list(placeholder = "Make a selection to see information", maxItems = 1)) 
}) 

#To keep it simple, when you change profile, reset topic and vice versa.
observeEvent(input$profile_picked, { 
  if (input$topic_defined != "Show all" && input$profile_picked != "Show all"){ 
    updateSelectizeInput(session,"topic_defined", label = "Or by domain",
                         choices = topic_list_filter, selected = "Show all")}
})

observeEvent(input$topic_defined, { 
  if (input$profile_picked != "Show all" && input$topic_defined != "Show all"){ 
    updateSelectizeInput(session,"profile_picked", label = "Filter by profile",
                         choices = profile_list_filter, selected = "Show all")}
})

###############################################.
# Creating text and titles for info to display

#reactive selection for single indicator
indicator_selected <- reactive({ 
  filter(techdoc, techdoc$indicator_name==input$indicator_selection)
})

#Text for title of indicator selected
output$indicator <- renderValueBox({
  valueBox(p(indicator_selected()$indicator_name, style="color: white; font-size: 30px; font-weight: bold;"), 
           HTML(paste("<b>","Profile:","</b>",indicator_selected()$profile,br(),
                      "<b>","Domain:","</b>",indicator_selected()$domain)), icon = icon ("book"),color = "blue")
})

# Text for all metadata parts of the indicator
output$definition <- renderText ({indicator_selected()$indicator_definition })
output$rationale <- renderText ({ indicator_selected()$inclusion_rationale})
output$source <- renderText ({indicator_selected()$data_source})
output$numerator <- renderText ({indicator_selected()$numerator})
output$diagnosis <- renderText ({indicator_selected()$diagnostic_code_position})
output$numerator <- renderText ({indicator_selected()$numerator})
output$measure <- renderText ({indicator_selected()$measure})
output$rounding <- renderText ({indicator_selected()$rounding})
output$year <- renderText ({indicator_selected()$year_type})
output$geos <- renderText ({indicator_selected()$available_geographies})
output$trends_from <- renderText ({indicator_selected()$trends_from})
output$notes <- renderText ({indicator_selected()$notes_caveats})
output$last_updated <- renderText ({indicator_selected()$last_updated})
output$denominator <- renderText ({indicator_selected()$denominator})
output$disclosure <- renderText ({indicator_selected()$disclosure_control})
output$age <- renderText ({indicator_selected()$age_group})
output$sex <- renderText ({indicator_selected()$sex})  #change this when combined
output$aggregation <- renderText ({indicator_selected()$aggregation})
output$update_frequency <- renderText ({indicator_selected()$update_frequency})
output$confidence_interval <- renderText ({indicator_selected()$confidence_interval_method})
output$related_pubs <- renderText ({indicator_selected()$related_publications})
output$supporting_info <- renderText ({indicator_selected()$supporting_information})
output$next_update <- renderText ({indicator_selected()$next_update})


## Download techdoc data from 2nd conditional panel (detailed indicator)
#Download definitions table for selected indicator - not

indicator_csv <- reactive({ format_definitions_csv(indicator_selected()) })

allindicator_csv <- reactive({ format_definitions_csv(techdoc) })

output$download_detailtechdoc_csv <- downloadHandler(
  filename ="indicator_definitions.csv",
  content = function(file) {
    write.csv(indicator_csv(),
              file, row.names=FALSE) } 
)

output$download_alltechdoc_csv <- downloadHandler(
  filename ="indicator_definitions.csv",
  content = function(file) {
    write.csv(allindicator_csv(),
              file, row.names=FALSE) } 
)