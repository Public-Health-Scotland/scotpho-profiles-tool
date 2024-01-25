###############################################
#
# Server logic for data tab
#
###############################################


## Selected dataset -------
selectedData <- reactive({
  if(input$dataset_selector == "Main Dataset") {
    optdata
  } else {
    depr_data
  }
})


# update profile choices based on chosen dataset -----
observe({

  available_profile_choices <- switch(input$dataset_selector,
                            "Main Dataset" = profile_list,
                            "Inequalities Dataset" = depr_profile_list)

  updateVirtualSelect(session = session,
                      inputId = "profile_selector",
                      choices = available_profile_choices)
})





# Update indicator filter choices ----
# based on dataset and geography selected
# (and further updating if profile also selected)
observe({

  # return selected geographies
  paths <- sapply(input$geography_selector_checked_paths, `[[`, "path")

  # filter selected dataset by selected geographies
  data <- selectedData() |>
    subset(path %in% paths)

  # create vector of available indicators
  available_indicators <- unique(data$indicator)

  # Store the current selection of indicators (if there were any)
  # i.e. if you've switched dataset but had previously selected some indicators
  current_selected_indicators <- input$indicator_selector


  # Further filter indicators if a profile is selected
  if (!is.null(input$profile_selector) && input$profile_selector != "") {

    profile_filtered_data <- data |>
      filter(if_any(contains("profile_domain"),
                    ~ substr(.x, 1, 3) %in% input$profile_selector))


    available_indicators <- unique(profile_filtered_data$indicator)

  }


  # Update the filter choices
  updateVirtualSelect(session = session,
                      inputId = "indicator_selector",
                      choices = available_indicators)

  # Reapply the previous selection if they are still valid
  valid_selections <- intersect(current_selected_indicators, available_indicators)

  if (!is.null(valid_selections) && length(valid_selections) > 0) {
    updateVirtualSelect(session = session,
                        inputId = "indicator_selector",
                        selected = valid_selections)

  }

})



## reset all filters -----
# when 'clear filters' button is clicked 
observeEvent(input$clear_table_filters, {
  
  # reset the dataset selector to "Main Dataset"
  updateRadioGroupButtons(session, 
                          inputId = "dataset_selector", 
                          selected = "Main Dataset")
  
  # reset the geographies to those available for the main dataset
  jstreeUpdate(session, "geography_selector", optdata_geo_nodes)
  
  
  # reset the time period filter to max year per indicator
  updateRadioGroupButtons(session = session,
                          inputId = "time_period",
                          selected = "Latest available year")
  
  # reset the indicator list to those present in main dataset
  updateVirtualSelect(session = session,
                      inputId = "indicator",
                      selected = NULL,
                      choices = NULL)
  
  # reset the profile filter
  updateVirtualSelect(session = session,
                      inputId = "profile_selector",
                      selected = NULL,
                      choices = profile_list)
  
})




# determine choices for geography filter -----
GeographyNodes <- reactive({
  if(input$dataset_selector == "Main Dataset") {
    optdata_geo_nodes
  } else {
    depr_data_geo_nodes
  }
})



# render the geography filter ----
#using the dynamic choices from step above
output$geography_selector <- renderJstree({
  jstree(
    GeographyNodes(),
    checkboxes = TRUE,
    selectLeavesOnly = TRUE,
    theme = "proton"
  )
})




# observe changes in data selected and update geographies
observeEvent(input$dataset_selector, {

  updated_nodes <- GeographyNodes() # what is available with currently selected dataset

  jstreeUpdate(session, "geography_selector", updated_nodes) # update the choices

})



# data to display/download ----
tableData <- reactive({
  
  # selected dataset
  data <- selectedData() 
  
  # filter by selected geographies
  paths <- sapply(input$geography_selector_checked_paths, `[[`, "path")
  data <- data |> subset(path %in% paths)
  
  
  # filter by time period 
  if(input$time_period_selector == "Latest available year") {
    setDT(data) # switch to data.table format here as quicker than grouping using dplyr
    data <- data[, .SD[year == max(year)], by = indicator]
  } else data
  
  
  # filter by quint type (if inequalities dataset selected)
  if(input$dataset_selector == "Inequalities Dataset") {
    if(input$quint_type_selector == "Scotland") {
      data <- data |> filter(quint_type == "sc_quin")
    } else {
      data <- data |> filter(quint_type != "sc_quin")
    }
  } else data
  
  
  # if profile selected (but indicators have not been)
  # then filter by selected profiles only 
  if(isTruthy(input$profile_selector) & !isTruthy(input$indicator_selector)) {
    data <- data |>
      filter(if_any(contains("profile_domain"),
                    ~ substr(.x, 1, 3) %in% input$profile_selector))
    
    
    # if a profile has been selected (and some indicators too)
    # then filter by profile and indicator
  } else if(isTruthy(input$profile_selector) & isTruthy(input$indicator_selector)) {
    
    data <- data |>
      filter(if_any(contains("profile_domain"),
                    ~ substr(.x, 1, 3) %in% input$profile_selector)) |>
      filter(indicator %in% input$indicator_selector)
    
    
    
    # if no profile has been selected but some indicators have
    # then filter by indicators only 
  } else if(!isTruthy(input$profile_selector) & isTruthy(input$indicator_selector)) {
    
    data <- data |>
      filter(indicator %in% input$indicator_selector)
    
  } else {
    
    # if nothings been selected from profile or indicator filter then return all available indicators
    # for chosen dataset/geography/time period
    data <- data
  }
  
  # rename some columns 
  data <- data |>
    rename(area_code = code, 
           area_type = areatype, 
           area_name = areaname, 
           period = def_period, 
           upper_confidence_interval = upci, 
           lower_confidence_interval = lowci)
  
  # columns to return if main dataset was selected
  if(input$dataset_selector == "Main Dataset") {
    
    data <- data |>
      select(area_code, area_type, area_name, year, period, type_definition,
             indicator, numerator, measure, 
             upper_confidence_interval, lower_confidence_interval) } else {
        
        # columns to return if inequalities dataset was selected
        # note this requires some reshaping due to the format of the inequalities dataset
        
        # all inequalities data
        data <- data |>
          rename(value = measure,
                 measure = type_definition)
        
        # sii data
        sii <- data |>
          filter(quintile == "Total") |>
          mutate(value = sii,
                 upper_confidence_interval = upci_sii,
                 lower_confidence_interval = lowci_sii) |>
          mutate(measure = "Slope index of inequality (SII)")
        
        # rii data
        rii <- data |>
          filter(quintile == "Total") |>
          mutate(value = rii,
                 upper_confidence_interval = upci_rii,
                 lower_confidence_interval = lowci_rii) |>
          mutate(measure = "Relative index of inequality (RII)")
        
        # par data
        par <- data |>
          filter(quintile == "Total") |>
          mutate(value = par,
                 upper_confidence_interval = upci_rii_int,
                 lower_confidence_interval = lowci_rii_int) |>
          mutate(measure = "Population attributable risk (PAR)")
        
        # different inequalities measures combined
        data <- bind_rows(data, rii, sii, par) |>
          select(area_code, area_type, area_name, year, period, indicator, 
                 quintile, measure, value, upper_confidence_interval, 
                 lower_confidence_interval, label_inequality) |>
          arrange(indicator, area_name, year)
      }
 
})


# table of results ---------
output$data_tab_table <- renderDT({
  
  # columns to hide in table
  if(input$dataset_selector == "Main Dataset") {
  cols_to_display = list(list(visible=FALSE, targets=c(0,3, 9,10)))

  } else {
    cols_to_display = list(list(visible=FALSE, targets=c(0,3,9,10,11)))
  }
  

  
  # column names for table
  if(input$dataset_selector == "Main Dataset") {
    col_names = c("hidden", "Type", "Area", "hidden", "Period", "Indicator", 
                  "Measure type", "Numerator", "Measure", "hidden", "hidden")

  } else {
    col_names = c("hidden", "Type", "Area", "hidden", "Period", "Indicator", 
                  "Quintile", "Measure", "Value", "hidden", "hidden", "hidden")
  }


  datatable(tableData(),
            style = 'bootstrap', 
            rownames = FALSE,
            colnames = col_names,
            options = list(scrollX = TRUE,
                           scrollY = "600px", 
                           pageLength = 20,
                           dom = 't',
                           searching = FALSE,
                           language = list(
                            zeroRecords = "Select atleast one geography to display results."),
                           columnDefs = cols_to_display
                           ))
})




# data downloads -------
downloadDataButtonsServer(id = "datatable_downloads", 
                          data = tableData)


## END