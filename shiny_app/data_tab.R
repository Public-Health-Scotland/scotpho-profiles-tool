# Code for data tab
#####################################.      
#### Reactive data ----
#####################################. 
# Reactive data for IZ and locality filters
#Filter iz_list and hscl list by parent area selection
interzone_filtered <- reactive({ 
  sort(parent_iz_list$areaname[parent_iz_list$parent_area==input$iz_parent]) 
})

hsclocality_filtered <- reactive({ 
  sort(parent_hscl_list$areaname[parent_hscl_list$parent_area==input$hscl_parent]) 
})

###############################################.
## Reactive filters ----
###############################################.
#Filter IZ's by Parent area

output$iz_filtered <- renderUI ({ 
  
  if (input$iz_parent == "Show all"){ 
    choices_selected <- intzone_name
  } else { # if a partnership selected reduce the list of IZs shown
    choices_selected <- interzone_filtered()
  }
  
  selectizeInput("iz_true", label = NULL, choices = choices_selected, 
                 selected = NULL, multiple=TRUE, 
                 options = list(maxOptions = 1300, placeholder = "Select or type specific intermediate zone"))
}) 

#Filter HSCL's by parent area
output$hscl_filtered <- renderUI ({ 
  if (input$hscl_parent == "Show all"){ 
    choices_selected <- locality_name
  } else {  # if a partnership selected reduce the list of localities shown
    choices_selected <- hsclocality_filtered()
  }
  selectizeInput("hscl_true", label = NULL, choices = choices_selected, 
                 selected = NULL, multiple=TRUE, options = 
                   list(placeholder = "Select or type specific HSC locality"))
}) 

#########.
#Clearing and modifying inputs through observes
#select all IZ's belonging to a certain parent-area
observe({
  if (input$iz_parent_all == "FALSE")
    return(if (input$iz_parent == "Show all"){ 
      
      updateSelectizeInput(session,"iz_true", label = NULL,
                           choices = intzone_name, selected = character(0), 
                           options = list(maxOptions = 1300, 
                                          placeholder = "Select or type specific intermediate zone")) 
    } else {
      updateSelectizeInput(
        session,"iz_true", label = NULL, choices = interzone_filtered(), 
        selected = character(0), options = 
          list(placeholder = "Select or type specific intermediate zone")) 
    }
    ) #return bracket
  
  isolate({
    updateSelectizeInput(
      session, "iz_true", label = NULL, choices = intzone_name, 
      selected = interzone_filtered(), options = 
        list(placeholder = "Select or type specific intermediate zone")) })
})#end of observe

#when you change initial filter, clear the second list of geographies and checkbox
observeEvent(input$iz_parent, {
  
  updateCheckboxInput(session, "iz_parent_all", label = NULL, value = FALSE)
  
  if (input$iz_parent == "Show all"){ 
    selectizeInput(
      "iz_true", label = NULL, 
      choices = intzone_name, selected = NULL, multiple=TRUE, options = 
        list(maxOptions = 1300, placeholder = "Select or type specific intermediate zone")) 
  } else {
    selectizeInput(
      "iz_true", label = NULL, choices = interzone_filtered(), 
      selected = character(0), multiple=TRUE, options =
        list(placeholder = "Select or type specific intermediate zone"))
  }
})


#select all IZ's belonging to a certain parent-area
observe({
  if (input$hscl_parent_all == "FALSE")
    return(if (input$hscl_parent == "Show all"){ 
      updateSelectizeInput(
        session,"hscl_true", label = NULL,choices = locality_name, 
        selected = character(0), options = 
          list(maxOptions = 1300, placeholder = "Select or type specific HSC locality")) 
    } else {
      updateSelectizeInput(
        session,"hscl_true", label = NULL, choices = hsclocality_filtered(), 
        selected = character(0), options = 
          list(placeholder = "Select or type specific HSC locality"))
    }
    ) #return bracket
  
  isolate({
    updateSelectizeInput(
      session, "hscl_true", label = NULL, choices = locality_name, 
      selected = hsclocality_filtered(), options = 
        list(placeholder = "Select or type specific HSC locality")) 
  })
}) #end of observe

#when you change initial filter, clear the second list of geographies anc checkbox
observeEvent(input$hscl_parent, {
  updateCheckboxInput(session, "hscl_parent_all", label = NULL, value = FALSE)
  
  if (input$hscl_parent == "Show all"){ 
    selectizeInput(
      "hscl_true", label = NULL, choices = locality_name, 
      selected = NULL, multiple=TRUE, options = 
        list(maxOptions = 1300, placeholder = "Select or type specific HSC locality")) 
  } else {
    selectizeInput(
      "hscl_true", label = NULL, choices = hsclocality_filtered(), 
      selected = character(0), multiple=TRUE, options = 
        list(placeholder = "Select or type specific HSC locality"))
  }
})

#to clear choices when boxes are unticked/radio button is changed
observeEvent(input$iz=="FALSE", { #for IZs
  updateCheckboxInput(session, "iz_parent_all", label = NULL, value = FALSE)
  updateSelectizeInput(session, "iz_true", label = NULL,
                       choices = intzone_name, selected = character(0), options = 
                         list(placeholder = "Select or type specific intermediate zone")) 
  updateSelectizeInput(session, "iz_parent", label = "Filter intermediate zone list by HSC partnership", 
                       selected = "Show all")
})

observeEvent(input$la=="FALSE", {#for CAs
  updateSelectizeInput(session, "la_true", label = NULL, choices = la_name, 
                       selected = character(0), options = 
                         list(placeholder = "Select or type specific council area")) 
})

observeEvent(input$hb=="FALSE", { #for HBs
  updateSelectizeInput(session, "hb_true", label = NULL,
                       choices = hb_name, selected = character(0),
                       options = list(placeholder = "Select or type specific health board")) 
})

observeEvent(input$hscl=="FALSE", { #for localities
  updateSelectizeInput(session, "hscl_true", label = NULL,
                       choices = locality_name, selected = character(0), options = 
                         list(placeholder = "Select or type specific HSC locality"))
  
  updateSelectizeInput(session, "hscl_parent", label = "Filter locality list by HSC partnership",
                       selected = "Show all")
})

observeEvent(input$hscp=="FALSE", { #for hsc partnerships
  updateSelectizeInput(session, "hscp_true", label = NULL,
                       choices = partnership_name, selected = character(0), options = 
                         list(placeholder = "Select or type specific HSC partnership"))
})

observeEvent(input$adp=="FALSE", { #for ADPs
  updateSelectizeInput(session, "adp_true", label = NULL,
                       choices = adp_name, selected = character(0), options = 
                         list(placeholder = "Select or type specific ADP")) 
})

observeEvent(input$product_filter, { # for indicator/topic/profile filters
  updateSelectizeInput(session,"indicator_filter", label = NULL,
                       choices = indicator_list, selected = character(0), options = 
                         list(maxOptions = 1000,
                              placeholder = "Click or type indicators to filter by"))
  
  updateSelectizeInput(session,"topic_filter", label = NULL, choices = topic_list, 
                       selected = NULL, options = 
                         list(maxOptions = 1000, 
                              placeholder = "Click or type domains to filter by"))
  
  updateSelectizeInput(session,"profile_filter", label = NULL, choices = profile_list, 
                       selected = NULL, options = 
                         list(maxOptions = 1000, 
                              placeholder = "Click or type profiles to filter by"))
})

#Clearing all user inputs to default
observeEvent(input$clear, {
  updateCheckboxInput(session, "iz", label = NULL, value = FALSE)
  updateSelectizeInput(session, "iz_true", label = NULL,
                       choices = intzone_name, selected = character(0), options = list(maxOptions = 1300, placeholder = "Select specific intermediate zones"))
  updateSelectInput(session, "iz_parent", label = NULL,
                    choices = parent_geo_list, selected = "Show All")
  updateCheckboxInput(session, "la", label = NULL, value = FALSE)
  updateSelectInput(session, "la_true", label = NULL,
                    choices = la_name, selected = character(0))
  updateCheckboxInput(session, "hb", label = NULL, value = FALSE)
  updateSelectInput(session, "hb_true", label = "Type in the box to search",
                    choices = hb_name, selected = character(0))
  updateCheckboxInput(session, "hscl", label = NULL, value = FALSE)
  updateSelectInput(session, "hscl_true", label = "Type in the box to search",
                    choices = locality_name, selected = character(0))
  updateCheckboxInput(session, "hscp", label = NULL, value = FALSE)
  updateSelectInput(session, "hscp_true", label = "Type in the box to search",
                    choices = partnership_name, selected = character(0))
  updateCheckboxInput(session, "scotland", label = NULL, value = FALSE)
  updateCheckboxInput(session, "all_data", label = NULL, value = FALSE)
  updateSelectInput(session, "code", label = NULL,
                    choices = code_list, selected = character(0))
  updateSliderInput(session, "date_from", label = "From", value = c(min_year,max_year),
                    min = min_year, max = max_year, step = 1)
  updateSelectizeInput(session,"indicator_filter", label = NULL,
                       choices = indicator_list, selected = character(0),
                       options = list(maxOptions = 1000, placeholder = "Click or type indicators you would like to filter by"))
  updateSelectizeInput(session,"topic_filter", label = NULL, choices = topic_list, selected = NULL,
                       options = list(maxOptions = 1000, placeholder = "Click or type domains you would like to filter by"))
  updateSelectizeInput(session,"profile_filter", label = NULL, choices = profile_list, selected = NULL,
                       options = list(maxOptions = 1000, placeholder = "Click or type profiles you would like to filter by"))
  updateAwesomeRadio(session,"product_filter", label=NULL, choices = c("Indicator", "Domain", "Profile"), selected = NULL, inline = FALSE,
                     status = "primary", checkbox = TRUE)
  
})

###############################################.
## Reactive data ----
###############################################.
filter_table <- reactive ({
  if (is.null(input$indicator_filter) & is.null(input$topic_filter) & 
      is.null(input$profile_filter)) {
    # if no data selected create empty dataset to avoid app crashing
    table <- data.frame(code = factor(), areaname = factor(), areatype = factor(), 
                        indicator = factor(), year = double(), 
                        def_period = factor(), numerator =double(), measure =double(), 
                        lowci =double(), upci=double(), type_definition = factor())
    
    #if list of indicators selected
  } else {
    if (!is.null(input$indicator_filter)) { #if indicator selected
      if (input$all_data == TRUE) {
        filtered_geos <- optdata %>%  
          filter(year>=input$date_from[1] & year<=input$date_from[2] & 
                   indicator %in% input$indicator_filter)
      } else {
        filtered_geo <- optdata %>% 
          filter((areaname %in% input$iz_true & areatype == "Intermediate zone")|
                   (areaname %in% input$la_true & areatype == "Council area")|
                   (areaname %in% input$hb_true & areatype == "Health board")|
                   (areaname %in% input$adp_true & areatype == "Alcohol & drug partnership")|
                   (areaname %in% input$hscl_true & areatype == "HSC locality")|
                   (areaname %in% input$hscp_true & areatype == "HSC partnership")|
                   (code %in% input$code)) %>% 
          filter(year>=input$date_from[1] & year<=input$date_from[2] & 
                   indicator %in% input$indicator_filter)
        
        filtered_geo2 <- if (input$scotland == TRUE) {
          optdata %>% filter(areaname == "Scotland" &
                               (year>=input$date_from[1] & year<=input$date_from[2]) &
                               indicator %in% input$indicator_filter)
        }      
        
        filtered_geos <- rbind(filtered_geo, filtered_geo2)
        
      }
      
      #if list of domains selected
    } else if (!is.null(input$topic_filter)) { 
      if (input$all_data == TRUE) {
        filtered_geos <- optdata %>%  
          filter(year>=input$date_from[1] & year<=input$date_from[2] &
                   (substr(profile_domain1, 5, nchar(as.vector(profile_domain1))) %in%  input$topic_filter |
                      substr(profile_domain2, 5, nchar(as.vector(profile_domain2))) %in%  input$topic_filter))
        
      } else {
        
        filtered_geo <- optdata %>% 
          filter((areaname %in% input$iz_true & areatype == "Intermediate zone")|
                   (areaname %in% input$la_true & areatype == "Council area")|
                   (areaname %in% input$hb_true & areatype == "Health board")|
                   (areaname %in% input$adp_true & areatype == "Alcohol & drug partnership")|
                   (areaname %in% input$hscl_true & areatype == "HSC locality")|
                   (areaname %in% input$hscp_true & areatype == "HSC partnership")|
                   code %in% input$code) %>% 
          filter(year>=input$date_from[1] & year<=input$date_from[2] & 
                   (substr(profile_domain1, 5, nchar(as.vector(profile_domain1))) == input$topic_filter |
                      substr(profile_domain2, 5, nchar(as.vector(profile_domain2))) == input$topic_filter))
        
        filtered_geo2 <- if (input$scotland == TRUE) {
          optdata %>% 
            filter(areaname == "Scotland" & 
                     year>=input$date_from[1] & year<=input$date_from[2] & 
                     (substr(profile_domain1, 5, nchar(as.vector(profile_domain1))) %in%  input$topic_filter |
                        substr(profile_domain2, 5, nchar(as.vector(profile_domain2))) %in%  input$topic_filter))
        }  
        
        # Merging together Scotland and other areas selected
        filtered_geos <- rbind(filtered_geo, filtered_geo2)
        
      } #end of else statement
      
      #if list of profiles selected    
    } else if (!is.null(input$profile_filter)) { 
      if (input$all_data == TRUE) {
        filtered_geos <- optdata %>%  
          filter(year>=input$date_from[1] & year<=input$date_from[2]) %>% 
          filter((grepl((paste0("^",input$profile_filter,collapse="|")),
                        profile_domain1))|(grepl((paste0("^",input$profile_filter,collapse="|")),
                                                 profile_domain2)))
      } else {
        filtered_geo <- optdata %>% 
          filter((areaname %in% input$iz_true & areatype == "Intermediate zone")|
                   (areaname %in% input$la_true & areatype == "Council area")|
                   (areaname %in% input$hb_true & areatype == "Health board")|
                   (areaname %in% input$adp_true & areatype == "Alcohol & drug partnership")|
                   (areaname %in% input$hscl_true & areatype == "HSC locality")|
                   (areaname %in% input$hscp_true & areatype == "HSC partnership")|
                   (code %in% input$code)) %>% 
          filter(year>=input$date_from[1] & year<=input$date_from[2]) %>% 
          filter((grepl((paste0("^",input$profile_filter,collapse="|")),
                        profile_domain1))|
                   (grepl((paste0("^",input$profile_filter,collapse="|")),
                          profile_domain2)))
        
        filtered_geo2 <- if (input$scotland == TRUE) {
          optdata %>% 
            filter(areaname == "Scotland" &
                     year>=input$date_from[1] & year<=input$date_from[2]) %>% 
            filter((grepl((paste0("^",input$profile_filter,collapse="|")),
                          profile_domain1))|(grepl((paste0("^",input$profile_filter,collapse="|")),
                                                   profile_domain2)))
          
        }
        # Merging together Scotland and other areas selected
        filtered_geos <- rbind(filtered_geo,filtered_geo2)
        
      }
    } else { #ending the profile selection bit
      # if all available geographies checkbox checked
      if (input$all_data == TRUE) {
        filtered_geos <- optdata %>%  
          filter(year>=input$date_from[1] & year<=input$date_from[2])
        
      } else {
        filtered_geo <- optdata %>% 
          filter((areaname %in% input$iz_true & areatype == "Intermediate zone")|
                   (areaname %in% input$la_true & areatype == "Council area")|
                   (areaname %in% input$hb_true & areatype == "Health board")|
                   (areaname %in% input$adp_true & areatype == "Alcohol & drug partnership")|
                   (areaname %in% input$hscl_true & areatype == "HSC locality")|
                   (areaname %in% input$hscp_true & areatype == "HSC partnership")|
                   (code %in% input$code)) %>% 
          filter(year>=input$date_from[1] & year<=input$date_from[2])
        
        filtered_geo2 <- if (input$scotland == TRUE) {
          optdata %>% 
            filter(areaname == "Scotland" &
                     year>=input$date_from[1] & year<=input$date_from[2])
          
        }
        # Merging together Scotland and other areas selected
        filtered_geos <- rbind(filtered_geo,filtered_geo2)
        
      } 
      
    } #end of the else if statement for all available geographies
    
    table <- filtered_geos %>% select(code, areaname, areatype, indicator, year, 
                                      def_period, numerator, measure, lowci, upci, type_definition)
  } #end of the whole if statement (if users have selected any data)
  
})

###############################################.
## Table ----
###############################################.

#display table based on selection made by user on indicator tab
output$table_filtered <- DT::renderDataTable({
  
  DT::datatable(filter_table(),
                style = 'bootstrap', rownames = FALSE, options = list(dom = 'tp', language = list(
                  zeroRecords = "No records found matching your selection - 
                    have you selected a geography and at least one indicator/domain/profile? 
                    See 'Indicator definitions' under the Info tab for Geographies available."), 
                  columnDefs = list(list(visible=FALSE, targets=c(4,8,9)))), 
                colnames = c("Area code", "Area", "Type", "Indicator", "Year","Period", "Numerator", 
                             "Measure", "Lower CI","Upper CI", "Definition")
  )
})

###############################################.
## Downloads ----
###############################################.
# Downloading data in csv format
table_csv <- reactive({ format_csv(filter_table()) })

#The filters the user applies in the data table will determine what data they download - indicator tab table
output$download_table_csv <- downloadHandler(
  filename ="scotpho_data_extract.csv",
  content = function(file) {
    write.csv(table_csv(),
              file, row.names=FALSE) } 
)

##END