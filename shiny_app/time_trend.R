#Code for trend chart tab

###############################################.        
#### Modal  ----
###############################################.  
# Trend help pop-up
observeEvent(input$help_trend, {
  
  showModal(modalDialog(
    title = "How to use this chart",
    p("The trend chart is designed to explore how a single indicator has changed over time for one or more geograpical area."),
    p(column(7,
             img(src="help_trend_chart2.png")),
      column(5,
             p("First select an indicator using the 'step 1' filter."),
             p("Then add one or more geographical area to the chart using the geography filters in 'Step 2'."),
             p("You can add more than one area or area type (e.g. NHS board or council area) to the trend chart."),
             p("There may be some indicators where data is not available for the full time series or at a particular geography level."),
             p("Use the mouse to hover over a data point to see detailed information on its value, time period and area."),
             p("Confidences intervals (95%) can be added or removed from the chart using the options in 'step 3'. These are shown as shaded areas."),
             p("Confidence intervals give an indication of the precision of a rate or percentage. The width of a confidence interval is related to sample size, smaller geographies like intermediate zones often have wider intervals."),
             p("Display controls in 'Step 3' allow you to switch the graph from a measure (e.g. rate or percentage) to actual numbers (e.g numbers of deaths/hospitalisations)."))),
    size = "l", easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
}) 

###############################################.
## Reactive controls ----
###############################################.
#Controls for chart. Dynamic selection of locality and iz.
output$loc_ui_trend <- renderUI({
  selectizeInput("locname_trend", "HSC locality", 
                 choices = c("Select localities" = "", paste(unique(geo_lookup$areaname[
                   geo_lookup$parent_area == input$loc_iz_trend &
                     geo_lookup$areatype == 'HSC locality' ]))),
                 multiple=TRUE, selected = "")
})

output$iz_ui_trend <- renderUI({
  selectizeInput("izname_trend", "Intermediate zone", 
                 choices = c("Select intermediate zones" = "", 
                             paste(unique(geo_lookup$areaname[
                               geo_lookup$parent_area == input$loc_iz_trend &
                                 geo_lookup$areatype == 'Intermediate zone' ]))),
                 multiple=TRUE, selected = "")
})


###############################################.
# disabling controls if no data available for a type of geography and
# changing labels to indicate no data is available
observeEvent(input$indic_trend, {
  
  trend <- optdata %>% subset(indicator == input$indic_trend) %>% 
    droplevels() 
  
  toggleState ("adpname_trend", condition= 
                 "Alcohol & drug partnership" %in%   unique(trend$areatype) )
  if (!("Alcohol & drug partnership" %in% unique(trend$areatype) ) ) {
    updateSelectizeInput(session, "adpname_trend", 
                         label = "Alcohol & drug partnership (not available)")
  } else {
    updateSelectizeInput(session, "adpname_trend", 
                         label = "Alcohol & drug partnership")
  }
  
  toggleState ("partname_trend", condition= 
                 "HSC partnership" %in%  unique(trend$areatype))
  if (!("HSC partnership" %in% unique(trend$areatype) ) ) {
    updateSelectizeInput(session, "partname_trend", 
                         label = "HSC partnership (not available)")
  } else {
    updateSelectizeInput(session, "partname_trend", 
                         label = "HSC partnership")
  }
  
  toggleState ("loc_iz_trend", 
               condition = "Intermediate zone" %in%  unique(trend$areatype) |
                 "HSC locality" %in%  unique(trend$areatype))
  
  toggleState ("locname_trend", condition= 
                 "HSC locality" %in%  unique(trend$areatype))
  if (!("HSC locality" %in% unique(trend$areatype) ) ) {
    updateSelectizeInput(session, "locname_trend", 
                         label = "HSC locality (not available)")
  } else {
    updateSelectizeInput(session, "locname_trend", 
                         label = "HSC locality")
  }
  
  toggleState ("izname_trend", condition= 
                 "Intermediate zone" %in%  unique(trend$areatype)) 
  if (!("Intermediate zone" %in% unique(trend$areatype) ) ) {
    updateSelectizeInput(session, "izname_trend", 
                         label = "Intermediate zone (not available)")
  } else {
    updateSelectizeInput(session, "izname_trend", 
                         label = "Intermediate zone")
  }
  
  toggleState("caname_trend",
              condition = ("Council area" %in%  unique(trend$areatype)))
  if (!("Council area" %in% unique(trend$areatype) ) ) {
    updateSelectizeInput(session, "caname_trend", 
                         label = "Council area (no available)")
  } else {
    updateSelectizeInput(session, "caname_trend", 
                         label = "Council area")
  }
  
  toggleState("hbname_trend",
              condition = ("Health board" %in%  unique(trend$areatype)))
  if (!("Health board" %in% unique(trend$areatype) ) ) {
    updateSelectizeInput(session, "hbname_trend", 
                         label = "Health board (not available)")
  } else {
    updateSelectizeInput(session, "hbname_trend", 
                         label = "Health board")
  }
  
  # Disabling numerator/rate radio buttons if no numerator available
  toggleState("var_plot_trend",
              condition = all(is.na(trend_data()$numerator)) == FALSE)
  
  # if moving to one indicator without numerator, switch to rate
  if (all(is.na(trend_data()$numerator)) == TRUE & input$var_plot_trend == "numerator") {
    updateAwesomeRadio(session, "var_plot_trend", selected = "measure")
  }
  
})

# Disabling and unchecking CI option if numerator selected or no cis available
# Observe with multiple events code from here: https://stackoverflow.com/questions/34731975/how-to-listen-for-more-than-one-event-expression-within-a-shiny-eventreactive-ha
observeEvent({
  input$var_plot_trend 
  input$indic_trend}, {
    # Disabling and unchecking CI option if numerator selected or no cis available
    if (input$var_plot_trend == "numerator" | (all(is.na(trend_data()$upci)) == TRUE)) {
      disable("ci_trend")
      updateAwesomeCheckbox(session, "ci_trend", value = FALSE)
    } else if (input$var_plot_trend == "measure") {
      enable("ci_trend")
    }
    
  })

###############################################.
## Indicator efinitions ----
###############################################.
#Subsetting by domain and profile. Profile is fiddly as vector uses abbreviations 
# so needs to be converted to the names to match techdoc.
defs_data_trend <- reactive({techdoc %>% subset(input$indic_trend == indicator_name)})

output$defs_text_trend <- renderUI({
  
  HTML(paste(sprintf("<b><u>%s</b></u> <br> %s ", defs_data_trend()$indicator_name, 
                     defs_data_trend()$indicator_definition), collapse = "<br><br>"))
})

###############################################.
## Reactive data ----
###############################################.
#Time trend data. Filtering based on user input values.
trend_data <- reactive({ 
  
  trend <- optdata %>% 
    subset((areaname %in% input$hbname_trend & areatype == "Health board" |
              areaname %in% input$caname_trend & areatype == "Council area" |
              input$scotname_trend == TRUE & areatype == "Scotland"  |
              areaname %in% input$adpname_trend  & areatype == "Alcohol & drug partnership" |
              areaname %in% input$locname_trend  & areatype == "HSC locality" |
              areaname %in% input$partname_trend & areatype == "HSC partnership"   |
              areaname %in% input$izname_trend & areatype == "Intermediate zone") & 
             indicator == input$indic_trend) %>% 
    droplevels() %>% 
    mutate(areaname_full = as.factor(areaname_full),
           # adjusting levels of areatype, so Scotland always plotted as black
           areatype = factor(areatype,
                             levels = c("Scotland", "Health board", "Council area",
                                        "Alcohol & drug partnership", "HSC partnership",
                                        "HSC locality", "Intermediate zone"))) %>% 
    arrange(year, areatype, areaname_full) #Needs to be sorted by year for Plotly
})

#####################.
# Creating plot ----
#####################.
# titles 
output$title_trend <- renderText(paste0(input$indic_trend))
output$subtitle_trend <- renderText(paste0(trend_type()))                                     

trend_type <- function () {
  # y axis title
  yaxis_title <- case_when(input$var_plot_trend == "measure" ~ paste0(unique(trend_data()$type_definition)), 
                           input$var_plot_trend == "numerator" ~ "Number")
}


#####################.
#Plot 
plot_trend_chart <- function() {
  #If no data available for that period then plot message saying data is missing
  # Also if numerator is all NA
  if (is.data.frame(trend_data()) && nrow(trend_data()) == 0  |
      (all(is.na(trend_data()$numerator)) == TRUE & input$var_plot_trend == "numerator"))
  {
    plot_nodata()
  } else { #If data is available then plot it
    
    #Creating palette of colors: colorblind proof
    #First obtaining length of each geography type, if more than 6, then 6, 
    # this avoids issues. Extra selections will not be plotted
    trend_length <- ifelse(length(input$scotname_trend)+length(input$hbname_trend)+ length(input$caname_trend)+
                             length(input$adpname_trend)+length(input$partname_trend)+
                             length(input$locname_trend)+length(input$izname_trend) > 12, 12,
                           length(input$scotname_trend)+length(input$hbname_trend)+ length(input$caname_trend)+
                             length(input$adpname_trend)+length(input$partname_trend)+
                             length(input$locname_trend)+length(input$izname_trend))
    
    # First define the palette of colours used, then set a named vector, so each color
    # gets assigned to an area. I think is based on the order in the dataset, which
    # helps because Scotland is always first so always black.
    trend_palette <- c("#000000", "#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99",
                       "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#b15928")
    
    trend_scale <- c(setNames(trend_palette, unique(trend_data()$areaname_full)[1:trend_length]))
    trend_col <- trend_scale[1:trend_length]
    
    #Modifying standard layout
    yaxis_plots[["title"]] <- trend_type()
    
    
    # Same approach for symbols
    symbols_palette <-  c('circle', 'diamond', 'circle', 'diamond', 'circle', 'diamond',
                          'square','triangle-up', 'square','triangle-up', 'square','triangle-up')
    symbols_scale <- c(setNames(symbols_palette, unique(trend_data()$areaname_full)[1:trend_length]))
    symbols_trend <- symbols_scale[1:trend_length]
    
    #Text for tooltip
    tooltip_trend <- c(paste0(trend_data()$areaname, "<br>", trend_data()$trend_axis,
                              "<br>", paste0(unique(trend_data()$type_definition)),": ", trend_data()$measure,
                              "<br>", "Numerator: ", round(trend_data()$numerator, 1)))
    
    #Creating time trend plot
    trend_plot <- plot_ly(data=trend_data(), x=~trend_axis,  y = ~get(input$var_plot_trend),
                          color = ~areaname_full, colors = trend_col, 
                          text=tooltip_trend, hoverinfo="text", height = 600 ) %>% 
      add_trace(type = 'scatter', mode = 'lines+markers', marker = list(size = 8),
                symbol = ~areaname_full, symbols = symbols_trend) %>% 
      #Layout 
      layout(annotations = list(), #It needs this because of a buggy behaviour of Plotly
             margin = list(b = 160, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots, xaxis = xaxis_plots, font = font_plots,
             showlegend = TRUE,
             legend = list(orientation = 'h', x = 0, y = 1.18)) %>%  #legend on top
      config(displayModeBar = FALSE, displaylogo = F) # taking out plotly logo button
    
    #Adding confidence intervals depending on user input
    if (input$ci_trend == TRUE & input$var_plot_trend != "Numerator") {
      trend_plot %>% 
        add_ribbons(data = trend_data(), ymin = ~lowci, ymax = ~upci, showlegend = F,
                    opacity = 0.2) 
      
    } else if (input$ci_trend == FALSE) {
      trend_plot
    }
  }
}
# Creating plot for ui side
output$trend_plot <- renderPlotly({ plot_trend_chart()  }) 

###############################################.
## Downloads ----
###############################################.
#Downloading data
trend_csv <- reactive({ format_csv(trend_data()) })

output$download_trend <- downloadHandler(filename =  'timetrend_data.csv',
                                         content = function(file) {write.csv(trend_csv(), file, row.names=FALSE)})

# Downloading chart  
output$download_trendplot <- downloadHandler(
  filename = 'trend.png',
  content = function(file){
    export(p = plot_trend_chart() %>% 
             layout(title = paste0(input$indic_trend), margin = list(t = 140)), 
           file = file, zoom = 3)
  })

##END
