#Code to create ScotPHO's Shiny profile platform
#In this script include all the server side functions: plots, reactive objects, etc.

# TODO:
#Data manipulation: 
#   Lookups need to be checked/refined: names areas (consistency & and), indicator measures, etc.
#   Figure out what to do with topic variables
#   Indicator information in lookup as with geography
#----------------.
#General:
#   See how to organize dropdown better, using lists, using conditional dropdowns 
#   Add saving buttons to all plots
#   Mini map for selecting geographies? include appendix with locations? like pdf?
#   Each tab should have a brief intro. Work on the text
#   Selection and order of years/time periods needs to be improved.
#   Fix bookmarking
#   Take out row numbers and non-needed columns from download files
#   Create function for downloading files (instead of repeating code)
#   Do extensive testing of the app
#   Add feedback button/tab
#   Include reporting functionality -Rmarkdown?
#   Create user guide
#   add loading/progress bars
#   Incorporate Google analytics - https://shiny.rstudio.com/articles/google-analytics.html
#   Sanitize error messages - use tpp code
#   I feel we should host in the website and link from here: shapefiles, lookups, etc
#----------------.
#Spine chart:
#   Needs to be re-thought.
#   Fix issues spine chart: too many dots in iz (diff graph for different plots?),
#   Fix spine chart size issue, both in app and pdf download
#   add table to spine chart, figure out behaviour when signicance cannot be calculated
#   Add error bars in/out button in plots/also in/out for other geographies in spine chart
#   How to introduce an order/grouping system for spine chart (once profile is selected, like current domains)
#   Figure out how to include definition, time period etc.
#   What to do do with population indicators? Exclude? or show as numbers, but no plot?
#   Why size fo each indicator row does not work for all geographies?
#   Go for another type of visualization: 
#     * just a dot with a color, include percentile is classed (e.g. %5 lowe)
#     * No box plot, just cloud of points and marking with color
#----------------.
#Heatmap: 
#   Create
#----------------.
#Time trend: 
#   Maximum number of series in plot? 5, 10? (maxOptions = 5) <- for an specific dropdown
#   Work on the arrangement of dropdowns (to limit number of IZs and localities)
#   Adding numerator/rate tick box?
#   Figure out how to make palettes work nicely
#   Y axis title size, make smaller, split in two or reduce size of labels
#----------------.
#Rank chart
#   Make it work with all geo_types.
#   Divide very long labels in two with <br>, try to do it automaticly
#   Y axis title size, make smaller, split in two or reduce size of labels
#   Years dependant on indicator chosen. Maybe not year but time period.
#   Fix tooltip - include areaname
#   Color coding to show significance?
#   Vertical bars instead?
#   Include error bars, maybe tick box
#----------------.
#Table:
#----------------.
#Map:
#   Avoid redrawing of map:leafletProxy
#   Add intermediate zones to map, or is it going to be too big?
#   What about partnerships?
#   How to save map? Move away from Leaflet? Will likely be faster
#   Years dependant on indicator chosen. Maybe not year but time period.
#   For IZ's and localities maybe something like this: https://isresearchnc.shinyapps.io/CMaps/
#----------------.
#Deprivation
#   See how to deal with deprivation, same app?
#----------------.
#Projection
#   Figure out what is best model for data projections
#   Decide if we want to do anything with this.
#   Move from code to areaname
#----------------.
###############################################.

## Define a server for the Shiny app
function(input, output) {
  
#####################################.    
##### Spine chart   ---- 
#####################################.   
#controls for spine chart
  output$geotype_ui_spine <- renderUI({
    selectInput("geotype_spine", "Geography level", 
                choices= areatype_noscot_list)
  })
  
  output$geoname_ui_spine <- renderUI({
    selectInput("geoname_spine", "Area", 
                choices =list("Area" = c(unique(subset(geo_lookup, geo_lookup$areatype == input$geotype_spine, select= c("areaname"))))),
                selectize=TRUE, selected = "")
  })

#Reactive data for spine chart, one for the area selected, one for the boxplot(areatype)
#and one for the comparator chosen.
  spine_areaname <- reactive({
    optdata %>% 
      subset(year == input$year_spine &
               code == as.character(geo_lookup$code[geo_lookup$areaname == input$geoname_spine]) &
               (topic1 %in% input$topic_spine | topic2 %in% input$topic_spine)) 
  }) 
  
#for this two only including data for indicators that the area selected has.
  spine_areatype <- reactive({
    optdata %>% 
      subset(year == input$year_spine & 
               indicator %in% spine_areaname()$indicator &
               code %in% as.character(geo_lookup$code[geo_lookup$areatype == input$geotype_spine]) &
               (topic1 %in% input$topic_spine | topic2 %in% input$topic_spine)) 
  }) 
  
  spine_compar <- reactive({
    optdata %>% 
      subset(year == input$year_spine & 
               indicator %in% spine_areaname()$indicator &
               code == as.character(geo_lookup$code[geo_lookup$areaname == input$geocomp_spine]) &
               (topic1 %in% input$topic_spine | topic2 %in% input$topic_spine)) 
  })
  
  
 #Plotting the spine chart and making a function to allow download (does not work with reactive command)
  spineplot <- function(){
    ggplot() +
      geom_boxplot(data=spine_areatype(), aes(indicator, measure_sc), fill="lightsteelblue", width=0.4) +
      geom_dotplot(data=spine_areatype(), aes(indicator, measure_sc), binaxis='y', stackdir='center'
                   , fill="black", dotsize=0.5) +
      geom_point(data=spine_areaname(), aes(indicator, measure_sc),  
                 size=4, shape=21, color="black", 
                 #colors based on overlap of CI with comparator measure, different when high is best and when is worst
                 fill=ifelse(spine_areaname()$lowci < spine_compar()$measure & spine_areaname()$upci > spine_compar()$measure,'white',
                             ifelse(spine_areaname()$lowci > spine_compar()$measure & spine_areaname()$interpret == "Highgood", 'blue',
                                    ifelse(spine_areaname()$lowci > spine_compar()$measure & spine_areaname()$interpret == "Lowgood", 'red',
                                           ifelse(spine_areaname()$upci < spine_compar()$measure & spine_areaname()$interpret == "Lowgood", 'blue',
                                                  ifelse(spine_areaname()$upci < spine_compar()$measure & spine_areaname()$interpret == "Highgood", 'red', 
                                                         "lightblue"))))))+
      coord_flip() +
      labs(title = paste(input$geoname_spine, "|", input$topic_spine, "|", input$year_spine))+ #title
      scale_x_discrete(labels = function(x) str_wrap(spine_areaname()$indicator, width = 40))+
      theme(axis.title.x=element_blank(), #Taking out x axis title
            axis.text.x=element_blank(), # taking out x axis labels
            axis.ticks.x=element_blank(), # taking out x axis tick marks
            axis.title.y=element_blank(), #Taking out y axis title
            axis.line.y = element_line(colour = "black"), # Creating axis line
            panel.background = element_blank(),#Blanking background
            panel.border = element_rect(fill=NA), #frame of plot
            legend.position="none", #taking out legends
            text = element_text(size=16) # changing font size
      )
  }
  
  #attempt to make dynamic size
  # Calculates number of different indicators and then multiplies by pixels per row
  getVarHeight <- function() {
    nrow(spine_areaname() )*35 
  }
  output$dynamic_spine <- renderPlot(spineplot(), height=getVarHeight)
  

  #Download handler command
  output$down_spineplot <- downloadHandler(
    filename =  "Spine_chart.pdf",
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      ggsave(file, plot = spineplot(), device = "pdf", scale=3, limitsize=FALSE)
  })


###############################################.        
#### Time trend plot ----
###############################################.   
  
  #Time trend data. Filtering based on user input values.
  trend_data <- reactive({ 

        trend <- optdata %>% 
      filter((code %in% as.character(geo_lookup$code[geo_lookup$areaname %in% input$hbname_trend &
                                                        geo_lookup$areatype == "Health board"]) |
                code %in% as.character(geo_lookup$code[geo_lookup$areaname %in% input$laname_trend
                                                        & geo_lookup$areatype == "Council area"]) |
                code %in% as.character(geo_lookup$code[geo_lookup$areaname %in% input$scotname_trend
                                                        & geo_lookup$areatype == "Scotland"])  |
                code %in% as.character(geo_lookup$code[geo_lookup$areaname %in% input$locname_trend
                                                        & geo_lookup$areatype == "HSC Locality"]) |
                code %in% as.character(geo_lookup$code[geo_lookup$areaname %in% input$partname_trend
                                                        & geo_lookup$areatype == "HSC Partnership"])   |
                code %in% as.character(geo_lookup$code[geo_lookup$areaname %in% input$izname_trend
                                                        & geo_lookup$areatype == "Intermediate zone"]) ) & 
               indicator == input$indic_trend) %>% 
      droplevels()
  
  #Cannot be done in the same pipe operation as it does not work with the areaname mutate
  trend <- merge(x=trend, y=geo_lookup, by="code", all.x = TRUE)
      
    trend <- trend[order(trend$year),]
  })
  
  #Plotting 
  output$trend_plot <- renderPlotly({
    #If no data available for that period then plot message saying data is missing
    if (is.data.frame(trend_data()) && nrow(trend_data()) == 0)
    {
      #plotting empty plot just with text
      text_na <- list(x = 5, y = 5, text = "No data available" ,
                      xref = "x", yref = "y",  showarrow = FALSE)
      
      plot_ly() %>%
        layout(annotations = text_na,
               #empty layout
               yaxis = list(showline = FALSE, showticklabels = FALSE, showgrid = FALSE),
               xaxis = list(showline = FALSE, showticklabels = FALSE, showgrid = FALSE)) %>% 
        config( displayModeBar = FALSE) # taking out plotly logo and collaborate button
      
    }
    else {

  plot_ly(data=trend_data(), x=~trend_axis,  y = ~measure, 
          type = 'scatter', mode = 'lines',
          color = ~areaname , colors = trend_pal) %>% 
    #Layout
    layout(annotations = list(), #It needs this because of a buggy behaviour
           margin = list(b = 160), #to avoid labels getting cut out
           yaxis = list(title = ~type_definition, rangemode="tozero", 
                        size = 4, tickfont =list(size=10)), 
           xaxis = list(title = "Time period", tickfont =list(size=10), tickangle = 270),  #axis parameter
           hovermode = 'false') %>%  # to get hover compare mode as default
    config(displaylogo = F, collaborate=F, editable =F) # taking out plotly logo and collaborate button
  
    }
  })

  #Downloading data
  output$download_trend <- downloadHandler(
    filename =  'timetrend_data.csv',
    content = function(file) {
      write.csv(trend_data(), file)
    }
  )

#####################################          
#### Rank plot ----
###############################################.     
  #Controls for rank chart #BREAKS the app, not sure why
#For indicator selection
#   output$indic_ui_rank <- renderUI({
#     selectInput("indic_rank", "Indicator", choices=indicator_list)
#   })
  
  #Dropdown for time period based on indicator selection  
  #Maybe try using rank_bar_data instead of optdata, or %in%
#   output$year_ui_rank <- renderUI({
#     selectInput("year_rank", "Time period", 
#                 choices = unique(optdata$trend_axis[optdata$indicator == input$indic_rank]),
#                 selectize=TRUE)
#   })
  
  
  # Comparator data rank plot. Could be moved inside rank_bar_data
  rank_compar <- reactive({
    filter(optdata, trend_axis == input$year_rank & 
             code == as.character(geo_lookup$code[geo_lookup$areaname == input$geocomp_rank]) &
             indicator == input$indic_rank) %>% 
      mutate(areaname = geo_lookup$areaname[geo_lookup$code == code]) #arename from lookup
    
    })
  
  #Rank plot data
  rank_bar_data <- reactive({
    if (input$geotype_rank %in% c("Scotland", "Health board", "Council area", "HSC Partnership"))
    {
      rank_bar <-optdata %>% 
      filter(code %in% as.character(geo_lookup$code[geo_lookup$areatype == input$geotype_rank]) &  
             trend_axis == input$year_rank &
             indicator == input$indic_rank) 
      
      #Cannot be done in the same pipe operation as it does not work with the areaname mutate
      rank_bar <- rank_bar %>% 
      mutate(areaname = geo_lookup$areaname[geo_lookup$code %in% rank_bar$code]) %>%  #arename from lookup
      mutate(comp_value = rank_compar()$measure) %>% #comparator value and name
      mutate(comp_name = rank_compar()$areaname) %>% 
      arrange(desc(measure)) # for ranking by value
    }
      else { #if locality or IZ it needs to filter based on the parent area and be the right area type.
        rank_bar <-  optdata %>% 
          filter(code %in% as.character(geo_lookup$code[geo_lookup$areatype == input$geotype_rank]) &
                   code %in% as.character(geo_lookup$code[geo_lookup$parent_area == input$loc_iz_rank]) &
                   trend_axis == input$year_rank &
                   indicator == input$indic_rank) 
          
        #Cannot be done in the same pipe operation as it does not work with the areaname mutate
          rank_bar <-rank_bar %>% 
            mutate(areaname = geo_lookup$areaname[geo_lookup$code %in% rank_bar$code]) %>%  #arename from lookup
            mutate(comp_value = rank_compar()$measure) %>% #comparator value and name
            mutate(comp_name = rank_compar()$areaname) %>% 
            arrange(desc(measure)) # for ranking by value
      }
    })

# Create Rank plot
 output$rank_plot <- renderPlotly({
    
    #If no data available for that period then plot message saying data is missing
    if (is.data.frame(rank_bar_data()) && nrow(rank_bar_data()) == 0)
    {
      #plotting empty plot just with text
      text_na <- list(x = 5, y = 5, text = "No data available" ,
                      xref = "x", yref = "y",  showarrow = FALSE)
      
      plot_ly() %>%
        layout(annotations = text_na,
               #empty layout
               yaxis = list(showline = FALSE, showticklabels = FALSE, showgrid = FALSE),
               xaxis = list(showline = FALSE, showticklabels = FALSE, showgrid = FALSE)) %>% 
        config( displayModeBar = FALSE) # taking out plotly logo and collaborate button
      
    }
    else {

    plot_ly(data = rank_bar_data(), x = ~areaname) %>% 
      #adding bar layer
      add_bars(y = ~measure, name=~areaname, 
        marker = list(color = '#08519c'), showlegend = FALSE) %>% 
      #Comparator line
      add_trace(y = ~comp_value, name = ~unique(comp_name), type = 'scatter', mode = 'lines',
                line = list(color = '#FF0000')) %>% #changing line color
      #Layout
      layout(annotations = list(), #It needs this because of a buggy behaviour
             yaxis = list(title = ~type_definition),
             xaxis = list(title = ~def_period, tickangle = 270, tickfont =list(size=10), #axis parameters
                          categoryorder="array", #order of plotting
                          categoryarray = ~measure),
             margin=list(b = 160),
             hovermode = 'false') %>% # to get hover compare mode as default
      config(displaylogo = F, collaborate=F, editable =F) 
    }
  }) 

  #Downloading data
  output$download_rank <- downloadHandler(
    filename =  'rankplot_data.csv',
    content = function(file) {
      write.csv(rank_bar_data(), file) 
    }
  )

#####################################.      
#### Projection  ----
#####################################.      
  #Context sensitive menu creation for prediction
  output$indic_pred1 <- renderUI({
    selectInput("indic_choose1", "Indicator", unique(optdata$indicator), multiple = FALSE)
  })
  output$indic_pred2 <- renderUI({
    selectInput("indic_choose2", "Area/s", indic_geog[[input$indic_choose1]], multiple = TRUE)
  })
  
  # Select data on basis of chosen indicator and geographies
  by_pred_data <- eventReactive(input$do_pred,{
    optdata %>% 
      filter(indicator == input$indic_choose1,
             code %in% input$indic_choose2) 
  })

  
  # Create plot of observed and predicted indicator measures
  output$by_pred_plot <- renderPlot({
    ggplot(by_pred_data(), aes(x = year, y = measure, colour = code, fill = code)) +
      geom_point() +
      geom_smooth(method = "gam", fullrange = TRUE, limits = c(0, NA)) +
      geom_vline(xintercept = max(by_pred_data()$year))+
      scale_x_continuous("Year", limits = c(2000, input$xaxis)) +
      scale_y_continuous(unique(by_pred_data()$indicator)) +
      scale_colour_discrete("Chosen area/s") +
      scale_fill_discrete("Chosen area/s") +
      coord_cartesian(ylim=c(0, max(by_pred_data()$measure)), expand = TRUE)
  })

#####################################.    
#### Map ----
#####################################.      
  
  #Merging shapefile with dynamic selection of data
  #First for Local authority
  CA_pol <- reactive({
    ca_map <- optdata %>% 
      subset(code %in% as.character(geo_lookup$code[geo_lookup$areatype == "Council area"]) &
               trend_axis==input$year_map & 
               indicator==input$indic_map) %>% 
      rename(GSS_COD = code) %>% 
      droplevels() #dropping missing factor levels to allow merging
    
    ca_map <- merge(CA_bound, ca_map, by='GSS_COD')
    }) 

  #Second for Health Board
  
  HB_pol <- reactive({
    hb_map <- optdata %>% 
      subset(code %in% as.character(geo_lookup$code[geo_lookup$areatype == "Health board"]) &
               trend_axis==input$year_map & 
               indicator==input$indic_map) %>% 
      rename(HBCode = code) %>% 
      droplevels() #dropping missing factor levels to allow merging
    
    hb_map <- merge(HB_bound, hb_map, by='HBCode')
  })   
  
  #title
  output$title_map <- renderText(paste(input$indic_map, " - ", unique(CA_pol()$def_period)))
  
  #Plotting map
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      #Maybe not needed
      #setView(-4.6519999, 56.33148888, zoom = 8) %>% # setting initial view point
      #fitBounds(-10, 60, 0, 54)  %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      #Adding polygons with HB
      addPolygons(data=HB_pol(), group="Health board",
                  color = "#444444", weight = 2, smoothFactor = 0.5, 
                  label = (sprintf(
                    "<strong>%s</strong><br/>Total: %g<br/>Measure: %g",
                    HB_pol()$HBName, HB_pol()$numerator, HB_pol()$measure) 
                    %>% lapply(htmltools::HTML)),
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~colorQuantile(c('#2c7bb6','#abd9e9', '#ffffbf','#fdae61','#d7191c'), measure_sc, n=5)(measure_sc),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)
      ) %>% 
      #Adding polygons with CA
      addPolygons(data=CA_pol(), group="Council area",
                  color = "#444444", weight = 1, smoothFactor = 0.5, 
                  label = (sprintf(
                    "<strong>%s</strong><br/>Total: %g<br/>Measure: %g",
                    CA_pol()$NAME, CA_pol()$numerator, CA_pol()$measure) 
                    %>% lapply(htmltools::HTML)),
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~colorQuantile(c('#2c7bb6','#abd9e9', '#ffffbf','#fdae61','#d7191c'), measure_sc, n=5)(measure_sc),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)
      ) %>% 
      #Adding layer control
      addLayersControl( 
        overlayGroups = c("Health board", "Council area"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      hideGroup(c("Health board", "Council area")) %>% 
      #Adding legend
      addLegend("bottomright", pal=colorQuantile(c('#2c7bb6','#abd9e9', '#ffffbf','#fdae61','#d7191c'), CA_pol()$measure_sc, n=5), values = CA_pol()$measure_sc, title = "Percentile")
  })
  
  #####################################.      
  #### Table ----
  #####################################.      
  
  # Table data
  table_data <- reactive({
    
    table <- merge(x=optdata, y=geo_lookup, by="code", all.x = TRUE)
    
    table <-table %>% 
      subset(select=c("code", "areaname", "areatype", "indicator", "year", 
               "numerator", "measure", "lowci","upci", "def_period"))
  })
  
  #Actual table.
  output$table_opt <- DT::renderDataTable({
    DT::datatable(table_data(), style = 'bootstrap', filter = 'top', rownames = FALSE,
                  colnames = c("Code", "Area", "Type", "Indicator", "Year", "Numerator", "Measure", "Lower CI", "Upper CI", 
                               "Definition" )
    )
  })
  
  #Downloading data 
  output$download_table <- downloadHandler(
    filename =  'table_data.csv',
    content = function(file) {
      write.csv(table_data()[input[["table_explorer_rows_all"]], ], file) 
    }
  )

#####################################    
## For allowing bookmarking ----
#####################################.      
  enableBookmarking(store = "url")
}

#########################  END ----








