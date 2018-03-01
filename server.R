#Code to create ScotPHO's Shiny profile platform
#In this script include all the server side functions: plots, reactive objects, etc.

# TODO:
#Data manipulation: 
#   Lookups need to be checked/refined: names areas (consistency & and), indicator measures, etc.
#   Figure out what to do with topic variables
#   Indicator information in lookup as with geography
#   Add domain to indicator lookup.
#   Include deprivation indicators in lookup
#   Deprivation needs PAR and rounding of numeric variables
#----------------.
#General:
#   Add saving buttons to all plots
#   Fix bookmarking - check if server solution in .io ?enableBookmarking
#   Take out row numbers and non-needed columns from download files
#   Create function for downloading files (instead of repeating code)
#   Add feedback button/tab
#   Include reporting functionality -Rmarkdown?
#   Create user guide
#   Check/work on responsiveness
#   Incorporate Google analytics - https://shiny.rstudio.com/articles/google-analytics.html
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
#   Long labels of indicators are an issue (plotly_ind_name in lookup?)
#   legend names not working in plotly
#   Deal with tooltip:period, value
#   axis position not working in plotly
#   Domains, what to do with it.
#   Add functionaly to instead of comparing against an area, comparing against a baseline/past year
#----------------.
#Time trend: 
#   Adding numerator/rate tick box?
#----------------.
#Rank chart
#   Issue with long label names, take them out?
#   Vertical bars instead?
#----------------.
#Table:
#   Add deprivation data to table (maybe with switch or just merging everything)
#   Change placeholder text in filters (require javascript)
#----------------.
#Map:
#   Avoid redrawing of map:leafletProxy
#   Add intermediate zones to map, or is it going to be too big?
#   What about partnerships?
#   How to save map? Move away from Leaflet? Will likely be faster
#   For IZ's and localities maybe something like this: https://isresearchnc.shinyapps.io/CMaps/
#----------------.
#Deprivation
#   Do we want CI's? Error bars? polygon areas for trime trends?
#   Include PAR information and charts (trend and bar)
#   Add slope of SII to bar chart
#   instead of time period dropdown do slider
#----------------.

###############################################.

## Define a server for the Shiny app
function(input, output) {
  
  ###############################################.        
  #### Heatmap ----
  ###############################################.   
  
  #controls for heatmap:area name
  output$geoname_ui_heat <- renderUI({
    selectInput("geoname_heat", "Area", 
                choices =list("Area" = c(unique(subset(geo_lookup, geo_lookup$areatype == input$geotype_heat, select= c("areaname"))))),
                selectize=TRUE, selected = "")
  })
  
  #Heatmap data. Filtering based on user input values.
  heat_chosenarea <- reactive({ 
    
    heat_chosenarea <- optdata %>% 
      filter(code == as.character(geo_lookup$code[geo_lookup$areaname == input$geoname_heat])) %>% 
      select(c(year, indicator, measure, lowci, upci, interpret, def_period, type_definition)) %>% 
      droplevels()
    
  })
  
  #Select comparator based on years available for area selected.
  heat_chosencomp <- reactive({ 
    
    heat_chosencomp <- optdata %>% 
      filter(code == as.character(geo_lookup$code[geo_lookup$areaname == input$geocomp_heat])) %>% 
      select(c(year, indicator, measure)) %>% 
      rename(comp_m=measure) %>% 
      droplevels()
    
  })
  
  #Plotting 
  output$heat_plot <- renderPlotly({
    
    heat <- merge(heat_chosenarea(), heat_chosencomp(), by=c("indicator", "year")) %>% 
      arrange(year) # for ranking by value
    
    heat$color <- ifelse(heat$lowci < heat$comp_m & heat$upci > heat$comp_m,'gray',
                         ifelse(heat$lowci > heat$comp_m & heat$interpret == "H", 'blue',
                                ifelse(heat$lowci > heat$comp_m & heat$interpret == "L", 'red',
                                       ifelse(heat$upci < heat$comp_m & heat$interpret == "L", 'blue',
                                              ifelse(heat$upci < heat$comp_m & heat$interpret == "H", 'red', 'gray')))))
    
    ggplotly(
      ggplot(heat, aes(x = year, y = indicator, fill = color,
                       text= paste0(heat$indicator, "<br>", #Tooltip
                                    heat$def_period, "<br>",
                                    heat$type_definition, "<br>",
                                    heat$measure))) + 
        geom_tile(color = "black") +
        geom_text(aes(label = round(measure, 0), size =0.8)) +
        xlab("Less recent <- | Time period |-> More recent") + #x axis title
        scale_fill_manual(name = "Legend", labels = c("Significantly better", "Not significantly different", "Significantly worse"),
                          values = c(blue = "#3d99f5", gray = "#999999", red = "#ff9933")) + #tile color scale and legend
        scale_x_discrete(position = "top", expand = c(0, 0)) + #moving the x axis title to the top
        theme(axis.text.x=element_blank(), # taking out x axis labels
              axis.ticks=element_blank(), # taking out axis tick marks
              axis.title.y=element_blank(), #Taking out y axis title
              panel.background = element_blank(),#Blanking background
              legend.position="none", #taking out legend
              text = element_text(size=10) # changing font size
        ),
      tooltip=c("text")
    ) %>% 
      config(displayModeBar = TRUE, displaylogo = F, collaborate=F, editable =F) # taking out plotly logo and collaborate button
    
    
  })
  
  #Downloading data
  #   output$download_heat <- downloadHandler(
  #     filename =  'overview_data.csv',
  #     content = function(file) {
  #       write.csv(heat_data(), file)
  #     }
  #   )
  
  
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
  #Controls for chart. Dynamic selection of locality and iz.
  output$loc_ui_trend <- renderUI({
    selectInput("locname_trend", "HSC Locality", 
                choices = unique(geo_lookup$areaname[
                  geo_lookup$parent_area == input$loc_iz_trend &
                    geo_lookup$areatype == 'HSC Locality' ]),
                multiple=TRUE, selectize=TRUE, selected = "")
  })
  
  output$iz_ui_trend <- renderUI({
    selectInput("izname_trend", "Intermediate zone", 
                choices = unique(geo_lookup$areaname[
                  geo_lookup$parent_area == input$loc_iz_trend &
                    geo_lookup$areatype == 'Intermediate zone' ]),
                multiple=TRUE, selectize=TRUE, selected = "")
  })
  
  
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
    trend <- merge(x=trend, y=geo_lookup, by="code", all.x = TRUE) %>% 
      droplevels() #needed so palette works correctly
    
    trend <- trend[order(trend$year),] #Needs to be sorted by year for Plotly
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
      
      #Creating palette of colors with a tone for each geography type
      #First obtaining length of each geography type
      hb_length <- length(input$hbname_trend)
      ca_length <- length(input$laname_trend)
      scot_length <- length(input$scotname_trend)
      part_length <- length(input$partname_trend)
      loc_length <- length(input$locname_trend)
      iz_length <- length(input$izname_trend)
      colorblind_length <- hb_length+ca_length+scot_length+part_length+loc_length+iz_length
      
      #Then creating a gradient scale for each geography type based on its length
      hb_scale <- scales::seq_gradient_pal("#08519c", "#9ecae1", "Lab")(seq(0,1,length.out=hb_length))
      ca_scale <- scales::seq_gradient_pal("#006d2c", "#a1d99b", "Lab")(seq(0,1,length.out=ca_length))
      scot_scale <- scales::seq_gradient_pal("#000000", "#000000", "Lab")(seq(0,1,length.out=scot_length))
      part_scale <- scales::seq_gradient_pal("#FF0000", "#ffd6cc", "Lab")(seq(0,1,length.out=part_length))
      loc_scale <- scales::seq_gradient_pal("#dadaeb", "#54278f", "Lab")(seq(0,1,length.out=loc_length))
      iz_scale <- scales::seq_gradient_pal("#ffff66", "#4d4d00", "Lab")(seq(0,1,length.out=iz_length))
      colorblind_scale <- scales::seq_gradient_pal("#08306b", "#deebf7", "Lab")(seq(0,1,length.out=colorblind_length))
      
      #Then creating vector for each geography type with area names and color
      scot_cols <- setNames(scot_scale, "Scotland")
      hb_cols <- setNames(hb_scale, unique(trend_data()$areaname[trend_data()$areatype == "Health board"]))
      ca_cols <- setNames(ca_scale, unique(trend_data()$areaname[trend_data()$areatype == "Council area"]))
      part_cols <- setNames(part_scale, unique(trend_data()$areaname[trend_data()$areatype == "HSC Partnership"]))
      loc_cols <- setNames(loc_scale, unique(trend_data()$areaname[trend_data()$areatype == "HSC Locality"]))
      iz_cols <- setNames(iz_scale, unique(trend_data()$areaname[trend_data()$areatype == "Intermediate zone"]))
      colorblind_cols <- c(setNames(colorblind_scale, unique(trend_data()$areaname)))
      
      #Combining them all in the final palette. Using different palette if colour bling option checked
      if(input$colorblind_trend == FALSE){
        trend_col <- c(scot_cols, hb_cols, ca_cols, part_cols, loc_cols, iz_cols)
      }
      else{
        trend_col <- colorblind_cols
      }
      
      plot_ly(data=trend_data(), x=~trend_axis,  y = ~measure, 
              type = 'scatter', mode = 'lines+markers',
              color = ~areaname, colors = trend_col) %>% 
        #Layout 
        layout(annotations = list(), #It needs this because of a buggy behaviour
               margin = list(b = 160), #to avoid labels getting cut out
               yaxis = list(title = ~type_definition, rangemode="tozero", 
                            size = 4, titlefont =list(size=10), tickfont =list(size=9)), 
               xaxis = list(title = FALSE, tickfont =list(size=10), tickangle = 270)  #axis parameter
        ) %>%  # to get hover compare mode as default
        config(displayModeBar = TRUE, displaylogo = F, collaborate=F, editable =F) # taking out plotly logo and collaborate button
      
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
  #Controls for rank chart 
  #Dropdown for time period based on indicator selection  
  output$year_ui_rank <- renderUI({
    time_period <- sort(unique(optdata$trend_axis[optdata$indicator == input$indic_rank]))
    
    selectInput("year_rank", "Time period",
                choices = time_period, selected = last(time_period))
  })
  
  
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
        mutate(lowci_diff = measure - lowci) %>% 
        mutate(upci_diff = upci - measure) %>% 
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
        mutate(lowci_diff = measure - lowci) %>% 
        mutate(upci_diff = upci - measure) %>% 
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
      
      if (input$ci_rank == FALSE) {
        #Coloring based on if signicantly different from comparator
        color_pal <- ifelse(rank_bar_data()$lowci < rank_bar_data()$comp_value & rank_bar_data()$upci > rank_bar_data()$comp_value,'#999999',
                            ifelse(rank_bar_data()$lowci > rank_bar_data()$comp_value & rank_bar_data()$interpret == "H", '#3d99f5',
                                   ifelse(rank_bar_data()$lowci > rank_bar_data()$comp_value & rank_bar_data()$interpret == "L", '#ff9933',
                                          ifelse(rank_bar_data()$upci < rank_bar_data()$comp_value & rank_bar_data()$interpret == "L", '#3d99f5',
                                                 ifelse(rank_bar_data()$upci < rank_bar_data()$comp_value & rank_bar_data()$interpret == "H", '#ff9933', '#999999')))))
        
        #Text for tooltip
        tooltip_rank <- c(paste0(rank_bar_data()$areaname, ": ", rank_bar_data()$measure, "<br>",
                                 input$geocomp_rank, ": ", rank_bar_data()$comp_value))
        
        
        plot_ly(data = rank_bar_data(), x = ~areaname) %>% 
          #adding bar layer
          add_bars(y = ~ measure, text=tooltip_rank, hoverinfo="text",
                   marker = list(color = color_pal)) %>% 
          #Comparator line
          add_trace(y = ~comp_value, name = ~unique(comp_name), type = 'scatter', mode = 'lines',
                    line = list(color = '#FF0000'), showlegend = FALSE, hoverinfo="skip") %>% 
          #Layout
          layout(annotations = list(), #It needs this because of a buggy behaviour
                 title = ~def_period,
                 yaxis = list(title = ~type_definition, titlefont =list(size=10), 
                              tickfont =list(size=9)),
                 xaxis = list(title = "", tickangle = 270, 
                              tickfont =list(size=9), #axis parameters
                              categoryorder="array", #order of plotting
                              categoryarray = ~measure),
                 margin=list(b = 160),
                 hovermode = 'false') %>% # to get hover compare mode as default
          config(displayModeBar = TRUE, displaylogo = F, collaborate=F, editable =F)
        
      }
      else{
        
        #Coloring based on if signicantly different from comparator
        color_pal <- ifelse(rank_bar_data()$lowci < rank_bar_data()$comp_value & rank_bar_data()$upci > rank_bar_data()$comp_value,'#999999',
                            ifelse(rank_bar_data()$lowci > rank_bar_data()$comp_value & rank_bar_data()$interpret == "H", '#3d99f5',
                                   ifelse(rank_bar_data()$lowci > rank_bar_data()$comp_value & rank_bar_data()$interpret == "L", '#ff9933',
                                          ifelse(rank_bar_data()$upci < rank_bar_data()$comp_value & rank_bar_data()$interpret == "L", '#3d99f5',
                                                 ifelse(rank_bar_data()$upci < rank_bar_data()$comp_value & rank_bar_data()$interpret == "H", '#ff9933', '#999999')))))
        
        #Text for tooltip
        tooltip_rank <- c(paste0(rank_bar_data()$areaname, ": ", rank_bar_data()$measure, "<br>",
                                 input$geocomp_rank, ": ", rank_bar_data()$comp_value))
        
        
        plot_ly(data = rank_bar_data(), x = ~areaname) %>% 
          #adding bar layer
          add_bars(y = ~ measure, text=tooltip_rank, hoverinfo="text",
                   marker = list(color = color_pal), error_y = list(
                     type = "data",
                     symmetric = FALSE,
                     array = ~upci_diff,
                     arrayminus = ~lowci_diff)) %>% 
          #Comparator line
          add_trace(y = ~comp_value, name = ~unique(comp_name), type = 'scatter', mode = 'lines',
                    line = list(color = '#FF0000'), showlegend = FALSE, hoverinfo="skip") %>% 
          #Layout
          layout(annotations = list(), #It needs this because of a buggy behaviour
                 title = ~def_period,
                 yaxis = list(title = ~type_definition, titlefont =list(size=10), 
                              tickfont =list(size=9)),
                 xaxis = list(title = "", tickangle = 270, 
                              tickfont =list(size=9), #axis parameters
                              categoryorder="array", #order of plotting
                              categoryarray = ~measure),
                 margin=list(b = 160),
                 hovermode = 'false') %>% # to get hover compare mode as default
          config(displayModeBar = TRUE, displaylogo = F, collaborate=F, editable =F)
        
      }
    }
  }) 
  
  #Downloading data
  output$download_rank <- downloadHandler(
    filename =  'rankplot_data.csv',
    content = function(file) {
      write.csv(rank_bar_data(), file) 
    }
  )
  
  ###############################################.        
  #### Deprivation ----
  ###############################################.   
  
  #Controls for chart. Dynamic selection of area depending on area type.
  output$geoname_ui_simd <- renderUI({
    selectInput("geoname_simd", "Select the location", 
                choices = unique(subset(geo_lookup$areaname, geo_lookup$areatype == input$geotype_simd)),
                selected = "Scotland")
  })
  
  
  #Reactive datasets
  #reactive dataset for the simd bar plot
  simd_bar_data <- reactive({
    deprivation %>% 
      subset(code == as.character(geo_lookup$code[geo_lookup$areaname == input$geoname_simd]) & 
               indicator == input$indic_simd &
               trend_axis == input$year_simd) %>%
      mutate(average = rate[quintile == "Total"]) %>% 
      filter(quintile != "Total") %>% 
      droplevels()
  })
  
  simd_trend_data <- reactive({
    deprivation %>% 
      subset(code == as.character(geo_lookup$code[geo_lookup$areaname == input$geoname_simd]) & 
               indicator == input$indic_simd) %>% 
      droplevels()
  })
  
  #Plotting 
  output$simd_bar_plot <- renderPlotly({
    #If no data available for that period then plot message saying data is missing
    if (is.data.frame(simd_bar_data()) && nrow(simd_bar_data()) == 0)
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
      
      #Text for tooltip
      #     tooltip_simd <- c(paste0("Quintile: ", simd_bar_data()$quintile, "<br>",
      #                              "Value: ", abs(simd_bar_data()$rate)))
      
      plot_ly(data=simd_bar_data(), x=~quintile#,
              # text=tooltip_simd, hoverinfo="text"
      ) %>% 
        add_bars(y=~rate, color = ~quintile , colors = pal_simd_bar) %>%
        #Comparator line
        add_trace(y = ~average, name = "Average", type = 'scatter', mode = 'lines',
                  line = list(color = '#FF0000')) %>% #changing line color
        layout(bargap = 0.1, 
               margin=list(b = 160),
               legend = list(orientation = 'h'),
               yaxis = list(title = ~type_definition), 
               xaxis = list(showline = TRUE, title = FALSE, showticklabels = FALSE)) %>% 
        config(displaylogo = F, collaborate=F, editable =F) # taking out plotly logo and collaborate button
    }
  })
  
  #Plotting 
  output$simd_trend_plot <- renderPlotly({
    #If no data available for that period then plot message saying data is missing
    if (is.data.frame(simd_trend_data()) && nrow(simd_trend_data()) == 0)
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
      if (input$measure_simd == "Index of inequality") {
        
        simd_index <- simd_trend_data() %>% 
          filter((quintile == "Total" & code == "S00000001")|
                   (quintile == "2" & code != "S00000001"))
        
        plot_ly(data=simd_index, x=~trend_axis) %>% 
          add_lines(y = ~slope_coef, name = "Slope index of inequality", type = 'scatter', mode = 'lines',
                    line = list(color = '#74add1')) %>% #changing line color
          add_lines(y = ~rii, name = "Relative index of inequality", type = 'scatter', mode = 'lines',
                    line = list(color = '#313695'), yaxis = "y2") %>% #changing line color
          #Layout
          layout(annotations = list(), #It needs this because of a buggy behaviour
                 margin = list(b = 160), #to avoid labels getting cut out
                 yaxis = list(side = "left", rangemode="tozero", 
                              title = "", tickfont = list(color = "#74add1")), 
                 yaxis2 = list(side = "right", overlaying = "y", rangemode="tozero",
                               title = FALSE, tickfont = list(color = "#313695")),
                 xaxis = list(title = "Time period", tickfont =list(size=10), tickangle = 270),  #axis parameter
                 margin=list(pad = 50, l = 160, r = 200, b = 160),
                 legend = list(orientation = 'h', x = 20, y = 100),
                 hovermode = 'false') %>%  # to get hover compare mode as default
          config(displaylogo = F, collaborate=F, editable =F) # taking out plotly logo and collaborate button
        
      }
      
      else {
        
        #Text for tooltip
        #       tooltip_simd <- c(paste0("Quintile: ", simd_bar_data()$quintile, "<br>",
        #                                "Value: ", abs(simd_bar_data()$rate)))
        #,
        #text=tooltip_simd, hoverinfo="text"
        plot_ly(data=simd_trend_data(), x=~trend_axis,  y = ~rate, 
                type = 'scatter', mode = 'lines',
                color = ~quintile , colors = pal_simd_trend) %>% 
          #Layout
          layout(annotations = list(), #It needs this because of a buggy behaviour
                 margin = list(b = 160), #to avoid labels getting cut out
                 yaxis = list(title = FALSE, rangemode="tozero", 
                              size = 4, tickfont =list(size=10)), 
                 xaxis = list(title = "Time period", tickfont =list(size=10), tickangle = 270),  #axis parameter
                 margin=list(b = 160),
                 showlegend = FALSE,
                 hovermode = 'false') %>%  # to get hover compare mode as default
          config(displaylogo = F, collaborate=F, editable =F) # taking out plotly logo and collaborate button
        
      }
    }
  })
  
  #Downloading data
  output$download_simd <- downloadHandler(
    filename =  'deprivation_data.csv',
    content = function(file) {
      write.csv(simd_trend_data(), file)
    }
  )

#####################################.    
#### Map ----
#####################################.      
  
  output$year_ui_map <- renderUI({
    time_period <- sort(unique(optdata$trend_axis[optdata$indicator == input$indic_map]))
    
    selectInput("year_map", "Time period",
                choices = time_period, selected = last(time_period))
  })
  
  
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
      setView(-4.6519999, 56.33148888, zoom = 8) %>% # setting initial view point
      fitBounds(-10, 60, 0, 54)  %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      #Adding polygons with HB
      addPolygons(data=HB_pol(), group="Health board",
                  color = "#444444", weight = 2, smoothFactor = 0.5, 
                  label = (sprintf(
                    "<strong>%s</strong><br/>Total: %g<br/>Measure: %g",
                    HB_pol()$HBName, HB_pol()$numerator, HB_pol()$measure) 
                    %>% lapply(htmltools::HTML)),
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~colorQuantile(pal_map, measure_sc, n=5)(measure_sc),
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
                  fillColor = ~colorQuantile(pal_map, measure_sc, n=5)(measure_sc),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)
      ) %>% 
      #Adding layer control
      addLayersControl( 
        overlayGroups = c("Health board", "Council area"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      hideGroup(c("Council area")) #%>% 
      #Adding legend
      #addLegend("bottomright", pal=colorQuantile(pal_map, CA_pol()$measure_sc, n=5), values = CA_pol()$measure_sc, title = "Percentile")
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
                  colnames = c("Code", "Area", "Type", "Indicator", "Year", "Numerator", 
                               "Measure", "Lower confidence interval", "Upper confidence interval", 
                               "Definition" )
    )
  })
  
  #Downloading data 
  output$download_table <- downloadHandler(
    filename =  'table_data.csv',
    content = function(file) {
      write.csv(table_data()[input[["table_opt_rows_all"]], ], file) 
    }
  )

#####################################    
## For allowing bookmarking ----
#####################################.      
  enableBookmarking(store = "server")
}

#########################  END ----








