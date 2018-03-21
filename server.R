#Code to create ScotPHO's Shiny profile platform
#In this script include all the server side functions: plots, reactive objects, etc.

#TODO:
#see global syntax
###############################################.

## Define a server for the Shiny app
function(input, output) {
  
  ###############################################.        
  #### Overview ----
  ###############################################.   
  
  # Reactive controls for heatmap:area name depending on areatype selected
  output$geoname_ui_heat <- renderUI({
    
    areas_heat <- if (input$geotype_heat %in% c("Health board", "Council area", "HSC Partnership"))
      {
      sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_heat])
      } else {
        sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_heat
                                   & geo_lookup$parent_area == input$loc_iz_heat])
      }

    selectInput("geoname_heat", "Area", choices = areas_heat,
                selectize=TRUE, selected = "")

  })
  
  #Heatmap data for the chosen area. Filtering based on user input values.
  heat_chosenarea <- reactive({ 
      optdata %>% 
      subset(areaname == input$geoname_heat &
              areatype == input$geotype_heat &
               (domain1 %in% input$topic_heat | domain2 %in% input$topic_heat |  domain3 %in% input$topic_heat)) %>% 
      select(c(indicator, areaname, numerator, measure, lowci, upci, interpret, 
               year, def_period, type_definition)) %>% 
      droplevels()
    
  })
  
  #Select comparator based on years available for area selected.
  heat_chosencomp <- reactive({ 
    
    heat_chosencomp <- optdata %>% 
      subset(areaname == input$geocomp_heat &
               indicator != "Mid-year population estimate - all ages" &
               areatype %in% c("Health board", "Council area", "Scotland") &
               (domain1 %in% input$topic_heat | domain2 %in% input$topic_heat |  domain3 %in% input$topic_heat)) %>% 
      select(c(year, indicator, measure)) %>% 
      rename(comp_m=measure) %>% 
      droplevels()
    
  })
  
  # Calculates number of different indicators and then multiplies by pixels per row
  # it needs the sum at the end as otherwise small domains plots will be too small
  get_height_heat <- function() {
    (nrow(heat_chosenarea() )*3)+150 
  }
  
  ###############.
  #Overview plot
  output$heat_plot <- renderPlotly({
    
    #Merging comparator and chosen area
    heat <- merge(heat_chosenarea(), heat_chosencomp(), by=c("indicator", "year")) 
    
    #Creating a palette of colours based on statistical significance
    heat$color <- ifelse(heat$interpret == "O", 'white',
                         ifelse(heat$lowci <= heat$comp_m & heat$upci >= heat$comp_m,'gray',
                         ifelse(heat$lowci > heat$comp_m & heat$interpret == "H", 'blue',
                         ifelse(heat$lowci > heat$comp_m & heat$interpret == "L", 'red',
                         ifelse(heat$upci < heat$comp_m & heat$interpret == "L", 'blue',
                         ifelse(heat$upci < heat$comp_m & heat$interpret == "H", 'red', 'gray'))))))
    
    #Tooltip
    heat_tooltip <- paste0(heat$indicator, "<br>", heat$def_period, "<br>",
                           heat$type_definition, "<br>", heat$measure)
    
    # Plotting data
    plot_overview <- ggplot(heat, aes(x = year, y = indicator, fill = color,
                                      text= heat_tooltip)) + 
      geom_tile(color = "black") +
      geom_text(aes(label = round(measure, 0)), size =2.5) +
      #Another step needed to make the palette of colours for the tile work
      scale_fill_manual(name = "Legend", labels = c("Significantly better", "Not significantly different", "Significantly worse", "Significance is not calculated"),
                        values = c(blue = "#3d99f5", gray = "#999999", red = "#ff9933", white = "#ffffff")) + 
      #Giving the right dimensions to the plot
      scale_x_discrete(position = "top", expand = c(0, 0), limits = indicator_list) + 
      #Layout
      theme(axis.text.x=element_blank(), # taking out x axis labels
            axis.ticks=element_blank(), # taking out axis tick marks
            axis.title.x=element_text("Less recent <- Time period -> More recent"), #Taking out x axis title
            axis.title.y=element_blank(), #Taking out y axis title
            panel.background = element_blank(),#Blanking background
            legend.position="none", #taking out legend
            text = element_text(size=9) # changing font size
      )
    
    #Converting ggplot into a Plotly object
    ggplotly(plot_overview, tooltip=c("text"), height=get_height_heat()) %>% 
      # margins needed as long labels don't work well with Plotly
      layout(margin = list(l = 400, t = 100), 
             xaxis = list(side = 'top', title = "Less recent <- Time period -> More recent")) %>% 
      config(displayModeBar = TRUE, displaylogo = F, collaborate=F, editable =F) # taking out plotly logo and collaborate button
    
  })

 # Downloading data
    heat_csv <- reactive({ format_csv(heat_chosenarea()) })
  
    output$download_heat <- downloadHandler( filename =  'overview_data.csv',
      content = function(file) { write.csv(heat_csv(), file, row.names=FALSE) })
  

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
      subset((areaname %in% input$hbname_trend &  areatype == "Health board" |
               areaname %in% input$laname_trend & areatype == "Council area" |
               areaname %in% input$scotname_trend & areatype == "Scotland"  |
               areaname %in% input$locname_trend  & areatype == "HSC Locality" |
               areaname %in% input$partname_trend & areatype == "HSC Partnership"   |
               areaname %in% input$izname_trend & areatype == "Intermediate zone") & 
               indicator == input$indic_trend) %>% 
      droplevels() 
    
    trend <- trend[order(trend$year),] #Needs to be sorted by year for Plotly
  })
  
  #################
  #Creating plot
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
    else { #If data is available then plot it
      
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
      scot_cols <- setNames(scot_scale, unique(trend_data()$areaname_full[trend_data()$areatype == "Scotland"]))
      hb_cols <- setNames(hb_scale, unique(trend_data()$areaname_full[trend_data()$areatype == "Health board"]))
      ca_cols <- setNames(ca_scale, unique(trend_data()$areaname_full[trend_data()$areatype == "Council area"]))
      part_cols <- setNames(part_scale, unique(trend_data()$areaname_full[trend_data()$areatype == "HSC Partnership"]))
      loc_cols <- setNames(loc_scale, unique(trend_data()$areaname_full[trend_data()$areatype == "HSC Locality"]))
      iz_cols <- setNames(iz_scale, unique(trend_data()$areaname_full[trend_data()$areatype == "Intermediate zone"]))
      colorblind_cols <- c(setNames(colorblind_scale, unique(trend_data()$areaname_full)))
      
      #Combining them all in the final palette. Using different palette if colour bling option checked
      if(input$colorblind_trend == FALSE){
        trend_col <- c(scot_cols, hb_cols, ca_cols, part_cols, loc_cols, iz_cols)
      }
      else{
        trend_col <- colorblind_cols
      }
      
      #Text for tooltip
      tooltip_trend <- c(paste0(trend_data()$areaname, "<br>", trend_data()$year,
                                "<br>", trend_data()$measure))
      
      #Creating time trend plot
        plot_ly(data=trend_data(), x=~trend_axis,  y = ~measure, 
                text=tooltip_trend, hoverinfo="text",
                type = 'scatter', mode = 'lines+markers',
                color = ~areaname_full, colors = trend_col) %>% 
        #Layout 
        layout(annotations = list(), #It needs this because of a buggy behaviour of Plotly
               margin = list(b = 160, t=50), #to avoid labels getting cut out
               yaxis = list(title = ~type_definition, rangemode="tozero", 
                            size = 4, titlefont =list(size=10), tickfont =list(size=9)), 
               xaxis = list(title = FALSE, tickfont =list(size=10), tickangle = 270)) %>%  
        config(displayModeBar = TRUE, displaylogo = F, collaborate=F, editable =F) # taking out plotly logo and collaborate button
      
    }
  }) 
  
  #Downloading data
  trend_csv <- reactive({ format_csv(trend_data()) })
  
  output$download_trend <- downloadHandler(filename =  'timetrend_data.csv',
    content = function(file) {write.csv(trend_csv(), file, row.names=FALSE)})
  
  #####################################          
  #### Rank plot ----
  ###############################################.     
  #Dropdown for time period based on indicator selection  
  output$year_ui_rank <- renderUI({
    time_period <- sort(unique(optdata$trend_axis[optdata$indicator == input$indic_rank]))
    
    selectInput("year_rank", "Time period",
                choices = time_period, selected = last(time_period))
  })
  
  
  # Comparator data rank plot. Could be moved inside rank_bar_data
  rank_compar <- reactive({
    subset(optdata, trend_axis == input$year_rank & 
             areatype %in% c("Health board", "Council area", "Scotland") &
             areaname == input$geocomp_rank &
             indicator == input$indic_rank) %>% 
      droplevels()
  })
  
  #Rank plot data based on user input
  rank_bar_data <- reactive({
    #Makes different subsets depending on the geography type selected by the user
    if (input$geotype_rank %in% c("Scotland", "Health board", "Council area", "HSC Partnership"))
    {
      rank_bar <-optdata %>% 
        subset(areatype == input$geotype_rank &  
                 trend_axis == input$year_rank &
                 indicator == input$indic_rank) %>% 
        droplevels()
      
      #Cannot be done in the same pipe operation as it does not work with the areaname mutate
      rank_bar <- rank_bar %>% 
        mutate(comp_value = rank_compar()$measure) %>% #comparator value and name
        mutate(comp_name = rank_compar()$areaname) %>% 
        mutate(lowci_diff = measure - lowci) %>% 
        mutate(upci_diff = upci - measure) %>% 
        arrange(desc(measure)) # for ranking by value
    }
    else { #if locality or IZ it needs to filter based on the parent area and be the right area type.
      rank_bar <-  optdata %>% 
        subset(areatype == input$geotype_rank &
                 parent_area == input$loc_iz_rank &
                 trend_axis == input$year_rank &
                 indicator == input$indic_rank) %>% 
        droplevels()
      
      #Cannot be done in the same pipe operation as it does not work with the areaname mutate
      rank_bar <-rank_bar %>% 
        mutate(comp_value = rank_compar()$measure) %>% #comparator value and name
        mutate(comp_name = rank_compar()$areaname) %>% 
        mutate(lowci_diff = measure - lowci) %>% 
        mutate(upci_diff = upci - measure) %>% 
        arrange(desc(measure)) # for ranking by value
    }
  })
  
  ############################.
  # Creating  plot
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
    else { #If data is available then plot it
      
      #Coloring based on if signicantly different from comparator
      color_pal <- ifelse(rank_bar_data()$interpret == "O", '#ccccff',
                   ifelse(is.na(rank_bar_data()$lowci) | is.na(rank_bar_data()$upci) | is.na(rank_bar_data()$comp_value) | is.na(rank_bar_data()$measure) |rank_bar_data()$measure == 0, '#ccccff',
        ifelse(rank_bar_data()$lowci <= rank_bar_data()$comp_value & rank_bar_data()$upci >= rank_bar_data()$comp_value,'#999999',
                     ifelse(rank_bar_data()$lowci > rank_bar_data()$comp_value & rank_bar_data()$interpret == "H", '#3d99f5',
                           ifelse(rank_bar_data()$lowci > rank_bar_data()$comp_value & rank_bar_data()$interpret == "L", '#ff9933',
                                   ifelse(rank_bar_data()$upci < rank_bar_data()$comp_value & rank_bar_data()$interpret == "L", '#3d99f5',
                                         ifelse(rank_bar_data()$upci < rank_bar_data()$comp_value & rank_bar_data()$interpret == "H", '#ff9933', '#ccccff')))))))

      # Text for tooltip
      tooltip_rank <- c(paste0(rank_bar_data()$areaname, ": ", rank_bar_data()$measure, "<br>",
                               input$geocomp_rank, ": ", rank_bar_data()$comp_value))
      
      #Creating a vector with the area names in the order they are going to be plotted
      order_areas <- as.vector(rank_bar_data()$areaname)
      
      # General plot and layout, bars with or without error bars will be added after user input
      p <-   plot_ly(data = rank_bar_data(), x = ~areaname) %>% 
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
                            categoryarray = order_areas),
               margin=list(b = 160, t=80), # to prevent labels getting cut out
               hovermode = 'false') %>% # to get hover compare mode as default
        config(displayModeBar = TRUE, displaylogo = F, collaborate=F, editable =F)
      
      #Respond to user input regarding confidence intervals
      if (input$ci_rank == FALSE) {  
        #adding bar layer without confidence intervals
        p %>%  add_bars(y = ~ measure, text=tooltip_rank, hoverinfo="text",
                        marker = list(color = color_pal))
                   
      }
      else{ 
        #adding bar layer with error bars
        p %>% add_bars(y = ~ measure, text=tooltip_rank, hoverinfo="text",
                   marker = list(color = color_pal), 
                   error_y = list(type = "data",color='#000000',
                     symmetric = FALSE, array = ~upci_diff, arrayminus = ~lowci_diff)) 
      }
    }
  }) 
  
  #Downloading data
  rank_csv <- reactive({ format_csv(rank_bar_data()) })

  output$download_rank <- downloadHandler(filename =  'rank_data.csv',
    content = function(file) {write.csv(rank_csv(), file, row.names=FALSE) })
  
  ###############################################.        
  #### Deprivation ----
  ###############################################.   

  #Controls for chart. Dynamic selection of area depending on area type.
  output$geoname_ui_simd <- renderUI({
    
    list_areas <- sort(as.vector(subset(geo_lookup$areaname, geo_lookup$areatype == input$geotype_simd)))
    
    selectInput("geoname_simd", "Select the location", 
              choices = list_areas, selected = "Scotland")
  })
  
  #Dynamic selection of year depending on what years are available for each indicator.
  output$year_ui_simd <- renderUI({
    time_period <- sort(unique(deprivation$trend_axis[deprivation$indicator == input$indic_simd]))
    
    selectInput("year_simd", "Time period",
                choices = time_period, selected = last(time_period))
  })
  
  #reactive dataset for the simd bar plot
  simd_bar_data <- reactive({
    deprivation %>%
      subset(code == as.character(geo_lookup$code[geo_lookup$areaname == input$geoname_simd]) &
               indicator == input$indic_simd &
               trend_axis == input$year_simd) %>%
      mutate(average = measure[quintile == "Total"]) %>%
      filter(quintile != "Total") %>%
      droplevels()
  })
  
  #reactive dataset for the simd trend plot
  simd_trend_data <- reactive({
    deprivation %>%
      subset(code == as.character(geo_lookup$code[geo_lookup$areaname == input$geoname_simd]) &
               indicator == input$indic_simd) %>%
      droplevels()
  })
  
  #########################.
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
    else { #If data is available plot it

      #Text for tooltip
          tooltip_simd <- c(paste0("Quintile ", simd_bar_data()$quintile, "<br>",
                                   "Value: ", simd_bar_data()$measure))
      
      #Creating plot    
      plot_ly(data=simd_bar_data(), x=~quintile,
              text=tooltip_simd, hoverinfo="text"
      ) %>%
        add_bars(y=~measure, color = ~quintile , colors = pal_simd_bar) %>%
        #Comparator line
        add_trace(y = ~average, name = "Average", type = 'scatter', mode = 'lines',
                  line = list(color = '#FF0000'), hoverinfo="skip") %>% 
        layout(bargap = 0.1,
               margin=list(b = 160), #to avoid labels getting cut out
               legend = list(orientation = 'h'),
               yaxis = list(title = ~type_definition, titlefont =list(size=10), 
                            tickfont =list(size=9)),
               xaxis = list(showline = TRUE, title = FALSE, showticklabels = FALSE)) %>%
        config(displayModeBar = TRUE, displaylogo = F, collaborate=F, editable =F) # taking out plotly logo and collaborate button
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
    else { #If there is data plot it
      #Depending on what the user wants plot one or another
      if (input$measure_simd == "Index of inequality") {

        simd_index <- simd_trend_data() %>%
          filter((quintile == "Total" & code == "S00000001")|
                   (quintile == "2" & code != "S00000001"))
        
        # #Text for tooltip
        tooltip_simd <- c(paste0(simd_index$trend_axis, "<br>",
                                 "Relative: ", simd_index$rii, "<br>",
                                 "Absolute: ", simd_index$slope_coef))

        #Create plot
        plot_ly(data=simd_index, x=~trend_axis,
                text=tooltip_simd, hoverinfo="text") %>%
          add_lines(y = ~slope_coef, name = "Absolute level of inequality (SII)", type = 'scatter', mode = 'lines',
                    line = list(color = '#74add1')) %>% #changing line color
          add_lines(y = ~rii, name = "Relative level of inequality (RII)", type = 'scatter', mode = 'lines',
                    line = list(color = '#313695'), yaxis = "y2") %>% #changing line color
          #Layout
          layout(annotations = list(), #It needs this because of a buggy behaviour
                 margin = list(b = 160), #to avoid labels getting cut out
                 yaxis = list(side = "left", rangemode="tozero",
                              title = "", tickfont = list(color = "#74add1")),
                 yaxis2 = list(side = "right", overlaying = "y", rangemode="tozero",
                               title = FALSE, tickfont = list(color = "#313695")),
                 xaxis = list(title = " ", tickfont =list(size=10), tickangle = 270),  #axis parameter
                 margin=list(pad = 50, l = 160, r = 200, b = 160),
                 legend = list(orientation = 'h', x = 20, y = 100)) %>% 
          config(displayModeBar = TRUE, displaylogo = F, collaborate=F, editable =F) # taking out plotly logo and collaborate button

      }

      else { #This will show the Rate/percentage plot with all quintiles trend

        #Text for tooltip
              tooltip_simd <- c(paste0(simd_trend_data()$quintile, "<br>",
                                       simd_trend_data()$trend_axis, ": ", simd_trend_data()$measure))
        
        #Creating plot
        plot_ly(data=simd_trend_data(), x=~trend_axis,  y = ~measure,
                type = 'scatter', mode = 'lines',
                text=tooltip_simd, hoverinfo="text",
                color = ~quintile , colors = pal_simd_trend) %>%
          #Layout
          layout(annotations = list(), #It needs this because of a buggy behaviour
                 margin = list(b = 160), #to avoid labels getting cut out
                 yaxis = list(title = FALSE, rangemode="tozero",
                              size = 4, tickfont =list(size=10)),
                 xaxis = list(title = "", tickfont =list(size=10), tickangle = 270),  #axis parameter
                 margin=list(b = 160),
                 showlegend = FALSE) %>%
          config(displayModeBar = TRUE, displaylogo = F, collaborate=F, editable =F) # taking out plotly logo and collaborate button

      }
    }
  })

  #Downloading data
  simd_csv <- reactive({
    simd_trend_data() %>% 
      select(c(indicator, code, quintile, def_period, numerator, measure, 
               lowci, upci, type_definition, rii,	lowci_rii,	upci_rii,	
               slope_coef,	lowci_slope,	upci_slope)) %>% 
      rename(lower_confidence_interval=lowci, upper_confidence_interval=upci,
             period = def_period, definition = type_definition, relative_inequality = rii,
             lower_confidence_interval_relative = lowci_rii, upper_confidence_interval_relative = upci_rii,
             absolute_inequality = slope_coef, lower_confidence_interval_absolute = lowci_slope,
             upper_confidence_interval_absolute = upci_slope)
  })	

  
  output$download_simd <- downloadHandler(filename =  'deprivation_data.csv',
    content = function(file) {write.csv(simd_csv(), file, row.names=FALSE)})

#####################################.    
#### Map ----
#####################################.      
  #Dynamic selection of the last period available based on indicator selected
  output$year_ui_map <- renderUI({
    time_period <- sort(unique(optdata$trend_axis[optdata$indicator == input$indic_map]))
    
    selectInput("year_map", "Time period",
                choices = time_period, selected = last(time_period))
  })
  
  #Merging shapefile with dynamic selection of data
  #Council area
  ca_pol <- reactive({
    ca_map <- optdata %>% 
      subset(areatype == "Council area" &
               trend_axis==input$year_map & 
               indicator==input$indic_map) %>% 
      rename(GSS_COD = code) %>% 
      droplevels() #dropping missing factor levels to allow merging
    
    ca_map <- merge(ca_bound, ca_map, by='GSS_COD')
  }) 
  
  #Health Board
  
  hb_pol <- reactive({
    hb_map <- optdata %>% 
      subset(areatype == "Health board" &
               trend_axis==input$year_map & 
               indicator==input$indic_map) %>% 
      rename(HBCode = code) %>% 
      droplevels() #dropping missing factor levels to allow merging
    
    hb_map <- merge(hb_bound, hb_map, by='HBCode')
  })   
  
  #title of the map
  output$title_map <- renderText(paste(input$indic_map, " - ", unique(ca_pol()$def_period)))
  
  #Plotting map
    output$map <- renderLeaflet({
    leaflet() %>% 
      setView(-4.6519999, 56.33148888, zoom = 8) %>% # setting initial view point
      fitBounds(-10, 60, 0, 54)  %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      #Adding health board polygons 
      addPolygons(data=hb_pol(), group="Health board",
                  color = "#444444", weight = 2, smoothFactor = 0.5, 
                  #tooltip
                  label = (sprintf(
                    "<strong>%s</strong><br/>Total: %g<br/>Measure: %g",
                    hb_pol()$HBName, hb_pol()$numerator, hb_pol()$measure) 
                    %>% lapply(htmltools::HTML)),
                  opacity = 1.0, fillOpacity = 0.5,
                  #Colours
                  fillColor = ~colorQuantile(pal_map, measure_sc, n=5)(measure_sc),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)
      ) %>% 
      #Adding council area polygons
      addPolygons(data=ca_pol(), group="Council area",
                  color = "#444444", weight = 1, smoothFactor = 0.5, 
                  #tooltip
                  label = (sprintf(
                    "<strong>%s</strong><br/>Total: %g<br/>Measure: %g",
                    ca_pol()$NAME, ca_pol()$numerator, ca_pol()$measure) 
                    %>% lapply(htmltools::HTML)),
                  opacity = 1.0, fillOpacity = 0.5,
                  #Colours
                  fillColor = ~colorQuantile(pal_map, measure_sc, n=5)(measure_sc),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)
      ) %>% 
      #Adding layer control: layer selected, shown and how they are switched
      addLayersControl( 
        baseGroups = c("Health board", "Council area"), #Radios buttons
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      hideGroup(c("Council area")) 
  })

  
  #Downloading data
  map_csv <- reactive({
    optdata %>% 
      subset(areatype %in% c("Health board", "Council area") &
               trend_axis==input$year_map & indicator==input$indic_map) %>% 
      format_csv()
  })  
  
  output$download_map <- downloadHandler(filename =  'map_data.csv',
    content = function(file) { write.csv(map_csv(), file, row.names=FALSE)})
  
  #####################################.      
  #### Table ----
  #####################################.      
  
  # Table data
  table_data <- reactive({
    optdata %>%  subset(select=c("code", "areaname", "areatype", "indicator", "year", 
               "numerator", "measure", "lowci","upci", "def_period"))
  })
  
  #Actual table.
  output$table_opt <- DT::renderDataTable({
    
    DT::datatable(table_data(), style = 'bootstrap', filter = 'top', rownames = FALSE,
                  colnames = c("Code", "Area", "Type", "Indicator", "Year", "Numerator", 
                               "Measure", "Lower confidence interval", "Upper confidence interval", 
                               "Definition" ))
  })
  
  #Downloading data 
  table_csv <- reactive({ format_csv(table_data()) })
  
  #The filters the user applies in the data table will determine what data they download
  output$download_table <- downloadHandler(
    filename =  'table_data.csv',
    content = function(file) {
      write.csv(table_csv()[input[["table_opt_rows_all"]], ], 
                file, row.names=FALSE) } 
    )

}

#########################  END ----








