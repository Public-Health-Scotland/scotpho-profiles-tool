#Code to create ScotPHO's Shiny profile platform
#In this script include all the server side functions: plots, reactive objects, etc.

# TODO:
#Data manipulation:
#   Decide what goes in the dataset and what doesn't
#   Do we need names loaded? or would be better to call to the lookup files
#   Refine indicator lookup
#   Figure out what to do with topic variables
#   Need to create a geography lookup with parent geographies (which level to use CA, HB?)
#----------------.
#Spine chart:
#   Fix issues spine chart: too many dots in iz (diff graph for different plots?),
#   Fix spine chart size issue, both in app and pdf download
#   add table to spine chart, figure out behaviour when signicance cannot be calculated
#   Add error bars in/out button in plots/also in/out for other geographies in spine chart
#   How to introduce an order/grouping system for spine chart (once profile is selected, like current domains)
#   Try google charts package for table + charts
#   Figure out how to include definition, time period etc.
#   What to do do with population indicators? Exclude? or show as numbers, but no plot?
#   Why size fo each indicator row does not work for all geographies?
#----------------.
#Time trend: 
#   Switch to Plotly? If not fix dygraphs labels and other issues
#----------------.
#Rank chart
#   Make barcharts work with IZ (needs reducing numbers to plot)
#   Fix issue that breaks R
#   Switch to Plotly?
#----------------.
#Table:
#   Add "all" for table tab. Can be done?
#----------------.
#General:
#   See how to organize dropdown better, using lists, using conditional dropdowns 
#   Improve selection methods for dropdowns for localities (first select HB, which then limits selection)
#   Choices of dropdown boxes as vectors: c(), better than uniques, subsets, saves computation...
#   Add saving buttons to all plots
#   Mini map for selecting geographies? include appendix with locations? like pdf?
#   Sanitize error messages 
#   Loading screen/bars?
#   Each tab should have a brief intro. Work on the text
#   Include report functionality
#   Create user guide
#   Idea: One dashboard per profile, reduce complexity, but maybe not enough
#   add progress bars
#   Incorporate Google analytics
#----------------.
#Map:
#   Avoid redrawing of map:leafletProxy
#   Add intermediate zones to map, or is it going to be too big?
#   What about partnerships?
#   How to save map? Move away from Leaflet? Will likely be faster
#----------------.
#Deprivation
#   See how to deal with deprivation, same app?
#----------------.
#Projection
#   Figure out what is best model for data projections
#----------------.
#URGENT ONES
#   Dygraph to plotly as now does not work
#   Fix map so it works with new data set
#   Fix rank as ir does not work with locality, iz nor partnerhsip

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
                choices =list("Area" = c(unique(subset(optdata, optdata$areatype == input$geotype_spine, select= c("areaname"))))),
                selectize=TRUE, selected = "")
  })

#Reactive data for spine chart, one for the area selected, one for the boxplot(areatype)
#and one for the comparator chosen.
  spine_areaname <- reactive({subset(optdata, areaname==input$geoname_spine &
                              year == input$year_spine & 
                              (topic1 %in% input$topic_spine | topic2 %in% input$topic_spine)) }) 
#for this two only including data fro indicators that the area selected has.
  spine_areatype <- reactive({subset(optdata, areatype==input$geotype_spine &
                                   year == input$year_spine & indicator %in% spine_areaname()$indicator &
                                     (topic1 %in% input$topic_spine | topic2 %in% input$topic_spine)) }) 
  spine_compar <- reactive({subset(optdata,  areaname == input$geocomp_spine &
                            year == input$year_spine & indicator %in% spine_areaname()$indicator &
                              (topic1 %in% input$topic_spine | topic2 %in% input$topic_spine)) })
  
  
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
  trend_data <- reactive({ optdata %>% subset( 
                                (areaname %in% input$hbname_trend
                                 | areaname %in% input$laname_trend
                                 | areaname %in% input$scotname_trend                       
                                 | areaname %in% input$locname_trend)
                                & indicator == input$indic_trend,
                                select= c("areaname","measure", "year", "indicator")) %>% 
                            dcast(year+indicator ~ areaname, value.var="measure")
  })

  # Create time trend plot
  
  output$trend_plot <- renderDygraph({
    dygraph(trend_data(), main=paste(input$indic_trend)) %>% 
    dyRangeSelector() %>% 
    dyLegend(width = 400) %>% 
    dyOptions(axisLineWidth = 1.5, drawGrid = FALSE, drawPoints = TRUE, pointSize = 2) 
  })
  
  
#   plot_plotly <- plot_ly(data=data, x=data[,xvar], y = round(data[,yvar],1),
#                          type = 'scatter', mode = 'lines',
#                          color=as.factor(data[,group]), colors = pal_chose[1:cat_length],
#                          width = 650, height = 500) %>%
#     #Layout
#     layout(title = paste(title, "<br>", "<sup><i>Source: ", sourc, sep=""), #title
#            titlefont = list(size=15), #title size
#            annotations = list(), #It needs this because of a buggy behaviour
#            yaxis = list(title = yaxtitle, rangemode="tozero"),
#            xaxis = list(title = xaxtitle, tickangle = 270, tickfont =list(size=10)), #axis parameter
#            margin=list( l = 70, r = 50, b = 150, t = 50, pad = 4 ), #margin-paddings
#            hovermode = 'false', # to get hover compare mode as default
#            images = scotpho_logo) %>%
#     config(displaylogo = F, collaborate=F, editable =F) # taking out plotly logo and collaborate button

   #Plotting 
#   output$trend_plot <- renderPlotly({
#         #Text for tooltip
#       tooltip <- c(paste0(trend_data()$measure, "<br>",
#                           trend_data()$quarter_name, "<br>",
#                           input$measure_trend, ": ", trend_data()[[input$measure_trend]]))
#       
#       #Plotting time trend
#       plot_ly(data=trend_data(), x=~quarter_date2, 
#               y = ~get(input$measure_trend), 
#               text=tooltip, hoverinfo="text",
#               type = 'scatter', mode = 'lines+markers',
#               color=~measure, colors = trend_pal) %>% 
#         #Layout
#         layout(annotations = list(), #It needs this because of a buggy behaviour
#                yaxis = list(title = input$measure_trend, rangemode="tozero"), 
#                xaxis = list(title = "Time period"),  #axis parameter
#                hovermode = 'false') %>%  # to get hover compare mode as default
#         config(displaylogo = F, collaborate=F, editable =F) # taking out plotly logo and collaborate button
#     
#   }) 
  
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
  #Rank plot data
  rank_bar_data <- reactive({filter(optdata, areatype==input$geotype_rank &
                           year == input$year_rank & indicator == input$indic_rank)})
  # Comparator data rank plot
  rank_compar <- reactive({filter(optdata, year == input$year_rank &
                                  areaname == input$geocomp_rank & indicator == input$indic_rank)})
  
# Create Rank plot
  
  output$rank_plot <- renderPlot({
    # ggiraph(code = print(
    ggplot(data=rank_bar_data(), aes(reorder(areaname, -measure), measure) ) +
      geom_bar_interactive(stat = "identity", fill="steelblue", 
                           aes(tooltip= paste("<font size=2><u>", areaname, "</u>", "<br>",  "Measure: ", "<b>", measure, "</b>",  "<br>",  "Numerator: ",  
                                              "<b>", numerator, "</b>", "<br>",  "CI: ", "<b>", lowci, " - ", upci, "</b>", "</font>"), 
                               data_id=areaname)) +
      geom_errorbar(aes(ymax=upci, ymin=lowci), width=0.5)+
      geom_hline(data = rank_compar(), aes(yintercept=measure,  col = areaname)) + #comparator
      labs(title = paste(input$indic_rank),
           subtitle = paste(unique(rank_bar_data()$def_period)),
           y = "Measure") + #title and subtitle
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), #rotating labels
            axis.title.x=element_blank(), #Taking out x axis title
            axis.title.y=element_blank(), #Taking out y axis title
            axis.line = element_line(colour = "black"), # Creating axis line
            panel.background = element_blank(),#Blanking background
            panel.grid.major = element_blank(), #taking out grid lines
            panel.grid.minor = element_blank(),
            legend.position="none" #taking out legends
      )
#   ), 
#   width = .7, 
#   hover_css = "cursor:pointer;fill:lightsteelblue;" , 
#   tooltip_extra_css = "background-color:#99BF9E; 
#   border: 2px #45B563 solid; border-radius: 5px; font-family:Calibri;")
}) 
  
  #Downloading data
  output$download_rank <- downloadHandler(
    filename =  'rankplot_data.csv',
    content = function(file) {
      write.csv(rank_bar_data(), file) 
    }
  )
  
#####################################.      
#### Table ----
#####################################.      

#Table data
  table_data <- reactive({
    subset(optdata, select=c("code", "areaname", "areatype", "indicator", "year", 
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

#####################################.      
#### Projection  ----
#####################################.      
  # Context sensitive menu creation again
  output$indic_pred1 <- renderUI({
    selectInput("indic_choose1", "Indicator", unique(optdata$indicator), multiple = FALSE)
  })
  output$indic_pred2 <- renderUI({
    selectInput("indic_choose2", "Area/s", indic_geog[[input$indic_choose1]], multiple = TRUE)
  })
  # Select data on basis of chosen indicator and geographies
  by_pred_data <- eventReactive(input$do_pred,{
    optdata %>% filter(indicator == input$indic_choose1, areaname %in% input$indic_choose2) 
  })
  # Create plot of observed and predicted indicator measures
  output$by_pred_plot <- renderPlot({
    ggplot(by_pred_data(), aes(x = year, y = measure, colour = areaname, fill = areaname)) +
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
  CA_pol <- reactive({merge(CA_bound, 
                            optdata %>% subset(areatype=="Local Authority" & year==input$year_map
                                                              & indicator==input$indic_map) %>% 
                              droplevels() %>% #dropping missing factor levels to allow merging
                              rename(GSS_COD=code) , 
                            by='GSS_COD')
    }) 

  #Second for Health Board
  
  HB_pol <- reactive({merge(HB_bound, 
                            optdata %>% subset(areatype=="Health Board" & year==input$year_map
                                               & indicator==input$indic_map) %>% 
                              droplevels() %>% #dropping missing factor levels to allow merging
                              rename(HBCode=code) , 
                            by='HBCode')
  })   
  
  #title
  output$title_map <- renderText(paste(input$indic_map, " - ", unique(CA_pol()$def_period)))
  
  #Plotting map
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      setView(-4.6519999, 56.33148888, zoom = 8) %>% # setting initial view point
      fitBounds(-10, 60, 0, 54)  %>%
      addProviderTiles(providers$OpenMapSurfer) %>%
      #Adding polygons with HB
      addPolygons(data=HB_pol(), group="Health Board",
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
      addPolygons(data=CA_pol(), group="Local Authority",
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
        overlayGroups = c("Health Board", "Local Authority"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      hideGroup(c("Health Board", "Local Authority")) %>% 
      #Adding legend
      addLegend("bottomright", pal=colorQuantile(c('#2c7bb6','#abd9e9', '#ffffbf','#fdae61','#d7191c'), CA_pol()$measure_sc, n=5), values = CA_pol()$measure_sc, title = "Percentile")
  })
  

#####################################    
## For allowing bookmarking ----
#####################################.      
  enableBookmarking(store = "url")
}

#########################  END ----








