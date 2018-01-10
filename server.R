#Code to create ScotPHO's Shiny profile platform
#In this script include all the server side functions: plots, reactive objects, etc.

# TODO:
#   Fix issues spine chart: too many dots in iz (diff graph for different plots?),
#   Fix spine chart size issue, both in app and pdf download
#   add table to spine chart, figure out behaviour when signicance cannot be calculated
#   Check why Young people not in emplyment and others not working in dygraph
#   Make barcharts work with IZ (needs reducing numbers to plot)
#   Add "all" for table tab. Can be done?
#   See how to organize dropdown better, using lists, using conditional dropdowns 
#   Improve selection methods for dropdowns for localities (first select HB, which then limits selection)
#   Choices of dropdown boxes as vectors: c(), better than uniques, subsets...
#   Avoid redrawing of map:leafletProxy
#   Add error bars in/out button in plots/also in/out for other geographies in spine chart
#   fix dygraphs issue w/ time periods(through js?)
#   Add saving buttons to all plots
#   Add intermediate zones to map
#   See how to deal with deprivation
#   How to introduce an order/grouping system for spine chart (once profile is selected, like current domains)
#   Mini map for selecting geographies? include appendix with locations? like pdf?
#   Figure out what is best model for data projections
#   Change from read.csv to fread once R version is updated
#   Sanitize error messages 
#   Loading screen/bars?


###############################################.

## Define a server for the Shiny app
function(input, output) {
  
#####################################.    
##### Spine chart   ---- 
#####################################.   
#controls for spine chart
  output$geotype_ui_spine <- renderUI({
    selectInput("geotype_spine", "Geography level", choices=c("Health Board", "Local Authority", "Intermediate Zone"))
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
  timetrend <- reactive({ optdata %>% subset( 
                                (areaname %in% input$hbname_trend
                                 | areaname %in% input$laname_trend
                                 | areaname %in% input$scotname_trend                       
                                 | areaname %in% input$locname_trend)
                                & indicator == input$indic_trend,
                                select= c("areaname","measure", "year", "indicator")) %>% 
                            dcast(year+indicator ~ areaname, value.var="measure")
  })

  # Create time trend plot
  
  output$timetrendPlot <- renderDygraph({
    dygraph(timetrend(), main=paste(input$indic_trend)) %>% 
    dyRangeSelector() %>% 
    dyLegend(width = 400) %>% 
    dyOptions(axisLineWidth = 1.5, drawGrid = FALSE, drawPoints = TRUE, pointSize = 2) 
  })

#####################################          
#### Bar plot ----
###############################################.     
  #Bar plot data
  barpl <- reactive({filter(optdata, areatype==input$geotype_bar &
                           year == input$year_bar & indicator == input$indic_bar)})
  # Comparator data bar plot
  compar_bar <- reactive({filter(optdata, year == input$year_bar &
                                  areaname == input$geocomp_bar & indicator == input$indic_bar)})
  
#   # Create bar plot
  
  output$barPlot <- renderggiraph({
    ggiraph(code = print(
    ggplot(data=barpl(), aes(reorder(areaname, -measure), measure) ) +
      geom_bar_interactive(stat = "identity", fill="steelblue", 
                           aes(tooltip= paste("<font size=2><u>", areaname, "</u>", "<br>",  "Measure: ", "<b>", measure, "</b>",  "<br>",  "Numerator: ",  
                                              "<b>", numerator, "</b>", "<br>",  "CI: ", "<b>", lowci, " - ", upci, "</b>", "</font>"), 
                               data_id=areaname)) +
      geom_errorbar(aes(ymax=upci, ymin=lowci), width=0.5)+
      geom_hline(data = compar_bar(), aes(yintercept=measure,  col = areaname)) + #comparator
      labs(title = paste(input$indic_bar),
           subtitle = paste(unique(barpl()$def_period)),
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
  ), 
  width = .7, 
  hover_css = "cursor:pointer;fill:lightsteelblue;" , 
  tooltip_extra_css = "background-color:#99BF9E; 
  border: 2px #45B563 solid; border-radius: 5px; font-family:Calibri;")}) 
  
#####################################.      
#### Table ----
#####################################.      
#controls for table
  output$topic_ui_table <- renderUI({
    selectInput("topic_table", "Topic", choices = c(unique(optdata$topic1), unique(optdata$topic2)),
                selectize=TRUE, selected = "Scotland")
  })
  
  output$indic_ui_table <- renderUI({
    selectInput("indic_table", "Indicator", 
               choices=unique(subset(optdata,optdata$topic1 == input$topic_table 
                                     | optdata$topic2 == input$topic_table, select=c("indicator"))),
               multiple=TRUE)
    })

#Table data
  table_data <- reactive({subset(optdata, year>=input$year_table[1] & year<=input$year_table[2]
                                 & areaname %in% input$geoname_table & indicator %in% input$indic_table &
                                 (topic1 %in% input$topic_table | topic2 %in% input$topic_table),
                                 select=c("code", "areaname", "areatype", "indicator", "year", "numerator", "measure", "lowci",
                                          "upci", "def_period"))})

#Actual table.
  output$mytable <- DT::renderDataTable({
    DT::datatable(table_data(), style = 'bootstrap', filter = 'top', rownames = FALSE,
      colnames = c("Code", "Area", "Type", "Indicator", "Year", "Numerator", "Measure", "Lower CI", "Upper CI", 
                 "Definition" )
    )
  })
  

#####################################.      
#### Predicted indicators  ----
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
#### For downloading data ----
#for bar plot
  output$download_bar <- downloadHandler(
    filename =  'barplot_data.csv',
    content = function(file) {
      write.csv(barpl(), file) 
    }
  )
#for table  
  output$download_table <- downloadHandler(
    filename =  'table_data.csv',
    content = function(file) {
      write.csv(table_data(), file) 
    }
  )
  #for time trend   
  output$download_trend <- downloadHandler(
    filename =  'timetrend_data.csv',
    content = function(file) {
      write.csv(timetrend(), file)
    }
  )
#####################################    
## For allowing bookmarking ----
#####################################.      
  enableBookmarking(store = "url")
}
`
#########################  END ----








