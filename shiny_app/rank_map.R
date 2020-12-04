#Code for rank and map tab

###############################################.       
#### Rank plot ----
###############################################.   
###############################################.
## Reactive controls ----
###############################################.
# Reactive controls - used for the map as well.
## Remember the selected samples
# creates reactive values to remember user selection of the geography level
# so it only changes when the user changes it on purpose
georank_chosen <- reactiveValues(value_geo = "Health board")
observeEvent(input$geotype_rank, 
             isolate({ georank_chosen$value_geo <- input$geotype_rank})
)


#Dropdown for time period based on indicator selection  
output$year_ui_rank <- renderUI({
  time_period <- sort(unique(optdata$trend_axis[optdata$indicator == input$indic_rank&
                                                  optdata$areatype == input$geotype_rank]))
  
  selectInput("year_rank",shiny::HTML("<p>Step 4. Select time period <br/> <br/></p>"),
              choices = time_period, selected = last(time_period))
})

#Dropdown for geotype based on what data is available for that indicator
output$geotype_ui_rank <- renderUI({
  areas <- sort(unique(optdata$areatype[optdata$indicator == input$indic_rank]))
  areas <- areas [! areas %in% c("Scotland")] #taking out Scotland
  selectInput("geotype_rank", label = "Step 2. Select a geography level",
              choices = areas, selected = georank_chosen$value_geo)
})

# Years to compare with depending on what data is available
output$yearcomp_ui_rank <- renderUI({
  rank_data <- optdata %>% subset(areatype == input$geotype_rank & 
                                    indicator==input$indic_rank)
  
  years <- c(min(rank_data$year):max(rank_data$year))
  periods <- c(sort(paste0(unique(rank_data$trend_axis[rank_data$year>=min(rank_data$year) &
                                                         rank_data$year<=max(rank_data$year)]))))
  div(title="Use this option to change the baseline year (the black circle in chart)",
      selectInput("yearcomp_rank","Step 3b. Select comparison baseline year", choices = periods,
                  selectize=TRUE))
})


###############################################.
# Indicator definitions
#Subsetting by domain and profile. Profile is fiddly as vector uses abbreviations 
# so needs to be converted to the names to match techdoc.
defs_data_rank <- reactive({techdoc %>% subset(input$indic_rank == indicator_name)})

output$defs_text_rank <- renderUI({
  
  HTML(paste(sprintf("<b><u>%s</b></u> <br> %s ", defs_data_rank()$indicator_name, 
                     defs_data_rank()$indicator_definition), collapse = "<br><br>"))
})


##Rank modal dialog help 

# Rank help main window call
observeEvent(input$rank_help, {showModal(rank_help_main_modal)})
#call to open rank area chart help modal window
observeEvent(input$rank_area_help,  {showModal(rank_byarea_modal) })
#call to open rank time chart help modal window
observeEvent(input$rank_time_help,  {showModal(rank_bytime_modal) })
#call from either area or time window back to main
observeEvent(input$rank_help_back,  {showModal(rank_help_main_modal) })

#Initial help screen - users need to pick whether they want help on area or time comparison.
rank_help_main_modal <- modalDialog(
  title = "How to use this chart",
  p("There are two types of charts available designed to provide comparisons for a single indicator."),
  p("The default chart shows a simple comparison of areas of a specific type e.g. NHS board"),
  p("Use menu selections (Step 1 to Step 4) to set the indicator or geography type to be compared."),
  p("In 'step 3' the comparator can be changed to 'Time', this changes the chart shown and adds an extra dimension to the area comparison."),
  actionButton("rank_area_help","How to use area comparisons chart"),
  actionButton("rank_time_help","How to use time comparisons chart"),
  size = "l", easyClose = TRUE, fade=FALSE,
  footer = modalButton("Close (Esc)"))

#Help page for area comparison 
rank_byarea_modal <- modalDialog(
  title = "How to use the area comparisons chart",
  p("The area comparison chart makes it easy to see which areas have high or low values for a particular indicator."),
  p("The red line that appears on the bar chart shows a comparator area, in the example below the Scotland average."),
  p("The comparator area can be changed using the 'Step 3b' drop-down menu."),
  p("The colours of the bars indicate whether an area is statistically significantly different to that comparator."),
  p("Confidence intervals are used to determine if an indicator is significantly different to the comparator. By default confidence intervales are not shown on the chart but can be added by ticking the option '95% confidence intervals'."),
  p(tags$a(img(src="help_rank_areachart.png"))),
  size = "l", easyClose = TRUE, fade=FALSE,
  footer=tagList(
    actionButton("rank_time_help","Help for time comparison chart"),
    actionButton("rank_help_back","Back"),
    modalButton("Close (Esc)")))

#Help page for time comparison
rank_bytime_modal <- modalDialog(
  title = "How to use the time comparisons chart",
  p("The time comparison chart shows how a particular indicator has changed over time across a set of geographies"),
  p("The example below shows how alcohol-related hospital stays have changed between 2009/10 and 2017/18."),
  p("The solid black circle show an indicator value at the baseline year, the other circle shows the latest value for that indicator. The 
    colour of the circle indicates if there is a statistically significant difference between the two time points"),
  p(tags$a(img(src="help_rank_lollychart.PNG"))),
  size = "l", easyClose = TRUE, fade=FALSE,
  footer=tagList(
    actionButton("rank_area_help","Help for area comparison chart"),
    actionButton("rank_help_back","Back"),
    modalButton("Close (Esc)")))

###############################################.
## Reactive data ----
###############################################.
# Comparator data rank plot. 
rank_compar <- reactive({
  
  if (input$comp_rank == 1){
    #Fiddly way of selecting period because some cases (e.g. Life expectancy)
    #might have different periods for the same year, e.g. IZ 2013 is 2011-2015
    #and HB 2013 is 2012-2014.
    year_chosen <- unique(optdata$year[optdata$trend_axis == input$year_rank])
    
    rank_compar <- optdata %>% subset(year %in% year_chosen & 
                                        areatype %in% c("Health board", "Council area", "Scotland") &
                                        areaname == input$geocomp_rank &
                                        indicator == input$indic_rank) %>% 
      droplevels()
    
    # dealing with councils and boards with the same name
    if (length(unique(rank_compar$areatype)) != 1) {
      rank_compar <- rank_compar %>% subset(areatype == "Health board")
    } else {
      rank_compar <- rank_compar
    }
    
  } else if (input$comp_rank == 2) { #if time comparison selected
    
    rank_compar <- optdata %>% subset(areatype == input$geotype_rank &
                                        trend_axis == input$yearcomp_rank & 
                                        indicator==input$indic_rank) %>% 
      rename(comp_value = measure, comp_name = areaname) %>% 
      select(code, comp_value, comp_name) %>% #to allow merging
      droplevels()
    
  }
})


#Rank plot data based on user input
rank_bar_data <- reactive({
  #Makes different subsets depending on the geography type selected by the user
  if (input$geotype_rank %in% c("Scotland", "Health board", "Council area", 
                                "Alcohol & drug partnership", "HSC partnership"))
  {
    rank_bar <- optdata %>% 
      subset(areatype == input$geotype_rank &  
               trend_axis == input$year_rank &
               indicator == input$indic_rank) %>% droplevels()
  } else { #if locality or IZ it needs to filter based on the parent area and be the right area type.
    rank_bar <-  optdata %>% 
      subset(areatype == input$geotype_rank &
               parent_area == input$loc_iz_rank &
               trend_axis == input$year_rank &
               indicator == input$indic_rank) %>% droplevels()
  }
  
  if (input$comp_rank == 1) { #for area comparisons
    rank_bar <- rank_bar %>% 
      mutate(comp_value = rank_compar()$measure, #comparator value and name
             comp_name = rank_compar()$areaname,
             lowci_diff = measure - lowci, 
             upci_diff = upci - measure) %>% 
      arrange(desc(measure)) # for ranking by value
  } else if (input$comp_rank == 2) { #if time comparison selected
    #This helps to deals with cases of incomplete data, e.g. 2011 has all HB but 2013 not.
    #Works for those cases where there are more data in the past (comparator)
    #and one for those which have more data in the present (chosen area)
    rank_bar <- left_join(x = rank_bar, y = rank_compar(), by = "code") %>% 
      mutate(lowci_diff = measure - lowci, 
             upci_diff = upci - measure) %>% 
      arrange(desc(measure)) # for ranking by value
    
  }
})

###############################################.
## Plots ----
###############################################.
############################.
#Title of plot
make_rank_subtitle <- function() {
  case_when(input$comp_rank == 1 ~ paste0(input$geotype_rank, "s compared against ",
                                          input$geocomp_rank, " - ",  input$year_rank),
            input$comp_rank == 2 ~ paste0("Changes within ", input$geotype_rank, 
                                          ": ", input$year_rank, " compared to ", input$yearcomp_rank))
}

output$rank_title <- renderText( paste0(input$indic_rank) )

output$rank_subtitle <- renderText({ make_rank_subtitle()  })

#visible summary of ui main panel to guide users
make_rank_summary <- function() {
  case_when(input$comp_rank == 1 ~ paste0("The bar chart and map below both show how areas of the same type (e.g. NHS board) compare to each other for a particular indicator."),
            input$comp_rank == 2 ~ paste0("The chart below is called a lollipop chart, it shows how areas compare with each other and also how each area has changed over time since the selected baseline year. The map shows a comparison for the selected year against the baseline year for each area."))
}
output$rank_summary <- renderText(make_rank_summary())

############################.
# Creating  plot
plot_rank_charts <- function(){
  
  #If no data available for that period then plot message saying data is missing
  if (is.data.frame(rank_bar_data()) && nrow(rank_bar_data()) == 0)
  {
    plot_nodata()
  }
  else { #If data is available then plot it
    
    # height of the plot
    # Calculates number of different indicators and then multiplies by pixels per row
    # it needs the sum at the end as otherwise small domains plots will be too small
    if (input$comp_rank == 1) {#if area comparison, standard length
      height_plot <- 500
    } else if (input$comp_rank == 2) {
      #Obtaining number of areas
      no_ind <- length(unique(rank_bar_data()$areaname))
      height_plot <- no_ind * 25 + 70
    }
    
    #Coloring based on if signicantly different from comparator
    rank_bar_data <- rank_bar_data() %>% 
      mutate(color_pal = case_when(interpret == "O" ~ '#999966',
                                   is.na(lowci) | is.na(upci) | is.na(comp_value) | is.na(measure) |measure == 0 ~ '#999966',
                                   lowci <= comp_value & upci >= comp_value ~'#cccccc',
                                   lowci > comp_value & interpret == "H" ~ '#4da6ff',
                                   lowci > comp_value & interpret == "L" ~ '#ffa64d',
                                   upci < comp_value & interpret == "L" ~ '#4da6ff',
                                   upci < comp_value & interpret == "H" ~ '#ffa64d', 
                                   TRUE ~ '#ccccff'))
    
    # Text for tooltip - one for each type of chart
    tooltip_bar <-c(paste0(rank_bar_data()$areaname, ": ", rank_bar_data()$measure, "<br>",
                           input$geocomp_rank, ": ", rank_bar_data()$comp_value))
    
    tooltip_dumbbell <- c(paste0(rank_bar_data()$areaname, "<br>",
                                 input$year_rank, ": ", rank_bar_data()$measure, "<br>",
                                 input$yearcomp_rank, ": ", rank_bar_data()$comp_value))
    
    #Creating a vector with the area names in the order they are going to be plotted
    order_areas <- as.vector(rank_bar_data()$areaname)
    
    ###############################################.
    # Starting the plot 
    # General plot and layout, bars with or without error bars will be added after user input
    rank_plot <- plot_ly(data = rank_bar_data, height = height_plot) 
    
    if (input$comp_rank == 1) {
      #Comparator line
      rank_plot <- rank_plot %>% 
        add_trace(x = ~areaname, y = ~comp_value, name = ~unique(comp_name), type = 'scatter', mode = 'lines',
                  line = list(color = '#FF0000'), showlegend = FALSE, hoverinfo="skip")
      
      #Respond to user input regarding confidence intervals
      if (input$ci_rank == FALSE) {  
        #adding bar layer without confidence intervals
        rank_plot <- rank_plot %>% add_bars(x = ~areaname, y = ~ measure, text=tooltip_bar, hoverinfo="text",
                                            marker = list(color = ~color_pal))
        
      }
      else { 
        #adding bar layer with error bars
        rank_plot <- rank_plot %>% add_bars(x = ~areaname,y = ~ measure, text=tooltip_bar, hoverinfo="text",
                                            marker = list(color = ~color_pal), 
                                            error_y = list(type = "data",color='#000000',
                                                           symmetric = FALSE, array = ~upci_diff, arrayminus = ~lowci_diff)) 
      }
      
      # Adding layout
      rank_plot %>% layout(annotations = list(), #It needs this because of a buggy behaviour
                           yaxis = list(title = ~type_definition, titlefont =list(size=14), 
                                        tickfont =list(size=14), fixedrange=TRUE),
                           xaxis = list(title = "", tickangle = 270, fixedrange=TRUE,
                                        tickfont =list(size=13), #axis parameters
                                        categoryorder="array", #order of plotting
                                        categoryarray = order_areas),
                           font = font_plots,
                           margin=list(b = 180, t = 5), # to prevent labels getting cut out
                           hovermode = 'false') %>% # to get hover compare mode as default
        config(displayModeBar = FALSE, displaylogo = F)
      
    } else if (input$comp_rank == 2) {#if time comparison selected, plot dumbbell plot
      
      rank_plot <- rank_plot %>% 
        add_segments(y = ~areaname, yend = ~areaname, x = ~measure, xend = ~comp_value, 
                     showlegend = FALSE, color = I("gray80"), hoverinfo="skip") %>% 
        # value of the area in the selected baseline period -comparator
        add_trace(y = ~areaname, x = ~comp_value, name = ~unique(comp_name), 
                  type = 'scatter', mode = 'markers', showlegend = FALSE, 
                  marker = list(color = 'black', size = 10), text=tooltip_dumbbell, hoverinfo="text") %>% 
        # value of the area in the selected period
        add_trace(y = ~areaname, x = ~measure, type = 'scatter', mode = 'markers',
                  marker = list(color = ~color_pal, size = 10,
                                line = list(color = 'gray', width = 2)), 
                  showlegend = FALSE, text=tooltip_dumbbell, hoverinfo="text") %>% 
        # Adding layout
        layout(xaxis = list(title = ~type_definition, titlefont =list(size=14), 
                            side = "top", tickfont =list(size=14), fixedrange=TRUE),
               yaxis = list(title = "", fixedrange=TRUE,
                            tickfont =list(size=13), #axis parameters
                            categoryorder="array", #order of plotting
                            categoryarray = rev(order_areas)),
               font = font_plots,
               margin=list(l = 170, t=40)) %>%  # to prevent labels getting cut out
        config(displayModeBar = FALSE, displaylogo = F)
      
    }
  } # bracket for "plot if data"
} 

# Calling the renderPlotly object
output$rank_plot <- renderPlotly({plot_rank_charts()  }) 

###############################################.
## Downloads ----
###############################################.
#Downloading data
rank_csv <- reactive({ format_csv(rank_bar_data(), extra_vars = c("comp_value", "comp_name")) %>%
    rename("comparator_value" = "comp_value", "comparator_name" = "comp_name") })

output$download_rank <- downloadHandler(filename =  'rank_data.csv',
                                        content = function(file) {write.csv(rank_csv(), file, row.names=FALSE) })

#Downloading chart
output$download_rankplot <- downloadHandler(
  filename = 'rank.png',
  content = function(file){
    export(p = plot_rank_charts() %>% 
             layout(title = paste0(input$indic_rank, "<br>", make_rank_subtitle()),
                    margin = list(t = 180)),
           file = file)
  })

#####################################.    
### Map ----
#####################################.
#####################.
# Reactive controls: it uses the ones from the rank section
###############################################.
# Indicator definitions
#Subsetting by domain and profile. Profile is fiddly as vector uses abbreviations
# so needs to be converted to the names to match techdoc.
defs_data_map <- reactive({techdoc %>% subset(input$indic_rank == indicator_name)})

output$defs_text_map <- renderUI({
  
  HTML(paste(sprintf("<b><u>%s</b></u> <br> %s ", defs_data_map()$indicator_name,
                     defs_data_map()$indicator_definition), collapse = "<br><br>"))
})

#####################.
# # Dynamic data - uses some of the rank ones

#Merging shapefile with dynamic selection of data
poly_map <- reactive({
  if (input$geotype_rank == "Council area"){
    map_pol <- sp::merge(ca_bound, rank_bar_data(), by='code')
  } else if(input$geotype_rank == "Health board"){
    map_pol <- sp::merge(hb_bound, rank_bar_data(), by='code')
  } else if(input$geotype_rank == "HSC partnership"){
    map_pol <- sp::merge(hscp_bound, rank_bar_data(), by='code')
  } else if(input$geotype_rank == "HSC locality"){
    map_pol <- sp::merge(hscloc_bound, rank_bar_data(), by='code')
    map_pol <- map_pol %>% subset(parent_area == input$loc_iz_rank)
  } else if(input$geotype_rank == "Intermediate zone"){
    map_pol <- sp::merge(iz_bound, rank_bar_data(), by='code')
    map_pol <- map_pol %>% subset(parent_area == input$loc_iz_rank)
  } else {
    map_pol <- data.frame(matrix(vector(), 0, 3)) #empty data frame
  }
  
})

#####################.
# Plotting map
#Function to create color palette based on if signicantly different from comparator
create_map_palette <- function(){
  case_when(
    poly_map()$interpret == "O" ~ '#999966',
    is.na(poly_map()$lowci) | is.na(poly_map()$upci) |
      is.na(poly_map()$comp_value) | is.na(poly_map()$measure) |
      poly_map()$measure == 0 ~ '#999966',
    poly_map()$lowci <= poly_map()$comp_value & poly_map()$upci >= poly_map()$comp_value ~'#999999',
    poly_map()$lowci > poly_map()$comp_value & poly_map()$interpret == "H" ~ '#3d99f5',
    poly_map()$lowci > poly_map()$comp_value & poly_map()$interpret == "L" ~ '#ff9933',
    poly_map()$upci < poly_map()$comp_value & poly_map()$interpret == "L" ~ '#3d99f5',
    poly_map()$upci < poly_map()$comp_value & poly_map()$interpret == "H" ~ '#ff9933',
    TRUE ~ '#ffffff')
}

#Plotting map
output$map <- renderLeaflet({
  
  #For some reason it needs the second line to work correctly in the map, if not
  #areas with no data are shown as dark grey.
  color_map <- create_map_palette() #palette
  color_map[is.na(color_map)] <- "#ffffff"
  
  #Actual map
  leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(data=poly_map(),
                color = "#444444", weight = 2, smoothFactor = 0.5,
                #tooltip
                label = (sprintf(
                  "<strong>%s</strong><br/>Total: %g<br/>Measure: %g<br/>%s",
                  poly_map()$area_name, poly_map()$numerator, poly_map()$measure,
                  poly_map()$type_definition) %>% lapply(htmltools::HTML)),
                opacity = 1.0, fillOpacity = 0.5, fillColor = color_map, #Colours
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE)
    )
})

# If no data or shapefile plot no map available
output$map_ui <- renderUI({
  if(is.data.frame(poly_map()) && nrow(poly_map()) == 0) {
    h4("No map available for that geographic level.", style = "color:black")
  } else {
    withSpinner(leafletOutput("map", width="100%",height="550px"))
  }
})

#####################.
# Downloading data
#Function to create map that can be downloaded
plot_map_download <- function(){
  
  if(is.data.frame(poly_map()) && nrow(poly_map()) == 0) {
    plot.new()
    text(0.5,0.5,"No map available for that geographic level.")
  } else {
    color_map <- create_map_palette() #palette
    title_map <- paste0(input$indic_rank, "\n", make_rank_subtitle())
    
    plot(poly_map(), col=color_map)
    title(title_map, cex.main = 3,  line = -1) # adding title
  }
}


#Function to filter the data needed for downloading data
map_csv <- function(){
  optdata %>%
    subset(areatype == input$geotype_map &
             trend_axis==input$year_map & indicator==input$indic_map) %>%
    format_csv()
}

#Downloading map data
output$download_map <- downloadHandler(filename =  'map_data.csv',
                                       content = function(file) { write.csv(map_csv(), file, row.names=FALSE)})

#Donwloading map chart
output$download_mapplot <- downloadHandler(
  filename = 'map.png',
  content = function(file){
    png(file, width = 2000, height = 2000, units = "px")
    plot_map_download()
    dev.off()
  })

#rank legend text
output$rank_legend <- renderUI({
  if (input$comp_rank == 1) {
    p(tags$b("Legend"), 
      br(),
      img(src='signif_better.png', height=12, style="padding-right: 2px; vertical-align:middle"),"Better than comparator",
      img(src='non_signif.png', height=12, style="padding-right: 2px; vertical-align:middle"), "Not different to comparator", 
      br(),
      img(src='signif_worse.png', height=12, style="padding-right: 2px; vertical-align:middle"), "Worse than comparator", 
      img(src='signif_nocalc2.png', height=12, style="padding-right: 2px; vertical-align:middle"), "No differences can be calculated")
  } else {
    p(tags$b("Chart legend"), br(),
      img(src='signif_better.png', height=12, style="padding-right: 2px; vertical-align:middle"),"Better than comparator",
      img(src='non_signif.png', height=12, style="padding-right: 2px; vertical-align:middle"), "Not different to comparator", br(),
      img(src='signif_worse.png', height=12, style="padding-right: 2px; vertical-align:middle"), "Worse than comparator", 
      img(src='signif_nocalc2.png', height=12, style="padding-right: 2px; vertical-align:middle"), "No differences can be calculated",br(),
      img(src='baseline_year_color.png', height=12, style="padding-right: 2px; vertical-align:middle"), "Baseline year comparison")
  }
})
