#Code to create ScotPHO's Shiny profile platform
#In this script include all the server side functions: plots, reactive objects, etc.

#TODO:
#see global syntax
###############################################.

## Define a server for the Shiny app
function(input, output, session) {
  ################################################################
  #    MoDAL
  ################################################################
  #Welcome Modal
  showModal(modalDialog(
    br(),
    p(img(src="scotpho_reduced.png", height=100)),
    br(),
    br(),
    p(tags$h4("Welcome to the ScotPHO Profiles Tool"), tags$br(),tags$br(),tags$div(h5("This interactive tool provides access to a range of public health related indicators at different"),
                                                                                    h5("geographies including NHS boards, council areas and health and social care partnerships.")),
      style = "color:0E3E5D; font-size:20px"),
    br(),
    br(),
    p(tags$h5("Find out more about how to get the most out of the tool")),
    #br(),
    actionButton("tour","Take a tour", icon("play-circle")),
    HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
    #For when we make any updates in future
    actionButton("updates","Latest updates", icon("wrench")),
    HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
    modalButton("Go to the tool", icon("times-circle")),
    size = "l", align= "center",
    easyClose = FALSE, fade=TRUE, footer = NULL
  ))
  
  #"Take a Tour" Modal - first window
  observeEvent(input$tour, {
    showModal(modalDialog(
      fluidRow( 
        column(12,
               p(tags$div(" To begin, select the visualisation you want to see", style = "color:0E3E5D; font-size:20px; width: 90%; text-align: left;")),
               br(), tags$h5("There are different ways to navigate around the tool.", style = "width: 90%; text-align: left; " ), br(), br())), 
      fluidRow(
        column(5, offset=1,tags$h5("You can select visualisations via the tabs at the top of the page")),
        column(3, br(), img(src="tabs_select.png", height=43, width=400))),
      fluidRow(
        column(6, br(), br(), br(), br(), tags$a(img(src="landing_select.png", height=150, width=250, align="right"))),
        column(4, br(), br(), br(),br(),  br(), br(), br(), br(), tags$h5("Or select to view a visualisation using the option buttons shown on the Home Page"))),
      fluidRow(
        column(5, offset=1,  br(), br(), br(), br(), br(), h5("To return to the profiles Home Page at any point just click on the house button at the top of the page."),
               h5("Clicking on the ScotPHO logo will take you to the main ScotPHO website.")),
        column(3,br(),br(), br(), br(), br(), br(), br(), img(src="home_select.png", height=50, width=300))
      ),
      br(),
      br(),
      actionButton("next1","Next", icon("play-circle")),
      HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
      modalButton("Exit", icon("times-circle")),
      size = "l", align= "center",
      easyClose = FALSE, fade=TRUE, footer = NULL
    ))
  })
  
  #"Take a Tour" Modal - second window
  observeEvent(input$next1, {
    showModal(modalDialog(
      fluidRow(
        column(12,
               p(tags$div("Next use the menus to select the data you want to see", style = "color:0E3E5D; font-size:20px; width: 90%; text-align: left; ")),                 
               br(),
               br(),
               p(tags$h5("Some of our visualisations allow you to look at a predefined set of indicators related to a particular topic or profile e.g. ‘health and wellbeing’ or ‘children and young people’,
                         while others are designed to look at a single indicator in more detail. Using menus next to a visualisation you can select the area you are interested in and the time period.", style = "width: 90%; text-align: left; "))),
        column(6,
               br(),
               br(),
               br(),
               br(),
               br(),
               tags$h5("Use the drop-down menus to select filters of interest",  style="width:300px; "),
               p(tags$a(img(src="select_indic_single.png", height=200, width=300))),
               br()
        ),
        column(6,
               br(),
               br(),
               br(),
               p(tags$a(img(src="select_indicator.png", height=180, width=260))),
               p(tags$h5("Some fields may allow for multiple entries and most fields can be searched by typing into the field, as well as scrolling down the list of options", style = "width: 350px")),
               p(tags$a(img(src="type_select.png", height=180, width=260))),
               br()
        )),
      br(),
      p(tags$h5("For more information about geographic areas within Scotland, please refer to" , tags$a(href="http://statistics.gov.scot/home", "statistics.gov.scot.", 
                                                                                                        class="externallink"))),
      br(),
      actionButton("next2","Next", icon("play-circle")),
      HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
      modalButton("Exit", icon("times-circle")),
      size = "l", align= "center",
      easyClose = FALSE, fade=TRUE, footer = NULL
               ))
  })
  
  
  #"Take a Tour" Modal - third window
  observeEvent(input$next2, {
    showModal(modalDialog(
      fluidRow(
        column(12,
               p(tags$div("Finally, save the visualisation or download data", style = "color:0E3E5D; font-size:20px; width: 90%; text-align: left; ")),                 
               br(),
               br(),
               p(tags$h5("Our tool allows you to download the visualisation images and the data behind them.", style = "width: 90%; text-align: left; "))),
        column(6,
               br(),
               br(),
               br(),
               tags$h5("Charts displayed can be saved by clicking on the ‘Save chart’ button",  style="width:300px; "),
               p(tags$a(img(src="save_chart2.png", height=60, width=230))),
               br()
        ),
        column(6,
               br(),
               br(),
               br(),
               p(tags$h5("The raw data used to draw a chart can also be downloaded in csv format", style = "width: 350px")),
               p(tags$a(img(src="save.png", height=60, width=230))),
               br()
        )),
      br(),
      p(tags$h5("The ", tags$b("'Table'")," tab within our tool allows users to quickly and easily download larger selections of data covering multiple area or multiple indicators.", style = "width: 750px")),
      br(),
      actionButton("next3","Next", icon("play-circle")),
      HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
      modalButton("Exit", icon("times-circle")),
      size = "l", align= "center",
      easyClose = FALSE, fade=TRUE, footer = NULL
    ))
  })
  
  #"See latest updates" Modal window - Activate if Update is carried out once rolled out live to users
  observeEvent(input$updates, {
    showModal(modalDialog(
      fluidRow(
        column(12,
               p(tags$div("We are continuously developing our tool", style = "color:0E3E5D; font-size:20px; width: 90%; text-align: left; ")),                 
               br(),
               br(),
               p(tags$h5("Some new exciting features that have been added to our tool include:", style = "width: 90%; text-align: left; "))),
        column(12,
               tags$ul(tags$li(tags$h5("You can now filter by Alcohol and drug partnerships, for indicators where data is available")   
               )))),
      br(),
      p(tags$h5("Thanks for stopping by!")),
      p(tags$h5("For any further questions or other developments you would like to suggest for our current tool, please contact us at", tags$a(href="mailto:ScotPHO@nhs.net", "ScotPHO@nhs.net", class="externallink"), style = "width: 700px")),
      br(),
      modalButton("Exit", icon("times-circle")),
      size = "l", align= "center",
      easyClose = FALSE, fade=TRUE, footer = NULL
    ))
  })
  
  #"Take a Tour" Modal - last window  
  observeEvent(input$next3, {
    showModal(modalDialog(
      fluidRow(
        column(12,
               p(tags$div("Please note", style = "color:0E3E5D; font-size:20px; width: 90%; text-align: left; ")))),          
      br(),
      p(tags$h5("The profiles are intended to increase understanding of local health and social issues and to prompt further investigation, rather than to be used as a performance management tool.
        The information needs to be interpreted within a local framework; an indicator may be higher or lower in one area compared to another, but local knowledge is needed to understand and interpret differences.", style = "width: 700px")),
      br(),
      br(),
      br(),
      p(tags$h5("If you require any further information by bespoke geographies or have any questions regarding the tool, please", tags$a(href="mailto:ScotPHO@nhs.net", "contact us.", class="externallink"), style = "width: 600px")),
      br(),
      br(),
      p(tags$h5("Please also note this tool is being continuously developed and we welcome any ", tags$a(href="mailto:ScotPHO@nhs.net", "feedback", class="externallink"), " you may have on it.", style = "width: 600px")),
      br(),
      modalButton("Exit", icon("times-circle")),
      size = "l", align= "center",
      easyClose = FALSE, fade=TRUE, footer = NULL
    ))
  })
  
  ###############################################.
  ## Landing page ----
  ###############################################.
  # Creating events that take you to different tabs
  # activated when pressing buttons from the landing page
  observeEvent(input$jump_to_ring, {
    updateTabsetPanel(session, "intabset", selected = "ring")
  })
  
  observeEvent(input$jump_to_heat, {
    updateTabsetPanel(session, "intabset", selected = "heat")
  })
  
  observeEvent(input$jump_to_barcode, {
    updateTabsetPanel(session, "intabset", selected = "barcode")
  })
  
  observeEvent(input$jump_to_trend, {
    updateTabsetPanel(session, "intabset", selected = "trend")
  })
  
  observeEvent(input$jump_to_rank, {
    updateTabsetPanel(session, "intabset", selected = "rank")
  })
  
  observeEvent(input$jump_to_map, {
    updateTabsetPanel(session, "intabset", selected = "map")
  })
  
  observeEvent(input$jump_to_simd, {
    updateTabsetPanel(session, "intabset", selected = "simd")
  })
  
  observeEvent(input$jump_to_table, {
    updateTabsetPanel(session, "intabset", selected = "table")
  })
  
  observeEvent(input$jump_to_about, {
    updateTabsetPanel(session, "intabset", selected = "about")
  })
  
  observeEvent(input$jump_to_evidence, {
    updateTabsetPanel(session, "intabset", selected = "evidence")
  })
  
  observeEvent(input$jump_to_resources, {
    updateTabsetPanel(session, "intabset", selected = "resources")
  })
  
  observeEvent(input$jump_to_others, {
    updateTabsetPanel(session, "intabset", selected = "others")
  })
  
  ###############################################.        
  #### Profile Summary (ring plot) ----
  ###############################################.   
  
  ## Profile Summary (ring) help pop-up modal dialog - still need to create image file.
  observeEvent(input$help_ring, {
    showModal(modalDialog(
      title = "What does this chart show...",
      p(img(src="help_ring.PNG",height=600)),size = "l",
      easyClose = TRUE, fade=FALSE
    ))
  })
  
  ## REACTIVE CONTROLS
  # Reactive controls for areatype depending on profile selected
  output$geotype_ui_ring <- renderUI({
    areas <- optdata$areatype[substr(optdata$profile_domain1, 1, 3) == input$profile_ring |
                                substr(optdata$profile_domain2, 1, 3) == input$profile_ring] %>% 
      droplevels() %>% unique() %>%  sort()
    selectInput("geotype_ring", "Geography level", choices=areas,
                selected = "Health board")
  })
  
  # Reactive controls for profile summary:area name depending on areatype selected
  output$geoname_ui_ring <- renderUI({
    areas_ring <- if (input$geotype_ring %in% c("Health board", "Council area", "HSC Partnership", "Alcohol & drug partnership"))
    {
      sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_ring])
    } else {
      sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_ring
                               & geo_lookup$parent_area == input$loc_iz_ring])
    }
    selectInput("geoname_ring", "Area", choices = areas_ring,
                selectize=TRUE, selected = "Health board")
  })
  
  # Years to compare with depending on what data is available
  output$yearcomp_ui_ring <- renderUI({
    ring_years <- optdata %>%
      subset (areaname == input$geoname_ring &
                domain1 != "Population" &   #exclude population indicators
                areatype  == input$geotype_ring) %>% #not sure this is strictly neccessary?
      select(year) %>%
      droplevels()
    years <- c(min(ring_years$year):max(ring_years$year))
    selectInput("yearcomp_ring", "Baseline year", choices = years,selectize=TRUE, selected = "2011")
  })
  
  ## DATA SELECTIONS
  # Ring data for the chosen area. Filtering based on user input values.
  ring_chosenarea <- reactive({ 
    optdata %>%
      group_by(indicator) %>%
      mutate(max_year=max(year))%>%
      subset ((year==max_year) &
                areaname == input$geoname_ring &
                domain1 != "Population" &  #exclude population indicators
                (substr(profile_domain1, 1, 3) == input$profile_ring |
                   substr(profile_domain2, 1, 3) == input$profile_ring) &
                areatype  == input$geotype_ring) %>%
      droplevels()
  })
  
  #Select comparator based on years available for area selected.
  ring_chosencomp <- reactive({   
    #filter data to create series for chosen comparator
    if(input$comp_ring == 1){
      ring_chosencomp <- optdata %>%
        group_by(indicator) %>%
        mutate(max_year=max(year))%>%
        subset (year==max_year &
                  areaname == input$geocomp_ring &
                  domain1 != "Population" &
                  (substr(profile_domain1, 1, 3) == input$profile_ring |
                     substr(profile_domain2, 1, 3) == input$profile_ring) &
                  areatype %in% c("Health board", "Council area", "Scotland")) %>% 
        select(c(indicator, measure, year, areatype)) %>% 
        rename(comp_m =measure,comp_areatype=areatype) %>% 
        droplevels()
    }else if(input$comp_ring==2){
      ring_chosencomp <- optdata %>%
        group_by(indicator) %>%
        subset (year==input$yearcomp_ring &
                  areaname == input$geoname_ring &
                  domain1 != "Population" &
                  (substr(profile_domain1, 1, 3) == input$profile_ring |
                     substr(profile_domain2, 1, 3) == input$profile_ring) &
                  areatype %in% c("Health board", "Council area", "Scotland")) %>% 
        select(c(indicator, measure)) %>% 
        rename(comp_m =measure) %>% 
        droplevels()
    }
  })
  
  ## RING PLOT FUNCTION
  plot_ring <- function(){   
    
    #Merging comparator and chosen area
    if (input$comp_ring == 1){
      ring <- merge(ring_chosenarea(), ring_chosencomp(), by=c("indicator", "year"))
    } else if (input$comp_ring == 2) {
      ring <- merge(ring_chosenarea(), ring_chosencomp(), by=c("indicator"), all.x = TRUE)
    }
    
    #identify significant differences
    ring<-ring %>%  
      mutate(flag=ifelse(ring$interpret == "O",'No significance can be calculated',
                         ifelse(ring$lowci<=ring$comp_m & ring$upci>=ring$comp_m,'Statistically not significantly different from comparator average',
                                ifelse(ring$lowci > ring$comp_m & ring$interpret == "H",'Statistically significantly better than comparator average',
                                       ifelse(ring$lowci > ring$comp_m & ring$interpret == "L", 'Statistically significantly worse than comparator average',
                                              ifelse(ring$upci < ring$comp_m & ring$interpret == "L",'Statistically significantly better than comparator average',
                                                     ifelse(ring$upci < ring$comp_m & ring$interpret == "H",'Statistically significantly worse than comparator average','Statistically not significantly different from comparator average'))))))) %>%
      mutate(domain=ifelse(substr(ring$profile_domain1,1,3)==input$profile_ring,substr(ring$profile_domain1,5,nchar(as.vector(profile_domain1))),substr(ring$profile_domain2,5,nchar(as.vector(profile_domain2))))) %>% #identifies correct domain name for title
      select(domain,flag,indicator) %>%
      droplevels()
    
    #reorder flag factors levels to get correct order in chart
    ring$flag <- factor(ring$flag, levels=c("Statistically significantly better than comparator average","Statistically not significantly different from comparator average","Statistically significantly worse than comparator average","No significance can be calculated"))
    
    #group and count idicators within a domain by significance
    ring <- ring %>%
      group_by(domain,flag) %>%
      count(indicator) %>%
      summarise(count=sum(n))
    
    #group by domain to count total by domain
    ring <- ring %>%  
      group_by(domain) %>%
      arrange(domain) %>%
      mutate(fraction=count/sum(count)) %>%
      mutate(ymax=cumsum(fraction)) %>%
      mutate(ymin=c(0,head(ymax,n=-1))) %>%
      mutate(ind_sum=sum(count)) %>%
      mutate(bfrac=ifelse(flag =='Statistically significantly better than comparator average',fraction,0)) %>%
      mutate(bcount=ifelse(flag =='Statistically significantly better than comparator average',count,0)) %>%
      droplevels ()
    
    fill_df <- data.frame(flag = c ('No significance can be calculated','Statistically not significantly different from comparator average','Statistically significantly better than comparator average','Statistically significantly worse than comparator average'),stringsAsFactors = TRUE)
    fillcolours <- c("white","#999999","DodgerBlue", "#ff9933")
    names(fillcolours) <- levels(fill_df$flag)
    fill_colour <- scale_fill_manual(name = "flag",values = fillcolours)
    
    #create title and subtitle variables
    ring_title <- paste(profile_list," Summary: ",input$geoname_ring," (",input$geotype_ring,")",sep="")
    ring_subtitle <- if(input$comp_ring == 1){
      paste("Comparator geography: ",input$geocomp_ring,sep="")
    }else if(input$comp_ring==2){
      paste("Comparator geography: ",input$geoname_ring,"(",input$yearcomp_ring,")",sep="")
    }
    
    ggplot(ring, aes(fill=flag, ymax=ymax, ymin=ymin, xmax=4.5, xmin=1.5)) +
      geom_rect(colour='#555555') +
      geom_label(data=ring, label.size = NA, aes(label =paste(count), x = 3, y = (ymin + ymax)/2),show.legend = F,size = 4) +
      fill_colour +
      labs(title=ring_title,subtitle=ring_subtitle)+
      theme(
        axis.text=element_blank(), # taking out x axis labels
        axis.title = element_blank(),
        text = element_text(family="Helvetica Neue,Helvetica,Arial,sans-serif",colour ='#555555'),
        plot.title = element_text(face = "bold",size=16),
        plot.subtitle = element_text(size=14,margin=margin(0,0,40,0,unit="pt")),
        axis.ticks=element_blank(), # taking out x axis tick marks
        legend.position = "none",
        panel.background = element_blank(),#Blanking background
        panel.border = element_blank())+ #remove frame round plot plot
      facet_wrap(~ring$domain,labeller = label_wrap_gen(multi_line = TRUE)) +
      coord_polar(theta="y") +
      theme(
        strip.text.x = element_text(size=12,face="bold",colour='#555555',family="Helvetica Neue,Helvetica,Arial,sans-serif"),
        strip.background =element_rect(fill="snow"))+
      xlim(c(0,6))
  }
  
  #Render plot in app and resize 
  output$ring_plot <- renderPlot({
    plot_ring()
  }, height = function() {
    session$clientData$output_ring_plot_width
  })
  
  # Defined data file to down
  ring_csv <- reactive({
    #plot_ring  <- reactive({
    #Merging comparator and chosen area
    if (input$comp_ring == 1){
      ring <- merge(ring_chosenarea(), ring_chosencomp(), by=c("indicator", "year"))
    } else if (input$comp_ring == 2) {
      ring <- merge(ring_chosenarea(), ring_chosencomp(), by=c("indicator"), all.x = TRUE)
    }
    
    #identify significant differences
    ring<-ring %>%
      mutate(flag=ifelse(ring$interpret == "O",'No significance can be calculated',
                         ifelse(ring$lowci<=ring$comp_m & ring$upci>=ring$comp_m,'Statistically not significantly different from comparator average',
                                ifelse(ring$lowci > ring$comp_m & ring$interpret == "H", 'Statistically significantly better than comparator average',
                                       ifelse(ring$lowci > ring$comp_m & ring$interpret == "L", 'Statistically significantly worse than comparator average',
                                              ifelse(ring$upci < ring$comp_m & ring$interpret == "L",'Statistically significantly better than comparator average',
                                                     ifelse(ring$upci < ring$comp_m & ring$interpret == "H",'Statistically significantly worse than comparator average','Statistically not significantly different from comparator average'))))))) %>%
      mutate(domain=ifelse(substr(ring$profile_domain1,1,3)==input$profile_ring,substr(ring$profile_domain1,5,nchar(as.vector(profile_domain1))),substr(ring$profile_domain2,5,nchar(as.vector(profile_domain2))))) %>% #identifies correct domain name for title
      droplevels()
    
    ring <- ring %>%
      arrange(domain, flag) %>%
      select(c(domain, flag, indicator, areaname, areatype, def_period, numerator, measure,
               lowci, upci, type_definition, comp_m, comp_areatype)) %>%
      rename(lower_confidence_interval=lowci, upper_confidence_interval=upci,
             latest_period = def_period, definition = type_definition, comparator_measure = comp_m, difference=flag,comparator_geo=comp_areatype)
  })
  
  # Download data file
  output$download_ring <- downloadHandler( filename =  'ring_data.csv',
                                           content = function(file) { write.csv(ring_csv(), file, row.names=FALSE) })
  # Downloading chart  
  output$download_ringplot <- downloadHandler(
    filename = 'Profile_Summary.png',
    content = function(file){
      ggsave(file, plot = plot_ring(), device = "png",height = 15,width=15, limitsize=FALSE)
    })
  
  ###############################################.        
  #### Heatmap ----
  ###############################################.   
  # Heatmap help pop-up
  observeEvent(input$help_heat, {
    showModal(modalDialog(
      title = "What does the overview plot show...",
      p(img(src="help_overview.png"), height=500),  size = "l",
      easyClose = TRUE, fade=FALSE
    ))
  })
  
  #####################.
  # Reactive controls
  # Reactive controls for areatype depending on profile selected
  output$geotype_ui_heat <- renderUI({
    
    areas <- optdata$areatype[substr(optdata$profile_domain1, 1, 3) == input$profile_heat |
                                substr(optdata$profile_domain2, 1, 3) == input$profile_heat] %>% 
      droplevels() %>% unique() %>%  sort()
    
    selectInput("geotype_heat", "Geography level", choices=areas,
                selected = "Health board")
    
  })

  
  # Reactive controls for heatmap:area name depending on areatype selected
  output$geoname_ui_heat <- renderUI({
    
    areas_heat <- if (input$geotype_heat %in% c("Health board", "Council area", 
                                                "HSC Partnership", "Scotland", "Alcohol & drug partnership"))
      {
      sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_heat])
      } else {
        sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_heat
                                   & geo_lookup$parent_area == input$loc_iz_heat])
      }

    selectInput("geoname_heat", "Area", choices = areas_heat,
                selectize=TRUE, selected = "")

  })
  
  # Reactive controls for domain depending on profile
  output$topic_ui_heat <- renderUI({
    
    domain_list <- sort(profile_lookup$domain[profile_lookup$profile == input$profile_heat])
    
    selectInput("topic_heat", "Domain", choices = domain_list, selected='')
    
  })
  
  # Years to compare with depending on what data is available
  output$yearcomp_ui_heat <- renderUI({
    
    years <- c(min(heat_chosenarea()$year):max(heat_chosenarea()$year))
    
    selectInput("yearcomp_heat", "Baseline year", choices = years,
                selectize=TRUE)
    
  })
  
  #####################.
  # Reactive data
  #Heatmap data for the chosen area. Filtering based on user input values.
  heat_chosenarea <- reactive({ 
      optdata %>% 
      subset(areaname == input$geoname_heat &
              areatype == input$geotype_heat &
               !(indicator %in% c("Mid-year population estimate - all ages", "Quit attempts")) &
               (substr(profile_domain1, 1, 3) == input$profile_heat |
                  substr(profile_domain2, 1, 3) == input$profile_heat) &
               (substr(profile_domain1, 5, nchar(as.vector(profile_domain1))) == input$topic_heat |
                  substr(profile_domain2, 5, nchar(as.vector(profile_domain2))) == input$topic_heat)) %>% 
      select(c(indicator, areaname, areatype, numerator, measure, lowci, upci, interpret, 
               year, def_period, type_definition)) %>% 
      droplevels()
    
  })
  
  #Select comparator based on years available for area selected.
  heat_chosencomp <- reactive({ 
    
    if (input$comp_heat == 1){
      heat_chosencomp <- optdata %>% 
        subset(areaname == input$geocomp_heat &
                 indicator != "Mid-year population estimate - all ages" &
                 areatype %in% c("Health board", "Council area", "Scotland") &
                 (substr(profile_domain1, 1, 3) == input$profile_heat |
                    substr(profile_domain2, 1, 3) == input$profile_heat) &
                 (substr(profile_domain1, 5, nchar(as.vector(profile_domain1))) == input$topic_heat |
                    substr(profile_domain2, 5, nchar(as.vector(profile_domain2))) == input$topic_heat)) %>% 
        select(c(year, indicator, measure)) %>% 
        rename(comp_m=measure) %>% 
        droplevels()
    } else if (input$comp_heat == 2) { #if time comparison selected
      heat_chosencomp <- heat_chosenarea() %>% 
        subset(year == input$yearcomp_heat) %>% 
        select(c(indicator, measure)) %>% 
        rename(comp_m=measure) %>% 
        droplevels()
      
    }
    
  })
  
  #####################.
  #Heatmap plot
  
  # Calculates number of different indicators and then multiplies by pixels per row
  # it needs the sum at the end as otherwise small domains plots will be too small
  get_height_heat <- function() {
    
    #Obtaining number of indicators
    no_ind <- unique(heat_chosenarea()$indicator)
    length <- length(no_ind) * 28 + 125
  
  }
  
  #Title of plot
  output$title_heat <- renderText(paste0(input$geoname_heat, " - ", input$topic_heat))
  
  #Function to create ggplot, then used in renderPlot and ggsave
  plot_overview <- function(){
    
    #Merging comparator and chosen area
    if (input$comp_heat == 1){
      heat <- merge(heat_chosenarea(), heat_chosencomp(), by=c("indicator", "year"))
    } else if (input$comp_heat == 2) {
      heat <- merge(heat_chosenarea(), heat_chosencomp(), by=c("indicator"), all.x = TRUE)
    }
    
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
    ggplot(heat, aes(x = year, y = indicator, fill = color,
                     text= heat_tooltip)) +
      geom_tile(color = "black") +
      geom_text(aes(label = round(measure, 0)), size =2.5) +
      #Another step needed to make the palette of colours for the tile work
      scale_fill_manual(name = "Legend", labels = c("Significantly better", "Not significantly different", "Significantly worse", "Significance is not calculated"),
                        values = c(blue = "#3d99f5", gray = "#999999", red = "#ff9933", white = "#ffffff")) +
      #Giving the right dimensions to the plot
      scale_x_continuous(position = "top", breaks=seq(from = min(heat$year), to = max(heat$year), by =1)) +
      #Layout
      theme(axis.text.x = element_text(angle=90),
            axis.ticks.y=element_blank(), # taking out axis tick marks
            axis.title.x=element_blank(), #Taking out y axis title
            axis.title.y=element_blank(), #Taking out y axis title
            panel.background = element_blank(),#Blanking background
            legend.position="none", #taking out legend
            text = element_text(size=11) # changing font size
      )
  }
  
  output$heat_plot <- renderPlotly({
    #If no data available for that period then plot message saying data is missing
    if (is.data.frame(heat_chosenarea()) && nrow(heat_chosenarea()) == 0)
    {
      plot_nodata()
    }
    else { #If data is available then plot it
    #Converting ggplot into a Plotly object
      ggplotly(plot_overview(), tooltip=c("text"), height = get_height_heat()) %>%
      # margins needed as long labels don't work well with Plotly
        layout(margin = list(l = 400, t = 50),
             xaxis = list(side = 'top', fixedrange=TRUE), yaxis= list(fixedrange=TRUE),
             font = list(family = '"Helvetica Neue", Helvetica, Arial, sans-serif')
        ) %>%
        config(displayModeBar = FALSE, displaylogo = F, collaborate=F, editable =F) # taking out plotly logo and collaborate button
    }
  })
  
  #####################.
  # Downloading controls
  # Downloading data
    heat_csv <- reactive({ format_csv(heat_chosenarea()) })
  
    output$download_heat <- downloadHandler( filename =  'overview_data.csv',
      content = function(file) { write.csv(heat_csv(), file, row.names=FALSE) })
    
  # Downloading chart  
    output$download_overviewplot <- downloadHandler(
      filename = 'overview.png',
      content = function(file){
        ggsave(file, plot = plot_overview()+ ggtitle(paste0(input$geoname_heat, " - ", input$topic_heat)), 
               device = "png", scale=4, limitsize=FALSE)
      })
    
##############################################.
## Barcode ----
###############################################.
    
    # Barcode help pop-up
    observeEvent(input$help_bar, {
      showModal(modalDialog(
        title = "What does the barcode plot show...",
        p(img(src="help_bar.PNG",height=600)),size = "l",
        easyClose = TRUE, fade=FALSE
      ))
    })
    
    #####################.
    # Reactive controls
    # Reactive controls for areatype depending on profile selected
    output$geotype_ui_bar <- renderUI({
      
      areas <- optdata$areatype[substr(optdata$profile_domain1, 1, 3) == input$profile_bar |
                                  substr(optdata$profile_domain2, 1, 3) == input$profile_bar] %>% 
        droplevels() %>% unique() %>%  sort()
      
      areas <- areas [! areas %in% c("Scotland")]
      
      selectInput("geotype_bar", "Geography level", choices=areas,
                  selected = "Health board")
      
    })
    
    # Reactive controls for domain depending on profile
    output$topic_ui_bar <- renderUI({
      
      domain_list <- sort(profile_lookup$domain[profile_lookup$profile == input$profile_bar])
      
      selectInput("topic_bar", "Domain", choices = domain_list, selected='')
      
    })
    
    # Reactive controls for barcode:area name depending on areatype selected
    output$geoname_ui_bar <- renderUI({
      
      areas_bar <- if (input$geotype_bar %in% c("Health board", "Council area", 
                                                  "Alcohol & drug partnership", "HSC Partnership"))
      {
        sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_bar])
      } else {
        sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_bar
                                 & geo_lookup$parent_area == input$loc_iz_bar])
      }
      
      selectInput("geoname_bar", "Area", choices = areas_bar, selectize=TRUE, selected = "")
      
    })
    
#####################.
# Reactive data
    #Barcode all area data
    bar_allareas <- reactive({
      
      optdata %>%
        group_by (indicator) %>%
        mutate(max_year = max(year))%>%
        subset (year == max_year &
                  (substr(profile_domain1, 1, 3) == input$profile_bar |
                     substr(profile_domain2, 1, 3) == input$profile_bar) &
                  (substr(profile_domain1, 5, nchar(as.vector(profile_domain1))) == input$topic_bar |
                     substr(profile_domain2, 5, nchar(as.vector(profile_domain2))) == input$topic_bar) &
                  areatype  == input$geotype_bar &
                  !(indicator %in% c("Mid-year population estimate - all ages", "Quit attempts")))
    })
    
    #Barcode data for the chosen area. Filtering based on user input values.
    bar_chosenarea <- reactive({
      optdata %>%
        group_by (indicator) %>%
        mutate(max_year=max(year))%>%
        subset (year == max_year &
                  areaname == input$geoname_bar &
                  (substr(profile_domain1, 1, 3) == input$profile_bar |
                     substr(profile_domain2, 1, 3) == input$profile_bar) &
                  (substr(profile_domain1, 5, nchar(as.vector(profile_domain1))) == input$topic_bar |
                     substr(profile_domain2, 5, nchar(as.vector(profile_domain2))) == input$topic_bar) &
                  areatype  == input$geotype_bar &
                  !(indicator %in% c("Mid-year population estimate - all ages", "Quit attempts"))) %>%
        select(c(indicator, measure, lowci, upci)) %>%
        rename(measure_chosen= measure, lowci_chosen=lowci, upci_chosen= upci) %>%
        droplevels()
    })
    
    #Select comparator based on years available for area selected.
    bar_chosencomp <- reactive({
      
      optdata %>%
        group_by (indicator) %>%
        mutate(max_year=max(year))%>%
        subset (year==max_year &
                  areaname == input$geocomp_bar &
                  areatype %in% c("Health board", "Council area", "Scotland") &
                  (substr(profile_domain1, 1, 3) == input$profile_bar |
                     substr(profile_domain2, 1, 3) == input$profile_bar) &
                  (substr(profile_domain1, 5, nchar(as.vector(profile_domain1))) == input$topic_bar |
                     substr(profile_domain2, 5, nchar(as.vector(profile_domain2))) == input$topic_bar) &
                  !(indicator %in% c("Mid-year population estimate - all ages", "Quit attempts"))) %>%
        select(c(indicator, measure)) %>%
        rename(measure_comp =measure) %>%
        droplevels()
      
    })
#####################.
# Creating plot    
    #Dynamically set height of bars
    bar_plot_height<- function(){
      (nrow(bar_chosenarea())*70+120)
    }
    
    # Create barcode plot function
    plot_barcode <- function(){
      
      ind_count <- length(unique(bar_allareas()$ind_id)) #facet_wrap requires how many chart rows to render
      
      #Merging comparator and chosen area
      bar <- merge(bar_allareas(), bar_chosencomp(), by=c("indicator"))
      bar <- merge(bar, bar_chosenarea(), by=c("indicator"))
      
      #add variable denoting if sign diff between comparator
      bar<-bar %>%
        mutate(flag=ifelse(bar$interpret == "O",'No significance can be calculated',
                           ifelse(bar$lowci_chosen<=bar$measure_comp & bar$upci_chosen>=bar$measure_comp,'Statistically not significantly different from comparator average',
                                  ifelse(bar$lowci_chosen > bar$measure_comp & bar$interpret == "H", 'Statistically significantly better than comparator average',
                                         ifelse(bar$lowci_chosen > bar$measure_comp & bar$interpret == "L", 'Statistically significantly worse than comparator average',
                                                ifelse(bar$upci_chosen < bar$measure_comp & bar$interpret == "L", 'Statistically significantly better than comparator average',
                                                       ifelse(bar$upci_chosen < bar$measure_comp & bar$interpret == "H", 'Statistically significantly worse than comparator average','Statistically not significantly different from comparator average')))))))
      
      #Transposing data so that better is always to the right of plot
      bar <- bar %>%
        mutate(comp=1)%>%
        mutate(all=bar$measure/bar$measure_comp) %>%
        mutate(chosen=bar$measure_chosen/bar$measure_comp) %>%
        mutate(all2=ifelse(bar$interpret=='L' & bar$measure>bar$measure_comp, -(all-1),
                           ifelse(bar$interpret=='L' & bar$measure<=bar$measure_comp, (1-all),-(1-all)))) %>%
        mutate(chosen2=ifelse(bar$interpret=='L' & bar$measure_chosen>bar$measure_comp, -(chosen-1),
                              ifelse(bar$interpret=='L' & bar$measure_chosen<=bar$measure_comp, (1-chosen),-(1-chosen)))) %>%
        mutate(comp=0)
      
      #define x axis value to assign as intercept for significance
      minx <- min(bar$all2)-0.05
      
      #generate labels for comp and chosen bars
      data_labels <- bar %>%
        select(indicator, measure_comp, measure_chosen, chosen2, comp, type_definition,trend_axis, code) %>%
        group_by(indicator, type_definition, trend_axis, code) %>%
        summarise(comp_lab=measure_comp[1], chosen_lab=measure_chosen[1],
                  x_chosen=chosen2[1], x_comp = comp[1])%>%
        droplevels()
      
      bar_data <- bind_rows(bar %>% mutate(y=0),
                             bar %>% mutate(y=1))
      
      #Chart title text & subtitle
      areatype_name <- input$geotype_bar
      chosenarea_name <- input$geoname_bar
      comparea_name <- input$geocomp_bar
      topic_name <- input$topic_bar
      bar_subtitle <- paste("Topic:",input$topic_bar,sep=" ")
      
      #Create colour scale for lines & legend key.
      colour_lines <-  scale_colour_manual(" ",values= setNames(c("black", "lightseagreen", "goldenrod1"), c(areatype_name, chosenarea_name, comparea_name)))
      
      #Create fill colour scheme for significance.
      fill_df <- data.frame(flag = c('No significance can be calculated', 'Statistically not significantly different from comparator average', 'Statistically significantly better than comparator average','Statistically significantly worse than comparator average'),stringsAsFactors = TRUE)
      fillcolours <- c("white", "#999999","DodgerBlue","#ff9933")
      names(fillcolours) <- levels(fill_df$flag)
      colour_points <- scale_fill_manual(name = "flag",values = fillcolours)
      
      ggplot(bar_data, aes(x = all2, y = y, group=code, colour=areatype))+
        geom_line(alpha=0.4)+
        geom_line(aes(x = chosen2, colour=chosenarea_name), size=1) + #line for picked area
        geom_line(aes(x = comp, colour=comparea_name), size=1) + #line for comparator
        geom_point(aes(fill=flag, shape=flag, x= minx, y=0.5),size=4,colour="grey40") +
        geom_text(data=data_labels, aes(x=x_chosen,y=1.5,label=round(chosen_lab,digits=0)),
                  check_overlap = TRUE,size=5,colour = "lightseagreen",vjust=1, hjust=1) + #label for chosenarea
        geom_text(data=data_labels, aes(x=x_comp,y=-0.6,label=round(comp_lab,digits=0)),
                  check_overlap = TRUE,size=5,colour = "goldenrod1",vjust=0, hjust=0) + #label for comparator
        xlab("Worse  <-------------------->  Better")+
        scale_x_discrete(position = "top") +
        colour_lines+
        colour_points+
        scale_shape_manual(values=21:24) +
        guides(color = guide_legend(order = 1),fill = guide_legend(order = 0))+
        theme(axis.title.y=element_blank(), #Taking out y axis title
              axis.title.x=element_text(size=12, colour ='#555555',hjust=0.6), #x axis title contains better/worse
              #plot.title = element_text(hjust=0), #?
              #plot.subtitle = element_text(size=12,colour ='#555555'), #format substitle
              #plot.caption = element_text(colour ='#555555',hjust=0),
              text = element_text(family="Helvetica Neue,Helvetica,Arial,sans-serif"),
              legend.direction = "vertical",
              legend.position="top",
              legend.key = element_rect(colour = NA, fill = NA),
              legend.text = element_text(size=12,colour ='#555555', hjust=-1),
              legend.title = element_blank(),
              axis.text.x=element_blank(), # taking out x axis labels
              axis.text.y=element_blank(), # taking out y axis labels
              axis.ticks.x=element_blank(), # taking out x axis tick marks
              axis.ticks.y=element_blank(), # taking out x axis tick marks
              panel.background = element_blank(),#Blanking background
              panel.border = element_blank())+ #remove frame round plot plot
        facet_wrap(~indicator + type_definition + trend_axis, nrow=ind_count,ncol=1,scales="fixed",labeller = label_wrap_gen(multi_line = TRUE), strip.position="left")+ #facet wrap on indicator description to allow full description of indicator in strip text
        theme(strip.text.y = element_text(size=14,colour ='#555555', angle = 180, hjust = 0))+
        theme(strip.background =element_rect(fill="grey96"))
    }
    
    # Render plot
    output$bar_plot <- renderPlot({
      plot_barcode()
    })
    
    # Resize plot height for display in app
    output$ui_bar_plot <-renderUI({
      plotOutput("bar_plot", height=bar_plot_height(), width="100%")
    })
    
    
    # Topic select title for inclusion in app
    output$topic_selected<- renderText({
      c("<b>Topic: ",input$topic_bar,"</br>")
    })
    
#####################.
# Downloading plot and data  
    # Defined data file to download
    bar_csv <- reactive({
      
      #Merging comparator and chosen area
      bar <- merge(bar_allareas(), bar_chosencomp(), by=c("indicator"))
      bar <- merge(bar, bar_chosenarea(), by=c("indicator"))
      bar <- bind_cols(bar %>% mutate(topic=input$topic_bar))
      bar <- bind_cols(bar %>% mutate(comparator=input$geocomp_bar))
      
      bar<-bar %>%
        mutate(flag=ifelse(bar$interpret == "O",'NA',
                           ifelse(bar$lowci_chosen<=bar$measure_comp & bar$upci_chosen>=bar$measure_comp,'NS',
                                  ifelse(bar$lowci_chosen > bar$measure_comp & bar$interpret == "H", 'Better',
                                         ifelse(bar$lowci_chosen > bar$measure_comp & bar$interpret == "L", 'Worse',
                                                ifelse(bar$upci_chosen < bar$measure_comp & bar$interpret == "L", 'Better',
                                                       ifelse(bar$upci_chosen < bar$measure_comp & bar$interpret == "H", 'Worse','NS')))))))
      
      bar %>%
        select(c(indicator, areaname, areatype, def_period, numerator, measure,
                 lowci, upci, type_definition, topic, measure_comp, comparator, flag)) %>%
        rename(lower_confidence_interval=lowci, upper_confidence_interval=upci,
               latest_period = def_period, definition = type_definition, comparator_measure = measure_comp, difference=flag)
      
    })
    
    # Download barcode data
    output$download_bar <- downloadHandler( filename =  'barcode_data.csv',
                                             content = function(file) { write.csv(bar_csv(), file, row.names=FALSE) })
    
    # Downloading chart  
    output$download_barplot <- downloadHandler(
      filename = 'barcode.png',
      content = function(file){
        ggsave(file, plot = plot_barcode(), device = "png",width=15, limitsize=FALSE)
      })

###############################################.        
#### Time trend plot ----
###############################################.  
#####################.
# Reactive controls
  #Controls for chart. Dynamic selection of locality and iz.
  output$loc_ui_trend <- renderUI({
    selectInput("locname_trend", "HSC Locality", 
                choices = c("Select localities" = "", paste(unique(geo_lookup$areaname[
                  geo_lookup$parent_area == input$loc_iz_trend &
                    geo_lookup$areatype == 'HSC Locality' ]))),
                multiple=TRUE, selectize=TRUE, selected = "")
  })

  output$iz_ui_trend <- renderUI({
    selectInput("izname_trend", "Intermediate zone", 
                choices = c("Select intermediate zones" = "", 
                            paste(unique(geo_lookup$areaname[
                  geo_lookup$parent_area == input$loc_iz_trend &
                    geo_lookup$areatype == 'Intermediate zone' ]))),
                multiple=TRUE, selectize=TRUE, selected = "")
  })
  
#####################.
# Reactive data 
  #Time trend data. Filtering based on user input values.
  trend_data <- reactive({ 
    
    trend <- optdata %>% 
      subset((areaname %in% input$hbname_trend &  areatype == "Health board" |
               areaname %in% input$laname_trend & areatype == "Council area" |
               areaname %in% input$scotname_trend & areatype == "Scotland"  |
               areaname %in% input$adpname_trend  & areatype == "Alcohol & drug partnership" |
               areaname %in% input$locname_trend  & areatype == "HSC Locality" |
               areaname %in% input$partname_trend & areatype == "HSC Partnership"   |
               areaname %in% input$izname_trend & areatype == "Intermediate zone") & 
               indicator == input$indic_trend) %>% 
      droplevels() 
    
    trend$areaname_full <- as.factor(trend$areaname_full)
    trend <- trend[order(trend$year),] #Needs to be sorted by year for Plotly
  })
  
#####################.
# Creating plot
  #Function to create palette for trend plot
  create_trendpalette <- function(){
    #Creating palette of colors with a tone for each geography type
    #First obtaining length of each geography type
    hb_length <- length(input$hbname_trend)
    ca_length <- length(input$laname_trend)
    scot_length <- length(input$scotname_trend)
    adp_length <- length(input$adpname_trend)
    part_length <- length(input$partname_trend)
    loc_length <- length(input$locname_trend)
    iz_length <- length(input$izname_trend)
    colorblind_length <- hb_length+ca_length+scot_length+adp_length+part_length+
      loc_length+iz_length
    
    #Then creating a gradient scale for each geography type based on its length
    hb_scale <- scales::seq_gradient_pal("#08519c", "#9ecae1", "Lab")(seq(0,1,length.out=hb_length))
    ca_scale <- scales::seq_gradient_pal("#006d2c", "#a1d99b", "Lab")(seq(0,1,length.out=ca_length))
    scot_scale <- scales::seq_gradient_pal("#000000", "#000000", "Lab")(seq(0,1,length.out=scot_length))
    adp_scale <- scales::seq_gradient_pal("#FF0000", "#ffd6cc", "Lab")(seq(0,1,length.out=adp_length))
    part_scale <- scales::seq_gradient_pal("#FF0000", "#ffd6cc", "Lab")(seq(0,1,length.out=part_length))
    loc_scale <- scales::seq_gradient_pal("#dadaeb", "#54278f", "Lab")(seq(0,1,length.out=loc_length))
    iz_scale <- scales::seq_gradient_pal("#ffff66", "#4d4d00", "Lab")(seq(0,1,length.out=iz_length))
    colorblind_scale <- scales::seq_gradient_pal("#08306b", "#deebf7", "Lab")(seq(0,1,length.out=colorblind_length))
    
    #Then creating vector for each geography type with area names and color
    scot_cols <- setNames(scot_scale, unique(trend_data()$areaname_full[trend_data()$areatype == "Scotland"]))
    hb_cols <- setNames(hb_scale, unique(trend_data()$areaname_full[trend_data()$areatype == "Health board"]))
    ca_cols <- setNames(ca_scale, unique(trend_data()$areaname_full[trend_data()$areatype == "Council area"]))
    adp_cols <- setNames(adp_scale, unique(trend_data()$areaname_full[trend_data()$areatype == "Alcohol & drug partnership"]))
    part_cols <- setNames(part_scale, unique(trend_data()$areaname_full[trend_data()$areatype == "HSC Partnership"]))
    loc_cols <- setNames(loc_scale, unique(trend_data()$areaname_full[trend_data()$areatype == "HSC Locality"]))
    iz_cols <- setNames(iz_scale, unique(trend_data()$areaname_full[trend_data()$areatype == "Intermediate zone"]))
    colorblind_cols <- c(setNames(colorblind_scale, unique(trend_data()$areaname_full)))
    
    #Combining them all in the final palette. Using different palette if colour bling option checked
    if(input$colorblind_trend == FALSE){
      trend_col <- c(scot_cols, hb_cols, ca_cols, adp_cols, part_cols, loc_cols, iz_cols)
    }
    else{
      trend_col <- colorblind_cols
    }
  }
  
  #Title of plot
  output$title_trend <- renderText(paste0(input$indic_trend))
  

#Plot 
  output$trend_plot <- renderPlotly({
    #If no data available for that period then plot message saying data is missing
    if (is.data.frame(trend_data()) && nrow(trend_data()) == 0)
    {
      plot_nodata()
    }
    else { #If data is available then plot it
      
      #Creating palette of colors with a tone for each geography type
      trend_col <-create_trendpalette()
      
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
               margin = list(b = 160, t=5), #to avoid labels getting cut out
               yaxis = list(title = ~type_definition, rangemode="tozero", fixedrange=TRUE,
                            size = 4, titlefont =list(size=12), tickfont =list(size=11)), 
               xaxis = list(title = FALSE, tickfont =list(size=10), tickangle = 270, fixedrange=TRUE),
               font = list(family = '"Helvetica Neue", Helvetica, Arial, sans-serif')) %>%  
        config(displayModeBar = FALSE, displaylogo = F, collaborate=F, editable =F) # taking out plotly logo and collaborate button
      
    }
  }) 
  
#####################.
# Downloading data and plot
  #Function in ggplot to be able to save chart
  plot_trend_ggplot <- function(){
    
    trend_col <- create_trendpalette() #palette
    
    #Creating time trend plot
    ggplot(data=trend_data(), aes(y = measure,  x = trend_axis, group = areaname_full))+
      geom_line(aes(color=areaname_full))+
      geom_point(aes(color=areaname_full))+
      labs(title=title_wrapper(unique(trend_data()$indicator), width = 40), 
           y = unique(trend_data()$type_definition))+
      scale_color_manual(values=trend_col, name = "")+
      scale_y_continuous(expand = c(0, 2), limits=c(0, max(trend_data()$measure)+ (max(trend_data()$measure)/100)))+
      #Layout
      theme(text = element_text(size=11, family="Helvetica Neue,Helvetica,Arial,sans-serif"),
            axis.text.x = element_text(angle=90),
            axis.line.x = element_line(),
            axis.ticks = element_blank(),
            aspect.ratio=0.3,
            plot.title = element_text(hjust = 0.5), #centering title
            axis.title.x = element_blank(), #taking out y axis title
            legend.key=element_blank(), #taking out background from legend
            panel.grid.major = element_line(colour="#F0F0F0"),#grid lines
            panel.background = element_blank() #Blanking background
      )
  } 
  
  
  #Downloading data
  trend_csv <- reactive({ format_csv(trend_data()) })
  
  output$download_trend <- downloadHandler(filename =  'timetrend_data.csv',
    content = function(file) {write.csv(trend_csv(), file, row.names=FALSE)})
  
  # Downloading chart  
  output$download_trendplot <- downloadHandler(
    filename = 'trend.png',
    content = function(file){
      ggsave(file, plot = plot_trend_ggplot(), device = "png", scale=3, limitsize=FALSE)
    })
  
  
#####################################.       
#### Rank plot ----
###############################################.   
#####################.
# Reactive controls
  #Dropdown for time period based on indicator selection  
  output$year_ui_rank <- renderUI({
    time_period <- sort(unique(optdata$trend_axis[optdata$indicator == input$indic_rank&
                                                    optdata$areatype == input$geotype_rank]))
    
    selectInput("year_rank", "Time period",
                choices = time_period, selected = last(time_period))
  })
  
  #Dropdown for geotype based on what data is available for that indicator
  output$geotype_ui_rank <- renderUI({
    areas <- sort(unique(optdata$areatype[optdata$indicator == input$indic_rank]))
    areas <- areas [! areas %in% c("Scotland")] #taking out Scotland
    selectInput("geotype_rank", label = "Geography level",
                choices = areas, selected = "Health board")
  })
  
#####################.
# Reactive data  
  # Comparator data rank plot. Could be moved inside rank_bar_data
  rank_compar <- reactive({
    #Fiddly way of selecting period because some cases (e.g. Life expectancy)
    #might have different periods for the same year, e.g. IZ 2013 is 2011-2015
    #and HB 2013 is 2012-2014.
    year_chosen <- unique(optdata$year[optdata$trend_axis == input$year_rank])
    
    rank_compar <- optdata %>% subset(year %in% year_chosen & 
             areatype %in% c("Health board", "Council area", "Scotland") &
             areaname == input$geocomp_rank &
             indicator == input$indic_rank) %>% 
      droplevels()
  })
  
  #Rank plot data based on user input
  rank_bar_data <- reactive({
    #Makes different subsets depending on the geography type selected by the user
    if (input$geotype_rank %in% c("Scotland", "Health board", "Council area", 
                                  "Alcohol & drug partnership", "HSC Partnership"))
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
  
  #Title of plot
  output$title_rank <- renderText(paste0(input$indic_rank, " - ", unique(rank_bar_data()$def_period)))
  
  
  ############################.
  # Creating  plot
  output$rank_plot <- renderPlotly({
    
    #If no data available for that period then plot message saying data is missing
    if (is.data.frame(rank_bar_data()) && nrow(rank_bar_data()) == 0)
    {
      plot_nodata()
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
               yaxis = list(title = ~type_definition, titlefont =list(size=11), 
                            tickfont =list(size=11), fixedrange=TRUE),
               xaxis = list(title = "", tickangle = 270, fixedrange=TRUE,
                            tickfont =list(size=11), #axis parameters
                            categoryorder="array", #order of plotting
                            categoryarray = order_areas),
               font = list(family = '"Helvetica Neue", Helvetica, Arial, sans-serif'),
               margin=list(b = 160, t = 5), # to prevent labels getting cut out
               hovermode = 'false') %>% # to get hover compare mode as default
        config(displayModeBar = FALSE, displaylogo = F, collaborate=F, editable =F)
      
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
  
############################.
#Downloading plot and data
  # Function to save plot
  plot_rank_ggplot <- function(){
    #Coloring based on if signicantly different from comparator
    color_pal <- ifelse(rank_bar_data()$interpret == "O", '#ccccff',
                        ifelse(is.na(rank_bar_data()$lowci) | is.na(rank_bar_data()$upci) | is.na(rank_bar_data()$comp_value) | is.na(rank_bar_data()$measure) |rank_bar_data()$measure == 0, '#ccccff',
                               ifelse(rank_bar_data()$lowci <= rank_bar_data()$comp_value & rank_bar_data()$upci >= rank_bar_data()$comp_value,'#999999',
                                      ifelse(rank_bar_data()$lowci > rank_bar_data()$comp_value & rank_bar_data()$interpret == "H", '#3d99f5',
                                             ifelse(rank_bar_data()$lowci > rank_bar_data()$comp_value & rank_bar_data()$interpret == "L", '#ff9933',
                                                    ifelse(rank_bar_data()$upci < rank_bar_data()$comp_value & rank_bar_data()$interpret == "L", '#3d99f5',
                                                           ifelse(rank_bar_data()$upci < rank_bar_data()$comp_value & rank_bar_data()$interpret == "H", '#ff9933', '#ccccff')))))))
    
    
    #Creating a vector with the area names in the order they are going to be plotted
    color_pal <- setNames(color_pal, rank_bar_data()$areaname)
    
    #title for rank
    title_rank <- paste0(input$indic_rank, " - ", unique(rank_bar_data()$def_period))
    
    # General plot and layout, bars with or without error bars will be added after user input
    p <- ggplot(data=rank_bar_data(), aes(y = measure,  x = reorder(areaname, -measure)))+
      geom_bar(aes(fill=areaname), stat = "identity")+
      geom_hline(aes(yintercept=comp_value, color="red"))+
      labs(title=title_wrapper(title_rank, width = 40), y=unique(rank_bar_data()$type_definition))+
      scale_fill_manual(values=color_pal, name = "")+
      scale_y_continuous(expand = c(0, 0), limits=c(0, max(rank_bar_data()$upci)))+
      #Layout
      theme(text = element_text(size=11, family="Helvetica Neue,Helvetica,Arial,sans-serif"),
            axis.text.x = element_text(angle=90, hjust=1), 
            axis.line.x = element_line(), 
            axis.ticks = element_blank(),
            aspect.ratio=0.3,
            plot.title = element_text(hjust = 0.5), #centering title
            axis.title.x = element_blank(), #taking out x axis title
            legend.position = "none", #taking out background from legend
            panel.grid.major.y = element_line(colour="#F0F0F0"),
            panel.background = element_blank() #Blanking background
      )
    
    #Respond to user input regarding confidence intervals
    if (input$ci_rank == FALSE) {  
      p <- p 
      
    } else{ 
      #adding bar layer with error bars
      p <-  p +   geom_errorbar(aes(ymin=lowci, ymax=upci), width=.2,
                                position=position_dodge(.9))
      
    }
  }
  
  #Downloading data
  rank_csv <- reactive({ format_csv(rank_bar_data()) })

  output$download_rank <- downloadHandler(filename =  'rank_data.csv',
    content = function(file) {write.csv(rank_csv(), file, row.names=FALSE) })
  
  #Downloading chart
  output$download_rankplot <- downloadHandler(
    filename = 'rank.png',
    content = function(file){
      ggsave(file, plot = plot_rank_ggplot(), device = "png", scale=3, limitsize=FALSE)
    })
  
#####################################.    
### Map ----
#####################################. 
#####################.
# Reactive controls
  #Dynamic selection of the last period available based on indicator selected
  output$year_ui_map <- renderUI({
    time_period <- sort(unique(optdata$trend_axis[optdata$indicator == input$indic_map  &
                                                    optdata$areatype == input$geotype_map]))
    
    selectInput("year_map", "Time period",
                choices = time_period, selected = last(time_period))
  })
  
  #Dropdown for geotype based on what data is available for that indicator
  output$geotype_ui_map <- renderUI({
    areas <- sort(unique(optdata$areatype[optdata$indicator == input$indic_map]))
    #taking out areas without shapefiles
    areas <- areas [! areas %in% c("Scotland", "HSC Locality", 
                                   "Alcohol & drug partnership")]
    selectInput("geotype_map", label = "Geography level",
                choices = areas, selected = "Health board")
  })
  
  # Years to compare with depending on what data is available
  output$yearcomp_ui_map <- renderUI({
    map_pol <- optdata %>% subset(areatype == input$geotype_map & 
                                    indicator==input$indic_map)
    
    years <- c(min(map_pol$year):max(map_pol$year))
    periods <- c(sort(paste0(unique(map_pol$trend_axis[map_pol$year>=min(map_pol$year) &
                                                         map_pol$year<=max(map_pol$year)]))))
    
    selectInput("yearcomp_map", "Baseline year", choices = periods,
                selectize=TRUE)
  })
  
  #####################.
  # Dynamic data
  # Dataset for comparator chosen by user
  map_compar <- reactive({
    
    if (input$comp_map == 1){
      #Fiddly way of selecting period because some cases (e.g. Life expectancy)
      #might have different periods for the same year, e.g. IZ 2013 is 2011-2015
      #and HB 2013 is 2012-2014.
      year_chosen <- unique(optdata$year[optdata$trend_axis == input$year_map])
      
      map_compar <- optdata %>% subset(year %in% year_chosen & 
                                         areatype %in% c("Health board", "Council area", "Scotland") &
                                         areaname == input$geocomp_map &
                                         indicator == input$indic_map) %>% 
        droplevels()
      
    } else if (input$comp_map == 2) { #if time comparison selected
      
      map_compar <- optdata %>% subset(areatype == input$geotype_map &
                                         trend_axis == input$yearcomp_map & 
                                         indicator==input$indic_map) %>% 
        rename(comp_value = measure, comp_name = areaname) %>% 
        select(code, comp_value, comp_name) %>% #to allow merging
        droplevels()
      
    }
  }) 
  
  #Dataset for area type chosen by user
  map_chosenarea <- reactive({ 
    map_chosenarea <- optdata %>% 
      subset(areatype == input$geotype_map &
               trend_axis==input$year_map & 
               indicator==input$indic_map) %>% 
      droplevels() #dropping missing factor levels to allow merging
    
    if (input$comp_map == 1) { #for area comparisons
      map_chosenarea <- map_chosenarea %>% 
        mutate(comp_value = map_compar()$measure, #comparator value and name
               comp_name = map_compar()$areaname) 
      
    } else if (input$comp_map == 2) { #if time comparison selected
      #This helps to deals with cases of incomplete data, e.g. 2011 has all HB but 2013 not.
      #Works for those cases where there are more data in the past (comparator)
      #and one for those which have more data in the present (chosen area)
      map_chosenarea <- merge(x = map_chosenarea, y = map_compar(), by = "code", 
                              all.x = T)

    }
  }) 
  
  #Merging shapefile with dynamic selection of data
  poly_map <- reactive({
    if (input$geotype_map == "Council area"){
      map_pol <- merge(ca_bound, map_chosenarea(), by='code')
    } else if(input$geotype_map == "Health board"){
      map_pol <- merge(hb_bound, map_chosenarea(), by='code')
    } else if(input$geotype_map == "HSC Partnership"){
      map_pol <- merge(hscp_bound, map_chosenarea(), by='code')
    } else if(input$geotype_map == "Intermediate zone"){
      iz_bound <- iz_bound %>% subset(council == input$iz_map)
      
      map_pol <- merge(iz_bound, map_chosenarea(), by='code')
    }
    
  }) 
  
  #####################.
  # Plotting map
  #title of the map. if no data available then print "No data available"
  output$title_map <- renderText(
    if(is.data.frame(map_csv()) && nrow(map_csv()) == 0) {
      "No data available"
    } else {
      paste0(input$indic_map, " - ", unique(poly_map()$def_period))
    })
  
  #Function to create color palette based on if signicantly different from comparator
  create_map_palette <- function(){
    ifelse(poly_map()$interpret == "O", '#ffffff',
           ifelse(is.na(poly_map()$lowci) | is.na(poly_map()$upci) | is.na(poly_map()$comp_value) | is.na(poly_map()$measure) |poly_map()$measure == 0, '#ffffff',
                  ifelse(poly_map()$lowci <= poly_map()$comp_value & poly_map()$upci >= poly_map()$comp_value,'#999999',
                         ifelse(poly_map()$lowci > poly_map()$comp_value & poly_map()$interpret == "H", '#3d99f5',
                                ifelse(poly_map()$lowci > poly_map()$comp_value & poly_map()$interpret == "L", '#ff9933',
                                       ifelse(poly_map()$upci < poly_map()$comp_value & poly_map()$interpret == "L", '#3d99f5',
                                              ifelse(poly_map()$upci < poly_map()$comp_value & poly_map()$interpret == "H", '#ff9933', '#ffffff')))))))
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
  
  #####################.
  # Downloading data
  #Function to create map that can be downloaded
  plot_map_download <- function(){
    color_map <- create_map_palette() #palette
    plot(poly_map(), col=color_map)
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
  
  ###############################################.        
  #### Deprivation ----
  ###############################################.   
  #####################.
  # Reactive controls
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
  
  #####################.
  # Reactive data
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
      plot_nodata()
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
               font = list(family = '"Helvetica Neue", Helvetica, Arial, sans-serif'),
               yaxis = list(title = ~type_definition, titlefont =list(size=10), 
                            tickfont =list(size=11), fixedrange=TRUE),
               xaxis = list(showline = TRUE, title = FALSE, showticklabels = FALSE), fixedrange=TRUE) %>%
        config(displayModeBar = TRUE, displaylogo = F, collaborate=F, editable =F) # taking out plotly logo and collaborate button
    }
  })

  #Plotting
  output$simd_trend_plot <- renderPlotly({
    #If no data available for that period then plot message saying data is missing
    if (is.data.frame(simd_trend_data()) && nrow(simd_trend_data()) == 0)
    {
      plot_nodata()
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
                 xaxis = list(title = " ", tickfont =list(size=11), tickangle = 270),  #axis parameter
                 margin=list(pad = 50, l = 160, r = 200, b = 160),
                 font = list(family = '"Helvetica Neue", Helvetica, Arial, sans-serif'),
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
                 yaxis = list(title = FALSE, rangemode="tozero", fixedrange=TRUE,
                              size = 4, tickfont =list(size=11)),
                 xaxis = list(title = "", tickfont =list(size=11), tickangle = 270, fixedrange=TRUE),  #axis parameter
                 font = list(family = '"Helvetica Neue", Helvetica, Arial, sans-serif'),
                 margin=list(b = 160),
                 showlegend = FALSE) %>%
          config(displayModeBar = TRUE, displaylogo = F, collaborate=F, editable =F) # taking out plotly logo and collaborate button

      }
    }
  })
  
  #####################.
  # Downloading data
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
  #### Table ----
  #####################################.      
  #Filter iz_list and hscl list by parent area selection
  interzone_filtered <- reactive({ sort(parent_iz_list$areaname[parent_iz_list$parent_area==input$iz_parent]) })
  hsclocality_filtered <- reactive({ sort(parent_hscl_list$areaname[parent_hscl_list$parent_area==input$hscl_parent]) })
  
  #Filter IZ's by Parent area
  
  output$iz_filtered <- renderUI ({ 
    if (input$iz_parent == "Show all"){ selectizeInput("iz_true", label = NULL,
                                                       width = "229px", choices = intzone_name, selected = NULL, multiple=TRUE, options = list(maxOptions = 1300, placeholder = "Select or type specific intermediate zone")) 
    } else {selectizeInput("iz_true", label = NULL,
                           width = "229px", choices = interzone_filtered(), selected = NULL, multiple=TRUE, options = list(placeholder = "Select or type specific intermediate zone"))}
  }) 
  
  #select all IZ's belonging to a certain parent-area
  observe({
    if (input$iz_parent_all == "FALSE")
      return(if (input$iz_parent == "Show all"){ updateSelectizeInput(session,"iz_true", label = NULL,
                                                                      choices = intzone_name, selected = character(0), options = list(maxOptions = 1300, placeholder = "Select or type specific intermediate zone")) 
      } else {updateSelectizeInput(session,"iz_true", label = NULL,
                                   choices = interzone_filtered(), selected = character(0), options = list(placeholder = "Select or type specific intermediate zone")) })
    
    isolate({
      updateSelectizeInput(session, "iz_true", label = NULL,
                           choices = intzone_name, selected = interzone_filtered(), options = list(placeholder = "Select or type specific intermediate zone")) })
  })
  
  #when you change initial filter, clear the second list of geographies anc checkbox
  observeEvent(input$iz_parent, {
    updateCheckboxInput(session, "iz_parent_all", label = NULL, value = FALSE)
    if (input$iz_parent == "Show all"){ selectizeInput("iz_true", label = NULL,
                                                       width = "229px", choices = intzone_name, selected = NULL, multiple=TRUE, options = list(maxOptions = 1300, placeholder = "Select or type specific intermediate zone")) 
    } else {selectizeInput("iz_true", label = NULL,
                           choices = interzone_filtered(), selected = character(0), multiple=TRUE, options = list(placeholder = "Select or type specific intermediate zone"))}
    
  })
  
  #Filter HSCL's by parent area
  
  output$hscl_filtered <- renderUI ({ 
    if (input$hscl_parent == "Show all"){ selectizeInput("hscl_true", label = NULL,
                                                         width = "229px", choices = locality_name, selected = NULL, multiple=TRUE, options = list(maxOptions = 1300, placeholder = "Select or type specific HSC locality")) 
    } else {selectizeInput("hscl_true", label = NULL,
                           width = "229px", choices = hsclocality_filtered(), selected = NULL, multiple=TRUE, options = list(placeholder = "Select or type specific HSC locality"))}
  }) 
  
  #select all IZ's belonging to a certain parent-area
  observe({
    if (input$hscl_parent_all == "FALSE")
      return(if (input$hscl_parent == "Show all"){ updateSelectizeInput(session,"hscl_true", label = NULL,
                                                                        choices = locality_name, selected = character(0), options = list(maxOptions = 1300, placeholder = "Select or type specific HSC locality")) 
      } else {updateSelectizeInput(session,"hscl_true", label = NULL,
                                   choices = hsclocality_filtered(), selected = character(0), options = list(placeholder = "Select or type specific HSC locality"))})
    
    isolate({
      updateSelectizeInput(session, "hscl_true", label = NULL,
                           choices = locality_name, selected = hsclocality_filtered(),options = list(placeholder = "Select or type specific HSC locality")) })
  })
  
  #when you change initial filter, clear the second list of geographies anc checkbox
  observeEvent(input$hscl_parent, {
    updateCheckboxInput(session, "hscl_parent_all", label = NULL, value = FALSE)
    if (input$hscl_parent == "Show all"){ selectizeInput("hscl_true", label = NULL,
                                                         width = "229px", choices = locality_name, selected = NULL, multiple=TRUE, options = list(maxOptions = 1300, placeholder = "Select or type specific HSC locality")) 
    } else {selectizeInput("hscl_true", label = NULL,
                           choices = hsclocality_filtered(), selected = character(0), multiple=TRUE, options = list(placeholder = "Select or type specific HSC locality"))}
    
  })
  
  
  #to clear choices when boxes are unticked/radio button is changed
  observeEvent(input$iz=="FALSE", {
    updateCheckboxInput(session, "iz_parent_all", label = NULL, value = FALSE)
    updateSelectizeInput(session, "iz_true", label = NULL,
                         choices = intzone_name, selected = character(0), options = list(placeholder = "Select or type specific intermediate zone")) 
    updateSelectizeInput(session, "iz_parent", label = "Filter list by HSC Partnership", selected = "Show all")})
  
  observeEvent(input$la=="FALSE", {
    updateSelectizeInput(session, "la_true", label = NULL,
                         choices = la_name, selected = character(0), options = list(placeholder = "Select or type specific council area")) })
  observeEvent(input$hb=="FALSE", {
    updateSelectizeInput(session, "hb_true", label = NULL,
                         choices = hb_name, selected = character(0), options = list(placeholder = "Select or type specific health board")) })
  observeEvent(input$hscl=="FALSE", {
    updateSelectizeInput(session, "hscl_true", label = NULL,
                         choices = locality_name, selected = character(0), options = list(placeholder = "Select or type specific HSC locality"))
    updateSelectizeInput(session, "hscl_parent", label = "Filter list by HSC Partnership", selected = "Show all")})
  observeEvent(input$hscp=="FALSE", {
    updateSelectizeInput(session, "hscp_true", label = NULL,
                         choices = partnership_name, selected = character(0), options = list(placeholder = "Select or type specific HSC partnership")) })
  observeEvent(input$adp=="FALSE", {
    updateSelectizeInput(session, "adp_true", label = NULL,
                         choices = adp_name, selected = character(0), options = list(placeholder = "Select or type specific ADP")) })
  observeEvent(input$product_filter, {
    updateSelectizeInput(session,"indicator_filter", label = NULL,
                         choices = indicator_list, selected = character(0),
                         options = list(maxOptions = 1000, placeholder = "Click or type indicators you would like to filter by"))
    updateSelectizeInput(session,"topic_filter", label = NULL, choices = topic_list, selected = NULL,
                         options = list(maxOptions = 1000, placeholder = "Click or type topics you would like to filter by"))
    updateSelectizeInput(session,"profile_filter", label = NULL, choices = profile_list, selected = NULL,
                         options = list(maxOptions = 1000, placeholder = "Click or type profiles you would like to filter by"))
    
  })
  
  filter_table <- reactive ({
    
    #if list of indicators selected
    if (!is.null(input$indicator_filter)) { 
      if (input$all_data == TRUE) {
        filtered_geos <- optdata %>%  filter(year>=input$date_from[1] & year<=input$date_from[2]) %>% 
          filter(indicator %in% input$indicator_filter)
      } else {filtered_geo <- optdata %>% filter((areaname %in% input$iz_true & areatype == "Intermediate zone")|
                                                   (areaname %in% input$la_true & areatype == "Council area")|
                                                   (areaname %in% input$hb_true & areatype == "Health board")|
                                                   (areaname %in% input$hscl_true & areatype == "HSC Locality")|
                                                   (areaname %in% input$hscp_true & areatype == "HSC Partnership")|
                                                   (code %in% input$code)
      ) %>% filter(year>=input$date_from[1] & year<=input$date_from[2]) %>% filter(indicator %in% input$indicator_filter)
      
      filtered_geo2 <- if (input$scotland == TRUE) {
        optdata %>% filter(areaname == "Scotland") %>% filter(year>=input$date_from[1] & year<=input$date_from[2]) %>% 
          filter(indicator %in% input$indicator_filter)
        
      }      
      
      filtered_geos <- rbind(filtered_geo,filtered_geo2)
      
      }
      
      table <- filtered_geos %>% subset(select=c("code", "areaname", "areatype", "indicator","year", 
                                                 "def_period","numerator", "measure", "lowci","upci","type_definition"))
      
      #if list of topics selected
    } else if (!is.null(input$topic_filter)) { 
      if (input$all_data == TRUE) {
        filtered_geos <- optdata %>%  filter(year>=input$date_from[1] & year<=input$date_from[2]) %>% 
          filter((domain1 %in% input$topic_filter)|(domain2 %in% input$topic_filter)|(domain3 %in% input$topic_filter))
      } else {filtered_geo <- optdata %>% filter((areaname %in% input$iz_true & areatype == "Intermediate zone")|
                                                   (areaname %in% input$la_true & areatype == "Council area")|
                                                   (areaname %in% input$hb_true & areatype == "Health board")|
                                                   (areaname %in% input$hscl_true & areatype == "HSC Locality")|
                                                   (areaname %in% input$hscp_true & areatype == "HSC Partnership")|
                                                   (code %in% input$code)
      ) %>% filter(year>=input$date_from[1] & year<=input$date_from[2]) %>% 
        filter((domain1 %in% input$topic_filter)|(domain2 %in% input$topic_filter)|(domain3 %in% input$topic_filter))
      
      filtered_geo2 <- if (input$scotland == TRUE) {
        optdata %>% filter(areaname == "Scotland") %>% filter(year>=input$date_from[1] & year<=input$date_from[2]) %>% 
          filter((domain1 %in% input$topic_filter)|(domain2 %in% input$topic_filter)|(domain3 %in% input$topic_filter))
        
      }  
      
      filtered_geos <- rbind(filtered_geo,filtered_geo2)
      
      } 
      
      table <- filtered_geos %>% subset(select=c("code", "areaname", "areatype", "indicator","year", 
                                                 "def_period","numerator", "measure", "lowci","upci","type_definition"))
      
      #if list of profiles selected    
    } else if (!is.null(input$profile_filter)) { 
      if (input$all_data == TRUE) {
        filtered_geos <- optdata %>%  filter(year>=input$date_from[1] & year<=input$date_from[2]) %>% 
          filter((grepl((paste("^",input$profile_filter,sep="",collapse="|")),profile_domain1))|(grepl((paste("^",input$profile_filter,sep="",collapse="|")),profile_domain2)))
      } else {filtered_geo <- optdata %>% filter((areaname %in% input$iz_true & areatype == "Intermediate zone")|
                                                   (areaname %in% input$la_true & areatype == "Council area")|
                                                   (areaname %in% input$hb_true & areatype == "Health board")|
                                                   (areaname %in% input$hscl_true & areatype == "HSC Locality")|
                                                   (areaname %in% input$hscp_true & areatype == "HSC Partnership")|
                                                   (code %in% input$code)
      ) %>% filter(year>=input$date_from[1] & year<=input$date_from[2]) %>% 
        filter((grepl((paste("^",input$profile_filter,sep="",collapse="|")),profile_domain1))|(grepl((paste("^",input$profile_filter,sep="",collapse="|")),profile_domain2)))
      
      filtered_geo2 <- if (input$scotland == TRUE) {
        optdata %>% filter(areaname == "Scotland") %>% filter(year>=input$date_from[1] & year<=input$date_from[2]) %>% 
          filter((grepl((paste("^",input$profile_filter,sep="",collapse="|")),profile_domain1))|(grepl((paste("^",input$profile_filter,sep="",collapse="|")),profile_domain2)))
        
      }
      
      filtered_geos <- rbind(filtered_geo,filtered_geo2)
      
      } 
      
      table <- filtered_geos %>% subset(select=c("code", "areaname", "areatype", "indicator","year", 
                                                 "def_period","numerator", "measure", "lowci","upci","type_definition"))
    } else { 
      if (input$all_data == TRUE) {
        filtered_geos <- optdata %>%  filter(year>=input$date_from[1] & year<=input$date_from[2])
        
      } else {filtered_geo <- optdata %>% filter((areaname %in% input$iz_true & areatype == "Intermediate zone")|
                                                   (areaname %in% input$la_true & areatype == "Council area")|
                                                   (areaname %in% input$hb_true & areatype == "Health board")|
                                                   (areaname %in% input$hscl_true & areatype == "HSC Locality")|
                                                   (areaname %in% input$hscp_true & areatype == "HSC Partnership")|
                                                   (code %in% input$code)
      ) %>% filter(year>=input$date_from[1] & year<=input$date_from[2])
      
      filtered_geo2 <- if (input$scotland == TRUE) {
        optdata %>% filter(areaname == "Scotland") %>% filter(year>=input$date_from[1] & year<=input$date_from[2])
        
      }
      
      filtered_geos <- rbind(filtered_geo,filtered_geo2)
      
      } 
      
      table <- filtered_geos %>% subset(select=c("code", "areaname", "areatype", "indicator","year", 
                                                 "def_period","numerator", "measure", "lowci","upci","type_definition"))
      
    }
    
    
  })
  
  #display table based on selection made by user on indicator tab
  output$table_filtered <- DT::renderDataTable({
    
    DT::datatable(filter_table(),
                  style = 'bootstrap', rownames = FALSE, options = list(dom = 'tp', columnDefs = list(list(visible=FALSE, targets=c(4,8,9)))), 
                  colnames = c("Area code", "Area", "Type", "Indicator", "Year","Period", "Numerator", 
                               "Measure", "Lower CI","Upper CI", "Definition" )
    )
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
                         options = list(maxOptions = 1000, placeholder = "Click or type topics you would like to filter by"))
    updateSelectizeInput(session,"profile_filter", label = NULL, choices = profile_list, selected = NULL,
                         options = list(maxOptions = 1000, placeholder = "Click or type profiles you would like to filter by"))
    updateAwesomeRadio(session,"product_filter", label=NULL, choices = c("Indicator", "Topic", "Profile"), selected = NULL, inline = FALSE,
                       status = "primary", checkbox = TRUE)
    
  })
  
  
  #Downloading data in csv format
  table_csv <- reactive({ format_csv(filter_table()) })
  
  #The filters the user applies in the data table will determine what data they download - indicator tab table
  output$download_table_csv <- downloadHandler(
    filename ="scotpho_data_extract.csv",
    content = function(file) {
      write.csv(table_csv(),
                file, row.names=FALSE) } 
  )
  
} #server closing bracket

#########################  END ----








