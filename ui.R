#Code to create ScotPHO's Shiny profile platform
# This script includes the user-interface definition of the app.
#
#TODO:
#see global syntax


###############################################.
## Header ---- 
###############################################.
navbarPage(id = "intabset", #needed for landing page
           title = div(tags$a(img(src="scotpho_reduced.png", height=40), href= "http://www.scotpho.org.uk/"),
                       style = "position: relative; top: -5px;"), # Navigation bar
           windowTitle = "ScotPHO profiles", #title for browser tab
           theme = shinytheme("cerulean"), #Theme of the app (blue navbar)
           collapsible = TRUE, #tab panels collapse into menu in small screens
           header =         
             tags$head( #CSS styles
               beta_box,  ##### Feedback box. TAKE OUT AFTER BETA PERIOD
               tags$link(rel="shortcut icon", href="favicon_scotpho.ico"), #Icon for browser tab
               #Including Google analytics
               includeScript("google-analytics.js"),
               #Style sidebars/well panels
               tags$style(".well {background-color:#ffffff; border: 0px solid #336699;
                          padding: 5px; box-shadow: none; }",
                          #Background colour of header navBar
                          ".navbar-brand {background-color: white}",
                          ".navbar {font-size: 12px; border: 0}", #font size and border
                          ".dropdown-menu { font-size: 12px;}", #dropdown menu within navBar
                          ".container-fluid {padding-right: 0px}",
                          #Text size and line height. Padding needed for footer
                          "body { font-size: 11px; line-height: 1.1; padding-bottom:30px}",
                          ".checkbox label, .radio label, .checkbox-bs label, .radio-bs label
                          { line-height: 1.6 }",
                          ".radio-inline {line-height: 2}",
                          #Padding and margins of filters and labels
                          ".form-group {margin: 3px}",
                          ".li-custom {margin-bottom: 10px;}", #bullet point list items
                          ".shiny-options-group { margin-top: 3px; }",
                          ".selectize-control { margin-bottom: 3px}",
                          ".selectize-input {padding: 3px 3px; min-height: 10px}",
                          ".control-label { margin-bottom: 1px}",
                          #Padding of columns
                          ".col-xs-1, .col-sm-1, .col-md-1, .col-lg-1, .col-xs-2, .col-sm-2, .col-md-2, 
                          .col-lg-2, .col-xs-3, .col-sm-3, .col-md-3, .col-lg-3, .col-xs-4, .col-sm-4, 
                          .col-md-4, .col-lg-4, .col-xs-5, .col-sm-5, .col-md-5, .col-lg-5, .col-xs-6, 
                          .col-sm-6, .col-md-6, .col-lg-6, .col-xs-7, .col-sm-7, .col-md-7, .col-lg-7, 
                          .col-xs-8, .col-sm-8, .col-md-8, .col-lg-8, .col-xs-9, .col-sm-9, .col-md-9, 
                          .col-lg-9, .col-xs-10, .col-sm-10, .col-md-10, .col-lg-10, .col-xs-11, .col-sm-11, 
                          .col-md-11, .col-lg-11, .col-xs-12, .col-sm-12, .col-md-12, .col-lg-12 {
                          padding-left: 5px; padding-right: 5px;}",
                          #Style for download buttons
                          ".down{background-color:#4da6ff; color: white; background-image:none; min-width: 20vh;
                          font-size: 11px; padding: 5px 10px; margin-top: 5px; margin-left: 3px}",
                          #landing page boxes
                          ".landing-page-box {width:100%; height:100%; min-height:18vh; background-color:AliceBlue;
                          border: 1px solid #AAAAAA; margin-bottom: 5px; float: left; transition: 0.5s ease;}",
                          ".landing-page-box-about {width:100%; height:100%; min-height:9vh; background-color:AliceBlue;
                          border: 1px solid #AAAAAA; margin-bottom: 5px; float: left; transition: 0.5s ease; }",
                          ".landing-page-box:hover, .landing-page-box-about:hover {-webkit-transform: scale(1.05); 
                          -ms-transform: scale(1.05); transform: scale(1.05); }", #hover effect on boxes
                          #landing-page titles for boxes
                          ".landing-page-box-title {font-size: 14px; text-align:center; color: darkblue;
                          font-weight: bold; background-color: none; width:100%; max-height: 20px; margin-top: 10px; }",
                          #landing page titles for ABOUT boxes
                          ".landing-page-box-about-title {font-size: 14px; text-align:center; color: darkblue;
                          font-weight: bold; background-color: none; width:100%; max-height: 20px; margin-top: 5px; }",
                          #landing page buttons
                          ".landing-page-button {text-align:center;
                          background-image:none; color: black; white-space: normal; border-radius: 0;border: 0px;
                          font-size: 16px; position: static; min-height: 18vh; width: 100%; opacity: 0;}",
           
               
               ".landing-page-button-about {text-align:center;
               background-image:none; color: black; white-space: normal; border-radius: 0; border:0px;
               font-size: 14px; position: static; height: 7vh; width: 100%; opacity:0;}",
               ".landing-page-button:hover , .landing-page-button:active , .landing-page-button-about:hover, .landing-page-button-about:active {opacity: 1; 
               background-color: #fff; /* fallback */
               background-color: rgba(255, 255, 255, 0.8);
               color: darkblue;
               border-color: #fff; /* fallback */
               border-color: rgba(255, 255, 255, 0.8); transition: background-color 0.3s ease-in,
               color 0.3s ease-in;}",
               #landing-page column 
               ".landing-page-column {padding-right:3vh}",
               #landing-page icons
               ".icon-lp{font-size: 1.3em; padding-right: 4px;}",
               ".home-icon {font-size: 1.3em; width: 0.7em;}",
               #to avoid red text error messages in the whole app, take out for testing
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }",
               #External links underlined an open a new tab
               ".externallink{text-decoration: underline;} "),
               HTML("<base target='_blank'>")
               ),
###############################################.
## Landing page ----
###############################################.
tabPanel(
  title = " ", icon = icon("home", "home-icon"),
  mainPanel(
    width = 11, style="margin-left:4%; margin-right:4%",
    fluidRow(h4("Welcome to the ScotPHO profiles", style="margin-top:0px;")),
    fluidRow(h5("Explore data by profile or topic area", style="margin-top:0px;")),
    fluidRow(
      #Ring plot box
      column(4, class="landing-page-column",
             div(class="landing-page-box", style="background-image: url(donut9.png);
                 background-repeat: no-repeat; background-size: 35%; background-position: bottom 20px center; ;background-color: white;",
                 div("Profile Summary", class = "landing-page-box-title"),
                 actionButton('jump_to_ring', 'A high level view of an area across a suit of indicators', 
                              class="landing-page-button", 
                              icon = icon("arrow-circle-right", "icon-lp")))),
      #Heat map box
      column(4, class="landing-page-column",
             div(class="landing-page-box", style="background-image: url(heatmap.png);
                 background-repeat: no-repeat; background-size: 30%; background-position: bottom 25px center ;background-color: white;",
                 div("Topic Summary: Time Trends", class = "landing-page-box-title"),
                 actionButton('jump_to_heat', 'Explore how indicators for a topic area have changed over time', 
                              class="landing-page-button", icon = icon("arrow-circle-right", "icon-lp")))),
      #Barcode plot box
      column(4, class="landing-page-column",
             div(class="landing-page-box", style="background-image: url(barcode_2.png);
                 background-repeat: no-repeat; background-size: 60%; background-position: bottom 25px center ;background-color: white;",
                 div("Topic Summary: Geographic Distribution", class = "landing-page-box-title"),
                 actionButton('jump_to_barcode', 'Explore how indicators for a topic compare across different geographies',
                              class="landing-page-button", icon = icon("arrow-circle-right", "icon-lp"))))
    ),
    fluidRow(h5("Explore a single indicator in more detail")),
    #2nd row of boxes
    fluidRow(
      #Trend plot box
      column(4, class="landing-page-column",
             div(class="landing-page-box", style="background-image: url(trend_2.png);
                 background-repeat: no-repeat; background-size: 85%; background-position: center ;background-color: white; ",
                 div("Trend", class = "landing-page-box-title"),
                 actionButton('jump_to_trend', 'Look at how an indicator changes over time',
                              class="landing-page-button", 
                              icon = icon("arrow-circle-right", "icon-lp")))),
      #Rank plot box
      column(4, class="landing-page-column",
             div(class="landing-page-box", style="background-image: url(rank_4.png);
                 background-repeat: no-repeat; background-size: 55%; background-position: bottom 20px center ;background-color: white;",
                 div("Rank", class = "landing-page-box-title"),
                 actionButton('jump_to_rank', 'Compare geographical variation for an indicator using a bar chart', 
                              class="landing-page-button", icon = icon("arrow-circle-right", "icon-lp")))),
      #Map plot box
      column(4, class="landing-page-column",
             div(class="landing-page-box", style="background-image: url(map_colourr.png);
                 background-repeat: no-repeat; background-size: 44%; background-position: center ;background-color: white; ",
                 div("Map", class = "landing-page-box-title"),
                 actionButton('jump_to_map', 'Compare geographical variation for an indicator using a map', 
                              class="landing-page-button", icon = icon("arrow-circle-right", "icon-lp"))))
      #, - VE removed inequalities data for initial launch of profiles
      #Inequalities plots box
      # column(3, class="landing-page-column",
      #        div(class="landing-page-box", style="background-image: url(simd_lp.png);
      #            background-repeat: no-repeat; background-size: cover ",
      #            div("Inequalities", class = "landing-page-box-title"),
      #            actionButton('jump_to_simd', 'Explore the effects of deprivation on public health inequalities', 
      #                         class="landing-page-button", 
      #                         icon = icon("arrow-circle-right", "icon-lp"))))
    ),
    fluidRow(h5("Access the data behind the tool and find supporting information")),
    fluidRow(
      #Table box 
      column(4, class="landing-page-column",
             div(class="landing-page-box", style="background-image: url(table_4.png);
                 background-repeat: no-repeat; background-size: 35%; background-position: bottom 30px center ;background-color: white;", 
                 div("Table", class = "landing-page-box-title"),
                 actionButton('jump_to_table', 'View and download the data behind the tool', 
                              class="landing-page-button", 
                              icon = icon("arrow-circle-right", "icon-lp"))
             )),
      #About box
      column(4, class="landing-page-column",
             div(class="landing-page-box-about", style="background-image: url(info_yellow.png);
                 background-repeat: no-repeat; background-size: 15%; background-position: bottom 6px center ;background-color: white;", 
                 div("About", class = "landing-page-box-title"),
                 actionButton('jump_to_about', 'About ScotPHO Profiles', 
                              class="landing-page-button-about", 
                              icon = icon("arrow-circle-right", "icon-lp"))
             ),
             #Evidence box
             div(class="landing-page-box-about", style="background-image: url(link_yellow.png);
                 background-repeat: no-repeat; background-size: 15%; background-position: bottom 6px center ;background-color: white;", 
                 div("Related Links", class = "landing-page-box-title" ),
                 actionButton('jump_to_evidence', 'Links to websites or documents with useful profiles information', 
                              class="landing-page-button-about", 
                              icon = icon("arrow-circle-right", "icon-lp")))
      ),
      #Resources box
      column(4, class="landing-page-column", 
             div(class="landing-page-box-about", style="background-image: url(book_dark.png);
                 background-repeat: no-repeat; background-size: 15%; background-position: bottom 6px center ;background-color: white;", 
                 div("Technical Resources", class = "landing-page-box-title"),
                 actionButton('jump_to_resources', 'Find technical information about the ScotPHO profile definitions and methodology', 
                              class="landing-page-button-about", 
                              icon = icon("arrow-circle-right", "icon-lp"))
             ),
             #Other profiles
             div(class="landing-page-box-about", style="background-image: url(external_link.png);
                 background-repeat: no-repeat; background-size: 15%; background-position: bottom 6px center ;background-color: white;", 
                 div("Other profiles", class = "landing-page-box-title"),
                 actionButton('jump_to_others', 'Links to alternative profiling tools', 
                              class="landing-page-button-about", 
                              icon = icon("arrow-circle-right", "icon-lp")))
      )
  )#Fluidrow bracket
) #main Panel bracket
  ),# tab panel bracket
###############################################.
## Ring plot ----
###############################################.
tabPanel(title = "Profile Summary", icon = icon("adjust"), value = "ring",
         #Need to have the output in the ui to get the conditional Panel working
         span(textOutput("help_ring"), style="color:white; font-size:1px"), 
         sidebarPanel(width=3,
                      selectInput("profile_ring", "Profile", choices= profile_list, multiple=FALSE, selected = "HWB"),
                      uiOutput("geotype_ui_ring"),
                      conditionalPanel(#Conditional panel for extra dropdown for localities & IZ
                        condition = "input.geotype_ring== 'HSC Locality' | input.geotype_ring == 'Intermediate zone' ",
                        selectInput("loc_iz_ring", label = "Partnership for localities/intermediate zones",
                                    choices = partnership_name)
                      ),
                      uiOutput("geoname_ui_ring"),
                      br(),
                      p("This visualisation allows you to compare one area to another area or the same area over time"),
                      radioButtons("comp_ring", label = "Compare against:",
                                   choices = list("Area" = 1, "Time" = 2),
                                   selected = 1, inline=TRUE),
                      conditionalPanel(condition = "input.comp_ring == 1 ",
                                       selectInput("geocomp_ring", "Comparator", choices = comparator_list,
                                                   selectize=TRUE, selected = "Scotland")
                      ),
                      conditionalPanel(condition = "input.comp_ring == 2 ",
                                       uiOutput("yearcomp_ui_ring")),
                      #Legend
                      p(tags$b("Legend"), style="color: black;"),
                      p(img(src='signif_better.png', height=12, style="padding-right: 2px; vertical-align:middle"), 
                        "Better than comparator.", br(),
                        img(src='non_signif.png', height=12, style="padding-right: 2px; vertical-align:middle"), 
                        "Not different from comparator.", br(),
                        img(src='signif_worse.png', height=12, style="padding-right: 2px; vertical-align:middle"), 
                        "Worse than comparator.", br(),
                        img(src='signif_nocalc.png', height=12, style="padding-right: 2px; vertical-align:middle"), 
                        "No differences can be calculated."), 
                      # "X out of X - Denotes how many indicators within a profile domain are statistically significantly better than comparator average"),
                      br(),
                      downloadButton('download_ring', 'Download data', class = "down"),
                      br(),
                      savechart_button('download_ringplot', 'Save chart',  class = "down"),
                      br(),
                      actionButton("help_ring", label="Help", icon= icon('question-circle'), class ="down"),
                      br()
         ),
         mainPanel(width = 9,
                   plotOutput("ring_plot", height="auto")
         )
), #Tab panel bracket
###############################################.
## Heat map ----
###############################################.
tabPanel("Heatmap", icon = icon("list-ul"), value = "heat",
         wellPanel( #Filter options
           column(3,
                  selectInput("profile_heat", "Profile", choices = profile_list),
                  uiOutput("topic_ui_heat")
           ),
           column(3,
                  uiOutput("geotype_ui_heat"),
                  conditionalPanel(#Conditional panel for extra dropdown for localities & IZ
                    condition = "input.geotype_heat== 'HSC Locality' | input.geotype_heat == 'Intermediate zone' ",
                    selectInput("loc_iz_heat", label = "Partnership for localities/intermediate zones",
                                choices = partnership_name)
                  ),
                  uiOutput("geoname_ui_heat")
           ),
           column(2,
                  awesomeRadio("comp_heat", label = "Compare against:",
                               choices = list("Area" = 1, "Time" = 2), 
                               selected = 1, inline=TRUE),
                  conditionalPanel(condition = "input.comp_heat == 1 ",  
                                   selectInput("geocomp_heat", "Comparator", choices = comparator_list,
                                               selectize=TRUE, selected = "Scotland")
                  ),
                  conditionalPanel(condition = "input.comp_heat == 2 ", 
                                   uiOutput("yearcomp_ui_heat")
                  ) 
           ),
           column(2,
                  #Legend
                  p(tags$b("Legend"), style="color: black;"),
                  p(img(src='signif_better.png', height=12, style="padding-right: 2px; vertical-align:middle"), 
                    "Better than comparator.", br(),
                    img(src='non_signif.png', height=12, style="padding-right: 2px; vertical-align:middle"), 
                    "Not different from comparator.", br(),
                    img(src='signif_worse.png', height=12, style="padding-right: 2px; vertical-align:middle"), 
                    "Worse than comparator.", br(),
                    img(src='signif_nocalc.png', height=12, style="padding-right: 2px; vertical-align:middle"), 
                    "No differences are calculated.")
           ),
           column(2,
                  actionButton("help_heat",label="Help", icon= icon('question-circle'), class ="down"),
                  downloadButton('download_heat', 'Download data', class = "down"),
                  savechart_button('download_overviewplot', 'Save chart',  class = "down")
           )
         ),
         mainPanel(width = 12,
                   h5(textOutput("title_heat"), style="color: black; text-align: center"),
                   plotlyOutput("heat_plot")
        )
  ), #Tab panel bracket
#####################################################################.
## Barcode ----
#####################################################################.
tabPanel("Barcode", icon = icon("barcode"), value = "barcode",
         sidebarPanel(width=3,
                      selectInput("profile_bar", "Profile", choices = profile_list),
                      uiOutput("topic_ui_bar"),
                      uiOutput("geotype_ui_bar"),
                      conditionalPanel(#Conditional panel for extra dropdown for localities & IZ
                        condition = "input.geotype_bar== 'HSC Locality' | input.geotype_bar == 'Intermediate zone' ",
                        selectInput("loc_iz_bar", label = "Partnership for localities/intermediate zones", choices = partnership_name)),
                      uiOutput("geoname_ui_bar"),
                      selectInput("geocomp_bar", "Comparator", choices = comparator_list, selectize=TRUE, selected = "Scotland"),
                      actionButton("help_bar", label="Help", icon= icon('question-circle'), class ="down"),
                      downloadButton('download_bar', 'Download data', class = "down"),
                      savechart_button('download_barplot', 'Save chart',  class = "down")
         ),
         mainPanel(width=9,
                   p(tags$b("The chart below shows how indicator values for different geographical areas compare. "), style= "font-size:12px;"),
                   htmlOutput("topic_selected"),
                   uiOutput("ui_bar_plot")
         )
),
###############################################.
## Time trend ----
###############################################.
tabPanel("Trend", icon = icon("area-chart"), value = "trend",
                   sidebarPanel(width=3,
                          selectInput("indic_trend", "Indicator", choices=indicator_list),
                          shiny::hr(),
                          p(tags$b("Select the geographies you want to plot.
                                   You can select multiple areas per geography level")),
                          selectInput("hbname_trend", "Health board", choices = c("Select health boards" = "", paste(hb_name)),
                                      multiple=TRUE, selectize=TRUE, selected = ""),
                          selectInput("scotname_trend", "Scotland", choices = c("", "Scotland"), 
                                      selectize=TRUE, selected = "Scotland"),
                          selectInput("laname_trend", "Council area", choices =  c("Select council areas" = "", paste(la_name)),
                                      multiple=TRUE, selectize=TRUE, selected = ""),
                          selectInput("adpname_trend", "Alcohol & drug partnership", choices =  c("Select partnerships" = "", paste(adp_name)),
                                      multiple=TRUE, selectize=TRUE, selected = ""),
                          selectInput("partname_trend", "HSC Partnership", choices =  c("Select partnerships" = "", paste(partnership_name)),
                                      multiple=TRUE, selectize=TRUE, selected = ""),
                          selectInput("loc_iz_trend", "To choose a locality or intermediate zone first 
                                      select an HSC partnership", 
                                      choices = partnership_name),
                          uiOutput("loc_ui_trend"),
                          uiOutput("iz_ui_trend"),
                          downloadButton('download_trend', 'Download data', class = "down"),
                          savechart_button('download_trendplot', 'Save chart',  class = "down"),
                          awesomeCheckbox("colorblind_trend",  
                                 label = "Improved accessibility", value = FALSE)
                   ),
         mainPanel(width = 9, #Main panel
          h5(textOutput("title_trend"), style="color: black; text-align: center"),
          plotlyOutput("trend_plot")
         )
), #Tab panel bracket
###############################################.
## Rank chart ---- 
###############################################.
tabPanel("Rank", icon = icon("signal"), value = "rank",
         sidebarPanel(width=4, #Filter options
                  selectInput("indic_rank", "Indicator", choices=indicator_list),
                  uiOutput("geotype_ui_rank"),
                  conditionalPanel( #Conditional panel for extra dropdown for localities & IZ
                    condition = "input.geotype_rank == 'HSC Locality' | input.geotype_rank == 'Intermediate zone' ",
                    selectInput("loc_iz_rank", label = "Partnership for localities/intermediate zones",
                                choices = partnership_name)),
                  uiOutput("year_ui_rank"), 
                  selectInput("geocomp_rank", "Comparator", choices = comparator_list,
                              selectize=TRUE, selected = "Scotland"),
                  awesomeCheckbox("ci_rank", label = "95% confidence intervals", value = FALSE),
                  downloadButton('download_rank', 'Download data', class = "down"),
                  savechart_button('download_rankplot', 'Save chart', class = "down"),
                  shiny::hr(),
                  #Legend
                  p(tags$b("Legend"), style="color: black;"),
                  p(img(src='signif_better.png', height=12, style="padding-right: 2px; vertical-align:middle"), 
                    "Better than comparator.", br(),
                    img(src='non_signif.png', height=12, style="padding-right: 2px; vertical-align:middle"), 
                    "Not different from comparator.", br(),
                    img(src='signif_worse.png', height=12, style="padding-right: 2px; vertical-align:middle"), 
                    "Worse than comparator.", br(),
                    img(src='signif_nocalc2.png', height=12, style="padding-right: 2px; vertical-align:middle"), 
                    "No differences are calculated.")
         ),
         mainPanel(width = 8, #Main panel
                   h5(textOutput("title_rank"), style="color: black; text-align: center"),
                   plotlyOutput("rank_plot") 
         )
), #Tab panel bracket
###############################################.
###########Map ----
###############################################.
tabPanel("Map", icon = icon("globe"), value = "map",
         sidebarPanel(    
           selectInput("indic_map", "Indicator", choices=indicator_map_list),
           uiOutput("geotype_ui_map"),
           conditionalPanel(#Conditional panel for extra dropdown for localities & IZ
             condition = "input.geotype_bar== 'HSC Locality' | input.geotype_map == 'Intermediate zone' ",
             selectInput("iz_map", label = "Council for intermediate zones", choices = la_name)),
           uiOutput("year_ui_map"),
           awesomeRadio("comp_map", label = "Compare against:",
                        choices = list("Area" = 1, "Time" = 2), 
                        selected = 1, inline=TRUE),
           conditionalPanel(condition = "input.comp_map == 1 ",  
                            selectInput("geocomp_map", "Comparator", choices = comparator_list,
                                        selectize=TRUE, selected = "Scotland")
           ),
           conditionalPanel(condition = "input.comp_map == 2 ", 
                            uiOutput("yearcomp_ui_map")
           ), 
           downloadButton('download_map', 'Download data', class = "down"),
           savechart_button('download_mapplot', 'Save map', class = "down"),
           shiny::hr(),
           #Legend
           p(tags$b("Legend"), style="color: black;"),
           p(img(src='signif_better.png', height=12, style="padding-right: 2px; vertical-align:middle"), 
             "Better than comparator.", br(),
             img(src='non_signif.png', height=12, style="padding-right: 2px; vertical-align:middle"), 
             "Not different from comparator.", br(),
             img(src='signif_worse.png', height=12, style="padding-right: 2px; vertical-align:middle"), 
             "Worse than comparator.", br(),
             img(src='signif_nocalc.png', height=12, style="padding-right: 2px; vertical-align:middle"), 
             "No differences are calculated.")
         ), 
         mainPanel( #Main panel
           h5(textOutput("title_map"), style="color: black; text-align: center"),
           leafletOutput("map", width="100%",height="600px")
           )
), #Tab panel bracket
###############################################.
## Deprivation ---- 
###############################################.
# tabPanel("Inequalities", icon = icon("balance-scale"), value = "simd",
#          #Intro text
#          p(tags$b("This section is under development and a limited number of indicators are available."), style = "color: red "),
#          p(tags$b("Explore the data by different levels of deprivation. ")),
#          tags$ul( 
#            tags$li("ScotPHO are working on new ways to display information related to", 
#                    tags$a(href="http://www.scotpho.org.uk/life-circumstances/deprivation/key-points/", "deprivation",
#                           class="externallink"), ".")),
#          wellPanel( #Filter options
#                    column(6, selectInput("indic_simd", label = "Indicator",
#                                          choices = ind_depr_list)),
#                    column(3, selectInput("geotype_simd", label = "Geography level",
#                                          choices = areatype_depr_list, selected =  "Scotland")),
#                    column(3, uiOutput("geoname_ui_simd")),
#                    column(5, uiOutput("year_ui_simd")),
#                    column(5, selectInput("measure_simd", label = "Type of measure",
#                                          choices = c("Rate/Percentage", "Index of inequality"))),
#                    column(2, downloadButton(outputId = 'download_simd',
#                                             "Download data",class = "down"))
#          ),
#          mainPanel(width = 12, #Main panel
#                    column(6,
#                           plotlyOutput("simd_bar_plot")),
#                    column(6,
#                           plotlyOutput("simd_trend_plot"))
#          )
# ), #Tab panel bracket
###############################################.
## Table ----
###############################################.
tabPanel("Table", icon = icon("table"), value = "table",
         #Sidepanel for filtering data
         mainPanel(
           width = 12, style="margin-left:0.5%; margin-right:0.5%",
           
           #Row 1 for intro  
           fluidRow(
             column(10,
                    p("Filter ScotPHO Data by", style = "font-weight: bold; color: black;"),
                    
                    tags$div("Select appropriate conditions to filter data", 
                             tags$br(),
                             "To delete choices use RETURN or select item and DELETE"),
                    tags$br(),
                    tags$br()),
             column(2,
                    actionButton("clear", label = "Clear all filters", icon ("eraser"), style='background: #3399FF; color: #FFF; font-size:100%')
             )),
           
           #Row 2 for selections
           fluidRow(
             column(3,
                    p("Profile product", style = "font-weight: bold; color: black;"),  
                    tags$div("All available indicators will be displayed for",tags$br(),"selected geography if none specified"),
                    br(),
                    awesomeRadio("product_filter", label=NULL, choices = c("Indicator", "Topic", "Profile"), selected = NULL, inline = FALSE,
                                 status = "primary", checkbox = TRUE, width = NULL),
                    br(),
                    conditionalPanel(
                      condition="input.product_filter=='Indicator'",
                      selectizeInput("indicator_filter", label = NULL,
                                     width = "270px", choices = indicator_list, selected = NULL,
                                     multiple=TRUE, options = list(maxOptions = 1000, placeholder = "Select or type indicators you would like to filter by"))
                      
                    ),
                    conditionalPanel(
                      condition="input.product_filter=='Topic'",
                      selectizeInput("topic_filter", label = NULL,
                                     width = "270px", choices = topic_list, selected = NULL,
                                     multiple=TRUE, options = list(maxOptions = 1000, placeholder = "Select or type topics you would like to filter by"))
                    ),
                    conditionalPanel(
                      condition="input.product_filter=='Profile'",
                      selectizeInput("profile_filter", label = NULL,
                                     width = "270px", choices = profile_list, selected = NULL,
                                     multiple=TRUE, options = list(maxOptions = 1000, placeholder = "Select or type profiles you would like to filter by"))    
                      
                    )
                    
             ),
             column(3,
                    
                    p("Geography", style = "font-weight: bold; color: black;"),
                    
                    tags$style(type='text/css', ".selectize-input { font-size: 12px; line-height: 18px;} .selectize-dropdown { font-size: 12px; line-height: 18px; }"),
                    awesomeCheckbox("iz",label = "Intermediate zone", value = FALSE),
                    conditionalPanel(
                      condition = "input.iz == true",
                      selectizeInput("iz_parent", label = "Filter list by HSC Partnership",
                                     width = "229px", choices = parent_geo_list, selected = NULL, multiple=FALSE, options = list(placeholder = "Select or type intermediate zone of interest")),
                      conditionalPanel(
                        condition = "input.iz_parent != 'Show all'",
                        checkboxInput("iz_parent_all",label = "Select all intermediate zones in this area", value = FALSE)),
                      uiOutput("iz_filtered")),
                    
                    awesomeCheckbox("la",label = "Council area", value = FALSE),
                    conditionalPanel(
                      condition = "input.la == true",
                      selectizeInput("la_true", label = NULL,
                                     width = "229px", choices = la_name, selected = NULL, multiple=TRUE, options = list(placeholder = "Select or type council area of interest"))),
                    
                    awesomeCheckbox("hscl",label = "Health and Social Care Locality", value = FALSE),
                    conditionalPanel(
                      condition = "input.hscl == true",
                      selectizeInput("hscl_parent", label = "Filter list by HSC Partnership",
                                     width = "229px", choices = parent_geo_list, selected = NULL, multiple=FALSE, options = list(placeholder = "Select or type HSC locality of interest")),
                      conditionalPanel(
                        condition = "input.hscl_parent != 'Show all'",
                        checkboxInput("hscl_parent_all",label = "Select all HSC localities in this area", value = FALSE)),
                      uiOutput("hscl_filtered")),
                    
                    awesomeCheckbox("adp",label = "Alcohol and Drugs Partnership", value = FALSE),
                    conditionalPanel(
                      condition = "input.adp == true",
                      selectizeInput("adp_true", label = NULL,
                                     width = "229px", choices = adp_name, selected = NULL, multiple=TRUE,options = list(placeholder = "Select or type ADP of interest")))  
                    
             ),
             column(3,
                    br(),
                    awesomeCheckbox("hscp",label = "Health and Social Care Partnership", value = FALSE),
                    conditionalPanel(
                      condition = "input.hscp == true",
                      selectInput("hscp_true", label = NULL,
                                  width = "229px", choices = partnership_name, selected = NULL, multiple=TRUE)),
                    
                    awesomeCheckbox("hb",label = "Health board", value = FALSE),
                    conditionalPanel(
                      condition = "input.hb == true",
                      selectInput("hb_true", label = NULL,
                                  width = "229px", choices = hb_name, selected = NULL, multiple=TRUE)),
                    
                    
                    awesomeCheckbox("scotland",label = "Scotland", value = FALSE),
                    awesomeCheckbox("all_data",label = "All available geographies", value = FALSE),
                    selectizeInput("code", label = NULL,
                                   width = "229px", choices = code_list, options = list(placeholder = 'Or search by area code'), multiple=TRUE, selected = "")
                    
             ),
             column(3,
                    p("Time period", style = "font-weight: bold; color: black;"),
                    br(),
                    sliderInput("date_from",label = NULL, min = min_year, max = max_year, value = c(min_year,max_year), 
                                width = "260px", step = 1, sep="", round = TRUE, ticks = TRUE, dragRange = FALSE),
                    br(), br(), br(),
                    downloadButton("download_table_csv", 'Download data (csv)', class = "down")
             )
           ),
           
           #Row 3- Table
           fluidRow(  
             column(12, div(DT::dataTableOutput("table_filtered"), style = "font-size: 98%; width: 98%"))
           ))
         
 ), #Tab panel bracket   
###############################################.             
##############About----    
###############################################.
#Starting navbarMenu to have tab with dropdown list
navbarMenu("Info", icon = icon("info-circle"), 
           tabPanel("About", value = "about",
                    sidebarPanel(width=1),
                    mainPanel(width=8,
                              h4("Welcome to the ScotPHO Profiles Tool", style = "color:black;"),
                              p("The tool is designed to allow users to explore the various different profiles 
                                produced by the ", tags$a(href="http://www.scotpho.org.uk/about-us/about-scotpho/", "ScotPHO collaboration.", 
                                                          class="externallink")),
                              p("The profiles are intended to increase understanding of local health issues 
                                and to prompt further investigation, rather than to be used as a performance 
                                management tool. The information needs to be interpreted within a local 
                                framework; an indicator may be higher or lower in one area compared to another, 
                                but local knowledge is needed to understand and interpret differences."),
                              p("If you have any trouble accessing any information on this site or have
                                any further questions or feedback relating to the data or the tool, then please contact us at: ",
                                tags$b(tags$a(href="mailto:ScotPHO@nhs.net", "ScotPHO@nhs.net", class="externallink")),
                                "and we will be happy to help.")),
                    br()
           ),#Tab panel
###############################################.             
##############Resources----    
###############################################.      
           tabPanel("Technical Resources", value = "resources",
                    sidebarPanel(width=1),
                    mainPanel(
                      h4("Resources", style = "color:black;"),
                      p("We list a number of resources that help you to understand better the profiles or to
                        carry out similar analysis to ours"),
                      tags$ul( 
                        #Link to user guide
                        tags$li(class= "li-custom", tags$a(href="", "User guide",  class="externallink"), 
                                tags$b(" (coming soon)"), " - Learn how to use and get the most out of the tool."),
                        #Link to technical report
                        tags$li(class= "li-custom", tags$a(href="http://www.scotpho.org.uk/comparative-health/profiles/resources/",
                                                           "Technical reports",  class="externallink"), 
                                " - Detailed description of the methodology, statistics and caveats of the data presented."),
                        #Link to overview reports
                        tags$li(class= "li-custom", tags$a(href="http://www.scotpho.org.uk/comparative-health/profiles/resources/",
                                                           "Overview reports",  class="externallink"), 
                                " - These provide context, narrative and analysis for each profile."),
                        #Link to user guide
                        tags$li(class= "li-custom", tags$a(href="http://www.scotpho.org.uk/media/1026/explanation-of-statistics-used-in-profiles-v2.pptx", 
                                                           "Statistics of the profiles",  class="externallink"), 
                                " - A guide and explanation of the statistics used in the profiles."),
                        #Link to timetable of updates
                        tags$li(class= "li-custom", tags$a(href="https://docs.google.com/spreadsheets/d/e/2PACX-1vQUQMORMqe9RrMnS9WJSu51Q6ef0rubiF1M-QN3BYZIBueErtTvvbRe_kTZbWmnupiO_Uie80BoZCnK/pubhtml",
                                                           "Timetable of updates", class="externallink"), 
                                "- List of available indicators, date of last update and expected next update."),
                        #Link to Github repositories
                        tags$li(class= "li-custom", tags$a(href="https://github.com/Health-SocialCare-Scotland/ScotPHO-profile-indicators",
                                                           "Indicator production code", class="externallink"), 
                                " and ",
                                tags$a(href="https://github.com/Health-SocialCare-Scotland/ScotPHO-profile-tool",
                                       "Profile tool code", class="externallink"), 
                                "- Access the code used to produce the indicator data and this tool."),
                        #Link to population lookups
                        tags$li(class= "li-custom", tags$a(href="https://www.opendata.nhs.scot/dataset/population-estimates",
                                                           "Population estimate", class="externallink"),  " and ",
                                tags$a(href="                   https://www.opendata.nhs.scot/dataset/geography-codes-and-labels",
                                       "geography names and codes", class="externallink"), 
                                "- Where you can find the files with the populations and geographies
                                used for the analysis."),
                        #Link to shapefiles
                        tags$li(class= "li-custom", tags$a(href="https://data.gov.uk/publisher/scottish-government-spatial-data-infrastructure",
                                                           "Shapefiles", class="externallink"), 
                                "- Where you can find the shapefiles used for the map.")
                        ), #Bullet point list bracket
                      br()
                      ) # mainPanel bracket
), #Tab panel bracket
###############################################.             
##############Evidence for action----    
###############################################.      
tabPanel("Related Links", value = "evidence",
         sidebarPanel(width=1),
         mainPanel(
           h4("Related Links", style = "color:black;"),
           p("Below are links to organisations and documents that provide supporting evidence for action."),
           tags$ul( 
             #Link to HS
             tags$li(class= "li-custom", tags$a(href="http://www.healthscotland.scot/improve-policy-and-practice", 
                                                "NHS Health Scotland",  class="externallink")),
             #Link to HPHS
             tags$li(class= "li-custom", tags$a(href="http://www.knowledge.scot.nhs.uk/home/portals-and-topics/health-improvement/hphs/evidence-briefings.aspx", 
                                                "Health Promotion Health Service - Evidence briefings",  class="externallink")),
             #Link to What Works Scotland
             tags$li(class= "li-custom", tags$a(href="http://whatworksscotland.ac.uk/", 
                                                "What Works Scotland",  class="externallink")),
             #Link to NICE - Evidence UK
             tags$li(class= "li-custom", tags$a(href="https://www.evidence.nhs.uk/", 
                                                "NICE - Evidence UK",  class="externallink")),
             #NICE - guidance
             tags$li(class= "li-custom", tags$a(href="https://www.nice.org.uk/guidance", 
                                                "NICE - guidance",  class="externallink")),
             #Link to SIGN
             tags$li(class= "li-custom", tags$a(href="http://www.sign.ac.uk/", 
                                                "Scottish Intercollegiate Guidelines Network (SIGN)",  class="externallink")),
             #Link to Centre for Reviews and Dissemination
             tags$li(class= "li-custom", tags$a(href="https://www.york.ac.uk/crd/", 
                                                "Centre for Reviews and Dissemination",  class="externallink")),
             #Link to Cochrane Library
             tags$li(class= "li-custom", tags$a(href="http://www.cochranelibrary.com/home/topic-and-review-group-list.html?page=topic", 
                                                "Cochrane Library",  class="externallink")),
             #Link to EPPI-Centre
             tags$li(class= "li-custom", tags$a(href="http://eppi.ioe.ac.uk/cms/Default.aspx?tabid=56&language=en-US", 
                                                "EPPI-Centre - Evidence library",  class="externallink"))
           ), #Bullet point list bracket
           br()
           ) # mainPanel bracket
), #tabPanel bracket
###############################################.             
##############Other profiles----    
###############################################.
tabPanel("Other profiles", value = "others",
         sidebarPanel(width=1),
         mainPanel(
           h4("Alternative profiles", style = "color:black;"),
           p("A number of different organisations produce local area profile products. These different profile products have often
              been designed to serve a particular purpse or to suit a specific audience.  Occassionally the same or similar key indicators
              may appear across multiple different profile products, ScotPHO aim to align indicator definitions with recognised 
              national definitions or with other Scottish organisations.  We aim to highlight similarities or differences between defintions
              used by ScotPHO and some of the other major Scottish profiles in our techinical documentation. 
.             Below are links to some related profiling products."),
           tags$ul( 
             #Link to old tool
             tags$li(class= "li-custom", tags$a(href="https://scotpho.nhsnss.scot.nhs.uk/scotpho/homeAction.do", 
                                                "Historic ScotPHO profiles",  class="externallink"), 
                     " - The old style ScotPHO profiles are currently still accessible via our old profile platform"),
             #Link to GCPH
             tags$li(class= "li-custom", tags$a(href="http://www.understandingglasgow.com/",
                                                "Glasgow profiles",  class="externallink"), 
                     " - Glasgow Centre for Population Health."),
             #Link to Fife
             tags$li(class= "li-custom", tags$a(href="https://knowfife.fife.gov.uk/",
                                                "Fife profiles",  class="externallink"), 
                     " - Fife Council."),
             #Link to IS
             tags$li(class= "li-custom", tags$a(href="http://www.improvementservice.org.uk/community-planning-outcomes-profile.html",
                                                "Community planning outcomes profile",  class="externallink"), 
                     " - Improvement Service."),
             #Link to NRS
             tags$li(class= "li-custom", tags$a(href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/stats-at-a-glance/council-area-profiles", 
                                                "Council area profiles",  class="externallink"), 
                     " - National Records of Scotland."),
             #Link to NRS
             tags$li(class= "li-custom", tags$a(href="http://statistics.gov.scot/home", 
                                                "Open data and area profiles for Scotland",  class="externallink"), 
                     " - Statistics.gov.scot -  Scotland's official statistics site.")
           ), #Bullet point list bracket
           br()
           ) # mainPanel bracket
           ) #tabPanel bracket
), # NavbarMenu bracket
###############################################.             
##############Footer----    
###############################################.
#Copyright warning
tags$footer("© Scottish Public Health Observatory v2.0 2018", style = "
   position:fixed;
   text-align:center;
   left: 0;
   bottom:0;
   width:100%;
   z-index:1000;  
   height:30px; /* Height of the footer */
   color: white;
   padding: 10px;
   font-weight: bold;
   background-color: #1995dc"
    ) 
################################################.
) #Bracket  navbarPage

###END