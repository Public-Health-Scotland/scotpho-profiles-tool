#Code to create ScotPHO's Shiny profile platform
# This script includes the user-interface definition of the app.
#
#TODO:
#see global syntax

###############################################.
## Header ---- 
###############################################.
  navbarPage(
    title = div(img(src="scotpho_reduced.png", height=40),
                         style = "position: relative; top: -5px;"), # Navigation bar
             windowTitle = "ScotPHO profiles", #title for browser tab
             theme = shinytheme("cerulean"), #Theme of the app (blue navbar)
             collapsible = TRUE, #tab panels collapse into menu in small screens
             # shinythemes::themeSelector(),
             header =         
             tags$head( #CSS styles
               beta_box,  ##### Feedback box. TO TAKE OUT AFTER BETA PERIOD
               tags$link(rel="shortcut icon", href="favicon_scotpho.ico"), #Icon for browser tab
               #Including Google analytics
               includeScript("google-analytics.js"),
               #Style sidebars/well panels
               tags$style(".well {background-color:#ffffff; border: 0px solid #336699;
                          padding: 5px; box-shadow: none; }",
               #Text size and line height
               "body { font-size: 11px; line-height: 1.1}",
               ".navbar { font-size: 12px}",
               ".checkbox label, .radio label { line-height: 1.6 }",
               ".radio-inline {line-height: 2}",
               #Padding and margins of filters and labels
               ".form-group {margin: 0px}",
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
               ".down{background-color:#4da6ff; color: white; background-image:none;
               font-size: 12px; padding: 5px 10px}",
               #to avoid red text error messages in the whole app, take out for testing
               # ".shiny-output-error { visibility: hidden; }",
               #            ".shiny-output-error:before { visibility: hidden; }",
               #External links underlined an open a new tab
               ".externallink{text-decoration: underline;} "),
               HTML("<base target='_blank'>")
               ),
###############################################.
## Overview ----
###############################################.
tabPanel("Overview", icon = icon("heartbeat"),
         conditionalPanel(condition = "output.help_overview == 'TRUE' ",  
           p("Explore how an area compares to Scotland (or a comparator of your choice)
           over time."),
           tags$ul(
             tags$li("Use the ‘Topic’ menu to adjust the suite of indicators on display."),
             tags$li("Older indicator values appears on the left side of the grid,
                      the most recent data on the right."),
             tags$li("Hover over each tile to see indicator definitions and time periods."))
           ), 
         #Need to have the output in the ui to get the conditional Panel working
         span(textOutput("help_overview"), style="color:white; font-size:1px"), 
         wellPanel( #Filter options
             column(3,
                    selectInput("geotype_heat", "Geography level", choices= areatype_noscot_list),
                    conditionalPanel(#Conditional panel for extra dropdown for localities & IZ
                      condition = "input.geotype_heat== 'HSC Locality' | input.geotype_heat == 'Intermediate zone' ",
                      selectInput("loc_iz_heat", label = "Partnership for localities/intermediate zones",
                                  choices = partnership_name)
                    ),
                    uiOutput("geoname_ui_heat")
             ),
             column(3,
                    selectInput("topic_heat", "Topic", choices = topic_list,
                                selectize=TRUE, selected = "Scotland"),
                    radioButtons("comp_heat", label = "Compare against:",
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
             column(4,
                    #Legend
                    p(tags$b("Legend"), style="color: black;"),
                    p(img(src='signif_better.png', height=12, style="padding-right: 2px; vertical-align:middle"), 
                          "Statistically significantly better than comparator average.", br(),
                      img(src='non_signif.png', height=12, style="padding-right: 2px; vertical-align:middle"), 
                      "Statistically not significantly different from comparator average.", br(),
                      img(src='signif_worse.png', height=12, style="padding-right: 2px; vertical-align:middle"), 
                      "Statistically significantly worse than comparator average.", br(),
                      img(src='signif_nocalc.png', height=12, style="padding-right: 2px; vertical-align:middle"), 
                      "No significance can be calculated.")
             ),
             column(2,
                    actionLink("help_overview", label = tags$b("Help"), icon= icon('question-circle')),
                    br(),
                    downloadButton('download_heat', 'Download data', class = "down")
                    )
           ),
         mainPanel(width = 12,
                   plotlyOutput("heat_plot") 
        )
  ), #Tab panel bracket
###############################################.
## Time trend ----
###############################################.
tabPanel("Trend", icon = icon("area-chart"),
                   sidebarPanel(width=3,
                          selectInput("indic_trend", "Indicator", choices=indicator_list),
                          shiny::hr(),
                          p(tags$b("Select the geographies you want to plot")),
                          selectInput("hbname_trend", "Health board", choices = hb_name,
                                      multiple=TRUE, selectize=TRUE, selected = ""),
                          selectInput("scotname_trend", "Scotland", choices = c("", "Scotland"), 
                                      selectize=TRUE, selected = "Scotland"),
                          selectInput("laname_trend", "Council area", choices = la_name,
                                      multiple=TRUE, selectize=TRUE, selected = ""),
                          selectInput("partname_trend", "HSC Partnership", choices = partnership_name,
                                      multiple=TRUE, selectize=TRUE, selected = ""),
                          selectInput("loc_iz_trend", "To choose a locality or intermediate zone first 
                                      select an HSC partnership", 
                                      choices = partnership_name),
                          uiOutput("loc_ui_trend"),
                          uiOutput("iz_ui_trend"),
                          downloadButton('download_trend', 'Download data', class = "down"),
                          checkboxInput("colorblind_trend",  
                                 label = "Improved accessibility", value = FALSE)
                   ),
         mainPanel(width = 9, #Main panel
          plotlyOutput("trend_plot")
         )
), #Tab panel bracket
###############################################.
## Rank chart ---- 
###############################################.
tabPanel("Rank", icon = icon("signal"),
         sidebarPanel(width=4, #Filter options
                  selectInput("indic_rank", "Indicator", choices=indicator_list),
                  selectInput("geotype_rank", label = "Geography level",
                              choices = areatype_noscot_list, selected = "Health board"),
                  conditionalPanel( #Conditional panel for extra dropdown for localities & IZ
                    condition = "input.geotype_rank == 'HSC Locality' | input.geotype_rank == 'Intermediate zone' ",
                    selectInput("loc_iz_rank", label = "Partnership for localities/intermediate zones",
                                choices = partnership_name)),
                  uiOutput("year_ui_rank"), 
                  selectInput("geocomp_rank", "Comparator", choices = comparator_list,
                              selectize=TRUE, selected = "Scotland"),
                  checkboxInput("ci_rank",label = "95% confidence intervals", value = FALSE),
                  #Legend
                  p(tags$b("Legend"), style="color: black;"),
                  p(img(src='signif_better.png', height=12, style="padding-right: 2px; vertical-align:middle"), 
                    "Statistically significantly better than comparator average.", br(),
                    img(src='non_signif.png', height=12, style="padding-right: 2px; vertical-align:middle"), 
                    "Statistically not significantly different from comparator average.", br(),
                    img(src='signif_worse.png', height=12, style="padding-right: 2px; vertical-align:middle"), 
                    "Statistically significantly worse than comparator average.", br(),
                    img(src='signif_nocalc2.png', height=12, style="padding-right: 2px; vertical-align:middle"), 
                    "No significance can be calculated."),
                  downloadButton('download_rank', 'Download data', class = "down")
         ),
         mainPanel(width = 8, #Main panel
                   plotlyOutput("rank_plot") 
         )
), #Tab panel bracket
###############################################.
## Deprivation ---- 
###############################################.
tabPanel("Inequalities", icon = icon("balance-scale"),
         #Intro text
         p(tags$b("This section is under development and a limited number of indicators are available."), style = "color: red "),
         p(tags$b("Explore the data by different levels of deprivation. ")),
         tags$ul( 
           tags$li("ScotPHO are working on new ways to display information related to", 
                   tags$a(href="http://www.scotpho.org.uk/life-circumstances/deprivation/key-points/", "deprivation",
                          class="externallink"), ".")),
         wellPanel( #Filter options
                   column(6, selectInput("indic_simd", label = "Indicator",
                                         choices = ind_depr_list)),
                   column(3, selectInput("geotype_simd", label = "Geography level",
                                         choices = areatype_depr_list, selected =  "Scotland")),
                   column(3, uiOutput("geoname_ui_simd")),
                   column(5, uiOutput("year_ui_simd")),
                   column(5, selectInput("measure_simd", label = "Type of measure",
                                         choices = c("Rate/Percentage", "Index of inequality"))),
                   column(2, downloadButton(outputId = 'download_simd',
                                            "Download data",class = "down"))
         ),
         mainPanel(width = 12, #Main panel
                   column(6,
                          plotlyOutput("simd_bar_plot")),
                   column(6,
                          plotlyOutput("simd_trend_plot"))
         )
), #Tab panel bracket
###############################################.
###########Map ----
###############################################.
tabPanel("Map", icon = icon("globe"),
         sidebarPanel(    
           selectInput("indic_map", "Indicator", choices=indicator_map_list),
           uiOutput("year_ui_map"),
           downloadButton('download_map', 'Download data', class = "down"),
           shiny::hr(),
           p(tags$b("Legend"), style="color: black;"),
           img(src='legend_map.png', height=150, style = "align: right")
         ), 
         mainPanel( #Main panel
           h5(textOutput("title_map"), style="color: black;"),
           leafletOutput("map")
         )
    ), #Tab panel bracket
##########################################################.
## Barcode 1  ----
## Version uses measure values for bars
##########################################################.
tabPanel("Barcode1", icon = icon("barcode"),
         # beta_box,  ##looks better with text as title for chart?
         # p(tags$b("Barcode plot take a few seconds to load")),
         # tags$ul( 
         #  tags$li("This chart gives an indication of variation")),
         sidebarPanel(width=3,
                      selectInput("geotype_bar", "Geography level", choices= areatype_noscot_list),
                      conditionalPanel(#Conditional panel for extra dropdown for localities & IZ
                        condition = "input.geotype_bar== 'HSC Locality' | input.geotype_bar == 'Intermediate zone' ",
                        selectInput("loc_iz_bar", label = "Partnership for localities/intermediate zones", choices = partnership_name)),
                      uiOutput("geoname_ui_bar"),   
                      selectInput("geocomp_bar", "Comparator", choices = comparator_list, selectize=TRUE, selected = "Scotland"),
                      selectInput("topic_bar", "Topic", choices = topic_list,selectize=TRUE, selected = "Scotland"),
                      downloadButton('download_bar', 'Download data', class = "down")
         ),
         mainPanel(width=9,
                   p(tags$b("The chart below shows how indicator values for different geographical areas compare. "), style= "font-size:12px;"),
                   uiOutput("ui_plot")
         )
),
#####################################################################.
## Barcode 2 ----
## Version using % difference from comparator for bars
#####################################################################.
tabPanel("Barcode2", icon = icon("barcode"),
         # beta_box,  ##looks better with text as title for chart?
         # p(tags$b("Barcode plot take a few seconds to load")),
         # tags$ul(
         #  tags$li("This chart gives an indication of variation")),
         sidebarPanel(width=3,
                      selectInput("geotype_bar2", "Geography level", choices= areatype_noscot_list),
                      conditionalPanel(#Conditional panel for extra dropdown for localities & IZ
                        condition = "input.geotype_bar2== 'HSC Locality' | input.geotype_bar2 == 'Intermediate zone' ",
                        selectInput("loc_iz_bar2", label = "Partnership for localities/intermediate zones", choices = partnership_name)),
                      uiOutput("geoname_ui_bar2"),
                      selectInput("geocomp_bar2", "Comparator", choices = comparator_list, selectize=TRUE, selected = "Scotland"),
                      selectInput("topic_bar2", "Topic", choices = topic_list,selectize=TRUE, selected = "Scotland"),
                      downloadButton('download_bar2', 'Download data', class = "down")
         ),
         mainPanel(width=9,
                   p(tags$b("The chart below shows how indicator values for different geographical areas compare. "), style= "font-size:12px;"),
                   uiOutput("ui_bar_plot")
       )
),
###############################################.
## Table ----
###############################################.
tabPanel("Table", icon = icon("table"),
         #Intro text
         column(9,
                p(tags$b("Indicator data in a table format. ")),
                tags$ul( 
                  tags$li("Use the filters to select the data you are interested in."))
                ),
         column(3,
                downloadButton('download_table', 'Download data', class = "down")  #For downloading the data
         ),
         div(DT::dataTableOutput("table_opt"), style = "font-size:90%") #table
  ), #Tab panel bracket
###############################################.             
##############Help----    
###############################################.
tabPanel("Info", icon = icon("info-circle"),
         p(tags$b("Welcome to the ScotPHO Profiles Tool "), "designed to allow users 
           to view the various different profiles produced by the ScotPHO collaboration."),
         p("The profiles are intended to increase understanding of local health issues 
           and to prompt further investigation, rather than to be used as a performance 
           management tool. The information needs to be interpreted within a local 
           framework; an indicator may be higher or lower in one area compared to another, 
           but local knowledge is needed to understand and interpret differences."),
         p("If you have any trouble accessing any information on this site or have
           any further questions relating to the data or the tool, then please contact us at: ",
           tags$b(tags$a(href="mailto:ScotPHO@nhs.net", "ScotPHO@nhs.net", class="externallink")),
           "and we will be happy to help."),
         shiny::hr(), 
         # Resources
         h4("Resources", style="color: black;"), 
         tags$ul( 
           #Link to user guide
           tags$li(tags$a(href="", "User guide",  class="externallink"), 
                   " (coming soon) - Learn how to use and get the most out of the tool."
           ), #Link to technical report
           tags$li(tags$a(href="http://www.scotpho.org.uk/comparative-health/profiles/resources/",
                          "Technical reports",  class="externallink"), 
                   " - Detailed description of the methodology, statistics and caveats of the data presented."
           ),#Link to timetable of updates
           tags$li(tags$a(href="https://docs.google.com/spreadsheets/d/e/2PACX-1vQUQMORMqe9RrMnS9WJSu51Q6ef0rubiF1M-QN3BYZIBueErtTvvbRe_kTZbWmnupiO_Uie80BoZCnK/pubhtml",
                          "Timetable of updates", class="externallink"), 
                   "- List of available indicators, date of last update and expected next update."
           ),#Link to Github repositories
           tags$li(tags$a(href="https://github.com/Health-SocialCare-Scotland/ScotPHO-profile-indicators",
                          "Indicator production code", class="externallink"), 
                   " and ",
                   tags$a(href="https://github.com/Health-SocialCare-Scotland/ScotPHO-profile-tool",
                          "Profile tool code", class="externallink"), 
                   "- Access the code used to produce the indicator data and this tool."
           )
         ),
         #Copyright warning
         div(style="height: 40px; background: linear-gradient(#54b4eb, #2fa4e7 60%, #1d9ce5); display:inline-block; width:100%  ",
             tags$b("© Scottish Public Health Observatory v2.0 2018", style="color: white; padding-top: 14px; padding-left: 30px;
                    padding-right: 60%; line-height:40px; font-size:70%;")#,
             #bookmarkButton(style="height:25px; font-size:70%; vertical-align:middle; margin-bottom:3px ")
             )
         ) #Tab panel bracket
  )#, #Bracket  navbarPage
##END
