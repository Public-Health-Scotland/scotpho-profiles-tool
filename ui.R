#Code to create ScotPHO's Shiny profile platform
# This script includes the user-interface definition of the app.
#
#TODO:
#see global syntax

###############################################.
## Header ---- 
###############################################.
fluidPage(theme = shinytheme("cerulean"), #Theme of the app (blue navbar)
          tags$head( #CSS styles
            #color sidebars/well panels
            tags$style(".well {background-color:#ffffff; border: 0px solid #336699;}"), 
            #Style for download buttons
            tags$style(".down{background-color:#4da6ff;} .down{color: white;} 
                       .down{background-image:none;}"), 
            #to avoid red text error messages in the whole app, take out for testing
            tags$style(type="text/css", ".shiny-output-error { visibility: hidden; }",
              ".shiny-output-error:before { visibility: hidden; }" ),
            #External links underlined an open a new tab
            tags$style(".externallink{text-decoration: underline;} "),
            HTML("<base target='_blank'>")
          ),
#Header style and content, using html tags
  div(style="height: 80px;  ", 
    tags$a( #image that links to ScotPHO main page
      img(src='scotpho_logo.png', height=70, style="float: left; padding-right: 40px;"),
      href= "http://www.scotpho.org" 
    ),
    h3("ScotPHO Profiles", 
       style="vertical-align: bottom; padding-top:25px; "), 
    #asking for feedback TAKE OUT after beta period
    h6(tags$a(href="mailto:ScotPHO@nhs.net", "Tell us what you think of our new tool!"))
  ),
  navbarPage("", # Navigation bar
###############################################.
## Overview ----
###############################################.
tabPanel("Overview", icon = icon("heartbeat"),
         beta_box,  
         p(tags$b("Explore how an area compares to Scotland (or a comparator of your choice) 
           over time.")), #Intro text
         tags$ul( 
           tags$li("Use the ‘Topic’ menu to adjust the suite of indicators on display."),
           tags$li("Older indicator values appears on the left side of the grid, 
                   the most recent data on the right."),
           tags$li("Hover over each tile to see indicator definitions and time periods.")),
           wellPanel( #Filter options
             column(8,
                    selectInput("geotype_heat", "Geography level", 
                                choices= areatype_noscot_list),
                    conditionalPanel(#Conditional panel for extra dropdown for localities & IZ
                      condition = "input.geotype_heat== 'HSC Locality' | input.geotype_heat == 'Intermediate zone' ",
                      selectInput("loc_iz_heat", label = "Partnership for localities/intermediate zones",
                                  choices = partnership_name)
                    ),
                    uiOutput("geoname_ui_heat"),
                    selectInput("geocomp_heat", "Comparator", choices = comparator_list,
                                selectize=TRUE, selected = "Scotland")
             ),
             column(4,
                    selectInput("topic_heat", "Topic", choices = topic_list,
                                selectize=TRUE, selected = "Scotland"),
                    downloadButton('download_heat', 'Download data', class = "down"),
                    h5(tags$b("Legend"), style="color: black;"),
                    img(src='legend_rank.png', height = 130)
             )
           ),
           mainPanel(width=12, #Main panel
                     plotlyOutput("heat_plot", width = "100%") 
           )
  ), #Tab panel bracket
###############################################.
## Time trend ----
###############################################.
tabPanel("Trend", icon = icon("area-chart"),
         beta_box,  
         p(tags$b("Explore changes over time. ")), #Intro text
         tags$ul( 
           tags$li("Use menus to select an indicator of interest and the geographies 
                   you wish to display."),
           tags$li("Some indicator data is not available for all geographies."),
           tags$li("The ‘Download data’ button will download and save the chart data 
                   as a csv file.")),
         wellPanel( #Filter options
                   column(4,
                          selectInput("indic_trend", "Indicator", choices=indicator_list),
                          selectInput("hbname_trend", "Health board", choices = hb_name,
                                      multiple=TRUE, selectize=TRUE, selected = ""),
                          downloadButton('download_trend', 'Download data', class = "down"),
                          checkboxInput("colorblind_trend",  
                                        label = "Improved accessibility", value = FALSE)
                   ),
                   column(4,    
                          selectInput("scotname_trend", "Scotland", choices = c("", "Scotland"), 
                                      selectize=TRUE, selected = "Scotland"),
                          selectInput("laname_trend", "Council area", choices = la_name,
                                      multiple=TRUE, selectize=TRUE, selected = ""),
                          selectInput("partname_trend", "HSC Partnership", choices = partnership_name,
                                      multiple=TRUE, selectize=TRUE, selected = "")

                   ),
                   column(4, style ="border: 1px dashed #404040",
                          selectInput("loc_iz_trend", "Select a partnership to limit
                                      the options of localities/intermediate zones", 
                                      choices = partnership_name),
                          uiOutput("loc_ui_trend"),
                          uiOutput("iz_ui_trend")
                   )
         ),  
         mainPanel(width = 12, #Main panel
                   shiny::hr(),
          plotlyOutput("trend_plot")
         )
), #Tab panel bracket
###############################################.
## Rank chart ---- 
###############################################.
tabPanel("Rank", icon = icon("signal"),
         beta_box,  
         p(tags$b(" Compare geographical areas. ")), #Intro text
         tags$ul( 
           tags$li("Use menus to select an indicator of interest, time period and 
                   the geography level for comparison."),
           tags$li("Some indicator data is not available for all geographies."),
           tags$li("The ‘Download data’ button will download and save the chart data 
                   as a csv file.")),
         wellPanel( #Filter options
           column(6,
                  selectInput("geotype_rank", label = "Geography level",
                              choices = areatype_noscot_list, selected = "Health board"),
                  conditionalPanel( #Conditional panel for extra dropdown for localities & IZ
                    condition = "input.geotype_rank == 'HSC Locality' | input.geotype_rank == 'Intermediate zone' ",
                    selectInput("loc_iz_rank", label = "Partnership for localities/intermediate zones",
                                choices = partnership_name)
                  ),
                  checkboxInput("ci_rank",  
                                label = "95% confidence intervals", value = FALSE),
                  downloadButton('download_rank', 'Download data', class = "down"),
                  h5(tags$b("Legend"), style="color: black;"),
                  img(src='legend_rank.png', height = 130)
           ),  
           column(6,
                  selectInput("indic_rank", "Indicator", choices=indicator_list),
                  uiOutput("year_ui_rank"), 
                  selectInput("geocomp_rank", "Comparator", choices = comparator_list,
                              selectize=TRUE, selected = "Scotland")
           )  
         ),
         mainPanel(width = 12, #Main panel
                   plotlyOutput("rank_plot") 
         )
), #Tab panel bracket
###############################################.
## Deprivation ---- 
###############################################.
tabPanel("Deprivation", icon = icon("balance-scale"),
         beta_box,  
         #Intro text
         p(tags$b("This section is under development and a limited number of indicators are available."), style = "color: red "),
         p(tags$b("Explore the data by different levels of deprivation. ")),
         tags$ul( 
           tags$li("ScotPHO are working on new ways to display information related to", 
                   tags$a(href="http://www.scotpho.org.uk/life-circumstances/deprivation/key-points/", "deprivation",
                          class="externallink"), "."),
           tags$li("Use menus to select an indicator of interest and the geographies 
                   you wish to display."),
           tags$li("The ‘Download data’ button will download and save the charts data 
                   as a csv file.")),
         wellPanel( #Filter options
                   column(6, selectInput("indic_simd", label = "Indicator",
                                         choices = ind_depr_list)),
                   column(3, selectInput("geotype_simd", label = "Geography level",
                                         choices = areatype_depr_list, selected =  "Scotland")),
                   column(3, uiOutput("geoname_ui_simd")),
                   column(5, uiOutput("year_ui_simd")),
                   column(5, selectInput("measure_simd", label = "Type of measure",
                                         choices = c("Rate/Percentage", "Index of inequality"))),
                   column(2, downloadButton(outputId = 'download_simd', class = "down"))
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
         beta_box,  
         #Intro text
         p(tags$b("Indicator information shown in a map. ")),
         tags$ul( 
           tags$li("Use menus to select an indicator of interest, a time period, 
                    and the geographies you wish to display."),
           tags$li("The ‘Download data’ button will download and save the map data 
                   as a csv file.")),
         sidebarPanel(    
           selectInput("indic_map", "Indicator", choices=indicator_map_list),
           uiOutput("year_ui_map"),
           downloadButton('download_map', 'Download data', class = "down"),
           shiny::hr(),
           h5(tags$b("Legend")),
           img(src='legend_map.png', height=150, style = "align: right")
         ), 
         mainPanel( #Main panel
           h4(textOutput("title_map")),
           leafletOutput("map")
         )
    ), #Tab panel bracket
###############################################.
## Table ----
###############################################.
tabPanel("Table", icon = icon("table"),
         beta_box,  
         #Intro text
         column(9,
                p(tags$b("Indicator data in a table format. ")),
                tags$ul( 
                  tags$li("Use the filters to select the data you are interested in."),
                  tags$li("The ‘Download data’ button will download and save the table data 
                          as a csv file."))
                ),
         column(3,
                downloadButton('download_table', 'Download data', class = "down")  #For downloading the data
         ),
         div(DT::dataTableOutput("table_opt"), style = "font-size:70%") #table
  ), #Tab panel bracket
###############################################.             
##############Help----    
###############################################.
tabPanel("Help", icon = icon("question"),
         beta_box,       
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
) # Bracket fluidPage

##END
