#Code to create ScotPHO's Shiny profile platform
# This script includes the user-interface definition of the app.
#
#TODO:
#see server syntax 

###############################################.
## Header ---- 
###############################################.
fluidPage(theme = shinytheme("cerulean"),
          tags$head(
            tags$style( #to avoid red text error messages in the whole app, take out for testing
              type="text/css", ".shiny-output-error { visibility: hidden; }",
              ".shiny-output-error:before { visibility: hidden; }"
            )
          ),
#Title style and content, using html tags
  div(style="height: 80px;  ", 
    tags$a(
      img(src='scotpho_logo.png', height=70, style="float: left; padding-right: 40px;"),
      href= "http://www.scotpho.org" #hyperlink
    ),
    h3("Online Profile Tool", 
       style="vertical-align: bottom; padding-top:25px; "),
    h6(tags$a(href="mailto:ScotPHO@nhs.net", "Tell us what you think of our new tool!"))
  ),
  navbarPage("", # Navigation bar
###############################################.             
##############Introduction----    
###############################################.
    tabPanel("Intro", icon = icon("info-circle"),
      beta_box,       
      p(tags$b("Welcome to the ScotPHO Online Profiles Tool (OPT)"), "designed to allow users 
        to view the various different profiles produced by the ScotPHO collaboration."),
      p("The profiles are intended to increase understanding of local health issues 
        and to prompt further investigation, rather than to be used as a performance 
        management tool. The information needs to be interpreted within a local 
        framework; an indicator may be higher or lower in one area compared to another, 
        but local knowledge is needed to understand and interpret differences."),
      p("If you have any trouble accessing any information on this site or have
        any further questions relating to the data or the tool, then please contact us at: ",
        tags$b(tags$a(href="mailto:ScotPHO@nhs.net", "ScotPHO@nhs.net")),
        "and we will be happy to help."),
      shiny::hr(), 
      h4("Notes", style="color: black;"), # Notes
      tags$ul( 
        tags$li("The time periods can vary between indicators."),
        tags$li("Due to differences in data availability, data reported in the profiles 
        may not be consistent with that of published reports.")
      ),  
      shiny::hr(),
      h4("Resources", style="color: black;"), # Resources
      tags$ul( 
        tags$li(tags$a(href="http://www.scotpho.org.uk/opt/Reports/ScotPHO%20Online%20Profiles%20Tool%20-%20User%20Guide.pdf",
             "User guide"), #Link to user guide
          " (1.3 Mb) - Learn how to use and get the most out of the tool."
        ),
        tags$li(tags$a(href="http://www.scotpho.org.uk/opt/Reports/HWP-2015-technical-report-17072015.pdf",
             "Technical report"), #Link to technical report
          " - Detailed description of the methodology, statistics and caveats of the tool."
        ),
        tags$li(tags$a(href="http://www.scotpho.org.uk/opt/Reports/20170228-Rolling-updates-timetable.xlsx",
             "Timetable of updates"), #Link to timetable of updates
          "- List of available indicators, date of last update and expected next update"
        ),
        tags$li(tags$a(href="https://github.com/Health-SocialCare-Scotland/",
                       "Code"), #Link to Github repositories
                "- Access the code used to produce the indicator data and this tool."
        )
      ),
      div(style="height: 40px; background: linear-gradient(#54b4eb, #2fa4e7 60%, #1d9ce5); display:inline-block; width:100%  ",
          tags$b("© Scottish Public Health Observatory v2.0 2018", style="color: white; padding-top: 14px; padding-left: 30px;
               padding-right: 60%; line-height:40px; font-size:70%;")#,
        #bookmarkButton(style="height:25px; font-size:70%; vertical-align:middle; margin-bottom:3px ")
      )
    ),
###############################################.
## Overview/Heatmap ----
###############################################.
tabPanel("Overview", icon = icon("heartbeat"),
         beta_box,  
         sidebarLayout(  
           wellPanel(
             column(8,
                    selectInput("geotype_heat", "Geography level", 
                                choices= areatype_noscot_list),
                    conditionalPanel(#Conditional panel for extra dropdown for localities & IZ
                      condition = "input.geotype_heat== 'HSC Locality' | input.geotype_heat == 'Intermediate zone' ",
                      selectInput("loc_iz_heat", label = "Partnership for localities/intermediate zones",
                                  choices = partnership_name)
                    ),
                    uiOutput("geoname_ui_heat"),
                    selectInput("geocomp_heat", "Comparator", choices = area_list,
                                selectize=TRUE, selected = "Scotland")
             ),
             column(4,
                    selectInput("topic_heat", "Topic", choices = topic_list,
                                selectize=TRUE, selected = "Scotland"),
                    downloadButton('download_heat', 'Download data'),
                    h5(tags$b("Legend"), style="color: black;"),
                    img(src='legend_rank.png', height = 130)
             )
           ),
           mainPanel(width=12,
                     plotlyOutput("heat_plot", width = "100%") #Graph
           )
         )
  ),
# ###############################################.
# ## Time trend ---- 
# ###############################################.
tabPanel("Trend", icon = icon("area-chart"),
         beta_box,  
         p("This section allows you to see changes over time. You can use the filters 
           to select the data you are interested in.To download your data selection as a csv file 
           use the ‘download data’ button. On top of the chart you will see a 
           number of buttons which will allow you to select parts of the chart, zoom in 
           or out or save the chart as an image."),
         wellPanel(tags$style(".well {background-color:#ffffff; border: 0px solid #336699;}"), #color sidebars/well panels
                   column(4,
                          selectInput("indic_trend", "Indicator", choices=indicator_list),
                          selectInput("hbname_trend", "Health board", choices = hb_name,
                                      multiple=TRUE, selectize=TRUE, selected = ""),
                          selectInput("loc_iz_trend", "Partnership for localities/intermediate zones", 
                                      choices = partnership_name)  
                   ),
                   column(4,    
                          selectInput("scotname_trend", "Scotland", choices = c("", "Scotland"), 
                                      selectize=TRUE, selected = "Scotland"),
                          selectInput("laname_trend", "Council area", choices = la_name,
                                      multiple=TRUE, selectize=TRUE, selected = ""),
                          uiOutput("loc_ui_trend")
                   ),
                   column(4, 
                          br(),
                          column(8, downloadButton('download_trend', 'Download data')),
                          column(4, checkboxInput("colorblind_trend",  
                                                  label = "Improved accesibility", value = FALSE)),
                          selectInput("partname_trend", "HSC Partnership", choices = partnership_name,
                                      multiple=TRUE, selectize=TRUE, selected = ""),
                          uiOutput("iz_ui_trend")
                   )
         ),  
         mainPanel(width = 12,
          plotlyOutput("trend_plot")
         )
),
###############################################.
## Rank chart ---- 
###############################################.
tabPanel("Rank", icon = icon("signal"),
         beta_box,  
         p("This section allows you to see a comparison between areas in a specific moment in time
           and how they rate compared to an area of your interest. 
           You can use the filters 
           to select the data you are interested in. To download your data selection as a csv file use 
           the ‘download data’ button. On the top of the chart you will see a number of 
           buttons which will allow you to select parts of the chart, zoom in or out or save the 
           chart as an image."),
         wellPanel( 
           column(6,
                  selectInput("geotype_rank", label = "Geography level",
                              choices = areatype_noscot_list, selected = "Health board"),
                  conditionalPanel( #Conditional panel for extra dropdown for localities & IZ
                    condition = "input.geotype_rank == 'HSC Locality' | input.geotype_rank == 'Intermediate zone' ",
                    selectInput("loc_iz_rank", label = "Partnership for localities/intermediate zones",
                                choices = partnership_name)
                  ),
                  checkboxInput("ci_rank",  
                                label = "Confidence intervals?", value = FALSE),
                  h5(tags$b("Legend"), style="color: black;"),
                  img(src='legend_rank.png', height = 130), 
                  fluidRow(
                    downloadButton('download_rank', 'Download data'))  #For downloading the data
           ),  
           column(6,
                  selectInput("indic_rank", "Indicator", choices=indicator_list),
                  uiOutput("year_ui_rank"), 
                  selectInput("geocomp_rank", "Comparator", choices = area_list,
                              selectize=TRUE, selected = "Scotland")
           )  
         ),
         mainPanel(width = 12,
                   plotlyOutput("rank_plot") 
         )
),
###############################################.
## Deprivation ---- 
###############################################.
tabPanel("Deprivation", icon = icon("gbp"),
         beta_box,  
         p("This section allows you to explore the data by different levels of ",
           tags$a(href="http://www.scotpho.org.uk/life-circumstances/deprivation/key-points/", "deprivation"),
           ". You can use the filters to select the data you are interested in.
           To download your data selection as a csv file use the ‘download data’ button. On top of the chart
            you will see a number of buttons which  will allow you to select
           parts of the chart, zoom in or out or save the chart as an image."),
         wellPanel(tags$style(".well {background-color:#ffffff; border: 0px solid #336699;}"), #color sidebars/well panels
                   column(6, selectInput("indic_simd", label = "Indicator",
                                         choices = ind_depr_list)),
                   column(3, selectInput("geotype_simd", label = "Geography level",
                                         choices = areatype_depr_list, selected =  "Scotland")),
                   column(3, uiOutput("geoname_ui_simd")),
                   column(5, selectInput("year_simd", label = "Time period",
                                         choices = unique(deprivation$trend_axis))),
                   column(2, downloadButton(outputId = 'download_simd')),
                   column(5, selectInput("measure_simd", label = "Type of measure",
                                         choices = c("Rate/Percentage", "Index of inequality")))
         ),
         mainPanel(width = 12,
                   column(6,
                          plotlyOutput("simd_bar_plot")),
                   column(6,
                          plotlyOutput("simd_trend_plot"))
         )
),
###############################################.
###########Map ----
###############################################.
tabPanel("Map", icon = icon("globe"),
         beta_box,  
         p("This section allows you to see the indicator information in a map. You can use the filters 
          to select the indicator and time period you are interested in. Use the checkbox in the map to 
          select the geography layer of your interest. To download your data selection as a csv file use 
           the ‘download data’ button."),
         sidebarPanel(    
           selectInput("indic_map", "Indicator", choices=indicator_list),
           uiOutput("year_ui_map"),
           shiny::hr(),
           h5(tags$b("Legend")),
           img(src='legend_map.png', height=150, style = "align: right")
         ), 
         mainPanel(
           h4(textOutput("title_map")),
           leafletOutput("map")
         )
    ),
###############################################.
## Table ----
###############################################.
tabPanel("Table", icon = icon("table"),
         beta_box,  
         column(9,
                p("This section allows you to view the data in table format. 
                  You can use the filters to select the data you are interested in. 
                  You can also download the data as a csv using the download button.")
                ),
         column(3,
                downloadButton('download_table', 'Download data')  #For downloading the data
         ),
         DT::dataTableOutput('table_opt')
                )
  )#, #Bracket  navbarPage
) # Bracket fluidPage

##END
