#Code to create ScotPHO's Shiny profile platform
# This script includes the user-interface definition of the app.

###############################################.
## Header ---- 
###############################################.
tagList( #needed for shinyjs
  useShinyjs(),  # Include shinyjs
  navbarPage(id = "intabset", #needed for landing page
           title = div(tags$a(img(src="scotpho_reduced.png", height=40), href= "http://www.scotpho.org.uk/"),
                       style = "position: relative; top: -5px;"), # Navigation bar
           windowTitle = "ScotPHO profiles", #title for browser tab
           theme = shinytheme("cerulean"), #Theme of the app (blue navbar)
           collapsible = TRUE, #tab panels collapse into menu in small screens
           header =         
             tags$head( #CSS styles
               cookie_box, ##Cookie box
               tags$link(rel="shortcut icon", href="favicon_scotpho.ico"), #Icon for browser tab
               #Including Google analytics and Cookie control
               includeScript("google-analytics.js"),
               # HTML('<script src="https://cc.cdn.civiccomputing.com/8/cookieControl-8.x.min.js"></script>'),
               # includeScript("cookie-control.js"),
               #Style sidebars/well panels
               tags$style(".well {background-color:#ffffff; border: 0px solid #336699;
                          padding: 5px; box-shadow: none; }",
                          #Background colour of header navBar
                          ".navbar-brand {background-color: white}",
                          ".navbar {font-size: 14px; border: 0}", #font size and border
                          ".dropdown-menu { font-size: 14px;}", #dropdown menu within navBar
                          ".container-fluid {padding-right: 0px}",
                          #Text size and line height. Padding needed for footer
                          "body { font-size: 14px; line-height: 1.1; padding-bottom:30px}",
                          ".checkbox label, .radio label, .checkbox-bs label, .radio-bs label
                          { line-height: 1.6 }",
                          ".radio-inline {line-height: 2}",
                          #Padding and margins of filters and labels
                          ".form-group {margin: 3px}",
                          ".li-custom {margin-bottom: 10px;}", #bullet point list items
                          ".shiny-options-group { margin-top: 3px; }",
                          ".selectize-control { margin-bottom: 3px}",
                          ".selectize-input {padding: 3px 3px; min-height: 10px; line-height: 18px;}",
                          # ".selectize-input { font-size: 12px; line-height: 18px;}", 
                          ".selectize-dropdown {line-height: 18px; }",
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
                          ".down{background-color:#4da6ff; color: white; background-image:none; min-width: 22vh;
                          font-size: 14px; padding: 5px 10px; margin-top: 5px; margin-left: 3px}",
                          #landing page boxes
                          ".landing-page-box {width:100%; height:100%; min-height:22vh; background-color:white;
                          border: 1px solid #AAAAAA; margin-bottom: 5px; float: left; transition: 0.5s ease; position: relative; object-fit: scale-down;}",
                          ".landing-page-box-about {width:100%; height:100%; min-height:10.7vh; background-color:white;
                          border: 1px solid #AAAAAA; margin-bottom: 5px; float: left; position: relative; object-fit: scale-down;}",
                          ".landing-page-box:hover, .landing-page-box-about:hover {-webkit-transform: scale(1.05); 
                          -ms-transform: scale(1.05); transform: scale(1.05); }", #hover effect on boxes
                          #landing page icons
                          ".landing-page-icon {width:100%; height:85%; min-height:12vh; background-color: white;
                          border: 0px ; position: absolute; object-fit: scale-down;}",
                          ".landing-page-about-icon {width:100%; height:65%; min-height:5vh; background-color: white;
                          border: 0px; position: absolute; object-fit: scale-down;}",
                          #landing-page titles for boxes
                          ".landing-page-box-title {font-size: 16px; text-align:center; color: darkblue;
                          font-weight: bold; background-color: none; width:100%; max-height: 20px; margin-top: 10px; }",
                          #landing page titles for ABOUT boxes
                          ".landing-page-box-about-title {font-size: 16px; text-align:center; color: darkblue;
                          font-weight: bold; background-color: none; width:100%; max-height: 20px; margin-top: 5px; }",
                          #landing page buttons
                          ".landing-page-button {text-align:center;
                          background-image:none; color: black; white-space: normal; border-radius: 0;border: 0px;
                          font-size: 16px; min-height: 16vh; position: absolute; margin-bottom: 0px; margin-top: 5px; float: middle;width: 100%; opacity: 0;}",
                          ".landing-page-button-about {text-align:center;
                          background-image:none; color: black; white-space: normal; border-radius: 0; border:0px ;
                          font-size: 14px; position: absolute; min-height: 7vh; margin-bottom: 0px; margin-top: 1px; float: middle; width: 100%; opacity:0;}",
                          #hover effect on landing page buttons
                           ".landing-page-button:hover , .landing-page-button:active , .landing-page-button-about:hover, .landing-page-button-about:active {opacity: 1; 
                          background-color: #fff; /* fallback */
                          background-color: rgba(255, 255, 255, 0.8);
                          color: darkblue;
                          border-color: #fff; /* fallback */
                          border-color: rgba(255, 255, 255, 0.8); transition: background-color 0.3s ease-in,
                          color 0.3s ease-in;}",
                 #center image - for normal icons
                 "img.center {object-fit: scale-down; position:absolute; width:100%; height:100%; margin-left:auto; margin-right: auto; display: block; padding:20px;}",
                 #center image - for about icons
                 "img.centerabout {object-fit: scale-down; position:absolute; width:100%; height:100%; margin-left:auto; margin-right: auto; display: block; padding:8px;}",
               #landing-page column 
               ".landing-page-column {padding-right:3vh}",
               #landing-page icons
               ".icon-lp{font-size: 1.3em; padding-right: 4px;}",
               #to avoid red text error messages in the whole app, take out for testing
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }",
               #External links underlined an open a new tab
               ".externallink{text-decoration: underline;} ",
               ".definitionbox {width:100%; height:100%; text-align:left ;background-color:white;
        border: 2px solid #2FA4E7; padding: 10px; margin-top: 0px; margin-bottom: 5px; float: left; transition: 0.5s ease; position: relative; object-fit: scale-down;}"),
               HTML("<base target='_blank'>")
               ),
       
###############################################.
## Landing page ----
###############################################.
tabPanel(
  title = " Home", icon = icon("home"),
  mainPanel(
    width = 11, style="margin-left:4%; margin-right:4%",
    fluidRow(h3("Welcome to the ScotPHO profiles", style="margin-top:0px;")),
    fluidRow(h4("Explore data by profile or domain area", style="margin-top:0px; ")),
    fluidRow(
      #Ring plot box
      column(4, class="landing-page-column",
             div(class="landing-page-box", 
                 div("Profile summary", class = "landing-page-box-title"),
                 div(class = "landing-page-icon", style="background-image: url(donut_10.png);
                     background-size: auto 80%; background-position: center; background-repeat: no-repeat; "),
                 actionButton('jump_to_ring', 'A high level view of an area across a suit of indicators', 
                              class="landing-page-button", 
                              icon = icon("arrow-circle-right", "icon-lp")))),
      #Heat map box
      column(4, class="landing-page-column",
             div(class="landing-page-box",
                 div("Topic summary: Heatmap", class = "landing-page-box-title"),
                 div(class = "landing-page-icon", style="background-image: url(heatmap_2.png);
                     background-size: auto 80%; background-position: center; background-repeat: no-repeat; "),
                 actionButton('jump_to_heat', 'Explore how indicators for a topic area have changed over time', 
                              class="landing-page-button", icon = icon("arrow-circle-right", "icon-lp")))),
      #Barcode plot box
      column(4, class="landing-page-column",
             div(class="landing-page-box", 
                 div("Topic summary: Barcode", class = "landing-page-box-title"),
                 div(class = "landing-page-icon", style="background-image: url(barcode_3.png);
                     background-size: auto 80%; background-position: center; background-repeat: no-repeat; "),
                 actionButton('jump_to_barcode', 'Explore how indicators for a topic compare across different geographies',
                              class="landing-page-button", icon = icon("arrow-circle-right", "icon-lp"))))
    ),
    fluidRow(h4("Explore a single indicator in more detail")),
    #2nd row of boxes
    fluidRow(
      #Trend plot box
      column(4, class="landing-page-column",
             div(class="landing-page-box", 
                 div("Trend", class = "landing-page-box-title"),
                 div(class = "landing-page-icon", style="background-image: url(time_trend.png);
                     background-size: auto 80%; background-position: center; background-repeat: no-repeat; "),
                 actionButton('jump_to_trend', 'Look at how an indicator changes over time',
                              class="landing-page-button", 
                              icon = icon("arrow-circle-right", "icon-lp")))),
      #Rank/map plot box
      column(4, class="landing-page-column",
             div(class="landing-page-box", 
                 div("Rank", class = "landing-page-box-title"),
                 div(class = "landing-page-icon", style="background-image: url(maprank.png);
                     background-size: auto 75%; background-position: center; background-repeat: no-repeat; "),
                 actionButton('jump_to_rank', 'Compare geographical variation for an indicator', 
                              class="landing-page-button", icon = icon("arrow-circle-right", "icon-lp")))),
      #Inequalities box
      column(4, class="landing-page-column",
             div(class="landing-page-box", 
                 div("Health inequalities", class = "landing-page-box-title"),
                 div(class = "landing-page-icon", style="background-image: url(health_inequality.png);
                     background-size: auto 85%; background-position: center; background-repeat: no-repeat; "),
                 # Currently a link to the health inequalities app
                 actionButton('jump_to_simd', 'Explore how an indicator varies with socioeconomic deprivation', 
                              onclick ="window.open('https://scotland.shinyapps.io/scotpho-health-inequalities', '_blank')",
                              class="landing-page-button", icon = icon("arrow-circle-right", "icon-lp"))))
    ), # end of second row
    fluidRow(h4("Access the data behind the tool and find supporting information")),
    fluidRow(
      #Table box 
      column(4, class="landing-page-column",
             div(class="landing-page-box", 
                 div("Data", class = "landing-page-box-title"),
                 div(class = "landing-page-icon", style="background-image: url(data_table.png);
                     background-size: auto 80%; background-position: center; background-repeat: no-repeat; "),
                 actionButton('jump_to_table', 'View and download the data behind the tool', 
                              class="landing-page-button", 
                              icon = icon("arrow-circle-right", "icon-lp"))
             )),
      #About box
      column(4, class="landing-page-column",
             div(class="landing-page-box-about", 
                 div("About", class = "landing-page-box-title"),
                 div(class = "landing-page-about-icon", style="background-image: url(about_2.png);
                     background-size: auto 80%; background-position: center; background-repeat: no-repeat; "),
                 actionButton('jump_to_about', 'About ScotPHO Profiles', 
                              class="landing-page-button-about", 
                              icon = icon("arrow-circle-right", "icon-lp"))
             ),
             #Evidence box
             div(class="landing-page-box-about", 
                 div("Evidence for action", class = "landing-page-box-title" ),
                 div(class = "landing-page-about-icon", div(img(src="other_profile.png", class="centerabout"))),
                 actionButton('jump_to_efa', 'Links to ScotPHO evidence for action briefings', 
                              onclick ="window.open('https://www.scotpho.org.uk/comparative-health/profiles/resources/evidence-for-action/', '_blank')",
                              class="landing-page-button-about", 
                              icon = icon("arrow-circle-right", "icon-lp")))
      ),
      #Resources box
      column(4, class="landing-page-column", 
             div(class="landing-page-box-about",
                 div("Resources", class = "landing-page-box-title"),
                 div(class = "landing-page-about-icon", style="background-image: url(technical_resources.png);
                     background-size: auto 80%; background-position: center; background-repeat: no-repeat; "),
                 actionButton('jump_to_resources', 'Find technical information about the ScotPHO profile definitions and methodology', 
                              class="landing-page-button-about", 
                              icon = icon("arrow-circle-right", "icon-lp"))
             ),
             #Other profiles
             div(class="landing-page-box-about", 
                 div("Other profiles", class = "landing-page-box-title"),
                 div(class = "landing-page-about-icon", style="background-image: url(related_links.png);
                     background-size: auto 80%; background-position: center; background-repeat: no-repeat; "),
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
tabPanel(title = "Summary", icon = icon("adjust"), value = "ring",
         sidebarPanel(width=3,
                      actionButton("help_ring", label="Help", icon= icon('question-circle'), class ="down"),
                      selectInput("profile_ring", "Profile", choices= profile_list, multiple=FALSE, selected = "HWB"),
                      uiOutput("geotype_ui_ring"),
                      conditionalPanel(#Conditional panel for extra dropdown for localities & IZ
                        condition = "input.geotype_ring== 'HSC locality' | input.geotype_ring == 'Intermediate zone' ",
                        selectInput("loc_iz_ring", label = "Partnership for localities/intermediate zones",
                                    choices = partnership_name)
                      ),
                      uiOutput("geoname_ui_ring"),
                      br(),
                      p("This visualisation allows you to compare one area to another area or the same area over time"),
                      awesomeRadio("comp_ring", label = "Compare against",
                                   choices = list("Area" = 1, "Time" = 2),
                                   selected = 1, inline=TRUE, checkbox=TRUE),
                      conditionalPanel(condition = "input.comp_ring == 1 ",
                                       selectInput("geocomp_ring", "Compare with", choices = comparator_list,
                                                   selectize=TRUE, selected = "Scotland")
                      ),
                      conditionalPanel(condition = "input.comp_ring == 2 ",
                                       uiOutput("yearcomp_ui_ring")),
                      #Legend
                      br(),
                      p(img(src='signif_better.png', height=18, style="padding-right: 2px; vertical-align:middle"), 
                        "Better than comparator", br(),
                        img(src='non_signif.png', height=18, style="padding-right: 2px; vertical-align:middle"), 
                        "Not different to comparator", br(),
                        img(src='signif_worse.png', height=18, style="padding-right: 2px; vertical-align:middle"), 
                        "Worse than comparator", br(),
                        img(src='signif_nocalc.png', height=18, style="padding-right: 2px; vertical-align:middle"), 
                        "No differences can be calculated"), 
                      downloadButton('download_ring', 'Download data', class = "down"),
                      savechart_button('download_ringplot', 'Save chart',  class = "down")
         ),
         mainPanel(width = 9,
                   h4(textOutput("ring_title"), style="color: black; text-align: left"),
                   h5(textOutput("ring_subtitle"), style="color: black; text-align: left"),
                   withSpinner(plotOutput("ring_plot", height="auto"))
         )
), #Tab panel bracket
###############################################.
## Heat map ----
###############################################.
tabPanel("Heatmap", icon = icon("list-ul"), value = "heat",
         wellPanel( #Filter options
           column(2,
                  selectInput("profile_heat", "Profile", choices = profile_list),
                  uiOutput("topic_ui_heat")
           ),
           column(3,
                  uiOutput("geotype_ui_heat"),
                  conditionalPanel(#Conditional panel for extra dropdown for localities & IZ
                    condition = "input.geotype_heat== 'HSC locality' | input.geotype_heat == 'Intermediate zone' ",
                    selectInput("loc_iz_heat", label = "Partnership for localities/intermediate zones",
                                choices = partnership_name)
                  ),
                  uiOutput("geoname_ui_heat")
           ),
           column(2,
                  awesomeRadio("comp_heat", label = "Compare against",
                               choices = list("Area" = 1, "Time" = 2), 
                               selected = 1, inline=TRUE, checkbox = TRUE),
                  conditionalPanel(condition = "input.comp_heat == 1 ",  
                                   selectInput("geocomp_heat", "Compare with", choices = comparator_list,
                                               selectize=TRUE, selected = "Scotland")
                  ),
                  conditionalPanel(condition = "input.comp_heat == 2 ", 
                                   uiOutput("yearcomp_ui_heat")
                  ) 
           ),
           column(3,
                  #Legend
                  p(img(src='signif_better.png', height=18, style="padding-right: 2px; vertical-align:middle"), 
                    "Better than comparator", br(),
                    img(src='non_signif.png', height=18, style="padding-right: 2px; vertical-align:middle"), 
                    "Not different to comparator", br(),
                    img(src='signif_worse.png', height=18, style="padding-right: 2px; vertical-align:middle"), 
                    "Worse than comparator", br(),
                    img(src='signif_nocalc.png', height=18, style="padding-right: 2px; vertical-align:middle"), 
                    "No differences can be calculated")
           ),
           column(2,
                  actionButton("help_heat",label="Help", icon= icon('question-circle'), class ="down"),
                  actionButton("defs_heat",label="Definitions", icon= icon('info'), class ="down"),
                  downloadButton('download_heat', 'Download data', class = "down"),
                  savechart_button('download_heatplot', 'Save chart',  class = "down")
           )
         ),
         mainPanel(width = 12,
                   bsModal("mod_defs_heat", "Definitions", "defs_heat", htmlOutput('defs_text_heat')),
                   h4(textOutput("heat_title"), style="color: black; text-align: left"),
                   h5(textOutput("heat_subtitle"), style="color: black; text-align: left"),
                   withSpinner(plotlyOutput("heat_plot"))
        )
  ), #Tab panel bracket
#####################################################################.
## Barcode ----
#####################################################################.
tabPanel("Barcode", icon = icon("barcode"), value = "barcode",
         wellPanel(
           column(3,
                  selectInput("profile_spine", "Profile", choices = profile_list),
                  uiOutput("topic_ui_spine")
           ),
           column(3,
                  uiOutput("geotype_ui_spine"),
                  conditionalPanel(#Conditional panel for extra dropdown for localities & IZ
                    condition = "input.geotype_spine== 'HSC locality' | input.geotype_spine == 'Intermediate zone' ",
                    selectInput("loc_iz_spine", label = "Partnership for localities/intermediate zones", choices = partnership_name))
           ),
           column(2,
                  uiOutput("geoname_ui_spine")
           ),
           column(2,
                  selectInput("geocomp_spine", "Select a comparison area", choices = comparator_list, selectize=TRUE, selected = "Scotland")
           ),
           column(2,
                  actionButton("help_spine", label="Help", icon= icon('question-circle'), class ="down"),
                  bsModal("mod_defs_spine", "Definitions", "defs_spine", htmlOutput('defs_text_spine')),
                  actionButton("defs_spine",label="Definitions", icon= icon('info'), class ="down"),
                  downloadButton('download_spine', 'Download data', class = "down"),
                  savechart_button('download_spineplot', 'Save chart',  class = "down"))
         ),
         wellPanel(
           column(4,
                  h4(textOutput("spine_title"), style="color: black; text-align: left"),
                  h5(textOutput("spine_subtitle"), style="color: black; text-align: left")),
           column(3,
                  br(),
                  br(),
                  p(img(src='signif_better.png', height=18, style="padding-right: 2px; vertical-align:middle"), 
                    "Better than comparator", br(),
                    img(src='non_signif.png', height=18, style="padding-right: 2px; vertical-align:middle"), 
                    "Not different to comparator", br(),
                    img(src='signif_worse.png', height=18, style="padding-right: 2px; vertical-align:middle"), 
                    "Worse than comparator", br(),
                    img(src='signif_nocalc.png', height=18, style="padding-right: 2px; vertical-align:middle"), 
                    "No differences can be calculated")),
           column(5,
                  br(),
                  br(),
                  uiOutput("ui_spine_legend_selected"),
                  uiOutput("ui_spine_legend_areatype"),
                  uiOutput("ui_spine_legend_comparator"))),
         mainPanel(
           column(12,
           withSpinner(uiOutput("ui_spine_plot")))
          )
), #Tab panel bracket
###############################################.
## Time trend ----
###############################################.
tabPanel("Trend", icon = icon("area-chart"), value = "trend",
                   sidebarPanel(width=4,
                          selectInput("indic_trend", "Indicator", choices=indicator_list),
                          div(title= "Display the rate/percentage data or the raw numbers.",
                              awesomeRadio("var_plot_trend", label =NULL, inline = TRUE, 
                                            choices = c("Rate/Percentage" = "measure", 
                                                   "Numerator" = "numerator"))),
                          div(title="Show or hide the 95% confidence intervals for the data selected.", # tooltip
                            awesomeCheckbox("ci_trend", label = "95% confidence intervals", value = FALSE)),
                          actionButton("defs_trend", label="Definitions", icon= icon('info'), class ="down"),
                          shiny::hr(),
                          p(tags$b("Select the areas you want to plot. You can select multiple 
                                   areas for each type of geography.")),
                          awesomeCheckbox("scotname_trend", tags$b("Scotland"), value=TRUE),
                          column(6,
                                 selectizeInput("hbname_trend", "Health board", choices = c("Select health boards" = "", paste(hb_name)),
                                             multiple=TRUE, selected = ""),
                                 selectizeInput("partname_trend", "HSC partnership", choices =  c("Select partnerships" = "", paste(partnership_name)),
                                             multiple=TRUE, selected = "")),
                          column(6,
                            selectizeInput("caname_trend", "Council area", choices =  c("Select council areas" = "", paste(la_name)),
                                        multiple=TRUE, selected = ""),
                            selectizeInput("adpname_trend", "Alcohol & drug partnership", choices =  c("Select partnerships" = "", paste(adp_name)),
                                        multiple=TRUE, selected = "")),
                          selectInput("loc_iz_trend", "To choose a locality or intermediate zone, first 
                                      select an HSC partnership", choices = partnership_name),
                          column(6, uiOutput("loc_ui_trend")),
                          column(6,uiOutput("iz_ui_trend")),
                          downloadButton('download_trend', 'Download data', class = "down"),
                          savechart_button('download_trendplot', 'Save chart',  class = "down")
                   ),
         mainPanel(width = 8, #Main panel
          bsModal("mod_defs_trend", "Definitions", "defs_trend", htmlOutput('defs_text_trend')),
          h4(textOutput("title_trend"), style="color: black; text-align: left"),
          withSpinner(plotlyOutput("trend_plot"))
         )
), #Tab panel bracket
###############################################.
## Rank and map ---- 
###############################################.
tabPanel("Rank", icon = icon("signal"), value = "rank",
         wellPanel(#Filter options
           column(width = 4,
                 selectInput("indic_rank", "Indicator", choices=indicator_list),
                 uiOutput("geotype_ui_rank"),
                 conditionalPanel( #Conditional panel for extra dropdown for localities & IZ
                   condition = "input.geotype_rank == 'HSC locality' | input.geotype_rank == 'Intermediate zone' ",
                   selectInput("loc_iz_rank", label = "Partnership for localities/intermediate zones",
                               choices = partnership_name))
                 ),
           column(width = 3,
                  uiOutput("year_ui_rank"), 
                  awesomeRadio("comp_rank", label = "Compare against:",
                               choices = list("Area" = 1, "Time" = 2), 
                               selected = 1, inline=TRUE, checkbox=TRUE),
                  conditionalPanel(condition = "input.comp_rank == 1 ",  
                                   selectInput("geocomp_rank", "Comparator", choices = comparator_list,
                                               selectize=TRUE, selected = "Scotland")
                  ),
                  conditionalPanel(condition = "input.comp_rank == 2 ", 
                                   uiOutput("yearcomp_ui_rank")
                  )
           ),
           column(width = 3,
                 awesomeCheckbox("ci_rank", label = "95% confidence intervals", value = FALSE),
                 #Legend
                 p(img(src='signif_better.png', height=12, style="padding-right: 2px; vertical-align:middle"), 
                   "Better than comparator", br(),
                   img(src='non_signif.png', height=12, style="padding-right: 2px; vertical-align:middle"), 
                   "Not different to comparator", br(),
                   img(src='signif_worse.png', height=12, style="padding-right: 2px; vertical-align:middle"), 
                   "Worse than comparator", br(),
                   img(src='signif_nocalc2.png', height=12, style="padding-right: 2px; vertical-align:middle"), 
                   "No differences can be calculated", br(),
                   img(src='baseline_year_color.png', height=12, style="padding-right: 2px; vertical-align:middle"), 
                   "Baseline year comparison")
           ),
           column(width = 2,
                 actionButton("defs_rank", label="Definitions", icon= icon('info'), class ="down"), 
                 downloadButton('download_rank', 'Download data', class = "down"),
                 savechart_button('download_rankplot', 'Save chart', class = "down"),
                 savechart_button('download_mapplot', 'Save map', class = "down")
           )
         ), #well pannel bracket
         mainPanel(width = 12, #Main panel
           bsModal("mod_defs_rank", "Definitions", "defs_rank", htmlOutput('defs_text_rank')),
           column(width = 7, #rank bar
                  h4(textOutput("rank_title"), style="color: black; text-align: left"),  
                  h5(textOutput("rank_subtitle"), style="color: black; text-align: left"),  
                  withSpinner(plotlyOutput("rank_plot"))           ),
           column(width = 5, #map
             uiOutput("map_ui")
           )
         ) #main panel bracket
), #Tab panel bracket
###############################################.
## Data ----
###############################################.
tabPanel("Data", icon = icon("table"), value = "table",
         #Sidepanel for filtering data
         mainPanel(
           width = 12, style="margin-left:0.5%; margin-right:0.5%",
           #Row 1 for intro  
           fluidRow(
                    p("Filter ScotPHO data by", style = "font-weight: bold; color: black;"),
                    tags$div("Select appropriate conditions to filter data. ",
                             "To delete choices use RETURN or select item and DELETE"),
                    br()
                    ),
           #Row 2 for selections
           fluidRow(
             column(3,
                    p("Profile product", style = "font-weight: bold; color: black;"),  
                    tags$div("All available indicators will be displayed for
                             selected geography if none specified"),
                    awesomeRadio("product_filter", label=NULL, choices = c("Indicator", "Domain", "Profile"), selected = NULL, inline = FALSE,
                                 status = "primary", checkbox = TRUE),
                    conditionalPanel(condition="input.product_filter=='Indicator'",
                      selectizeInput("indicator_filter", label = NULL,
                                     choices = indicator_list, selected = NULL,
                                     multiple=TRUE, options = list(maxOptions = 1000, placeholder = "Select or type indicators to filter by"))
                      
                    ),
                    conditionalPanel(condition="input.product_filter=='Domain'",
                      selectizeInput("topic_filter", label = NULL,
                                     choices = topic_list, selected = NULL,
                                     multiple=TRUE, options = list(maxOptions = 1000, placeholder = "Select or type domains to filter by"))
                    ),
                    conditionalPanel(condition="input.product_filter=='Profile'",
                      selectizeInput("profile_filter", label = NULL,
                                     choices = profile_list, selected = NULL,
                                     multiple=TRUE, options = list(maxOptions = 1000, placeholder = "Select or type profiles to filter by"))    
                    )
             ),# column bracket
             column(3,
                    p("Geography", style = "font-weight: bold; color: black;"),
                    # Scotland selections
                    awesomeCheckbox("scotland",label = "Scotland", value = FALSE),
                    # Panel for health board selections
                    awesomeCheckbox("hb",label = "Health board", value = FALSE),
                    conditionalPanel(
                      condition = "input.hb == true",
                      selectInput("hb_true", label = NULL,
                                  choices = hb_name, selected = NULL, multiple=TRUE)),
                    # Panel for council area selections
                    awesomeCheckbox("la", label = "Council area", value = FALSE),
                    conditionalPanel(condition = "input.la == true",
                      selectizeInput("la_true", label = NULL,
                                     choices = la_name, selected = NULL, multiple=TRUE, 
                                     options = list(placeholder = "Select or type council area of interest"))),
                    # Panel for ADP selections
                    awesomeCheckbox("adp",label = "Alcohol & drug partnership", value = FALSE),
                    conditionalPanel(condition = "input.adp == true",
                      selectizeInput("adp_true", label = NULL,
                                     choices = adp_name, selected = NULL, multiple=TRUE,
                                     options = list(placeholder = "Select or type ADP of interest")))  
                    
             ), # column bracket
             column(3,
                    br(),
                    # Panel for HSC partnership selections
                    awesomeCheckbox("hscp",label = "Health & social care partnership", value = FALSE),
                    conditionalPanel(
                      condition = "input.hscp == true",
                      selectInput("hscp_true", label = NULL, choices = partnership_name, 
                                  selected = NULL, multiple=TRUE)),
                    # Panel for locality selections
                    awesomeCheckbox("hscl",label = "Health & social care locality", value = FALSE),
                    conditionalPanel(condition = "input.hscl == true",
                                     selectizeInput("hscl_parent", label = "Filter locality list by HSC partnership",
                                                    choices = parent_geo_list, 
                                                    selected = "Show all", multiple=FALSE),
                                     # if they haven't selected all, show tickbox so they can select all localities of parent area
                                     conditionalPanel(condition = "input.hscl_parent != 'Show all'",
                                                      checkboxInput("hscl_parent_all",label = "Select all HSC localities in this area", 
                                                                    value = FALSE)),
                                     uiOutput("hscl_filtered")),
                    # Panel for intermediate zone selections
                    awesomeCheckbox("iz",label = "Intermediate zone", value = FALSE),
                    conditionalPanel(condition = "input.iz == true",
                                     selectizeInput("iz_parent", label = "Filter intermediate zone list by HSC partnership",
                                                    choices = parent_geo_list, selected = "Show all", multiple=FALSE),
                                     # if they haven't selected all, show tickbox so they can select all izs of parent area
                                     conditionalPanel(condition = "input.iz_parent != 'Show all'",
                                                      checkboxInput("iz_parent_all",label = "Select all intermediate zones in this area", value = FALSE)),
                                     uiOutput("iz_filtered")),
                    # To select all available geographies
                    awesomeCheckbox("all_data",label = "All available geographies", value = FALSE),
                    # to search by code
                    selectizeInput("code", label = NULL, choices = code_list,
                                   options = list(placeholder = 'Or search by area code'), 
                                   multiple=TRUE, selected = "")
             ), #column bracket
             column(3,
                    p("Time period", style = "font-weight: bold; color: black;"),
                    sliderInput("date_from",label = NULL, min = min_year, max = max_year, value = c(min_year,max_year), 
                                 step = 1, sep="", round = TRUE, ticks = TRUE, dragRange = FALSE),
                    br(),
                    actionButton("clear", label = "Clear all filters",  icon ("eraser"), class = "down"),
                    downloadButton("download_table_csv", 'Download data', class = "down")
             ) #column bracket
           ), #filters fluid row bracket

           #Row 3- Table
           fluidRow(  
             column(12, div(DT::dataTableOutput("table_filtered"), 
                            style = "font-size: 98%; width: 98%"))
             )
           ) # main panel bracket
 ), #Tab panel bracket  
###############################################.             
##############About----    
###############################################.
#Starting navbarMenu to have tab with dropdown list
navbarMenu("Info", icon = icon("info-circle"), 
           tabPanel("About", value = "about",
                    sidebarPanel(width=1),
                    mainPanel(width=8,
                              h4("About", style = "color:black;"),
                              p("ScotPHO's profiles tool allows users to explore the various different profiles 
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
           ## Definitions ----
           ###############################################.
           tabPanel("Indicator definitions", value = "definition",
                    #Sidepanel for filtering data
                    fluidRow(
                      column(width = 5, offset= 1,
                             p("Indicator definitions and technical information", style = "font-weight: bold; color: black;"),
                             div(style="display:inline-block",
                                 selectizeInput("profile_defined", label = "Filter by Profile",
                                                width = "250px", choices = profile_list_filter, 
                                                selected = "Show all", multiple=FALSE)),
                             div(style="display:inline-block", 
                                 selectizeInput("topic_defined", label = "Or by Topic",
                                                width = "250px", choices = topic_list_filter, 
                                                selected = "Show all", multiple=FALSE)),
                             uiOutput("indicator_chosen")
                      ), # column bracket 
                      column(width=6,
                             br(), br(), br(),
                             downloadButton("definitions_by_indicator", 
                                            'Download indicator definition', class = "down")
                      )#column bracket
                    ), #fluidRow bracket
                    fluidRow(      
                      column(width=10, offset=1,
                             useShinydashboard(),
                             conditionalPanel(
                               condition="input.indicator_defined != null",
                               valueBoxOutput("indicator", width=12)))),
                    fluidRow(
                      column(width=5, offset=1,
                             conditionalPanel(
                               condition="input.indicator_defined != null",
                               div(class="definitionbox",
                                   p(paste("Definition"), style="font-weight:bold; font-size: 16px; color: #2FA4E7;"),
                                   h5(textOutput("definition"))),
                               div(class="definitionbox",   
                                   p(paste("Data source"), style="font-weight:bold; font-size: 16px; color: #2FA4E7;"),
                                   h5(textOutput("source"))),
                               div(class="definitionbox",   
                                   p(paste("Numerator"), style="font-weight:bold; font-size: 16px; color: #2FA4E7;"),
                                   h5(textOutput("numerator"))),
                               div(class="definitionbox",   
                                   p(paste("Measure"), style="font-weight:bold; font-size: 16px; color: #2FA4E7;"),
                                   h5(textOutput("measure"))),
                               div(class="definitionbox",   
                                   p(paste("Rounding and imputation"), style="font-weight:bold; font-size: 16px; color: #2FA4E7;"),
                                   h5(textOutput("rounding"))),
                               div(class="definitionbox",   
                                   p(paste("Year type"), style="font-weight:bold; font-size: 16px; color: #2FA4E7;"),
                                   h5(textOutput("year"))),
                               div(class="definitionbox",   
                                   p(paste("Trends from"), style="font-weight:bold; font-size: 16px; color: #2FA4E7;"),
                                   h5(textOutput("trends_from"))),
                               div(class="definitionbox",   
                                   p(paste("Geographies available"), style="font-weight:bold; font-size: 16px; color: #2FA4E7;"),
                                   h5(textOutput("geos"))),
                               div(class="definitionbox",   
                                   p(paste("Notes,caveats and other info"), style="font-weight:bold; font-size: 16px; color: #2FA4E7;"),
                                   h5(textOutput("notes"))),
                               div(class="definitionbox",   
                                   p(paste("Date last updated"), style="font-weight:bold; font-size: 16px; color: #2FA4E7;"),
                                   h5(textOutput("last_updated")))
                             ) # conditionalPanel bracket
                      ), #column bracket
                      column(width=5, 
                             conditionalPanel(
                               condition="input.indicator_defined != null",
                               div(class="definitionbox",
                                   p(paste("Rationale for inclusion"), style="font-weight:bold; font-size: 16px; color: #2FA4E7;"),
                                   h5(textOutput("rationale"))),
                               div(class="definitionbox",   
                                   p(paste("Diagnostic codes & position"), style="font-weight:bold; font-size: 16px; color: #2FA4E7;"),
                                   h5(textOutput("diagnosis"))),
                               div(class="definitionbox",
                                   p(paste("Denominator"), style="font-weight:bold; font-size: 16px; color: #2FA4E7;"),
                                   h5(textOutput("denominator"))),
                               div(class="definitionbox",
                                   p(paste("Disclosure control"), style="font-weight:bold; font-size: 16px; color: #2FA4E7;"),
                                   h5(textOutput("disclosure"))),
                               div(class="definitionbox",
                                   p(paste("Age group"), style="font-weight:bold; font-size: 16px; color: #2FA4E7;"),
                                   h5(textOutput("age"))),
                               div(class="definitionbox",
                                   p(paste("Sex"), style="font-weight:bold; font-size: 16px; color: #2FA4E7;"),
                                   h5(textOutput("sex"))),
                               div(class="definitionbox",
                                   p(paste("Aggregation"), style="font-weight:bold; font-size: 16px; color: #2FA4E7;"),
                                   h5(textOutput("aggregation"))),
                               div(class="definitionbox",
                                   p(paste("Frequency of update"), style="font-weight:bold; font-size: 16px; color: #2FA4E7;"),
                                   h5(textOutput("update_frequency"))),
                               div(class="definitionbox",
                                   p(paste("Confidence interval method"), style="font-weight:bold; font-size: 16px; color: #2FA4E7;"),
                                   h5(textOutput("confidence_interval"))),
                               div(class="definitionbox",
                                   p(paste("Links to supporting information"), style="font-weight:bold; font-size: 16px; color: #2FA4E7;"),
                                   h5(textOutput("supporting_info"))),
                               div(class="definitionbox",
                                   p(paste("Next update due"), style="font-weight:bold; font-size: 16px; color: #2FA4E7;"),
                                   h5(textOutput("next_update")))
                             ) #conditional Panel bracket 
                      )  #column bracket
                    )#fluidRow bracket
           ), #Tab panel bracket             
###############################################.             
##############Resources----    
###############################################.      
           tabPanel("Resources", value = "resources",
                    sidebarPanel(width=1),
                    mainPanel(
                      h4("Resources", style = "color:black;"),
                      p("We list a number of resources that help you to understand better the profiles or to
                        carry out similar analysis to ours"),
                      tags$ul( 
                        #Link to user guide
                        tags$li(class= "li-custom", tags$a(href="https://www.scotpho.org.uk/media/1691/scotpho-profiles-quick-reference-guide-sep2018.docx", 
                                                           "User guide",  class="externallink"), 
                                " - Learn how to use and get the most out of the tool"),
                        #Link to overview reports
                        tags$li(class= "li-custom", tags$a(href="http://www.scotpho.org.uk/comparative-health/profiles/resources/",
                                                           "Overview reports",  class="externallink"), 
                                " - These provide context, narrative and analysis for each profile"),
                        #Link to user guide
                        tags$li(class= "li-custom", tags$a(href="http://www.scotpho.org.uk/media/1026/explanation-of-statistics-used-in-profiles-v2.pptx", 
                                                           "Statistics of the profiles",  class="externallink"), 
                                " - A guide and explanation of the statistics used in the profiles"),
                        #Link to timetable of updates
                        tags$li(class= "li-custom", tags$a(href="https://docs.google.com/spreadsheets/d/e/2PACX-1vQUQMORMqe9RrMnS9WJSu51Q6ef0rubiF1M-QN3BYZIBueErtTvvbRe_kTZbWmnupiO_Uie80BoZCnK/pubhtml",
                                                           "Timetable of updates", class="externallink"), 
                                "- List of available indicators, date of last update and expected next update"),
                        #Link to Github repositories
                        tags$li(class= "li-custom", tags$a(href="https://github.com/ScotPHO/indicator-production",
                                                           "Indicator production code", class="externallink"), 
                                " and ",
                                tags$a(href="https://github.com/ScotPHO/scotpho-profiles-tool",
                                       "Profile tool code", class="externallink"), 
                                "- Access the code used to produce the indicator data and this tool"),
                        #Link to population lookups
                        tags$li(class= "li-custom", tags$a(href="https://www.opendata.nhs.scot/dataset/population-estimates",
                                                           "Population estimate", class="externallink"),  " and ",
                                tags$a(href="                   https://www.opendata.nhs.scot/dataset/geography-codes-and-labels",
                                       "geography names and codes", class="externallink"), 
                                "- Where you can find the files with the populations and geographies
                                used for the analysis"),
                        #Link to shapefiles
                        tags$li(class= "li-custom", tags$a(href="https://data.gov.uk/publisher/scottish-government-spatial-data-infrastructure",
                                                           "Shapefiles", class="externallink"), 
                                "- Where you can find the shapefiles used for the map")
                        ), #Bullet point list bracket
                      br()
                      ) # mainPanel bracket
), #Tab panel bracket
###############################################.             
##############Evidence for action----    
###############################################. 
tabPanel(a("Evidence for action", href="https://www.scotpho.org.uk/comparative-health/profiles/resources/evidence-for-action/", target="_blank")
), #tabPanel bracket
###############################################.             
##############Other profiles----    
###############################################.
tabPanel("Other profiles", value = "others",
         sidebarPanel(width=1),
         mainPanel(
           h4("Alternative profiles & resources", style = "color:black;"),
           p("There are a number of organisations that provide local information relating to the wider determinants of health in Scotland.
             Below are links to some of alternative profiling products."),
           tags$ul( 
             #Link to old tool
             tags$li(class= "li-custom", tags$a(href="https://scotpho.nhsnss.scot.nhs.uk/scotpho/homeAction.do", 
                                                "Historic ScotPHO profiles",  class="externallink"), 
                     " - The old style ScotPHO profiles are currently still accessible via our old profile platform"),
             #Link to GCPH
             tags$li(class= "li-custom", tags$a(href="http://www.nssdiscovery.scot.nhs.uk/",
                                                "NSS Discovery",  class="externallink")), 
             #Link to GCPH
             tags$li(class= "li-custom", tags$a(href="http://www.understandingglasgow.com/",
                                                "Glasgow Centre for Population Health (GCPH)",  class="externallink")), 
             #Link to Fife
             tags$li(class= "li-custom", tags$a(href="https://knowfife.fife.gov.uk/",
                                                "KnowFife Dataset",  class="externallink")), 
             #Link to IS
             tags$li(class= "li-custom", tags$a(href="http://www.improvementservice.org.uk/community-planning-outcomes-profile.html",
                                                "Improvement Service (IS) - Community planning outcomes profile (CPOP)",  class="externallink")), 
             #Link to NRS
             tags$li(class= "li-custom", tags$a(href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/stats-at-a-glance/council-area-profiles", 
                                                "National Records of Scotland (NRS) Council Area Profiles",  class="externallink")), 
             #Link to stats.gov.scot
             tags$li(class= "li-custom", tags$a(href="http://statistics.gov.scot/home", 
                                                "Statistics.gov.scot",  class="externallink")), 
             #Link to Scottish nation
             tags$li(class= "li-custom", tags$a(href="http://www.environment.gov.scot/", 
                                                "Scotland's Environment Hub",  class="externallink"))
           ), #Bullet point list bracket
           br()
           ) # mainPanel bracket
           ) #tabPanel bracket

)# NavbarMenu bracket
  ), #Bracket  navbarPage

###############################################.             
##############Footer----    
###############################################.
#Copyright warning
tags$footer(column(6, "© Scottish Public Health Observatory v2.0 2018"), 
            column(2, tags$a(href="mailto:ScotPHO@nhs.net", tags$b("Contact us!"), 
                              class="externallink", style = "color: white; text-decoration: none")), 
            column(3, tags$a(href="https://www.scotpho.org.uk/about-us/scotpho-website-policies-and-statements/privacy-and-cookies", tags$b("Privacy & cookies"), 
                             class="externallink", style = "color: white; text-decoration: none")), 
            column(1, actionLink("twitter_share", label = "Share", icon = icon("twitter"),
                              style= "color:white;", onclick = sprintf("window.open('%s')", 
                              "https://twitter.com/intent/tweet?text=Check%out%ScotPHO's%profile%tool&url=https://scotland.shinyapps.io/ScotPHO_profiles_tool/"))), 
  style = "
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
) #bracket tagList
###END