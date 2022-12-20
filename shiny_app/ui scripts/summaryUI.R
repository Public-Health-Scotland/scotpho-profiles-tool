###############################################
#
# UI for summary tab
#
###############################################



summaryTab <- tabPanel("Summary", icon = icon("list-ul"), value = "summary",
         introBox(
           wellPanel(fluidRow( #Filter options
             column(3,
                    div(title="Select a geography level first, then select the are you want from the list. You can click in the box, hit backspace and start to type if you want to start searching.",
                        p(tags$b("Step 1. Select a geography level and then an area of interest.")),
                        selectInput("geotype_summary", label = NULL, choices=areatype_list,
                                    selected = "Health board"),
                        conditionalPanel(#Conditional panel for extra dropdown for localities & IZ
                          condition = "input.geotype_summary== 'HSC locality' | input.geotype_summary == 'Intermediate zone' ",
                          div(title="This option restricts the HSC locality or IZ options below to only areas within a parent geography",                      
                              selectInput("loc_iz_summary", label = "Step 1b. Select a region for localities or intermediate zones",
                                          choices = partnership_name))
                        ),
                        uiOutput("geoname_ui_summary"))
             ),
             column(3,
                    div(title="Select the profile you are interested in. Not all profiles are available for all geographies",
                        p(tags$b("Step 2. Select a profile ")),
                        div(id= "summary_div", uiOutput("profile_ui_summary")),
                        # domain if spine selected
                        conditionalPanel(condition = 'input.chart_summary == "Spine"',
                                         uiOutput("topic_ui_spine")))
             ),
             column(3,
                    div(title="Compare against another area (e.g. Scotland) or against a previous period to see the evolution of the area",
                        p(tags$b("Step 3. Select to compare by ")),
                        awesomeRadio("comp_summary", label = NULL,
                                     choices = list("Area or" = 1, "Time" = 2), 
                                     selected = 1, inline=TRUE, checkbox = TRUE),
                        uiOutput("comp_ui_summary")) # comparator options
             ),
             column(3,
                    actionButton("help_summary",label="Help", icon= icon('question-circle'), class ="down"),
                    actionButton("defs_summary",label="Definitions", icon= icon('info'), class ="down"),
                    downloadButton('download_summary', 'Download data', class = "down"),
                    uiOutput("save_chart_ui"))),
             fluidRow(column(12,
                             column(3),#empty column to replicate offset and center content
                             column(6,
                                    p(tags$b("Step 4. Select what type of summary you want to see:"), 
                                      " snapshot is a comparison with the latest data available, 
                                      trend will show how things are changing over time, and 
                                      spine compares indicators with the rest of areas of the same level."),
                                    radioGroupButtons("chart_summary", status = "primary", justified = TRUE,
                                                      choices = c("Snapshot", "Trend", "Spine"), label=NULL  )),
                             column(3) #empty column to replicate offset and center content
                             )) # column and row brackets
           ), #well panel bracket
           data.step = 4, 
           data.intro =(p(h5("Throughout the tool use the dropdown menus to change which indicators or geographies are displayed in the charts."),
                          br(),
                          h5("While using dropdown menus mouse click within a dropdown menu and press backspace on your keyboard ('<-') then start typing a word to quickly find the options you are looking for"),
                          img(src='introjs_how_to_select.png')))
           
           ), #introbox bracket
         mainPanel(width = 12,
                   shiny::hr(),
                   bsModal("mod_defs_summary", "Definitions", "defs_summary",
                           htmlOutput('defs_text_summary')),
                   fluidRow(column(4,
                                   h4(textOutput("summary_title"), style="color: black; text-align: left"),
                                   h5(textOutput("summary_subtitle"), style="color: black; text-align: left")
                   ),
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
                   conditionalPanel(condition = 'input.chart_summary == "Spine"', 
                                    column(5,
                                           br(),
                                           br(),
                                           uiOutput("ui_spine_legend_selected"),
                                           uiOutput("ui_spine_legend_areatype"),
                                           uiOutput("ui_spine_legend_comparator")))),
                   # Depending what users selects different visualizations
                   uiOutput("summary_expl_text"),
                   uiOutput("summary_ui_plots")
         )
           ) #Tab panel bracket