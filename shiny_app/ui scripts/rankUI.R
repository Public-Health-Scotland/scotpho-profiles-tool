###############################################
#
# UI for rank tab
#
###############################################



rankTab <- 

tabPanel(div(
  div(class="fa fa-signal", role = "navigation"),
  "Rank"),
  value = "rank",
  wellPanel(#Filter options
    column(width = 4,
           div(title="Select an indicator to see comparative information. Click in this box, hit backspace and start to type if you want to quickly find an indicator.",
               selectInput("indic_rank", shiny::HTML("<p>Step 1. Select an indicator <span style='font-weight: 400'> <br/> 
                                                     (hit backspace and start typing to search for an indicator)</span></p>"), 
                           choices=indicator_list, selected = "Alcohol-related hospital admissions")),
           div(title="Use this option to change the type of geography displayed in the chart. 
               Some indicators are not be available for all geography types. 
               See the indicator definitions tab to find out which geographies indicators are available for.",
               uiOutput("geotype_ui_rank")),
           div(title="There are too many hscp localities or IZs to show in the rank chart a 
               selection must be made to limit localities or IZs to only those within a parent area",
               conditionalPanel( #Conditional panel for extra dropdown for localities & IZ
                 condition = "input.geotype_rank == 'HSC locality' | input.geotype_rank == 'Intermediate zone' ",
                 selectInput("loc_iz_rank", "Step 2b. Select a region for localities or intermediate zones",
                             choices = partnership_name)))
           ),
    column(width = 3,
           div(title="This option will change whether the chart compares areas to another area (e.g. the Scotland average) or against a different time period (e.g. figures for the year 2017 compared to the year 2010).",
               awesomeRadio("comp_rank", label =shiny::HTML("<p>Step 3. Select to compare by:<br/><br/></p>"), #br required to try and keep alignment across columns
                            choices = list("Area or"= 1, "Time" = 2), 
                            selected = 1, inline=TRUE, checkbox=TRUE)),
           conditionalPanel(condition = "input.comp_rank == 1 ",  
                            div(title="Use this option to change which area is the comparator (red line in barchart)",
                                selectInput("geocomp_rank", "Step 3b. Select comparator area", choices = comparator_list,
                                            selectize=TRUE, selected = "Scotland")),
                            div(tags$b("Step 3c. Decide how to present data in the chart.")),
                            div(title="Show or hide the 95% confidence intervals on chart.",
                                awesomeCheckbox("ci_rank", label = "95% confidence intervals", value = FALSE))),
           conditionalPanel(condition = "input.comp_rank == 2 ", 
                            uiOutput("yearcomp_ui_rank"))
    ),
    column(width = 3,
           div(title="Use this option to change the time period presented in the chart and map",
               uiOutput("year_ui_rank"))), 
    column(width = 2,
           introBox(
             actionButton("rank_help",label="Help", icon= icon('question-circle'), class ="down"),
             actionButton("defs_rank", label="Definitions", icon= icon('info'), class ="down"), 
             downloadButton('download_rank', 'Download data', class = "down"),
             savechart_button('download_rankplot', 'Save chart', class = "down", disabled=FALSE),
             savechart_button('download_mapplot', 'Save map', class = "down"),
             data.step = 5,
             data.intro =(p(h5("Throughout the tool look out for options in each window that provide"),
                            tags$li("indicator definitions or help to interpret a visualisation,",style="color: #007ba7"),
                            tags$li("data download options for individual charts,",style="color: #007ba7"),
                            tags$li("image downloads for individual charts.",style="color: #007ba7")))))
           ), #well pannel bracket
  mainPanel(width = 12, #Main panel
            bsModal("mod_defs_rank", "Definitions", "defs_rank", htmlOutput('defs_text_rank')),
            uiOutput("rank_summary"), #description of the charts
            shiny::hr(), #header row
            column(width = 7, #rank bar
                   h4(textOutput("rank_title"), style="color: black; text-align: left"),  
                   h5(textOutput("rank_subtitle"), style="color: black; text-align: left"),  
                   withSpinner(plotlyOutput("rank_plot"))),
            column(width = 5, #map
                   uiOutput("rank_legend"),
                   uiOutput("map_ui"))
  ) #main panel bracket
  ) #Tab panel bracket