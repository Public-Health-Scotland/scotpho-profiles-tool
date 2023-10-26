################################################
#
# UI for trend tab
#
################################################

trendTab <-

tabPanel(div(
  div(class="fa fa-line-chart", role = "navigation"),
  "Trend"),
  value = "trend",
  sidebarPanel(width=4,
               column(6,
                      actionButton("help_trend",label="Help", icon= icon('question-circle'), class ="down")),
               column(6,
                      actionButton("defs_trend", label="Definitions", icon= icon('info'), class ="down")),
               column(12,
                      shiny::hr(),
                      div(title="Select an indicator to see trend information. Click in this box, hit backspace and start to type if you want to quickly find an indicator.",
                          selectInput("indic_trend", shiny::HTML("<p>Step 1. Select an indicator <br/> <span style='font-weight: 400'>(hit backspace and start typing to search for an indicator)</span></p>"), 
                                      choices=indicator_list, selected = "Alcohol-related hospital admissions")),
                      shiny::hr(),
                      div(title="Use the options below to add geographies to the trend chart, remember some indicators may not be available for all geography types. See technical information to find out which geographies indicators are available for.",                      
                          p(tags$b("Step 2. Select areas to plot."),
                            p("(You can select multiple areas of any geography type)."))),
                      awesomeCheckbox("scotname_trend", tags$b("Scotland"), value=TRUE)),
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
               div(title="This option restricts the HSC locality or IZ options below to only areas within a parent geography",                      
                   selectInput("loc_iz_trend", "To choose a locality or intermediate zone, first 
                               select an HSC partnership", choices = partnership_name)),
               column(6,div(title="If greyed out locality data not available",uiOutput("loc_ui_trend"))),
               column(6,div(title="If greyed out IZ data not available",uiOutput("iz_ui_trend"))),
               column(12,
                      shiny::hr(),
                      div(tags$b("Step 3. Decide how to present data in the chart.")),
                      div(title= "Display the rate/percentage data or the raw numbers.",
                          awesomeRadio("var_plot_trend", label =NULL, inline = TRUE, 
                                       choices = c("Rate/Percentage" = "measure", 
                                                   "Numerator" = "numerator"))),
                      div(title="Show or hide the 95% confidence intervals for the data selected.", # tooltip
                          awesomeCheckbox("ci_trend", label = "95% confidence intervals", value = FALSE)),
                      downloadButton('download_trend', 'Download data', class = "down"),
                      savechart_button('download_trendplot', 'Save chart',  class = "down", disabled=FALSE))),
  mainPanel(width = 8, #Main panel
            bsModal("mod_defs_trend", "Definitions", "defs_trend", htmlOutput('defs_text_trend')),
            h4(textOutput("title_trend"), style="color: black; text-align: left"),
            h4(textOutput("gap_year_notice"), style="color: red; text-align: left"),
            h5(textOutput("subtitle_trend"), style="color: black; text-align: left"),
            withSpinner(plotlyOutput("trend_plot"))
  )
               ) #Tab panel bracket