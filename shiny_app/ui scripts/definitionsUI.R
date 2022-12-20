###############################################
#
# UI for indicator definitions tab
#
###############################################



definitionsTab <- 


tabPanel(
  "Indicator definitions",
  div(class = "container",style = "padding:30px;",
      h3("Indicator definitions and schedule", style = "font-weight:600;"),
      hr(),
      p("Use the filters below to search for indicators by profile and/or geography level. Alternatively you can search using key words (e.g. 'cancer'). You can then click on an indicator in the search results table to view metadata and links to quickly navigate to analysis in this tool for that particular indicator.",
        style = "font-size:16px; padding:5px"),
      div(class = "tech-doc-download",style = "display:flex; margin-bottom: 10px; justify-content:space-between;",
          p("To view technical information and updates schedule for all indicators at once, use the download button.",
            style = "font-size:16px;"),
          downloadButton('btn_techdoc_download', "Download as CSV", class = "button")),
      fluidRow(style = "border-top: 1px solid #eee;",
               column(3,
                      selectInput(
                        "profile_search", 
                        label = "Filter by profile",
                        choices = profile_list_filter)),
               column(3,
                      selectizeInput(
                        "geo_search",
                        label = "Filter by geography level",
                        choices = areatype_list,
                        selected = NULL,
                        multiple = TRUE,
                        options = list(placeholder = 'Select geography level(s)'))
               )),
      
      fluidRow(reactableOutput("ind_search_results") %>% withSpinner(color = "#0dc5c1"))
  ) # close container 
)