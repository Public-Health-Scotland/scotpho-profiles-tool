###############################################
#
# UI for data tab
#
###############################################


dataTab <- 

tabPanel(div(
  div(class="fa fa-table", role = "navigation"),
  "Data"),
  value = "table",
  #Sidepanel for filtering data
  mainPanel(
    width = 12, style="margin-left:0.5%; margin-right:0.5%",
    #Row 1 for intro  
    fluidRow(
      p("Download the data used in the tool", 
        style = "font-weight: bold; color: black;"),
      p("Use the filters below to select the data you want to download. ",
        "To delete choices use backspace or select item and delete"),
      br()
    ),
    #Row 2 for selections
    fluidRow(
      column(3,
             p("Select what data you want", style = "font-weight: bold; color: black;"),  
             div("All available indicators will be displayed for
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
             p("Select what areas you want", style = "font-weight: bold; color: black;"),
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
      column(3, style = "width:20%",
             p("Select the time period", style = "font-weight: bold; color: black;"),
             sliderInput("date_from",label = NULL, min = min_year, 
                         max = max_year, value = c(min_year,max_year), 
                         step = 1, sep="", round = TRUE, 
                         ticks = TRUE, dragRange = FALSE),
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
  ) #Tab panel bracket  