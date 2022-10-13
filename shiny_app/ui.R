#Code to create ScotPHO's Shiny profile platform
# This script includes the user-interface definition of the app.

###############################################.
## Header ---- 
###############################################.
tagList( #needed for shinyjs
  useShinyjs(),  # Include shinyjs
  useShinyjs(),
  introjsUI(), 

  # Required to enable introjs scripts
  navbarPage(id = "intabset", #needed for landing page
             title = div(class = "logo", 
                         div(tags$a(img(src="phs-logo.png", width=120, alt = "link to Public Health Scotland website"),
                                    href= "https://www.publichealthscotland.scot/",
                                    target = "_blank"),
                             style = "position: relative; top: -15px;"),
                         div(tags$a(img(src="scotpho_reduced.png", height=40,  alt = "link to ScotPHO website"), href= "http://www.scotpho.org.uk/"),
                
                             style = "position: relative; top: -10px;")),

           # Navigation bar
           windowTitle = "ScotPHO profiles", #title for browser tab
           collapsible = TRUE, #tab panels collapse into menu in small screens
           header = tags$head(includeCSS("www/styles.css"), # CSS styles
                              HTML("<html lang='en'>"),
                              tags$link(rel="shortcut icon", href="favicon_scotpho.ico"), #Icon for browser tab
                              #Including Google analytics
                              includeScript("google-analytics.js"),
                              HTML("<base target='_blank'>") ,
                              cookie_box
                              ),
          

         
###############################################.
## Landing page ----
###############################################.
tabPanel(
  div(
      div(class="fa fa-home", role = "navigation"),"Home"),
  value = "home",
  htmlTemplate("hero.html",
               latest_updates_button = actionButton('btn_indicator_updates', "View recent updates", class = "tour-button"),
               see_more_button = actionButton("jump_to_life_exp", "View life expectancy by SIMD", class = "tour-button"))),


###############################################.
## Summary ----
###############################################.
tabPanel(div(
  div(class="fa fa-list-ul", role = "navigation"),
  "Summary"),
  value = "summary",
         #sidebarLayout(
           
           #filters on the left
          # sidebarPanel(
             
            #geography level filters
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
            
                  uiOutput("geoname_ui_summary")
           ),
           
           #profile filter
                  p(tags$b("Step 2. Select a profile ")),
                  div(id= "summary_div", uiOutput("profile_ui_summary")),

           #comparator options
                  p(tags$b("Step 3. Select to compare by ")),
                  awesomeRadio("comp_summary", label = NULL,
                               choices = list("Area or" = 1, "Time" = 2), 
                               selected = 1, inline=TRUE, checkbox = TRUE),
                  uiOutput("comp_ui_summary"), # comparaison options depending on whether area or time selected
          # ),#close side panel
        # mainPanel(

                   # Depending what users selects different visualizations
           reactableOutput("summary_ui_plots") %>% withSpinner(color="#0dc5c1")
    
       # ) # close main panel
        
        #) # closes side bar layour 
  ), #Tab panel bracket

###############################################.
## Time trend ----
###############################################.
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
                   h5(textOutput("subtitle_trend"), style="color: black; text-align: left"),
                   withSpinner(plotlyOutput("trend_plot"))
         )
                      ), #Tab panel bracket
###############################################.
## Rank and map ---- 
###############################################.
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
), #Tab panel bracket
###############################################.
## Health inequalities ---- 
###############################################.
tabPanel(div(
  div(class="fa-solid fa-scale-unbalanced", role = "navigation"),
  "Inequalities"),
 value = "ineq",
         sidebarPanel(width = 3, #Filter options
                      actionButton("help_simd", label="Help", 
                                   icon= icon('question-circle'), class ="down"), 
                      actionButton("defs_simd",label="Definitions", icon= icon('info'), class ="down"),
                      div(style = "margin-top: 30px",
                          selectInput("geotype_simd", label = "Step 1 - Select a geography level and an area",
                                      choices = areatype_depr_list, selected =  "Scotland")),
                      uiOutput("geoname_ui_simd"),
                      selectInput("indic_simd", label = "Step 2 - Choose an indicator (type to search)",
                                  choices = ind_depr_list),
                      uiOutput("year_ui_simd"),
                      div(title="Select what aspect of inequality you want to explore.", # tooltip
                          style = "margin-top: 10px; margin-bottom: 20px;", 
                          radioGroupButtons("measure_simd", 
                                            label= "Step 4 - Select what aspect of inequality you want to explore.", 
                                            choices = depr_measure_types, status = "primary",
                                            justified = TRUE
                          )),
                      awesomeCheckbox("ci_simd", label = "Show/hide 95% confidence intervals", value = F),
                      tags$div(title="Select if you want to use local or national quintiles", # tooltip
                               awesomeRadio("quint_type", label= "Local/Scotland quintiles",
                                            choices = c("Local", "Scotland"),  inline=TRUE, checkbox = TRUE)),
                      downloadButton(outputId = 'download_simd',
                                     "Download data", class = "down"),
                      savechart_button('report_simd', 'Save charts', class = "down", disabled=FALSE)
         ),
         mainPanel(width = 9, #Main panel
                   bsModal("mod_defs_simd", "Definitions", "defs_simd", htmlOutput('defs_text_simd')),
                   #Overview: trend and bar chart
                   div(class= "depr-text-box",
                       div(class= "title", textOutput("simd_nutshell_title")),
                       div(class= "content", htmlOutput("simd_text"))),
                   conditionalPanel("input.measure_simd == 'Trend'",
                                    column(6,
                                           htmlOutput("simd_barplot_title"),
                                           withSpinner(plotlyOutput("simd_bar_plot"))),
                                    column(6,
                                           htmlOutput("simd_trendplot_title"),
                                           withSpinner(plotlyOutput("simd_trend_plot"))),
                                    column(12, align="center", #legend
                                           style= "padding-bottom: 40px;",
                                           p(column(1),
                                             column(2, img(src="quintile1.png", height = "16px"), "1 - most deprived"), 
                                             column(1, img(src="quintile2.png", height = "16px"), "2"),
                                             column(1, img(src="quintile3.png", height = "16px"), "3"),
                                             column(1, img(src="quintile4.png", height = "16px"), "4"),
                                             column(2, img(src="quintile5.png", height = "16px"), "5 - least deprived"),
                                             column(2, img(src="simd_overall.png", height = "8px"), "Average"),
                                             column(1)))
                   ),#trend minitab bracket
                   #Absolute and realtive inequality
                   conditionalPanel("input.measure_simd == 'Gap'",
                                    column(6, htmlOutput("title_sii"), br(),
                                           withSpinner(plotlyOutput("simd_sii_plot"))), 
                                    column(6, 
                                           htmlOutput("title_rii"),
                                           withSpinner(plotlyOutput("simd_rii_plot"))) 
                   ),
                   #Population attributable risk
                   conditionalPanel("input.measure_simd == 'Risk'",
                                    column(6,
                                           htmlOutput("simd_par_barplot_title"),
                                           withSpinner(plotlyOutput("simd_par_barplot")),
                                           p(img(src= "signif_worse.png", height = "16px"),
                                             "Attributable to inequality", 
                                             style= "text-align: center; padding-bottom: 40px")),
                                    column(6,
                                           htmlOutput("simd_par_trendplot_title"),
                                           withSpinner(plotlyOutput("simd_par_trendplot")))
                   )
         )
), #Tab panel bracket
###############################################.
## Data ----
###############################################.
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
  ), #Tab panel bracket  

###############################################.             
##############NavBar Menu----
###############################################.
#Starting navbarMenu to have tab with dropdown list
navbarMenu("Info",

           ###############################################.
           ## About ----
           ###############################################.
           tabPanel("About", value = "about",
                    sidebarPanel(width=1),
                    mainPanel(width=8,
                              h3("About this tool", style = "color:black; font-weight: 600"),
                              p("ScotPHO's profiles tool allows users to explore the various different profiles 
                                produced by the ", tags$a(href="http://www.scotpho.org.uk/about-us/about-scotpho/", "ScotPHO collaboration.", 
                                                          class="externallink")),
                              p("The profiles are intended to increase understanding of local health issues 
                                and to prompt further investigation, rather than to be used as a performance 
                                management tool. The information needs to be interpreted within a local 
                                framework; an indicator may be higher or lower in one area compared to another, 
                                but local knowledge is needed to understand and interpret differences."),
                              h3("About ScotPHO", style = "color:black; font-weight: 600"),
                              p("The Scottish Public Health Observatory (ScotPHO) collaboration is led 
                                by Public Health Scotland, and includes Glasgow Centre for Population Health, National Records of Scotland, 
                                the MRC/CSO Social and Public Health Sciences Unit and the Scottish Learning Disabilities Observatory."),
                              p("We aim to provide a clear picture of the health of the Scottish population and the factors 
                                that affect it. We contribute to improved collection and use of routine data on health, 
                                risk factors, behaviours and wider health determinants. We take a lead in determining 
                                Scotland's future public health information needs, develop innovations in public health 
                                information and provide a focus for new routine public health information development 
                                where gaps exist."),
                              h3("Referencing our work", style = "color:black; font-weight: 600"),
                              p("Organisations may cite material included within the ScotPHO profiles tool subject to the following conditions:",
                                tags$li("Quote the source as “Scottish Public Health Observatory”"),
                                tags$li("Include the following URL -", 
                                        tags$a(href ="https://scotland.shinyapps.io/ScotPHO_profiles_tool/", "https://scotland.shinyapps.io/ScotPHO_profiles_tool/", class = "externallink"))),
                              h3("Contact us", style = "color:black; font-weight: 600"),
                              p("If you have any trouble accessing any information on this site or have
                                any further questions or feedback relating to the data or the tool, then please contact us at: ",
                                tags$b(tags$a(href="mailto:phs.scotpho@phs.scot", "phs.scotpho@phs.scot", class="externallink")),
                                "and we will be happy to help.")),
                    br()
           ),#Tab panel
###############################################.
## Indicator definitions ----
###############################################.

           tabPanel("Indicator definitions",
                    fluidPage(class = "test-class",

                    fluidRow(style = "width:80%; ",
                             h3("Indicator definitions and technical information", style = "color:black; font-weight:600;"),
                             h5(style = "color:black",
                                "ScotPHO Profiles are made up of a collection of indicators related to a specific theme
                                e.g. 'Alcohol' or 'Drugs'. Profiles are further divided into topic areas to group similar indicators together.
                                 This page allows users to see available indicators and geographies as well as finding detailed technical information
                                  about how indicators are created.")),

                                fluidRow(
                                  column(4, selectInput("profile_search", label = "Filter by profile", 
                                            choices=profile_list_filter)),
                                  column(4, selectizeInput("geo_search", label = "Filter by geography level", choices = areatype_list, selected = NULL, multiple = TRUE,
                                               options = list(placeholder = 'Select geography level(s)')))),

                                fluidRow(reactableOutput("ind_search_results") %>% withSpinner(color="#0dc5c1"))
                                
                                ))
,

    

###############################################.             
############## Tour of the tool----    
###############################################.
tabPanel("Tour of the tool", value = "tour",
         sidebarPanel(width=1),
         mainPanel(width=10,
                   fluidRow(p(h4("Welcome to the ScotPHO Profiles Tool"),
                     h5("This interactive tool provides access to a range of public
              health related indicators at different geographies including NHS boards, council areas and health and
              social care partnerships.", style = "color:black;"),
                     h5("There are different ways to navigate around the tool.", style = "color:black;"),
                     h5("Different visualisations can be opened using the menu bar (the blue strip) at the top of the screen.",
                        style = "color:black;"),
                     img(src='introjs_tabset_panel.PNG',width=300),
                     br(),
                     h5("The 'Home' option in the menu bar will return to the profiles tool homepage.",
                        style = "color:black;"),
                     style = "font-size:20px")),
                   hr(),
                   fluidRow(column(6,
                          h5("The profile summary allows you to look at multiple indicators within an area at the same time.",
                             style = "color:black;")),
                   column(6, img(src='tour_summary1.PNG'))),
                   hr(),
                   fluidRow(column(3,
                          h5("The trend and rank charts allow detailed exploration of one indicator at a time.",
                             style = "color:black;")),
                   column(9, img(src='tour_trendrank1.PNG'))),
                   hr(),
                   fluidRow(p(h5("Throughout the tool use the dropdown menus to change which indicators or geographies are displayed in the charts.",
                                 style = "color:black;"),
                     img(src='tour_summary2.png', style = "vertical-align: middle; border-style: solid; border-color: black; border-width: 1px"),
                     column(6, h5("While using dropdown menus mouse click within a dropdown menu and press backspace on your keyboard ('<-') then start typing a word to quickly find the options you are looking for",
                                  style = "color:black;")),
                     column(6, img(src='introjs_how_to_select.png')))),
                   hr(),
                   br(),
                   fluidRow(column(8,
                          p(h5("Throughout the tool look out for options in each window that provide",
                               style = "color:black;"),
                            tags$ul( tags$li("indicator definitions or help to interpret a visualisation,"),
                            tags$li("data download options for individual charts,"),
                            tags$li("image downloads for individual charts.")))),
                   column(4, img(src='tour_rankmap2.PNG'))),
                   hr(),
                   br(),
                   fluidRow(column(6,
                          h5("The 'Data' window can be used to filter and download profiles data.",
                             style = "color:black;")),
                   column(6, img(src='tour_data1.PNG'))),
                   hr(),
                   br(),
                   fluidRow(column(6,
                          h5("The inequalities module allows exploration of deprivation effects for a selection of indicators from the main profiles tool.",
                             style = "color:black;")),
                   column(6, img(src='tour_ineq1.png'))),
                   hr(),
                   br(),
                   fluidRow(h5("There are also options to find out information such as detailed descriptions of the profile indicators, indicator update schedules and links to evidence for action briefings.",
                               style = "color:black;"),
                   img(src='tour_about1.PNG', width="100%"))
         )#main panel bracket
)
         


  )# NavbarMenu bracket
) )
################################################.
 #bracket tagList
###END
#)
