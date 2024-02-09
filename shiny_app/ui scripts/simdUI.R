###############################################
#
# UI for SIMD/Deprivation tab
#
###############################################


simdTab <- 
  
  
  tabPanel(div(
    div(class="fa-solid fa-scale-unbalanced", role = "navigation"),
    "SIMD"),
    value = "simd",
    sidebarPanel(width = 3, #Filter options
                 div(style = "margin-top: 30px",
                     selectInput("geotype_simd", label = "Step 1 - Select a geography level and an area",
                                 choices = areatype_depr_list, selected =  "Scotland")),
                 uiOutput("geoname_ui_simd"),
                 selectInput("indic_simd", label = "Step 2 - Choose an indicator (type to search)",
                             choices = ind_depr_list),
                 actionButton("defs_simd",label="Definitions", icon= icon('info'), class ="down"), #action button linked to module - needed to dislpay pop up with indicator description
                 uiOutput("year_ui_simd"),
                 div(title="Select what aspect of inequality you want to explore.", # tooltip
                     style = "margin-top: 10px; margin-bottom: 20px;", 
                     radioGroupButtons("measure_simd", 
                                       label= "Step 4 - Select what aspect of inequality you want to explore.", 
                                       choices = depr_measure_options, status = "primary",direction = "vertical",
                                       justified = TRUE)),
                 awesomeCheckbox("ci_simd", label = "Show/hide 95% confidence intervals", value = F),
                 tags$div(title="Select if you want to use local or national quintiles", # tooltip
                          awesomeRadio("quint_type", label= "Local/Scotland quintiles",
                                       choices = c("Local", "Scotland"),  inline=TRUE, checkbox = TRUE)),
                 actionButton("defs_quintile",label="Deprivation grouping", icon= icon('info'), class ="down"), #action button linked to module - needed to dislpay pop up with indicator description
                 downloadButton(outputId = 'download_simd',
                                "Download data", class = "down"),
                 savechart_button('report_simd', 'Save charts', class = "down", disabled=FALSE)
    ),
    mainPanel(width = 9, #Main panel
              style = "margin-top: 30px",
              bsModal("mod_defs_simd", "Definitions", "defs_simd", htmlOutput('defs_text_simd')), #modal box that pops up containting indicator definitions text
              bsModal("mod_defs_quintile", "About deprivation groupings", "defs_quintile", htmlOutput('defs_text_quintile')), #modal box that pops up containting indicator definitions text
              uiOutput("inequality_summary_text"), #dynamic bullet points - appearance controlled using reactive element in inequalities server script
              #Overview: trend and bar chart
              conditionalPanel("input.measure_simd == 'Patterns of inequality'",
                               column(6,
                                      htmlOutput("simd_barplot_title"),
                                      withSpinner(plotlyOutput("simd_bar_plot"))),
                               column(6,
                                      htmlOutput("simd_trendplot_title"),
                                      withSpinner(plotlyOutput("simd_trend_plot"))),
                               column(12, align="left", #legend hack for bar plot
                                      style= "padding-bottom: 40px;",
                                      p(#column(1),
                                        column(2, img(src="quintile1.png", height = "16px"), "1 - most deprived"), 
                                        column(1, img(src="quintile2.png", height = "16px"), "2"),
                                        column(1, img(src="quintile3.png", height = "16px"), "3"),
                                        column(1, img(src="quintile4.png", height = "16px"), "4"),
                                        column(2, img(src="quintile5.png", height = "16px"), "5 - least deprived"),
                                        column(2, img(src="simd_overall.png", height = "8px"), "Average"),
                                        column(3)))
              ),#trend minitab bracket
              
              #Absolute and realtive inequality
              conditionalPanel("input.measure_simd ==  'Inequality gap'",
                               column(6, 
                                      br(),#improve alignment with selection menu
                                      htmlOutput("title_sii"),
                                      br(),
                                      actionButton("help_sii", label="What does this chart show?", 
                                                   icon= icon('question-circle'), class ="down"), 
                                      br(),
                                      withSpinner(plotlyOutput("simd_sii_plot"))), 
                               column(6,
                                      br(),#improve alignment with selection menu
                                      htmlOutput("title_rii"),
                                      br(),
                                      actionButton("help_rii", label="What does this chart show?", 
                                                   icon= icon('question-circle'), class ="down"), 
                                      br(),
                                      withSpinner(plotlyOutput("simd_rii_plot"))) 
              ),
              
              #Population attributable risk
              conditionalPanel("input.measure_simd == 'Potential for improvement'",
                               column(6,
                                      br(),#improve alignment with sidepanel dropdowns 
                                      htmlOutput("simd_par_barplot_title"),
                                      actionButton("help_paf", label="What does this chart show?", 
                                                   icon= icon('question-circle'), class ="down"),
                                      p(img(src= "signif_better.png", height = "16px"),"Baseline",
                                        img(src= "signif_worse.png", height = "16px"),"Attributable to inequality",
                                        style= "text-align: left; padding-top: 10px; padding-bottom: 10px"),
                                      withSpinner(plotlyOutput("simd_par_barplot"))
                               ),
                               column(6,
                                      br(),#improve alignment with sidepanel dropdowns
                                      htmlOutput("simd_par_trendplot_title"),
                                      actionButton("help_paf2", label="What does this chart show?", 
                                                   icon= icon('question-circle'), class ="down"),
                                      p(" "), # create whitespace and help alignment of charts
                                      br(),
                                      withSpinner(plotlyOutput("simd_par_trendplot")))
              ),
              
              #Which measure to look at
              conditionalPanel("input.measure_simd == 'About these options'",
                               column(12,
                                      uiOutput("inequality_options_help") # text displayed generated in inequalities server script
                               ) # close column
                               
              )
    )
  ) #Tab panel bracket
