###############################################
#
# UI for inequalities tab
#
###############################################



inequalitiesTab <- 


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
) #Tab panel bracket