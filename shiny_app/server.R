#Code to create ScotPHO's Shiny profile platform
#In this script include all the server side functions: plots, reactive objects, etc.

###############################################.

## Define a server for the Shiny app
function(input, output, session) {

  ###############################################.
  ## Sourcing tab code  ----
  ###############################################.
  # Sourcing file with server code
  source(file.path("summary_tab.R"),  local = TRUE)$value # Summary tab
  source(file.path("time_trend.R"),  local = TRUE)$value # Time trend tab
  source(file.path("rank_map.R"),  local = TRUE)$value # Rank map
  source(file.path("inequalities.R"),  local = TRUE)$value # Health Inequalities (deprivation tab)
  source(file.path("data_tab.R"),  local = TRUE)$value # Data tab
  source(file.path("tech_doc_tab.R"),  local = TRUE)$value # Technical document tab
  
  ################################################################.
  #    Modal ----
  ################################################################.
  ## Latest indicator updates modal window ----
  updates_modal <- modalDialog(
    fluidRow(
      column(12,
             # text_intro("We are continuously updating and developing our tool"),                 
             p(div("We are continuously updating and developing our tool", 
                        style = "color:0E3E5D; font-size:20px; width: 90%; text-align: left; ")),
             br(),
             br(),
             p(h5("Recent indicator updates include:", 
                  style = "width: 90%; text-align: left; font-weight: bold; "))),
      column(12, #tells to display indicators updated within 60 days
             h5(HTML(paste(indicators_updated, collapse='<br>')))
      )),
    br(),
    p(h5("To find out when an indicator is due to be updated please refer to our ", 
                          tags$a(href="https://docs.google.com/spreadsheets/d/e/2PACX-1vQUQMORMqe9RrMnS9WJSu51Q6ef0rubiF1M-QN3BYZIBueErtTvvbRe_kTZbWmnupiO_Uie80BoZCnK/pubhtml", "updates schedule.", class="externallink"))),
    br(),
    p(h5("For any further questions or other developments you would like to 
              suggest for our current tool, please contact us at", 
              tags$a(href="mailto:phs.scotpho@phs.scot", "phs.scotpho@phs.scot", class="externallink"), 
              style = "width: 700px")),
    br(),
    size = "l", align= "center",
    easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
  )
  
  observeEvent(input$btn_indicator_updates, { showModal(updates_modal) }) # Link action button click to modal launch 
  
  ## IntroJS allow switching between tabs----
  # observeEvent(input$btn_landing, {
  #   introjs(session,
  #           events = list(onbeforechange = readCallback("switchTabs")))
  # })
  
  ###############################################.
  ## Landing page ----
  ###############################################.
  # Creating events that take you to different tabs
  # activated when pressing buttons from the landing page

  observeEvent(input$jump_to_summary, {
    updateTabsetPanel(session, "intabset", selected = "summary")
  })
  
  observeEvent(input$jump_to_trend, {
    updateTabsetPanel(session, "intabset", selected = "trend")
  })
  
  observeEvent(input$jump_to_rank, {
    updateTabsetPanel(session, "intabset", selected = "rank")
  })
  
  observeEvent(input$jump_to_ineq, {
    updateTabsetPanel(session, "intabset", selected = "ineq")
  })

  observeEvent(input$jump_to_table, {
    updateTabsetPanel(session, "intabset", selected = "table")
  })
  
  observeEvent(input$jump_to_about, {
    updateTabsetPanel(session, "intabset", selected = "about")
  })
  
  
  observeEvent(input$jump_to_definitions, {
    updateTabsetPanel(session, "intabset", selected = "definitions")
  })
  
  
  observeEvent(input$jump_to_resources, {
    updateTabsetPanel(session, "intabset", selected = "resources")
  })
  
  observeEvent(input$jump_to_others, {
    updateTabsetPanel(session, "intabset", selected = "others")
  })
  
  # Temporary until rintroJS fixed
  observeEvent(input$btn_landing, {
    updateTabsetPanel(session, "intabset", selected = "tour")
  })

} #server closing bracket

#########################  END ----
