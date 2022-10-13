#Code to create ScotPHO's Shiny profile platform
#In this script include all the server side functions: plots, reactive objects, etc.

###############################################.

## Define a server for the Shiny app
function(input, output, session) {
  
  output$latest_updates <- renderUI(latest_updates_output)
  
  observeEvent(input$jump_to_life_exp, {
    updateTabsetPanel(session, "intabset", selected = "ineq")
    updateSelectInput(session, "indic_simd", label = "Step 2 - Choose an indicator (type to search)",
                      choices = ind_depr_list, selected = "Cancer registrations") # change to life expectancy
   
  })
  
  
 
  
  
  ################################################################.
  #    Modal ----
  ################################################################.
  ## Latest indicator updates modal window ----
  updates_modal <- modalDialog(
    fluidRow(
      
      column(12, #tells to display indicators updated within 60 days
             h5(HTML(paste(indicators_updated, collapse='<br>')))),
      
      column(4, h5(HTML(paste(indicators_month_updated, collapse='<br>'))))
      ),
    
    size = "l", align= "center",
    easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
  )
  
  observeEvent(input$btn_indicator_updates, { showModal(updates_modal) }) # Link action button click to modal launch 
 

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
 
 
### summary dev work
  
  
### 1. reactive data 
  
  
  
  
  
  
  
  
} #server closing bracket

#########################  END ----

    


