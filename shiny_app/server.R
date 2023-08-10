###############################################
#
# Define server for the Shiny app
#
##############################################


function(input, output, session) {
  
 # Source files with server code for each tab -----------------------------------------
  source(file.path("server scripts/homepageServer.R"), local = TRUE)$value # Homepage tab
  source(file.path("server scripts/summaryServer.R"), local = TRUE)$value # Summary tab
  source(file.path("server scripts/trendServer.R"), local = TRUE)$value # Time trend tab
  source(file.path("server scripts/rankServer.R"), local = TRUE)$value # Rank map
  source(file.path("server scripts/inequalitiesServer.R"), local = TRUE)$value # Health Inequalities tab
  source(file.path("server scripts/dataServer.R"), local = TRUE)$value # Data tab
  source(file.path("server scripts/definitionsServer.R"), local = TRUE)$value # Indicator definitions tab

  # Keeps the shiny app from timing out quickly 
  autoInvalidate <- reactiveTimer(10000)
  observe({
    autoInvalidate()
    cat(".")
  })
  
}

 
## END 