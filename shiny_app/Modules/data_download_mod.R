###############################################################################
#
# Data download module ---- 
#
###############################################################################




# UI function 
downloadDataButtonsUI <- function(id) {
  
  ns <- NS(id) # namespace
  
  shinyWidgets::dropdownButton(
    label = "Download data",
    icon = icon("download"),
    circle = FALSE,
    status = 'download',
    #size = "lg",
    tooltip = shinyWidgets::tooltipOptions(placement = "right", title = "Download data"),
    shiny::downloadLink(ns("downloadCSV"), 
                        label = "as CSV", 
                        icon = NULL),
    shiny::downloadLink(ns("downloadRDS"), 
                        label = "as RDS", 
                        icon = NULL),
    shiny::downloadLink(ns("downloadJSON"), 
                        label = "as JSON", 
                        icon = NULL)
  )
}




# Server function 
downloadDataButtonsServer <- function(id, data, selectedColumns = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # check if data is reactive or not
    # check if any columns have been passed to the optional selectedColumns arg
    # if arg not used then all cols will be in the downloaded output
    getData <- function() {
      currentData <- if (is.reactive(data)) data() else data
      if (!is.null(selectedColumns)) {
        currentData[, selectedColumns, drop = FALSE]
      } else {
        currentData
      }
    }
    
    # download as csv
    output$downloadCSV <- downloadHandler(
      filename = paste0("data_", Sys.Date(), ".csv"),
      content = function(file) {
        write.csv(getData(), file, row.names = FALSE)
      }
    )
    
    # download as rds
    output$downloadRDS <- downloadHandler(
      filename = paste0("data_", Sys.Date(), ".rds"),
      content = function(file) {
        saveRDS(getData(), file)
      }
    )
    
    #download as json
    output$downloadJSON <- downloadHandler(
      filename = paste0("data_", Sys.Date(), ".json"),
      content = function(file) {
        jsonlite::write_json(as.list(getData()), file)
      }
      
    )
    
  })
}


