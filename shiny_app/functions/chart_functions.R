###############################################################################
#
# Functions used for charts
#
###############################################################################


# 1. title wrapper function --------------------------------------------------
# purpose: ensures titles not cut-off when downloading ggplot as png
title_wrapper <- function(x, ...)
{
  paste(strwrap(x, ...), collapse = "\n")
}


#2. data unavailable function (for plotly charts )-----------------------------
# purpose: create a plot saying 'no data available' when nothing to plot
plot_nodata <- function(height_plot = 450) {
  text_na <-
    list(x = 5,y = 5,
      text = "No data available" ,
      size = 20,
      xref = "x",
      yref = "y",
      showarrow = FALSE
    )
  
  plot_ly(height = height_plot) %>%
    layout(
      annotations = text_na,
      #empty layout
      yaxis = list(
        showline = FALSE,
        showticklabels = FALSE,
        showgrid = FALSE,
        fixedrange = TRUE
      ),
      xaxis = list(
        showline = FALSE,
        showticklabels = FALSE,
        showgrid = FALSE,
        fixedrange = TRUE
      ),
      font = list(family = '"Helvetica Neue", Helvetica, Arial, sans-serif')
    ) %>%
    config(displayModeBar = FALSE) # taking out plotly logo and collaborate button
} 



#3. data unavailable function (for ggplot charts )-----------------------------
# purpose: create a plot saying 'no data available' when nothing to plot
plot_nodata_gg <- function() {
  ggplot() +
    xlab("No data available") +
    scale_x_discrete(position = "top") +
    theme(
      panel.background = element_blank(),
      axis.title.x = element_text(size = 20, colour = '#555555')
    )
}


#3. summary tab plot layouts ------------------------------------------------
# purpose: create plots when user selects heatmap or snapshot on summary tab
sum_ui <- function(title, plot_name) {
  tagList(
    h5(title, style = "color: black; text-align: center; font-weight: bold;"),
    div(align = "center", withSpinner(
      plotlyOutput(plot_name, height = "auto")
    ))
  )
}



#4. saving charts  ---------------------------------------------------------
# purpose: save charts as png, unless button disabled
# if disabled display message explaining why
# note this will only affect the summary tab where visuals cannot currently be downloaded
savechart_button <- function(outputId, label = "Save chart", class=NULL, disabled=FALSE){
  
  if (disabled == TRUE){
    
    # Message to display when disabled button is clicked
    disabled_msg = list(p("A software update has disabled the save chart functionality. We are working on a replacement."),
                        p("In the meantime, you can:"),
                        tags$ul(
                          tags$li("Download the data with the Download data button and build new charts in tools like Excel"),
                          tags$li("Take a screenshot of the chart area using ",
                                  tags$a(href="https://support.microsoft.com/en-us/windows/open-snipping-tool-and-take-a-screenshot-a35ac9ff-4a58-24c9-3253-f12bac9f9d44",
                                         "Snipping Tool"),
                                  " or ",
                                  tags$a(href="https://blogs.windows.com/windowsexperience/2019/04/08/windows-10-tip-snip-sketch/",
                                         "Snip and Sketch."),
                                  "At least one of these tools is usually installed on recent versions of Windows."
                          )))
    
    # Create button without link
    disabled_button = tags$p(id = outputId, class = paste("btn btn-default shiny-download-link", class, "down_disabled"),
                             icon("image"), label)
    
    # Define popup message box
    disabled_popup = bsModal(paste0(outputId, "-disabled-modal"), "Save Chart Disabled", outputId, disabled_msg, size="small")
    
    # need to explicitly return both ui elements otherwise only the last will be returned
    return(tagList(disabled_button, disabled_popup))
    
    
  } else {
    tags$a(id = outputId, class = paste("btn btn-default shiny-download-link", class),
           href = "", target = "_blank", download = NA, icon("image"), label)
  }
  
  
}





