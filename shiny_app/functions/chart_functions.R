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



#4. saving charts  ---------------------------------------------------------
# purpose: save charts as png
savechart_button <- function(outputId, label = "Save chart", class=NULL, disabled = FALSE){

    tags$a(id = outputId, class = paste("btn btn-default shiny-download-link", class),
           href = "", target = "_blank", download = NA, icon("image"), label)
  
  
}






