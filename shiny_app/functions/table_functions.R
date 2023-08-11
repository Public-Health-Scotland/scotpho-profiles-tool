###############################################################################
#
# Functions used for tables
#
###############################################################################

# 1. reactable theme 
# purpose: style to use for tables built using the reactable package
table_theme <- function() {
  search_icon <- function(fill = "none") {
    # Icon from https://boxicons.com
    svg <- sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24"><path fill="%s" d="M10 18c1.85 0 3.54-.64 4.9-1.69l4.4 4.4 1.4-1.42-4.39-4.4A8 8 0 102 10a8 8 0 008 8.01zm0-14a6 6 0 11-.01 12.01A6 6 0 0110 4z"/></svg>', fill)
    sprintf("url('data:image/svg+xml;charset=utf-8,%s')", URLencode(svg))
  }
  reactableTheme(
    backgroundColor = 'white',
    borderWidth = '1px',
    borderColor = 'lightgrey',
    headerStyle = list(backgroundColor = "hsl(205, 93%, 16%)",
                       color = "white"),
    
    searchInputStyle = list(
      borderColor = '#cccccc',
      paddingLeft = "3.5rem",
      width = "100%",
      backgroundSize = "2rem",
      backgroundPosition = "left 1rem center",
      backgroundRepeat = "no-repeat",
      backgroundImage = search_icon("black")),
    
    #tableBodyStyle = list(flex = "auto"),
    rowStyle=list(height="65px")
    
    
    )
  
}


#2. reactable widget
# purpose: builds a table using reactable. Use for quicker rendering of inline charts 
widgetTable <- function(data, options = list(), columns = list(), deps = list(), ...){
  
  dt <- reactable(data, 
                  columns,
                  compact = TRUE,
                  defaultExpanded = T,
                  sortable = F,
                  defaultPageSize = nrow(data),
                  theme = table_theme(), # see function above
                  highlight = TRUE)
  
  
  dt %>% tagList(deps) %>% browsable
  

}


