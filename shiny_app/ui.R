###############################################
#
# User interface for the Shiny app
#
##############################################


# Source files with UI code for each tab --------------
walk(list.files("ui scripts", full.names = TRUE), ~ source(.x))


# define the UI structure -------------------------------
tagList(
  useShinyjs(), 
 navbarPage(
   
    # add PHS logo to navigation bar 
    title = div(style = "position: relative; 
                       top: -15px; 
                       margin-left: 10px; 
                       margin-top: 5px;",
                tags$a(img(src = "phs-logo-updated.png", 
                           width = 120, 
                           alt = "link to Public Health Scotland website"),
                       href = "https://www.publichealthscotland.scot/",
                       target = "_blank")
    ),
    
   
    # make navigation bar collapse on smaller screens
    windowTitle = "ScotPHO profiles",
    collapsible = TRUE,
    
    header = tags$head(
      
      # JS dependencies and custom function to download profiles summary table as pdf
     tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jspdf/1.5.3/jspdf.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/dom-to-image/2.6.0/dom-to-image.min.js"),
     tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jspdf-autotable/3.5.6/jspdf.plugin.autotable.min.js"),
     

      # sourcing css style sheet 
      includeCSS("www/styles.css"),
      
      # include scotpho icon in web browser
      HTML("<html lang='en'>"),
      tags$link(rel = "shortcut icon", 
                href = "favicon_scotpho.ico"), 
      
      # include google analytics scripts
      includeScript("google-analytics.js"), # GA 3 
      HTML('<script async src="https://www.googletagmanager.com/gtag/js?id=G-KE1C59RLNS"></script>'),
      includeScript("gtag.js") # GA 4 
      
    ),
    
    
    # order of tabs --------------------------------
    homepageTab,
    summaryTab,
    trendTab,
    rankTab,
    inequalitiesTab,
    dataTab,
    navbarMenu("Info",
               aboutTab,
               definitionsTab)
    
  ) # close navbarPage
) # close taglist


## END


