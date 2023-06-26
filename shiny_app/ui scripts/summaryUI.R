###############################################.
## Summary ----
###############################################.

summaryTab <- tabPanel(
  
  div(div(class="fa fa-list-ul", role = "navigation"), "Summary"),
  


  div(style = "margin:10px;",
      h3("Build a local area profile summary", style = "font-weight: bold;"),
      
      # sub-container for filters/guidance
      div(class = "summary-instructions", style = "display:flex; padding:5px;",
          
          # filters
          div(style = "display:flex; flex-direction:column; border-top:3px solid #d2d6de;",
              
              
              div(style = "display:flex; align-items: center;",
                  tags$i(class = "fa fa-filter", style = "color:#3F3685; font-size:2rem;"),
                  h4("Filters", style = "margin-left:5px;")),
              
              # filter 1: scotpho profile 
              selectInput("summary_profile", 
                          label= "Step 1: Select a profile:", 
                          choices = profile_list, 
                          selected = "HWB"),
              
              # filter 2 : geography level
              selectInput("geotype_summary", 
                          label = "Step 2. Select a geography level and then an area of interest.", 
                          choices=areatype_list,
                          selected = "Health board"),
              
              # conditional filter if IZ/locality selected
                         conditionalPanel(
                           condition = "input.geotype_summary== 'HSC locality' | input.geotype_summary == 'Intermediate zone' ",
                                            
              selectInput("loc_iz_summary", 
                          label = "Step 2b. Select a region for localities or intermediate zones",
                          choices = partnership_name)),
              
              # filter 3: geography area 
              # note: choices defined on server-side depending on chosen geography level
              selectInput("geoname_summary", 
                          label = NULL, 
                          choices = NULL),
              
              
              # downloads buttons
              div(style = "display:flex; align-items: center;",
                  tags$i(class = "fa fa-download", style = "color:#3F3685; font-size:2rem;"),
                  h4("Downloads", style = "margin-left:5px;")),
              
              
              # download as csv
              downloadButton("download_summary_csv",
                             "Download summary as CSV",
                             class = "button")#,
              
              # download as pdf (in progress)
              # actionButton("report", 
              #              "Download summary as PDF",
              #              class = "button")
         
          ), 
          
          
          # table instructions
          div(style = "padding:20px; margin-left: 20px; background-color:#F2F2F2; border-top:3px solid #d2d6de;",
              div(style = "display:flex; align-items: center;",
                  tags$i(class = "fa fa-info-circle", style = "color:#3F3685; font-size:2rem;"),
                  h4("How to interpret table results", style = "margin-left:5px")),
              
    
              p(style = "font-size:16px; padding:5px",
              "The results below provide a snapshot of the latest data for a chosen profile and geographical area, compared to Scotland.
               The colour of the result for your chosen area signals whether the results are statistically significantly different to Scotland."),
              
              fluidRow(
                column(1, tags$span(class="dot", style = "height: 25px; width: 25px; background-color: orange; border-radius: 50%; display: inline-block;")),
                column(9, p("Orange - statistically significantly worse than Scotland"))),
              
              fluidRow(
                column(1, tags$span(class="dot", style = "height: 25px; width: 25px; background-color: #1B7CED; border-radius: 50%; display: inline-block;")),
                column(9, p("Blue - statistically significantly better than Scotland"))),
              
              fluidRow(
                column(1, tags$span(class="dot", style = "height: 25px; width: 25px; background-color: grey; border-radius: 50%; display: inline-block;")),
                column(9, p("Grey - not statistically significantly different to Scotland"))),
               p("The shaded blue bar shows the range of values for that indicator across all areas within your chosen geography level. 
               The coloured dots show the relative position of your selected geography and the red line shows the value for Scotland. 
                 and how far it is from the best and worse performing areas at your chosen geographical level.")
          )
          
          
      )
  ),
  

  # summary table to be displayed ------
  div(class = "tableContainer", style = "margin-left:10px; margin-right:10px;",
               div(style = "display: flex; justify-content: space-between; background-color: hsl(205, 100%, 36%);",
                   div(style = "color: white; margin-left: 10px;", uiOutput("profile_title")), 
                   div(style = "margin: 5px;", img(src = "scotpho_reduced.png", width = 120))),
               uiOutput("summary_table")
      )
           
)


