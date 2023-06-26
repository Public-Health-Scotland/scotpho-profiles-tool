# ###############################################.
# ## Summary tab - server ----
# ###############################################.

### 1. Update choices for geography area filter based on geography level selected ------------

# create reactive object to store geography areas
area_choices <- reactiveVal()

observeEvent(c(input$geotype_summary, input$loc_iz_summary), {
  
  # ensures additional filtering of parent area when locality/IZ is selected as geography level
  # this is because when locality/IZ selected, an extra drop-down appears asking users to first select a parent area before continuing
  if (input$geotype_summary %in% c("HSC locality", "Intermediate zone") && !is.null(input$loc_iz_summary)) {
    
    area_choices(sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_summary &
                                            geo_lookup$parent_area == input$loc_iz_summary]))
    
  } else {
    
    area_choices(sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_summary]))
  }
  
  # update the filter with dynamic choices created in previous step
  updateSelectInput(session, "geoname_summary", choices = area_choices())
  
  
})



# 2. reactive dataset to be used for table  -----------------
summary_data <- reactive ({
  
  # convert to data.table format to make run faster 
  dt <- as.data.table(optdata)
  
  # filter by selected profile
  dt <- dt[substr(profile_domain1, 1, 3) == input$summary_profile | substr(profile_domain2, 1, 3) == input$summary_profile]
  
  # filter by chosen area and get latest data for each indicator
  chosen_area <- dt[areaname == input$geoname_summary & areatype == input$geotype_summary,
                    .SD[year == max(year)], by = indicator]
  
  # include scotland figures for comparison
  scotland <- dt[areaname == "Scotland"]
  chosen_area <- chosen_area[scotland, on = c("ind_id", "year"), scotland_value := scotland$measure]
  
  # find min and max values for each indicator within chosen geography level for spine chart (i.e. "best" and "worst" performing areas)
  other_areas <- dt[areatype == input$geotype_summary][chosen_area, on = .(ind_id, year), nomatch = 0][,
                                                                                                       .(min_value = min(measure),
                                                                                                         max_value = max(measure)),
                                                                                                       by = .(ind_id, year)]
  # add min and max values for each indicator to table
  chosen_area <- chosen_area[other_areas, on = c("ind_id", "year"),
                             c("min_value", "max_value") := .(other_areas$min_value, other_areas$max_value)]
  

  # transpose values for the spine plot to ensure worse = left and better = right
  # these columns will be used to populate the charts but will not be visible in the table
  final <- chosen_area %>%
    mutate(across(c(measure, min_value, max_value), ~
                    case_when(interpret=='L' & . > scotland_value ~ -(. / scotland_value - 1),
                              interpret=='L' & . <= scotland_value ~ (1 - . /scotland_value),
                              TRUE ~ -(1 - . / scotland_value)), .names = "{.col}_plot"),
           
           # ensures scotland is plotted in the centre of the chart
           scotland_plot = 0) %>%
    
    # round values displayed in table to 1 decimal place
    mutate(across(c(measure, scotland_value), ~ round(., digits = 1)))
  
  
  # assign colours to values depending on statistical significance
  final <- final %>%
    mutate(
      # colours for values in table
      colour = case_when(lowci <= scotland_value & upci >= scotland_value & interpret %in% c("H", "L") ~'gray',
                         lowci > scotland_value & interpret == "H" ~ 'blue',
                         lowci > scotland_value & interpret == "L" ~ 'red',
                         upci < scotland_value & interpret == "L" ~ 'blue',
                         upci < scotland_value & interpret == "H" ~ 'red',
                         interpret == "O" ~ 'white', TRUE ~ 'white'),
      
      # colours for values in spine plot
      marker_colour = case_when(colour == "blue" ~ "#1B7CED",
                                colour == "red" ~ "#FFA500",
                                colour == "gray" ~ "#6A6C6D", TRUE ~ "#FFFFFF"))
  
  # identify correct domain and arrange by domain
  # this is because there are 2 domain columns in dataset as some indicators belong to more than 1 profile/domain
  final <- final %>%
    mutate(domain = as.factor(case_when(
      substr(profile_domain1,1,3)== input$summary_profile ~
        substr(profile_domain1, 5, nchar(as.vector(profile_domain1))),
      TRUE ~ substr(profile_domain2, 5, nchar(as.vector(profile_domain2)))))) %>%
    arrange(domain)
  
  
  final$row_number <- 1:nrow(final) # assign each row an 'id' to be used when constructing highchart
  final$spine_chart <- NA # create empty column to populate with in-line highcharts
  
  # selecting columns required for table on dashboard
  final <- final %>%
    select(domain, indicator, measure, scotland_value, type_definition, def_period, colour,
           measure_plot, min_value_plot, max_value_plot, marker_colour, row_number, spine_chart)
})




# 3. dynamic table title  --------

output$profile_title <- renderUI({
  
  tableTitle <- h3(paste0(names(profile_list)[which(profile_list == input$summary_profile)], " profile: ", input$geoname_summary))
  
})


# 4. downloads --------


# download as csv 
output$download_summary_csv <- downloadHandler(
  filename = function() { 
    paste(input$summary_profile, "-summary-", input$geoname_summary, ".csv", sep="")
  },
  content = function(file) {
    write.csv(summary_data() %>% select(domain, indicator, measure, scotland_value, type_definition, def_period, colour), file, row.names=FALSE)
  })


# download as PDF
# observeEvent(input$report, {
#   # Get the reactive dataframe
#   data <- summary_data()
# 
#   # Convert the dataframe to a JSON string
#   json_str <- jsonlite::toJSON(data)
# 
#   jsCode <- paste0('
#     // step 1: create a pdf ------------------------------------------
#     var doc = new jsPDF("p", "pt", "a4");
# 
#     // step 2: define data/headers for table  ------------------------------
#     var tableData = ', json_str, ';
#     var headers = ["Domain", "Indicator", "Measure", "Scotland","Chart"];
# 
#     // step 3: function to include charts inside table -----------------------------
#     function captureHighcharts() {
#       // 3.a: get highchart ids -------
#       var highchartElements = $(".highchart.html-widget");
# 
#       // 3.b: keep track of completed captures ------
#       var capturesCompleted = 0;
# 
#       // 3.c: array to store image data for each row
#       var imageDataArray = [];
# 
#       // 3.d: Loop through each chart and convert to png -----
#       highchartElements.each(function(index, element) {
#         domtoimage.toPng(element)
#           .then(function(dataUrl) {
#             capturesCompleted++; // Increase the captures completed count
# 
#             // 3.e: Add each png to the image data array -----
#             imageDataArray.push(dataUrl);
# 
#             // 3.f: If all converted, build the table and add images -------
#             if (capturesCompleted === highchartElements.length) {
#               // 3.g: Build the table --------
#               var tableConfig = {
#                 head: [headers],
#                 body: tableData.map(function(row, rowIndex) {
#                   return [row.domain, row.indicator, row.measure, row.scotland_value];
#                 }),
#                 // 3.h: Define the column widths
#                 columnStyles: {
#                   0: { columnWidth: 70 },
#                   1: { columnWidth: 200 },
#                   2: { columnWidth: 40 },
#                   3: { columnWidth: 40 },
#                   3: { columnWidth: 100 }
#                 },
#                 // 3.i: Customize the table appearance if needed
#                 styles: { overflow: "linebreak" },
#                 // 3.j: Use didDrawCell to add images inside each cell
#                 didDrawCell: function(data) {
#                   if (data.column.index === 4 && data.cell.section === "body") {
#                     var rowIndex = data.row.index;
#                     var imageData = imageDataArray[rowIndex];
#                     doc.addImage(imageData, "PNG", data.cell.x + 5, data.cell.y + 5, 100, 40);
#                   }
#                 }
#               };
# 
#               doc.autoTable(tableConfig);
# 
# 
#               // 4. Save the PDF file ----------------------------------
#               doc.save("tbl.pdf");
#             }
#           });
#       });
#     }
# 
#     // Call the function
#     captureHighcharts();
#   ')

  # Use the shinyjs package to run the JS code
#   shinyjs::runjs(jsCode)
# })



# 5. summary table of results  ------------
output$summary_table <- renderUI({
  
  summary_table <- widgetTable(data = summary_data(), 
                               
                               columns = list(
                                 
                                 # Domain column ---------
                                 domain = colDef(
                                   name = "",
                                   maxWidth = 120,

                                   # this JS function hides domain name from appearing on every row
                                   # i.e. gives appearance of 'merged' cells
                                   style = JS("function(rowInfo, column, state) {

                                         const prevRow = state.pageRows[rowInfo.viewIndex - 1]

                                         if (prevRow && rowInfo.values['domain'] === prevRow['domain']) {

                                           return {visibility: 'hidden'}
                                         }
                                       }
                                     ")),
                                 
                                 # indicator column --------
                                 indicator = colDef(
                                   name = "",
                                   minWidth = 320,
                                   html = TRUE,

                                   # this JS function creates clickable links to view trend data
                                   # when user clicks an indicator, it navigates to 'trends' tab
                                   # and updates filters on that tab to that particular indicator and users chosen geography area
                                   cell = JS(" function(rowInfo){

                                                      // create variables to be used to determine what filters to be updated and with what values

                                                      var selectedGeoType = $('#geotype_summary').val(); // chosen geography level

                                                      var selectedGeoName = $('#geoname_summary').val(); // chosen geography area

                                                      var selectedParentArea = $('#loc_iz_summary').val(); // parent area (for IZ/locality selections)

                                                      var trendGeoFilter; // empty variable: filter to be updated on trend tab

                                                      var extraGeoFilter; // empty variable: additional filter to be updated on trend tab (for IZ/locality selections)


                                                      // populating the two empty variables above
                                                      // by determining which filter on trend tab is to be updated, based on users chosen area

                                                        if (selectedGeoType === 'Health board'){
                                                        trendGeoFilter = '#hbname_trend';

                                                        } else if(selectedGeoType === 'Council area'){
                                                        trendGeoFilter = '#caname_trend';

                                                        } else if(selectedGeoType === 'HSC partnership'){
                                                        trendGeoFilter = '#partname_trend';

                                                        } else if(selectedGeoType === 'HSC locality'){
                                                        trendGeoFilter = '#locname_trend';
                                                        extraGeoFilter = '#loc_iz_trend';

                                                        } else if(selectedGeoType === 'Intermediate zone'){
                                                        trendGeoFilter = '#izname_trend';
                                                        extraGeoFilter = '#loc_iz_trend';

                                                        } else {trendGeoFilter = '#adpname_trend';
                                                        }

                                                      // first step of 'onclick' logic - filter to matching indicator
                                                      var onclickScript = `$('#indic_trend')[0].selectize.setValue('${rowInfo.values['indicator']}');`;

                                                      // if IZ/locality selected then also filter the 'parent area' filter
                                                      if (extraGeoFilter === '#loc_iz_trend') {
                                                         onclickScript += `$('${extraGeoFilter}')[0].selectize.setValue('${selectedParentArea}');`;
                                                         }

                                                      // filter to chosen area
                                                      // note 'setTimeout' delays this step to give the 'choices' for that filter enough time to load since they are dynamic
                                                     onclickScript += `setTimeout(() => $('${trendGeoFilter}')[0].selectize.setValue('${selectedGeoName}'), 1000)`;

                                                      // the tab to navigate to
                                                      var tabDestination = `;$('a[data-value=&quot;trend&quot;]').tab('show');`;

                                                      // combine tab destination with filters to be changed
                                                      var final = onclickScript + tabDestination;


                                                      // formatting column
                                                      return `<div>
                                                                 <div>
                                                                    <a style = 'font-size:16px;' class = 'trend-link'
                                                                  onclick=\"${final}\" role='button'>${rowInfo.values['indicator']}</a>
                                                                     </div>
                                                                     <div style = 'font-size:1.2rem; margin-top: 3px;'><span style =
                                                                     'margin-right: 0.25rem;
                                                                      padding: 2px;
                                                                      background-color:#F5F5F5;
                                                                      border: 1px solid hsl(0, 0%, 75%);
                                                                      border-radius: 2px;'>${rowInfo.values['type_definition']}</span>
                                                                  <span aria-hidden='true'> • </span><span>${rowInfo.values['def_period']}
                                                                  </span>
                                                                     </div>
                                                                   </div>`;}")),

                                 
                                 # # Scotland column -------
                                  scotland_value = colDef(
                                                      maxWidth = 80,
                                                      name = "Scotland",
                                                      cell = function(value){
                                                           div(style = "margin-top: 10px;", value)
                                                         }),

                                 # # Chosen area column -------
                                 measure = colDef(
                                              maxWidth = 80,
                                              name = input$geoname_summary,

                                              # this function adds colour formatting to chosen area
                                              cell = function(value, index) {

                                     # define style of value in cell (adding rounded edges)
                                     styles <- list(`border-radius` = "16px", padding = "4px 12px")


                                     # define background colour of cell, depending on whether statistically significantly different to comparator
                                     background_colour_value <- case_when(summary_data()$colour[index] == "red" ~ "#FFBD88",
                                                                          summary_data()$colour[index] == "blue" ~ "hsl(230, 70%, 90%)",
                                                                          summary_data()$colour[index] == "gray" ~ "#D3D3D3",
                                                                          TRUE ~ "white")

                                     # define colour of cell, depending on whether statistically significantly different to comparator
                                     colour_value <- case_when(summary_data()$colour[index] == "red" ~ "hsl(350, 45%, 30%)",
                                                               summary_data()$colour[index] == "blue" ~ "hsl(230, 45%, 30%)",
                                                               summary_data()$colour[index] == "gray" ~ "black",
                                                               TRUE ~ "black")

                                     # assign text colour and background colour to styles
                                     styles$color <- colour_value
                                     styles$`background-color` <- background_colour_value

                                     # final value to be displayed, using styling created above
                                     div(style = "margin-top: 10px;",span(value, style = styles))
                                   }
                                 ),
                                 
                                 
                                 # in-line chart -------
                                  spine_chart = colDef(
                                                    html = T,
                                                    minWidth = 200,
                                                    header = div(
                                                        style = "display: flex;
                                                                 justify-content: space-between;
                                                                 margin-left: 25px;
                                                                 margin-right: 25px;",
                                                        div("<- worse"), div("better ->")),


                                                      # this function renders chart on each row
                                                      # taking values from that row to populate

                                                      cell = JS("function(rowInfo, global_min, global_max) {
                                                      
                                                      

                                                                             return `<div id='htmlwidget-${rowInfo.values['row_number']}' style='width:100%;height:70;' class='highchart html-widget'>
                                                                             </div><script type='application/json' data-for='htmlwidget-${rowInfo.values['row_number']}'>

                                                                             {\"x\":{\"hc_opts\":{\"chart\":{\"reflow\":true,\"inverted\":true,\"height\":70,\"accessibility\":{\"enabled\":true,\"linkedDescription\":\"alt text\"}},\"title\":{\"text\":null},
                                                                             \"yAxis\":{\"title\":{\"text\":[]},\"type\":\"linear\",\"min\":-1,\"max\":1,\"labels\":{\"format\":\"{enabled: false}\"},
                                                                             \"gridLineColor\":\"transparent\",\"plotLines\":[{\"color\":\"red\",\"width\":3,\"value\":0,\"zIndex\":1000}]},
                                                                             \"credits\":{\"enabled\":false},\"exporting\":{\"enabled\":false},\"boost\":{\"enabled\":false},
                                                                             \"plotOptions\":{\"series\":{\"label\":{\"enabled\":false},\"turboThreshold\":0,\"showInLegend\":false},
                                                                             \"treemap\":{\"layoutAlgorithm\":\"squarified\"},\"scatter\":{\"marker\":{}}},
                                                                             \"series\":[{\"group\":\"group\",\"data\":[{\"indicator\":\"${rowInfo.values['row_number']}\",
                                                                             \"low\":${rowInfo.values['min_value_plot']},\"high\":${rowInfo.values['max_value_plot']},\"name\":\"x\"}],\"type\"
                                                                             :\"columnrange\",\"enableMouseTracking\":false,\"borderRadius\":6},{\"group\":\"group\",\"data\":[{\"indicator\":\"${rowInfo.values['row_number']}\",\"color\":\"${rowInfo.values['marker_colour']}\",
                                                                             \"low\":${rowInfo.values['min_value_plot']},\"high\":${rowInfo.values['max_value_plot']},\"scotland_value\":0,\"measure\":${rowInfo.values['measure_plot']},\"y\":${rowInfo.values['measure_plot']}}],
                                                                             \"type\":\"line\",\"enableMouseTracking\":false}],\"xAxis\":{\"type\":\"category\",\"title\":\"\",\"labels\":{\"format\":\"{enabled: false}\"}}},
                                                                             \"theme\":{\"chart\":{\"backgroundColor\":\"transparent\"}},\"conf_opts\":{\"global\":{
                                                                             \"Date\":null}},\"type\":\"chart\",
                                                                             \"debug\":false},\"evals\":[],\"jsHooks\":[]}</script>`}")),

                                 # hide some columns
                                 # note these columns are hidden but are used within various functions above for those columns that are displayed
                                 type_definition = colDef(show = FALSE), # required for indicator col
                                 def_period = colDef(show = FALSE), # required for indicator col
                                 colour = colDef(show = FALSE), # required for measure col
                                 measure_plot = colDef(show = FALSE), # required for chart 
                                 min_value_plot = colDef(show = FALSE), # required for chart
                                 max_value_plot = colDef(show = FALSE), # required for chart
                                 row_number = colDef(show = FALSE), # required for chart
                                 marker_colour = colDef(show = FALSE) # required for chart
                                 
                      ), # close columns list 
                              
                               # include highchart dependencies otherwise charts won't render
                               deps = htmlwidgets::getDependency("highchart" , "highcharter"))
  
                        })







