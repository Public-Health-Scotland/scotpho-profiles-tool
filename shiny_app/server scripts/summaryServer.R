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
  dt <- dt[substr(profile_domain1, 1, 3) == input$summary_profile |
           substr(profile_domain2, 1, 3) == input$summary_profile |
           substr(profile_domain3, 1, 3) == input$summary_profile  ]
  
  # filter by chosen area and get latest data for each indicator
  chosen_area <- dt[areaname == input$geoname_summary & areatype == input$geotype_summary & type_definition != "Number",
                    .SD[!is.na(measure) & year == max(year)], by = indicator]
  
  # include scotland figures for comparison
  scotland <- dt[areaname == "Scotland"]
  chosen_area <- chosen_area[scotland, on = c("ind_id", "year"), scotland_value := scotland$measure]
  
  # calculate quantiles for each indicator within chosen geography level for spine chart 
  other_areas <- dt[areatype == input$geotype_summary][chosen_area, on = .(ind_id, year), nomatch = 0][,
                                                                                                       .(Q0 = quantile(measure, probs = 0, na.rm = TRUE),
                                                                                                         Q100 = quantile(measure, probs = 1, na.rm = TRUE),
                                                                                                         Q25 = quantile(measure, probs = 0.25, na.rm = TRUE),
                                                                                                         Q75 = quantile(measure, probs = 0.75, na.rm = TRUE)),
                                                                                                       by = .(ind_id, year)]
  # add quantile values for each indicator to table
  chosen_area <- chosen_area[other_areas, on = c("ind_id", "year"),
                             c("Q0", "Q100", "Q25", "Q75") := .(other_areas$Q0, 
                                                                other_areas$Q100,
                                                                other_areas$Q25,
                                                                other_areas$Q75)]
  

  # assign colours to values depending on statistical significance
  final <- chosen_area %>%
    mutate(marker_colour = case_when(lowci <= scotland_value & upci >= scotland_value & interpret %in% c("H", "L") ~'#6A6C6D',
                         lowci > scotland_value & interpret == "H" ~ '#1B7CED',
                         lowci > scotland_value & interpret == "L" ~ '#FFA500',
                         upci < scotland_value & interpret == "L" ~ '#1B7CED',
                         upci < scotland_value & interpret == "H" ~ '#FFA500',
                         interpret == "O" ~ '#FFFFFF', TRUE ~ '#FFFFFF'))


  # identify correct domain and arrange by domain
  # this is because there are 3 domain columns in dataset as some indicators belong to more than 1 profile/domain
  final <- final %>%
    mutate(domain = as.factor(case_when(
      substr(profile_domain1,1,3)== input$summary_profile ~
        substr(profile_domain1, 5, nchar(as.vector(profile_domain1))),
      substr(profile_domain2,1,3)== input$summary_profile ~
        substr(profile_domain2, 5, nchar(as.vector(profile_domain2))),
      TRUE ~ substr(profile_domain3, 5, nchar(as.vector(profile_domain3)))))) %>%
    arrange(domain)
  
  
  # creating spine chart data
  final <- final %>%
    # duplicate chosen area value in another column so one can be used in the table and one can be used for spine chart
    mutate(chosen_value = measure) %>%
    
    mutate(scale_min = case_when(scotland_value - Q0 > Q100 - scotland_value ~ Q0,
                                 TRUE ~ scotland_value - (Q100 - scotland_value))) %>%
    mutate(scale_max = case_when(scale_min == Q0 ~ scotland_value + (scotland_value - Q0), TRUE ~ Q100)) %>%
    mutate(across(c(chosen_value, Q0, Q25, Q75, Q100), ~ (. - scale_min) / (scale_max - scale_min))) 
  
  
  final <- final %>%
    mutate(across(c("Q0", "Q25", "Q75", "Q100", "chosen_value"), ~ case_when(interpret == "L" ~ 1 - ., TRUE ~ .)))
    

   # conditionally calculating worst to best, depending on whether a lower is value is better or a higher value is better
   # this ensures that 'worst' is always to the left of the spine, and 'best' is always to the right
   final <- final %>%
    mutate(one = case_when(interpret == "L" ~ Q0 - Q25, TRUE ~ Q100 - Q75), # worst
           two = case_when(interpret == "L" ~ Q75 - Q100, TRUE ~ Q25 - Q0), # 25th percentile
           three = case_when(interpret == "L" ~ Q25 - Q75, TRUE ~ Q75 - Q25), # 75th percentile
           four = case_when(interpret == "L" ~ Q100, TRUE ~ Q0) # best
           )

   
   final$row_number <- 1:nrow(final) # assign each row an 'id' to be used when constructing highchart
   final$spine_chart <- NA # create empty column to populate with in-line highcharts
   

  # selecting columns required for table
  final <- final %>%
    select(domain, indicator, measure, scotland_value, type_definition, def_period, 
           row_number, spine_chart, one, two, three, four, chosen_value, marker_colour)
  
  final <- final
  
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
    write.csv(summary_data() %>% select(domain, indicator, measure, scotland_value, type_definition, def_period), file, row.names=FALSE)
  })



# download as PDF
output$download_summary_pdf <- downloadHandler(

  filename = function() { 
    paste(input$summary_profile, "-summary-", input$geoname_summary, ".pdf", sep="")
  },
  
  content = function(file) {
    

    td <- tempdir()
    
    tempReport <- file.path(td, "spinecharts.Rmd")
    tempLogo <- file.path(td, "scotpho_reduced.png")
    
    file.copy("spinecharts.Rmd", tempReport, overwrite = TRUE)
    file.copy("scotpho_reduced.png", tempLogo, overwrite = TRUE)
    
    # Set up parameters to pass to Rmd document
    params <- list(reactive_df = summary_data(),
                   chosen_area = input$geoname_summary,
                   chosen_profile = input$summary_profile,
                   chosen_geography_level = input$geotype_summary
    )
    
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
    
    # unload package after each render execution 
    # otherwise users can only download 1 pdf, and any other download attempts will fail
    # details of issue here: https://stackoverflow.com/questions/46080853/why-does-rendering-a-pdf-from-rmarkdown-require-closing-rstudio-between-renders
    detach("package:kableExtra", unload=TRUE)
  }
)


# 5. summary table of results  ------------
output$summary_table <- renderUI({
  
  summary_table <- widgetTable(data = summary_data(), 
                               
                               columns = list(
                                 
                                 # # Domain column ---------
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


                                 # Scotland column -------
                                  scotland_value = colDef(
                                                      maxWidth = 80,
                                                      name = "Scotland",
                                                      cell = function(value){
                                                           div(style = "margin-top: 19px;font-size:1.5rem;", value)
                                                         }),
                                 
                                 # Chosen area column -------
                                 measure = colDef(
                                   maxWidth = 80,
                                   name = input$geoname_summary,
                                   cell = function(value){
                                     div(style = "margin-top: 19px;font-size:1.5rem;", value)
                                   }),

                                

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
                                                    ## SPINE PLOT
                                                    cell = JS("function(rowInfo) {



                                                                             return `<div id='htmlwidget-${rowInfo.values['row_number']}' style='width:100%;height:70;' class='highchart html-widget'>
                                                                             </div><script type='application/json' data-for='htmlwidget-${rowInfo.values['row_number']}'>

                                                                             {\"x\":{

                                                                             \"hc_opts\":{

                                                                             \"chart\":{
                                                                                        \"inverted\":true,
                                                                                        \"height\":70,
                                                                                        \"accessibility\":{\"enabled\":true,
                                                                                        \"linkedDescription\":\"alt text\"}
                                                                                        },

                                                                             \"title\":{\"text\":null},

                                                                             \"yAxis\":{\"title\":{\"text\":[]},
                                                                                        \"type\":\"linear\",
                                                                                        \"min\":0,
                                                                                        \"max\":1,
                                                                                        \"labels\":{\"format\":\"{enabled: false}\"},
                                                                                        \"gridLineColor\":\"transparent\",
                                                                                        \"plotLines\":[{\"color\":\"red\",\"width\":3,\"value\":0.5,\"zIndex\":1000}]
                                                                                        },

                                                                             \"credits\":{\"enabled\":false},
                                                                             \"exporting\":{\"enabled\":false},
                                                                             \"boost\":{\"enabled\":false},

                                                                             \"plotOptions\":{
                                                                             \"series\":{\"label\":{\"enabled\":false}
                                                                             },



                                                                             \"bar\":{\"dataLabels\":{\"enabled\":false},
                                                                             \"stacking\":\"normal\",
                                                                             \"borderWidth\":0,
                                                                             \"enableMouseTracking\":false}
                                                                             },


                                                                             \"series\":[


                                                                             {\"data\":[{\"name\":\"\",\"y\":${rowInfo.values['one']}}],
                                                                             \"type\":\"bar\",
                                                                             \"color\":\"#D3D3D3\"},



                                                                             {\"data\":[{\"name\":\"\",\"y\":${rowInfo.values['two']}}],
                                                                             \"type\":\"bar\",
                                                                             \"color\":\"#A4A4A4\"},


                                                                             {\"data\":[{\"name\":\"\",\"y\":${rowInfo.values['three']}}],
                                                                             \"type\":\"bar\",
                                                                             \"color\":\"#D3D3D3\"},


                                                                             {\"data\":[{\"name\":\"\",\"y\":${rowInfo.values['four']}}],
                                                                             \"type\":\"bar\",
                                                                             \"color\":\"white\"},

                                                                             {\"group\":\"group\",
                                                                             \"data\":[{\"y\":${rowInfo.values['chosen_value']}}],
                                                                             \"type\":\"scatter\",
                                                                             \"color\":\"${rowInfo.values['marker_colour']}\",
                                                                             \"marker\":{\"lineColor\":\"black\",\"lineWidth\":1,\"radius\":8},
                                                                             \"enableMouseTracking\":false}

                                                                             ],


                                                                             \"xAxis\":{
                                                                             \"type\":\"category\",
                                                                             \"title\":{\"text\":null}},

                                                                             \"legend\":{\"enabled\":false}

                                                                             },

                                                                             \"theme\":{
                                                                             \"chart\":{\"backgroundColor\":\"transparent\"}
                                                                             },

                                                                             \"conf_opts\":{
                                                                             \"global\":{
                                                                             \"Date\":null}
                                                                             },

                                                                             \"type\":\"chart\"

                                                                             },

                                                                             \"evals\":[],\"jsHooks\":[]

                                                                                  }</script>`}")



                                                    ),

                                                  

                                 # hide some columns
                                 # note these columns are hidden but are used within various functions above for those columns that are displayed
                                 type_definition = colDef(show = FALSE),#, # required for indicator col
                                 def_period = colDef(show = FALSE), # required for indicator col
                                 row_number = colDef(show = FALSE), # required for chart
                                 one = colDef(show = FALSE), # required for chart
                                 two = colDef(show = FALSE), # required for chart
                                 three = colDef(show = FALSE), # required for chart
                                 four = colDef(show = FALSE), # required for chart
                                 chosen_value = colDef(show = FALSE), # required for chart
                                 marker_colour = colDef(show = FALSE) # required for chart
                               
                      ), # close columns list 
                              
                               # include highchart dependencies otherwise charts won't render
                               deps = htmlwidgets::getDependency("highchart" , "highcharter")
                               
                               )
  
                        })





