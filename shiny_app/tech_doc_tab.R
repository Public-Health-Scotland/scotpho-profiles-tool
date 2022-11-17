#################################################.
##  Indicator definitions tab ----
#################################################.

# Reactive data to be used in table
# Filter data by a) profile and b) geography levels selected by user
tech_info <- reactive({
  
  ind_dat %>%
    filter(grepl(paste(input$profile_search, collapse="|"), profile_short)) %>%
    filter(grepl(paste(input$geo_search, collapse="|"), available_geographies))
  
})


# Download technical info data when button clicked
output$btn_techdoc_download <- downloadHandler(
  
  filename ="Indicator_definitions.csv",
  content = function(file) {
    write.csv(ind_dat %>% select(-next_update_column, -indicator_number, - profile_short), 
              file, row.names=FALSE) }
)



# Display indicator search results in a table with expandable rows
# Using reactive data from above
output$ind_search_results <- renderReactable({
  
  reactable(
    tech_info(), # reactive data created above
    searchable = TRUE, # include a global search bar
    defaultPageSize = 25, # set max number of rows per page
    theme = table_theme(), # table_theme() can be found in global script
    rowStyle = list(cursor = "pointer"),
    onClick = "expand", # expand rows when clicked on
    highlight = TRUE, # changes row colour when user hovers over it
    language = reactableLang(
      searchPlaceholder = "Type to search for an indicator", # olaceholder text for global search bar
      noData = "No results found", # text to display in table when no results to display
      pageInfo = "{rowStart}\u2013{rowEnd} of {rows} indicators" # text to display in table footer
    ),
    
    # Customising all the columns in the table:
    columns = list(
      # Column 1: Indicator name 
      indicator_name = colDef(
        minWidth = 350,
        show = T, 
        html = T, 
        name = "Indicator search results",
        # display profile(s) under each indicator name
        cell = function(value, index) {
          profile <- tech_info()$profile[index]
          div(
            div(style = list(fontWeight = "Medium"), value),
            div(style = list(fontSize = "1.0rem"), profile)
          )
        },
        # Technical information to display when row expanded
        # ${rowInfo.values['column name'] returns the value in that column for the particular row of data the user has expanded
        details = JS(" function(rowInfo) {

                     
                     const input = `${rowInfo.values['related_publications']}`
                     
                     const urlRegex = /(https?:\\/\\/(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{1,256}\\.[a-zA-Z0-9()]{1,6}\\b([-a-zA-Z0-9()@:%_\\+.~#?&//=]*))/g;
                     
                     const matches = input.match(urlRegex) || []
                     
                     let output = matches.length ? input : 'N/A';
                     
                     for(let match of matches){
                     output = output.replace(match, `<a style = 'text-decoration: underline; color:#337ab7;' href='${match}'>${match}</a>`);
                     }


                     return  `<div class = 'prof-dom' style='display: flex;'>
                     <div class='date-info' style = 'display: flex; font-size:medium; margin-right:20px;'>
                     <p style = 'font-weight:600'>Last updated:  </p>
                     <div>${rowInfo.values['last_updated']}</div>
                     </div>
                     <div class='date-info' style = 'display: flex; font-size:medium;'>
                     <p style = 'font-weight:600'>Next update due:  </p>
                     <div>${rowInfo.values['next_update']}</div>
                     </div>
                     </div>


                     <h4 style = 'font-weight:bold;'>Quick links</h4>
                     <div class = 'link-buttons'>
                     <a class = 'button' onclick = \"$('#indic_trend')[0].selectize.setValue('${rowInfo.values['indicator_name']}');$('a[data-value=&quot;trend&quot;]').tab('show'); \" role='button'>See time trend</a>
                     <a class = 'button' onclick = \"$('#indic_rank')[0].selectize.setValue('${rowInfo.values['indicator_name']}');$('a[data-value=&quot;rank&quot;]').tab('show'); \" role='button'>Compare different areas</a> 
                     <a class = 'button' onclick = \"$('#indic_simd')[0].selectize.setValue('${rowInfo.values['indicator_name']}');$('a[data-value=&quot;ineq&quot;]').tab('show'); \" role='button'>Explore inequalities</a>
                     </div>

  
                     <div class='indicator-info'>
                     <h4>Indicator definition</h4>
                     <div>${rowInfo.values['indicator_definition']}</div>
                     </div>

                     <div class='indicator-info'>
                     <h4>Data source</h4>
                     <div>${rowInfo.values['data_source']}</div>
                     </div>

                     <div class='indicator-info'>
                     <h4>Numerator</h4>
                     <div>${rowInfo.values['numerator']}</div>
                     </div>

                     <div class='indicator-info'>
                     <h4>Denominator</h4>
                     <div>${rowInfo.values['denominator']}</div>
                     </div>


                     <h4 style = 'font-weight:bold;'>Methodology</h4>
                     <table style='width:100%; table-layout: fixed' class = 'phs-table'>
                     <tr>
                     <th>Confidence interval</th>
                     <th>Rounding</th>
                     <th>Disclosure control</th>
                     <th>Measure</th>
                     </tr>
                     <tr>
                     <td>${rowInfo.values['confidence_interval_method']}</td>
                     <td>${rowInfo.values['rounding']}</td>
                     <td>${rowInfo.values['disclosure_control']}</td>
                     <td>${rowInfo.values['measure']}</td>
                     </tr>
                     <tr class='spacer' style = 'line-height:5px; border-style:none;'>
                     <td colspan='4'>&nbsp;</td>
                     </tr>
                     <tr>
                     <th>Age group</th>
                     <th>Sex</th>
                     <th>Available geographies</th>
                     <th>Inequalities</th>
                     </tr>
                     <tr>
                     <td>${rowInfo.values['age_group']}</td>
                     <td>${rowInfo.values['sex']}</td>
                     <td>${rowInfo.values['available_geographies']}</td>
                     <td>${rowInfo.values['`label inequality`']}</td>
                     </tr>
                     </table>
                      <br>
                     <div>For a detailed explanation on how our indicators are calculated, please refer to our <a href='http://www.scotpho.org.uk/media/1026/explanation-of-statistics-used-in-profiles-v2.pptx' target='_blank'>Explanation of Statistics</a></div>
 
                     <div class='indicator-info'>
                     <h4>Related publications</h4>
                     <div>${output}</div>
                     </div>

                     <div class='indicator-info'>
                     <h4>Supporting information</h4>
                     <div><a style = 'text-decoration: underline; color:#337ab7;' href='${rowInfo.values['output']}' target='_blank'>${rowInfo.values['supporting_information']}</a></div>
                     </div>

                     <div class='indicator-info'>
                     <h4>Inclusion rationale</h4>
                     <div>${rowInfo.values['inclusion_rationale']}</div>
                     </div>

                     <div class='indicator-info'>
                     <h4>Notes and caveats</h4>
                     <div>${rowInfo.values['notes_caveats']}</div>
                     </div>`
                     
}")),
      # 2. format date column
      next_update_column = colDef(cell = function(value) strftime(value, "%b-%Y"),
                           defaultSortOrder = "desc", sortable = TRUE, name = "Next update due", na = "TBC"),
      
      # 3. hide all other columns in the table. 
      profile = colDef(show = F),
      domain = colDef(show = F),
      last_updated = colDef(show = F),
      next_update = colDef(show = F),
      trends_from = colDef(show = F),
      indicator_definition = colDef(show = F),
      data_source = colDef(show = F),
      source_details = colDef(show = F),
      related_publications = colDef(show = F),
      supporting_information = colDef(show = F),
      statistics_publications = colDef(show = F),
      inclusion_rationale = colDef(show = F),
      numerator = colDef(show = F),
      available_geographies = colDef(show = F),
      disclosure_control = colDef(show = F),
      confidence_interval_method = colDef(show = F),
      measure = colDef(show = F),
      notes_caveats = colDef(show = F),
      denominator = colDef(show = F),
      year_type = colDef(show = F),
      diagnostic_code_position = colDef(show = F),
      rounding = colDef(show = F),
      aggregation = colDef(show = F),
      indicator_number = colDef(show = F),
      age_group = colDef(show = F),
      sex = colDef(show = F),
      update_frequency = colDef(show = F),
      profile_short = colDef(show = F) ,
      `label inequality` = colDef(show = F)
       )
        )
  })

### END 

