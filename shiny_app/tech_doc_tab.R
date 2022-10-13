#################################################.
##  Indicator definitions tab ----
#################################################.

# filter indicators by selected profile and geography level
tech_info <- reactive({
  ind_dat %>%
    filter(grepl(paste(input$profile_search, collapse="|"), profile_short)) %>%
    filter(grepl(paste(input$geo_search, collapse="|"), available_geographies))
})


# display indicator search results in table with expandable rows
output$ind_search_results <- renderReactable({
  
  reactable(
    tech_info(),
    searchable = TRUE, 
    theme = table_theme(), # table theme can be found in global script
    rowStyle = list(cursor = "pointer"),
    onClick = "expand", 
    highlight = TRUE,
    language = reactableLang(
      searchPlaceholder = "Type to search for an indicator",
      noData = "No results found",
      pageInfo = "{rowStart}\u2013{rowEnd} of {rows} indicators"
    ),
    columns = list(
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
                     <a class = 'tour-button' onclick = \"$('#indic_trend')[0].selectize.setValue('${rowInfo.values['indicator_name']}');$('a[data-value=&quot;trend&quot;]').tab('show'); \" role='button'>See time trend</a>
                     <a class = 'tour-button' onclick = \"$('#indic_rank')[0].selectize.setValue('${rowInfo.values['indicator_name']}');$('a[data-value=&quot;rank&quot;]').tab('show'); \" role='button'>Compare different areas</a> 
                     <a class = 'tour-button' onclick = \"$('#indic_simd')[0].selectize.setValue('${rowInfo.values['indicator_name']}');$('a[data-value=&quot;ineq&quot;]').tab('show'); \" role='button'>Explore inequalities</a>
                     </div>
                     <div class='card-test'>
                     <h4>Indicator definition</h4>
                     <div>${rowInfo.values['indicator_definition']}</div>
                     </div>
                     <div class='card-test'>
                     <h4>Numerator</h4>
                     <div>${rowInfo.values['numerator']}</div>
                     </div>
                     <div class='card-test'>
                     <h4>Denominator</h4>
                     <div>${rowInfo.values['denominator']}</div>
                     </div>
                     <h4 style = 'font-weight:bold;'>Finer details</h4>
                     <table style='width:100%; table-layout: fixed' class = 'table-test'>
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
                     <tr class='spacer' style = 'line-height:5px;'>
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
                     <div class='card-test'>
                     <h4>Data source</h4>
                     <div>${rowInfo.values['data_source']}</div>
                     </div>
                     <div class='card-test'>
                     <h4>Related publications</h4>
                     <div>${output}</div>
                     </div>
                     <div class='card-test'>
                     <h4>Supporting information</h4>
                     <div><a style = 'text-decoration: underline; color:#337ab7;' href='${rowInfo.values['output']}' target='_blank'>${rowInfo.values['supporting_information']}</a></div>
                     </div>
                     <div class='card-test'>
                     <h4>Inclusion rationale</h4>
                     <div>${rowInfo.values['inclusion_rationale']}</div>
                     </div>
                     <div class='card-test'>
                     <h4>Notes and caveats</h4>
                     <div>${rowInfo.values['notes_caveats']}</div>
                     
                     
                     </div>`
                     
}")),
      next_update = colDef(cell = function(value) strftime(value, "%b-%Y"),
                           defaultSortOrder = "desc", sortable = TRUE, name = "Next update due", na = "TBC"),
      # hide all other columns
      profile = colDef(show = F),
      domain = colDef(show = F),
      last_updated = colDef(show = F),
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

