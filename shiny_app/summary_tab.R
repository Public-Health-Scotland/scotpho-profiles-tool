###############################################.
## Summary - common objects ----
###############################################.
# Summary help pop-up
observeEvent(input$help_summary, {
  
  if (input$chart_summary == "Spine") {
    
    showModal(modalDialog(
      title = "How to use this chart",
      p(img(src="help_barcode2.png",height=600)),size = "l",
      easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")
    ))
    
  } else if (input$chart_summary == "Trend") {
    
    showModal(modalDialog(
      title = "How to use this chart",
      p(column(6,"Select 'Area' to compare one geographical area against another area, or select 'Time' to compare
               against a baseline year for the same area."),
        column(6, img(src="help_heatmap1.png"))),
      p(column(6, "Hover over each tile to see indicator definitions and time periods."),
        column(6, img(src="help_heatmap2.png"))), 
      p("Colours are used to indicate if the value for an indicator is statistically significantly different to the comparator, statistical confidence intervals are used to decide if differences are 'significant'."),
      h5("Area comparison example:  e.g. comparing 'Area X' against Scotland", style = "font-weight: bold; color: black; margin-bottom: 0px;"),
      p("In the example below the first row shows an indicator statistically significantly worse than Scotland up to the year 2010 but from 2013 onwards is statistically significantly better than Scotland.
        The second row is an example of an indicator not statistically significantly different to Scotland for the 4 years up to 2006 but from 2007 significantly worse than Scotland."),
      p(img(src="help_heatmap3.png")),      
      h5("Time comparison example: e.g. yearly comparisons of 'Area X' against the value for year 2003", style = "font-weight: bold; color: black; margin-bottom: 0px;"),
      p("Changing the comparison type to a time based comparison can provide differing insights.
        The example below is the same as that shown but viewed as a time comparison. Now the two rows both show indicators that are significantly better than in later years when compared to 2003.
        Combining the information from the area and time comparisons suggests that whilst both indicators are reducing for this area the reductions in admissions for heart disease in this area are not keeping pace with the reductions seen in Scotland as a whole."),
      p(img(src="help_heatmap4.png")),  
      
      size = "l", easyClose = TRUE, fade=FALSE, footer = modalButton("Close (Esc)")
      ))
  } else if (input$chart_summary == "Snapshot") {
    showModal(modalDialog(
      title = "How to use this chart",
      p(column(6, p("Select 'Area' to compare one geographical area against another area, or select 'Time' to compare
                    against a baseline year for the same area."),
               p("Hover over each box to see indicator values and time periods."),
               p("Colours are used to indicate if the value for an indicator is statistically significantly different to the comparator, 
                 statistical confidence intervals are used to decide if differences are 'significant'."),
               p("The different comparison types (area or time) can be used to provide different insights about the indicators.")),
        column(6, img(src="help_heatmap1.png"),
               br(), br(),
               img(src="help_snapshot1.png"),
               br()), br()),
      size = "l", easyClose = TRUE, fade=FALSE, footer = modalButton("Close (Esc)")
      ))
  }
})

###############################################.
# Indicator definitions
#Subsetting by domain and profile. Profile is fiddly as vector uses abbreviations 
# so needs to be converted to the names to match techdoc.
defs_data_summary <- reactive({
  if (input$chart_summary == "Spine") {
    
    techdoc %>% subset(grepl(input$topic_spine, domain) &
                         grepl(names(profile_list[unname(profile_list) == input$profile_summary]),
                               profile))
  } else {
    techdoc %>% 
      subset(grepl(names(profile_list[unname(profile_list) == input$profile_summary]), profile))  
  }
  
})

output$defs_text_summary <- renderUI({
  
  HTML(paste(sprintf("<b><u>%s</b></u> <br> %s ", defs_data_summary()$indicator_name, 
                     defs_data_summary()$indicator_definition), collapse = "<br><br>"))
})

#####################.
# Reactive controls
## Remember the selected samples
# creates reactive values to remember user selection of the profile
# so it only changes when the user changes it on purpose
prof_chosen <- reactiveValues(value_profile = "HWB")
observeEvent(input$profile_summary, 
             isolate({ prof_chosen$value_profile <- input$profile_summary})
)

# Reactive controls for profile depending on area selected
output$profile_ui_summary <- renderUI({
  
  profiles <- profile_areatype[input$geotype_summary]
  selectInput("profile_summary", label = NULL, choices = profiles, 
              prof_chosen$value_profile)
  
})

# Temporary fix for lack of saving charts functionality for snapshot and trend
output$save_chart_ui <- renderUI({
  if (input$chart_summary %in% c("Trend", "Snapshot") ){
    savechart_button('download_summaryplot_no', 'Save chart',  class = "down", disabled=TRUE)
  } else if (input$chart_summary == "Spine") {
    savechart_button('download_summaryplot', 'Save chart',  class = "down")
  }
  
})

# Reactive controls for heatmap:area name depending on areatype selected
output$geoname_ui_summary <- renderUI({
  
  areas_summary <- if (input$geotype_summary %in% c("Health board", "Council area", 
                                                    "HSC partnership", "Scotland", "Alcohol & drug partnership"))
  {
    sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_summary])
  } else {
    sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_summary
                             & geo_lookup$parent_area == input$loc_iz_summary])
  }
  
  selectizeInput("geoname_summary", label = NULL,  
                 choices = areas_summary, selected = "")
  
})


# Years to compare with depending on what data is available
output$comp_ui_summary <- renderUI({
  
  if (input$chart_summary %in% c("Snapshot", "Trend")) {
    if (input$comp_summary == 1) {
      selectInput("geocomp_summary", "Select a comparison area", choices = comparator_list,
                  selectize=TRUE, selected = "Scotland")
    } else if (input$comp_summary == 2) {
      
      years <- min(optdata$year):max(optdata$year) 
      
      selectizeInput("yearcomp_summary", "Baseline year", choices = years)
    }
  } else if (input$chart_summary == "Spine") {
    selectizeInput("geocomp_spine", "Select a comparison area", 
                   choices = comparator_list, selected = "Scotland")
  }
  
})

#####################.
# Reactive data
summary_data <- reactive({
  #data for the chosen area. Filtering based on user input values.
  summary_chosen_area <- optdata %>% 
    subset(areaname == input$geoname_summary &
             areatype == input$geotype_summary &
             !(indicator %in% c("Mid-year population estimate - all ages", "Quit attempts")) &
             (substr(profile_domain1, 1, 3) == input$profile_summary |
                substr(profile_domain2, 1, 3) == input$profile_summary)) %>% 
    droplevels()
  
  #Select comparator based on years available for area selected.
  if (input$comp_summary == 1){ #if area comparison selected
    summary_chosencomp <- optdata %>% 
      subset(areaname == input$geocomp_summary &
               indicator != "Mid-year population estimate - all ages" &
               areatype %in% c("Health board", "Council area", "Scotland") &
               (substr(profile_domain1, 1, 3) == input$profile_summary |
                  substr(profile_domain2, 1, 3) == input$profile_summary)) %>% 
      select(c(year, indicator, measure)) %>% 
      rename(comp_m=measure) 
  } else if (input$comp_summary == 2) { #if time comparison selected
    summary_chosencomp <- summary_chosen_area %>% 
      subset(year == input$yearcomp_summary) %>% 
      select(c(indicator, measure)) %>% 
      rename(comp_m=measure) 
  }
  
  #Merging comparator and chosen area
  if (input$comp_summary == 1){
    sum_data <- left_join(x = summary_chosen_area, y = summary_chosencomp, 
                          by=c("indicator", "year")) %>% droplevels()
  } else if (input$comp_summary == 2) {
    sum_data <- left_join(x = summary_chosen_area, y = summary_chosencomp, 
                          by=c("indicator")) %>% droplevels()
  }
  
  sum_data <- sum_data %>% 
    #Creating a palette of colours based on statistical significance
    mutate(color = case_when(lowci <= comp_m & upci >= comp_m 
                             & interpret %in% c("H", "L") ~'gray',
                             lowci > comp_m & interpret == "H" ~ 'blue',
                             lowci > comp_m & interpret == "L" ~ 'red',
                             upci < comp_m & interpret == "L" ~ 'blue',
                             upci < comp_m & interpret == "H" ~ 'red', 
                             interpret == "O" ~ 'white',
                             TRUE ~ 'white'),
           #identifies correct domain name for title
           domain = as.factor(case_when(
             substr(profile_domain1,1,3)==input$profile_summary ~
               substr(profile_domain1, 5, nchar(as.vector(profile_domain1))),
             TRUE ~ substr(profile_domain2, 5, nchar(as.vector(profile_domain2))))))
})

#####################.
# titles 
#create title and subtitle variables
output$summary_title <- renderText({
  paste0(names(profile_list[unname(profile_list) == input$profile_summary]),
         " profile")
})

output$summary_subtitle <- renderText({
  if(input$comp_summary == 1){ #if areacomparator selected
    paste0(input$geoname_summary," (",input$geotype_summary,") compared against ",
           input$geocomp_summary)
  } else if(input$comp_summary==2){#if time comparison selected
    paste0("Changes within ",input$geoname_summary,": latest data available",
           " compared to ", input$yearcomp_summary)
  }
})

###############################################.
# This will create a reactive user interface depending on type of visualization 
# and profile selected
output$summary_expl_text <- renderUI({
  # Preparing a brief explanation for each visualisation 
  if (input$chart_summary == "Snapshot") {
    p("This visualisation shows all the indicators of the profile you
      have chosen. The latest data available for each of them is
      compared against the selected comparator. The colour of the boxes reflects if
      the differences are statistically significant or not. Hover over the boxes to see the
      values for each indicator.")
  } else if (input$chart_summary == "Trend") {
    p("This visualisation shows all the indicators of the profile you
      have chosen. The coloured boxes show comparisons of indicator values for your selected area against the chosen comparator over time. The colour of the boxes reflects if
      the differences are statistically significant or not. Hover over the boxes to see the
      values for each indicator.")
  } else if (input$chart_summary == "Spine") {
    p("This visualisation shows all the indicators of the domain you
      have chosen. The latest data available for each of them is
      compared against the selected comparator. Each bar represents a different area.
      The colour of the square to the left of the bars reflects if
      the differences are statistically significant or not.")
  }
  })

output$summary_ui_plots <- renderUI({
  
  if (input$chart_summary == "Snapshot") {
    if (input$profile_summary == "HWB") {
      tagList(#Health and Wellbeing profile
        column(4,
               sum_ui("Behaviours", "summ_hwb_beha"),
               sum_ui("Social care & housing", "summ_hwb_socare"),
               sum_ui("Environment", "summ_hwb_env"),
               sum_ui("Life expectancy & mortality", "summ_hwb_lifexp")
        ),
        column(4,
               sum_ui("Women's & children's health", "summ_hwb_women"),
               sum_ui("Immunisations & screening", "summ_hwb_imm"),
               sum_ui("Economy", "summ_hwb_econ"),
               sum_ui("Crime", "summ_hwb_crime")
        ),
        column(4,
               sum_ui("Mental health", "summ_hwb_mh"),
               sum_ui("Ill health & injury", "summ_hwb_injury"),
               sum_ui("Education", "summ_hwb_educ")
        )
      ) #taglist bracket
    } else if (input$profile_summary == "CYP") {
      tagList(#Children and young people profile
        column(4,
               sum_ui("Active", "summ_cyp_active"),
               sum_ui("Healthy", "summ_cyp_health")
        ),
        column(4,
               sum_ui("Included", "summ_cyp_includ"),
               sum_ui("Nurtured", "summ_cyp_nurt"),
               sum_ui("Safe", "summ_cyp_safe")
        ),
        column(4,
               sum_ui("Achieving", "summ_cyp_achiev"),
               sum_ui("Responsible", "summ_cyp_respon")
        )
      )# taglist bracket
    } else if (input$profile_summary == "ALC") {
      tagList(#Alcohol profile
        column(4,
               sum_ui("Environment", "summ_alc_env"),
               sum_ui("Services", "summ_alc_serv")
        ),
        column(4,
               sum_ui("Community safety", "summ_alc_commsaf"),
               sum_ui("CAPSM/Families", "summ_alc_family")
        ),
        column(4,
               sum_ui("Prevalence", "summ_alc_preval"),
               sum_ui("Health", "summ_alc_health")
        )
      )#taglist bracket
    } else if (input$profile_summary == "DRG") {
      tagList(#Drugs profile
        column(4,
               sum_ui("Environment", "summ_drg_env"),
               sum_ui("Services", "summ_drg_serv"),
               sum_ui("Data quality", "summ_drg_data")
        ),
        column(4,
               sum_ui("Community safety", "summ_drg_commsaf"),
               sum_ui("CAPSM/Families", "summ_drg_family")
        ),
        column(4,
               sum_ui("Prevalence", "summ_drg_preval"),
               sum_ui("Health", "summ_drg_health")
        )
      )#taglist bracket
    } else if (input$profile_summary == "MEN") {
      tagList(#Mental Health profile
        column(4,
               sum_ui("Female adult", "summ_men_fem")
        ),
        column(4,
               sum_ui("Male adult", "summ_men_male")
        ),
        column(4,
               sum_ui("CYP Mental Health", "summ_men_cyp")
        )
      )#taglist bracket
    } else if (input$profile_summary == "TOB") {
      tagList(#Tobacco profile
        column(4,
               sum_ui("Smoking in school children", "summ_tob_school"),
               sum_ui("Smoking cessation & smoking cessation products", "summ_tob_cess")
        ),
        column(4,
               sum_ui("Smoking attributable deaths & diseases", "summ_tob_attrib"),
               sum_ui("Smoking during and post pregnancy", "summ_tob_pregn")
        ),
        column(4,
               sum_ui("Adult prevalence", "summ_tob_preval"),
               sum_ui("Retailer information", "summ_tob_retail")
        )
      )#taglist bracket
    } else if (input$profile_summary == "POP") {
      tagList(#Population profile
        sum_ui("Population", "summ_pop_pop")
      )#taglist bracket
    }
  } else if (input$chart_summary == "Trend") { #IF SELECTED HEATMAP
    if (input$profile_summary == "HWB") {
      tagList(#Health and Wellbeing profile
        sum_ui("Behaviours", "heat_hwb_beha"),
        sum_ui("Social care & housing", "heat_hwb_socare"),
        sum_ui("Environment", "heat_hwb_env"),
        sum_ui("Life expectancy & mortality", "heat_hwb_lifexp"),
        sum_ui("Women's & children's health", "heat_hwb_women"),
        sum_ui("Immunisations & screening", "heat_hwb_imm"),
        sum_ui("Economy", "heat_hwb_econ"),
        sum_ui("Crime", "heat_hwb_crime"),
        sum_ui("Mental health", "heat_hwb_mh"),
        sum_ui("Ill health & injury", "heat_hwb_injury"),
        sum_ui("Education", "heat_hwb_educ")
      )
    } else if (input$profile_summary == "CYP") {
      tagList(#Children and young people profile
        sum_ui("Active", "heat_cyp_active"),
        sum_ui("Healthy", "heat_cyp_health"),
        sum_ui("Included", "heat_cyp_includ"),
        sum_ui("Nurtured", "heat_cyp_nurt"),
        sum_ui("Safe", "heat_cyp_safe"),
        sum_ui("Achieving", "heat_cyp_achiev"),
        sum_ui("Responsible", "heat_cyp_respon")
      )# taglist bracket
    } else if (input$profile_summary == "ALC") {
      tagList(#Alcohol profile
        sum_ui("Environment", "heat_alc_env"),
        sum_ui("Services", "heat_alc_serv"),
        sum_ui("Community safety", "heat_alc_commsaf"),
        sum_ui("CAPSM/Families", "heat_alc_family"),
        sum_ui("Prevalence", "heat_alc_preval"),
        sum_ui("Health", "heat_alc_health")
      )#taglist bracket
    } else if (input$profile_summary == "DRG") {
      tagList(#Drugs profile
        sum_ui("Environment", "heat_drg_env"),
        sum_ui("Services", "heat_drg_serv"),
        sum_ui("Data quality", "heat_drg_data"),
        sum_ui("Community safety", "heat_drg_commsaf"),
        sum_ui("CAPSM/Families", "heat_drg_family"),
        sum_ui("Prevalence", "heat_drg_preval"),
        sum_ui("Health", "heat_drg_health")
      )#taglist bracket
    } else if (input$profile_summary == "MEN") {
      tagList(#Mental Health profile
        sum_ui("Female adult", "heat_men_fem"),
        sum_ui("Male adult", "heat_men_male"),
        sum_ui("CYP Mental Health", "heat_men_cyp")
      )#taglist bracket
    } else if (input$profile_summary == "TOB") {
      tagList(#Tobacco profile
        sum_ui("Smoking in school children", "heat_tob_school"),
        sum_ui("Smoking cessation & smoking cessation products", "heat_tob_cess"),
        sum_ui("Smoking attributable deaths & diseases", "heat_tob_attrib"),
        sum_ui("Smoking during and post pregnancy", "heat_tob_pregn"),
        sum_ui("Adult prevalence", "heat_tob_preval"),
        sum_ui("Retailer information", "heat_tob_retail")
      )#taglist bracket
    } else if (input$profile_summary == "POP") {
      tagList(#Population profile
        sum_ui("Population", "heat_pop_pop")
      )#taglist bracket
    } # end of if else == "Trend"
  } else if (input$chart_summary == "Spine") {
    # Resize plot height for display in app
    tagList(
      withSpinner( plotOutput("spine_plot", height=spine_plot_height(), width="90%")),
      br(), br()
    )
  } # end of if else == "Spine"
  
})

#####################.
# Downloading controls
# Downloading data
summary_csv <- reactive({ 
  if (input$chart_summary == "Snapshot") {
    format_csv(snapshot_data(), extra_vars = "comp_m") %>%
      mutate(comparator_name = ifelse(input$comp_summary == 1, input$geocomp_summary,
                                      paste0(input$yearcomp_summary))) %>% 
      rename(comparator_value = comp_m)
  } else if (input$chart_summary == "Trend") {
    format_csv(summary_data(), extra_vars = "comp_m") %>% 
      mutate(comparator_name = ifelse(input$comp_summary == 1, input$geocomp_summary,
                                      paste0(input$yearcomp_summary))) %>% 
      rename(comparator_value = comp_m)
  } else if (input$chart_summary == "Spine") {
    spine_csv()
  }
})

output$download_summary <- downloadHandler( 
  filename =  'summary_data.csv', content = function(file) { 
    write.csv(summary_csv(), file, row.names=FALSE) })

# Downloading chart  
output$download_summaryplot <- downloadHandler(
  filename = case_when(input$chart_summary != "Spine" ~ 'summary.html',
                       input$chart_summary == "Spine"~ 'spine.png'),
  content = function(file){
    if (input$chart_summary == "Snapshot") {
      
      
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "summary_charts.Rmd")
      file.copy("summary_charts.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(chart_summary = input$chart_summary,
                     snapshot_data = snapshot_data(),
                     profile_summary = input$profile_summary,
                     comp_summary = input$comp_summary,
                     geoname_summary = input$geoname_summary,
                     geotype_summary = input$geotype_summary,
                     geocomp_summary = input$geocomp_summary)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      
      # export(p = plot_snapshot_download(), file = file, zoom = 1)
    } else if (input$chart_summary == "Trend") {
      ggsave(file, plot = plot_heat()+
               ggtitle(paste0(names(profile_list[unname(profile_list) == input$profile_summary]),
                              " profile: "),
                       subtitle =       if(input$comp_heat == 1){
                         paste0(input$geoname_heat," (", input$geotype_heat, ") compared against ",
                                input$geocomp_heat)
                       } else if(input$comp_heat==2){
                         paste0("Changes within ",input$geoname_heat,": latest data available",
                                " compared to ", input$yearcomp_heat)
                       }
               ),
             device = "png", scale=4, limitsize=FALSE)
    } else if (input$chart_summary == "Spine") {
      ggsave(file, plot = plot_spine()+
               ggtitle(label=paste(names(profile_list[unname(profile_list) == input$profile_summary])," profile: ", input$topic_spine,sep=""),
                       subtitle =paste(input$geoname_summary," (",input$geotype_summary,") compared against ",input$geocomp_spine,sep="")),
             device = "png", width=15, limitsize=FALSE)
    }
  })

###############################################.
## Snapshot  ----
###############################################. 

# Reactive dataset
snapshot_data <- reactive({
  summary_data() %>% group_by(indicator) %>% top_n(1, year) %>% 
    ungroup() %>% droplevels() %>% 
    # Indicator names in two lines, but without splitting words
    # It split both sides of the cut point and identify if a word has been cut
    mutate(indicator = as.character(indicator), # needed for plot and substring
           pluschars = substring(indicator, 35),
           underchars = substring(indicator, 1, 34),
           char = substring(indicator, 35, 35),
           unfinished_word = gsub( " .*$", "", pluschars),
           plus_minus_unfinished = gsub( "^\\S*", "", pluschars)) %>%
    # if it's shorter keep name as it is, if it breaks a word paste different bits
    # there is one case for which it doesn't work well, so adhoc solution
    mutate(indic_multiline = 
             case_when(char == "" ~ paste0(indicator),
                       char == " " ~ paste0(underchars, "<br>", pluschars),
                       indicator == "Population prescribed drugs for anxiety/depression/psychosis" ~
                         "Population prescribed drugs for<br>anxiety/depression/psychosis",
                       TRUE ~ paste0(underchars, unfinished_word, "<br>", plus_minus_unfinished)))
})

###############################################.
# Function that creates a snapshot plot for a domain 
plot_profile_snapshot <- function(domainchosen) {
  
  # only selecting decided domain
  prof_snap_data <- snapshot_data() %>% subset(domain == domainchosen) %>%
    droplevels() %>% 
    mutate(indicator = as.factor(indicator))
  
  #If no data available for that period then plot message saying data is missing
  if (is.data.frame(prof_snap_data) && nrow(prof_snap_data) == 0)
  {
    plot_nodata(height = 50)
  } else {
    
    # Tooltip
    if (input$comp_summary == 1) {#depending if time or area comparison
      tooltip_summary <-  c(paste0("Area: ", round(prof_snap_data$measure, 1), " || ",
                                   "Comparator: ", round(prof_snap_data$comp_m, 1), "<br>",
                                   prof_snap_data$trend_axis, "<br>", prof_snap_data$type_definition))
    } else if (input$comp_summary == 2) {
      tooltip_summary <-  c(paste0(prof_snap_data$trend_axis, ": ",
                                   round(prof_snap_data$measure, 1), "  ||  ",
                                   "Baseline: ", round(prof_snap_data$comp_m, 1), "<br>",
                                   prof_snap_data$type_definition))
    }
    
    # eliminating both axis
    axis_layout <- list(title = "", fixedrange=TRUE, zeroline = FALSE, showline = FALSE,
                        showticklabels = FALSE, showgrid = FALSE)
    
    
    # obtaining height for plot based on number of rows of indicators
    n_ind <- prof_snap_data %>% nrow()
    # when 0 or 1 indicators the plot needs at least that size to 
    # prevent the spinner from showing and let the tooltip work
    height_plot <- case_when(n_ind > 1 ~ 38*n_ind+10,
                             TRUE ~ 75) 
    
    # defining plot function
    plot_ly(prof_snap_data, y = ~indicator,   color = ~color, height = height_plot,
            colors=  c(blue = "#4da6ff", gray = "gray88", red = "#ffa64d", white = "#ffffff")
    ) %>%
      add_bars(x =1, showlegend= FALSE, width=1,
               hoverinfo="text", hovertext = tooltip_summary,
               marker = list(line= list(color="black", width = 0.5))) %>%
      # adding indicator name at center of each bar
      add_text(text = ~indic_multiline, x =0.5,  showlegend= FALSE,
               textfont = list(color='black'), hoverinfo="skip" ) %>%
      layout(yaxis = axis_layout, xaxis = axis_layout,
             margin = list(b= 10 , t=5, l = 5, r = 0),
             font = font_plots) %>% # to get hover compare mode as default
      config(displayModeBar = FALSE, displaylogo = F)
  }
}

###############################################.
# Creating output plots for each domain of each profile
# Charts for Health and wellbeing profile
output$summ_hwb_beha <- renderPlotly({ plot_profile_snapshot("Behaviours")})
output$summ_hwb_socare <- renderPlotly({ plot_profile_snapshot("Social care & housing")})
output$summ_hwb_env <- renderPlotly({ plot_profile_snapshot("Environment")})
output$summ_hwb_lifexp <- renderPlotly({ plot_profile_snapshot("Life expectancy & mortality")})
output$summ_hwb_women <- renderPlotly({ plot_profile_snapshot("Women's & children's health")})
output$summ_hwb_imm <- renderPlotly({ plot_profile_snapshot("Immunisations & screening")})
output$summ_hwb_econ <- renderPlotly({ plot_profile_snapshot("Economy")})
output$summ_hwb_crime <- renderPlotly({ plot_profile_snapshot("Crime")})
output$summ_hwb_mh <- renderPlotly({ plot_profile_snapshot("Mental health")})
output$summ_hwb_injury <- renderPlotly({ plot_profile_snapshot("Ill health & injury")})
output$summ_hwb_educ <- renderPlotly({ plot_profile_snapshot("Education")})
# Charts for Children and young people profile
output$summ_cyp_active <- renderPlotly({ plot_profile_snapshot("Active")})
output$summ_cyp_health <- renderPlotly({ plot_profile_snapshot("Healthy")})
output$summ_cyp_safe <- renderPlotly({ plot_profile_snapshot("Safe")})
output$summ_cyp_includ <- renderPlotly({ plot_profile_snapshot("Included")})
output$summ_cyp_nurt <- renderPlotly({ plot_profile_snapshot("Nurtured")})
output$summ_cyp_achiev <- renderPlotly({ plot_profile_snapshot("Achieving")})
output$summ_cyp_respon <- renderPlotly({ plot_profile_snapshot("Responsible")})
# Charts for Alcohol profile
output$summ_alc_env <- renderPlotly({ plot_profile_snapshot("Environment")})
output$summ_alc_serv <- renderPlotly({ plot_profile_snapshot("Services")})
output$summ_alc_commsaf <- renderPlotly({ plot_profile_snapshot("Community safety")})
output$summ_alc_family <- renderPlotly({ plot_profile_snapshot("CAPSM/Families")})
output$summ_alc_preval <- renderPlotly({ plot_profile_snapshot("Prevalence")})
output$summ_alc_health <- renderPlotly({ plot_profile_snapshot("Health")})
# Charts for Drugs profile
output$summ_drg_env <- renderPlotly({ plot_profile_snapshot("Environment")})
output$summ_drg_serv <- renderPlotly({ plot_profile_snapshot("Services")})
output$summ_drg_commsaf <- renderPlotly({ plot_profile_snapshot("Community safety")})
output$summ_drg_family <- renderPlotly({ plot_profile_snapshot("CAPSM/Families")})
output$summ_drg_preval <- renderPlotly({ plot_profile_snapshot("Prevalence")})
output$summ_drg_health <- renderPlotly({ plot_profile_snapshot("Health")})
output$summ_drg_data <- renderPlotly({ plot_profile_snapshot("Data quality")})
# Charts for mental health profile
output$summ_men_fem <- renderPlotly({ plot_profile_snapshot("Female adult")})
output$summ_men_male <- renderPlotly({ plot_profile_snapshot("Male adult")})
output$summ_men_cyp <- renderPlotly({ plot_profile_snapshot("CYP Mental Health")})
# Charts for Tobacco profile
output$summ_tob_school <- renderPlotly({ plot_profile_snapshot("Smoking in school children")})
output$summ_tob_cess <- renderPlotly({ plot_profile_snapshot("Smoking cessation & smoking cessation products")})
output$summ_tob_attrib <- renderPlotly({ plot_profile_snapshot("Smoking attributable deaths & diseases")})
output$summ_tob_pregn <- renderPlotly({ plot_profile_snapshot("Smoking during and post pregnancy")})
output$summ_tob_preval <- renderPlotly({ plot_profile_snapshot("Adult prevalence")})
output$summ_tob_retail <- renderPlotly({ plot_profile_snapshot("Retailer information")})
# Charts for population profile
output$summ_pop_pop <- renderPlotly({ plot_profile_snapshot("Population")})


###############################################.        
#### Heatmap ----
###############################################.  
#####################.
#Heatmap plot
#Function to create ggplot, then used in renderPlot
plot_heat <- function(domain_plot){
  heat <- summary_data() %>% subset(domain == domain_plot) %>% droplevels() %>% 
    mutate(indicator = as.factor(indicator))
  # %>% 
  #   subset(year >= input$yearcomp_summary) #so it only shows years after baseline
  
  #If no data available for that period then plot message saying data is missing
  if (is.data.frame(heat) && nrow(heat) == 0)
  {
    plot_nodata()
  } else { #If data is available then plot it
    
    # Calculates number of different indicators and then multiplies by pixels per row
    # it needs the sum at the end as otherwise small domains plots will be too small
    #Obtaining number of indicators
    
    get_height_heat <- function() {
      no_ind <- unique(heat$indicator)
      height_plot <- length(no_ind) * 33 + 50
    }
    
    #Tooltip
    heat_tooltip <- paste0(heat$indicator, "<br>", heat$def_period, "<br>",
                           heat$type_definition, "<br>", round(heat$measure, 1))
    
    # Plotting data
    heat_p <- ggplot(heat, aes(x = year, y = indicator, fill = color, text = heat_tooltip)) +
      geom_tile(color = "black") +
      geom_text(aes(label = round(measure, 0)), size =3) +
      ylim(rev(levels(heat$indicator))) + #to order with A on top
      #Another step needed to make the palette of colours for the tile work
      scale_fill_manual(name = "Legend", labels = c("Significantly better", "Not significantly different", "Significantly worse", "Significance is not calculated"),
                        values = c(blue = "#4da6ff", gray = "gray88", red = "#ffa64d", white = "#ffffff")) +
      #Giving the right dimensions to the plot
      scale_x_continuous(position = "top", breaks=seq(from = min(summary_data()$year), 
                                                      to = 2018, by =1)) + #max(summary_data()$year)
      #Layout
      theme(axis.text.x = element_text(angle=90),
            axis.ticks.y=element_blank(), # taking out axis tick marks
            axis.title.x=element_blank(), #Taking out y axis title
            axis.title.y=element_blank(), #Taking out y axis title
            panel.background = element_blank(),#Blanking background
            legend.position="none", #taking out legend
            text = element_text(size=14) # changing font size
      )
    
    #Converting ggplot into a Plotly object
    ggplotly(heat_p, tooltip=c("text"), height= get_height_heat()) %>%
      # margins needed as long labels don't work well with Plotly
      layout(margin = list(l = 400, t = 50, b =0),
             xaxis = list(side = 'top', fixedrange=TRUE), yaxis= list(fixedrange=TRUE),
             font = font_plots) %>%
      config(displayModeBar = FALSE, displaylogo = F) # taking out plotly logo 
  }
}

###############################################.
# Creating output plots for each domain of each profile 
output$heat_hwb_beha <- renderPlotly({ plot_heat("Behaviours")})
output$heat_hwb_socare <- renderPlotly({ plot_heat("Social care & housing")})
output$heat_hwb_env <- renderPlotly({ plot_heat("Environment")})
output$heat_hwb_lifexp <- renderPlotly({ plot_heat("Life expectancy & mortality")})
output$heat_hwb_women <- renderPlotly({ plot_heat("Women's & children's health")})
output$heat_hwb_imm <- renderPlotly({ plot_heat("Immunisations & screening")})
output$heat_hwb_econ <- renderPlotly({ plot_heat("Economy")})
output$heat_hwb_crime <- renderPlotly({ plot_heat("Crime")})
output$heat_hwb_mh <- renderPlotly({ plot_heat("Mental health")})
output$heat_hwb_injury <- renderPlotly({ plot_heat("Ill health & injury")})
output$heat_hwb_educ <- renderPlotly({ plot_heat("Education")})
# Charts for Children and young people profile
output$heat_cyp_active <- renderPlotly({ plot_heat("Active")})
output$heat_cyp_health <- renderPlotly({ plot_heat("Healthy")})
output$heat_cyp_safe <- renderPlotly({ plot_heat("Safe")})
output$heat_cyp_includ <- renderPlotly({ plot_heat("Included")})
output$heat_cyp_nurt <- renderPlotly({ plot_heat("Nurtured")})
output$heat_cyp_achiev <- renderPlotly({ plot_heat("Achieving")})
output$heat_cyp_respon <- renderPlotly({ plot_heat("Responsible")})
# Charts for Alcohol profile
output$heat_alc_env <- renderPlotly({ plot_heat("Environment")})
output$heat_alc_serv <- renderPlotly({ plot_heat("Services")})
output$heat_alc_commsaf <- renderPlotly({ plot_heat("Community safety")})
output$heat_alc_family <- renderPlotly({ plot_heat("CAPSM/Families")})
output$heat_alc_preval <- renderPlotly({ plot_heat("Prevalence")})
output$heat_alc_health <- renderPlotly({ plot_heat("Health")})
# Charts for Drugs profile
output$heat_drg_env <- renderPlotly({ plot_heat("Environment")})
output$heat_drg_serv <- renderPlotly({ plot_heat("Services")})
output$heat_drg_commsaf <- renderPlotly({ plot_heat("Community safety")})
output$heat_drg_family <- renderPlotly({ plot_heat("CAPSM/Families")})
output$heat_drg_preval <- renderPlotly({ plot_heat("Prevalence")})
output$heat_drg_health <- renderPlotly({ plot_heat("Health")})
output$heat_drg_data <- renderPlotly({ plot_heat("Data quality")})
# Charts for mental health profile
output$heat_men_fem <- renderPlotly({ plot_heat("Female adult")})
output$heat_men_male <- renderPlotly({ plot_heat("Male adult")})
output$heat_men_cyp <- renderPlotly({ plot_heat("CYP Mental Health")})
# Charts for Tobacco profile
output$heat_tob_school <- renderPlotly({ plot_heat("Smoking in school children")})
output$heat_tob_cess <- renderPlotly({ plot_heat("Smoking cessation & smoking cessation products")})
output$heat_tob_attrib <- renderPlotly({ plot_heat("Smoking attributable deaths & diseases")})
output$heat_tob_pregn <- renderPlotly({ plot_heat("Smoking during and post pregnancy")})
output$heat_tob_preval <- renderPlotly({ plot_heat("Adult prevalence")})
output$heat_tob_retail <- renderPlotly({ plot_heat("Retailer information")})
# Charts for population profile
output$heat_pop_pop <- renderPlotly({ plot_heat("Population")})

##############################################.
## Spine/Barcode ----
###############################################.    
############################################.
## Reactive controls

# Reactive controls for domain depending on profile
output$topic_ui_spine <- renderUI({
  domain_list <- sort(profile_lookup$domain[profile_lookup$profile == input$profile_summary])
  selectInput("topic_spine", "Domain", choices = domain_list, selected='')
})

############################################.
## Spine Data
#Barcode all area data
spine_allareas <- reactive({
  
  optdata %>%
    group_by (indicator) %>%
    mutate(max_year = max(year))%>%
    subset (year == max_year &
              (substr(profile_domain1, 1, 3) == input$profile_summary |
                 substr(profile_domain2, 1, 3) == input$profile_summary) &
              (substr(profile_domain1, 5, nchar(as.vector(profile_domain1))) == input$topic_spine |
                 substr(profile_domain2, 5, nchar(as.vector(profile_domain2))) == input$topic_spine) &
              areatype  == input$geotype_summary &
              !(indicator %in% c("Mid-year population estimate - all ages", "Quit attempts")))
})

#Barcode data for the chosen area. Filtering based on user input values.
spine_chosenarea <- reactive({
  optdata %>%
    group_by (indicator) %>%
    mutate(max_year=max(year))%>%
    subset (year == max_year &
              areaname == input$geoname_summary &
              (substr(profile_domain1, 1, 3) == input$profile_summary |
                 substr(profile_domain2, 1, 3) == input$profile_summary) &
              (substr(profile_domain1, 5, nchar(as.vector(profile_domain1))) == input$topic_spine |
                 substr(profile_domain2, 5, nchar(as.vector(profile_domain2))) == input$topic_spine) &
              areatype  == input$geotype_summary &
              !(indicator %in% c("Mid-year population estimate - all ages", "Quit attempts"))) %>%
    select(c(indicator, measure, lowci, upci)) %>%
    rename(measure_chosen= measure, lowci_chosen=lowci, upci_chosen= upci) %>%
    droplevels()
})

#Select comparator based on years available for area selected.
spine_chosencomp <- reactive({
  
  optdata %>%
    group_by (indicator) %>%
    mutate(max_year=max(year))%>%
    subset (year==max_year &
              areaname == input$geocomp_spine &
              areatype %in% c("Health board", "Council area", "Scotland") &
              (substr(profile_domain1, 1, 3) == input$profile_summary |
                 substr(profile_domain2, 1, 3) == input$profile_summary) &
              (substr(profile_domain1, 5, nchar(as.vector(profile_domain1))) == input$topic_spine |
                 substr(profile_domain2, 5, nchar(as.vector(profile_domain2))) == input$topic_spine) &
              !(indicator %in% c("Mid-year population estimate - all ages", "Quit attempts"))) %>%
    select(c(indicator, measure)) %>%
    rename(measure_comp =measure) %>%
    droplevels()
  
})        

#####################.
# Creating Spine plot 

#Dynamically set height of bars
spine_plot_height<- function(){
  (nrow(spine_chosenarea())*50+70)
}

# Create barcode plot function
plot_spine <- function(){
  
  ind_count <- length(unique(spine_allareas()$indicator)) #facet_wrap requires how many chart rows to render
  
  #Merging comparator and chosen area
  spine <- merge(spine_allareas(), spine_chosencomp(), by=c("indicator"))
  spine <- left_join(spine, spine_chosenarea(), by=c("indicator"))
  
  #add variable denoting if sign diff between comparator
  spine<-spine %>%
    mutate(flag=case_when(
      interpret == "O" ~ 'No significance can be calculated',
      lowci_chosen<=measure_comp & upci_chosen>=measure_comp ~
        'Not different to comparator',
      lowci_chosen > measure_comp & interpret == "H" ~ 'Better than comparator',
      lowci_chosen > measure_comp & interpret == "L" ~ 'Worse than comparator',
      upci_chosen < measure_comp & interpret == "L" ~ 'Better than comparator',
      upci_chosen < measure_comp & interpret == "H" ~ 'Worse than comparator',
      TRUE ~ 'No significance can be calculated'))
  
  #Transposing data so that better is always to the right hand side of plot
  spine <- spine %>%
    mutate(comp = 1,
           all = measure/measure_comp,
           chosen = measure_chosen/measure_comp,
           all2=case_when(interpret=='L' & measure>measure_comp ~ -(all-1),
                          interpret=='L' & measure<=measure_comp ~ 
                            (1-all), TRUE ~ -(1-all)),
           chosen2=case_when(interpret=='L' & measure_chosen>measure_comp ~
                               -(chosen-1),
                             interpret=='L' & measure_chosen<=measure_comp ~
                               (1-chosen), TRUE ~ -(1-chosen))) %>%
    mutate(comp=0)
  
  #define x axis value to assign as intercept for significance
  minx <- min(spine$all2)-0.1
  
  #generate labels for comp and chosen bars
  data_labels <- spine %>%
    select(indicator, measure_comp, measure_chosen, chosen2, comp, type_definition,trend_axis, code) %>%
    group_by(indicator, type_definition, trend_axis, code) %>%
    summarise(comp_lab=measure_comp[1], chosen_lab=measure_chosen[1],
              x_chosen=chosen2[1], x_comp = comp[1])%>%
    droplevels()
  
  spine_data <- bind_rows(spine %>% mutate(y=0),
                          spine %>% mutate(y=1))
  
  #Chart title text & subtitle
  areatype_name <- input$geotype_summary
  chosenarea_name <- input$geoname_summary
  comparea_name <- input$geocomp_spine
  
  #Create colour scale for lines & legend key.
  colour_lines <-  scale_colour_manual(" ",values= setNames(c("black","#009999","#990000"), c(areatype_name, chosenarea_name, comparea_name)))
  
  #Create fill colour scheme for significance.
  fill_df <- data.frame(flag = c ('No significance can be calculated','Not different to comparator','Better than comparator','Worse than comparator'),stringsAsFactors = TRUE)
  fillcolours <- c("#4da6ff","white","grey88","#ffa64d")
  names(fillcolours) <- levels(fill_df$flag)
  colour_points <- scale_fill_manual(name = "flag",values = fillcolours)
  
  ggplot(spine_data, aes(x = all2, y = y, group=code, colour=areatype))+
    geom_line(alpha=0.6)+
    geom_line(aes(x = chosen2, colour=chosenarea_name), size=1) + #line for picked area
    geom_line(aes(x = comp, colour=comparea_name), size=1) + #line for comparator
    geom_point(aes(fill=flag, shape=flag, x= minx, y=0.5),size=8,colour="grey40") +
    geom_text(data=data_labels, aes(x=x_chosen,y=1.5,label=round(chosen_lab,digits=0)),
              check_overlap = TRUE,size=5,colour = "#009999",vjust=1, hjust=0) + #label for chosenarea - teal
    geom_text(data=data_labels, aes(x=x_comp,y=-0.6,label=round(comp_lab,digits=0)),
              check_overlap = TRUE,size=5,colour = "#990000",vjust=0, hjust=1) + #label for comparator - red
    xlab("Worse  <-------------------->  Better")+
    scale_x_discrete(position = "top") +
    colour_lines+
    colour_points+
    scale_shape_manual(values=c(22,22,22,22)) +
    guides(color = "none",fill = "none", shape="none")+
    theme(
      axis.title.y=element_blank(), #Taking out y axis title
      axis.title.x=element_text(size=12, colour ='#555555',hjust=0.6), #x axis title contains better/worse
      text = element_text(family="Arial"),
      legend.direction = "vertical",
      legend.justification = "top",
      legend.key = element_rect(colour = NA, fill = NA),
      legend.text = element_text(size=14,colour ='#555555', hjust=-1),
      legend.title = element_blank(),
      panel.grid = element_blank(),
      axis.text=element_blank(), # taking out x axis labels
      axis.ticks=element_blank(),
      panel.background = element_rect(fill="grey95"))+ # azure background
    facet_wrap(~indicator + type_definition + trend_axis, # fields to facet on
               nrow=ind_count,ncol=1,                   # how many rows  and colums to include
               scales="fixed",                          # fix scale so can compare % diff down column
               labeller = label_wrap_gen(multi_line = TRUE), # allow labels to wrap 
               strip.position="left")+                    # swap strip text to appear on left
    theme(strip.text.y = element_text(size=14,colour ='#555555', angle = 180, hjust = 0), #adjust font & rotation
          strip.switch.pad.wrap = unit(-1, "cm"), # reducing white space between strip panel & chart
          strip.placement = "outside",  # formatting again to try and limit white space
          strip.background = element_rect(fill="grey95"))           # change colour of facet wrap background
}        



# Render spine plot - inlucde if statement for situations where no data available
output$spine_plot <- renderPlot({
  if(is.data.frame(spine_chosenarea()) && nrow(spine_chosenarea())==0)
  {plot_nodata_gg()}
  else {plot_spine()}
})

#Create text output for responsive plot legend
#legend - selected area - green
output$ui_spine_legend_selected <- renderUI({
  img(src='spine_legend_selected.jpg', height=18, 
      style="padding-right: 2px; vertical-align:middle",
      paste0(input$geoname_summary))
}) 

#legend - comparator - pink
output$ui_spine_legend_comparator <- renderUI({
  img(src='spine_legend_comparator.jpg', height=18, 
      style="padding-right: 2px; vertical-align:middle",
      paste(input$geocomp_spine))
})

#legend - area type - grey bars
output$ui_spine_legend_areatype <- renderUI({
  
  # So it reads ok when user selects Scotland level
  text_legend <- case_when(input$geotype_summary == "Scotland" ~ "Scotland",
                           TRUE ~ paste0("Other ", tolower(input$geotype_summary), "s"))
  
  img(src='bar_legend_areatype.jpg', height=18, 
      style="padding-right: 2px; vertical-align:middle", text_legend)
}) 

#####################.
# Downloading spine plot and data  
# Define data file to download

spine_csv <- reactive({
  
  #Merging comparator and chosen area
  spine <- merge(spine_allareas(), spine_chosencomp(), by=c("indicator"))
  spine <- left_join(spine, spine_chosenarea(), by=c("indicator"))
  spine <- bind_cols(spine %>% mutate(topic=input$topic_spine))
  spine <- bind_cols(spine %>% mutate(comparator=input$geocomp_spine))
  
  spine<-spine %>%
    mutate(flag=case_when(
      interpret == "O" ~'NA',
      lowci_chosen<= measure_comp & upci_chosen>=measure_comp ~ 'NS',
      lowci_chosen > measure_comp & interpret == "H" ~ 'Better',
      lowci_chosen > measure_comp & interpret == "L" ~ 'Worse',
      upci_chosen < measure_comp & interpret == "L" ~ 'Better',
      upci_chosen < measure_comp & interpret == "H" ~ 'Worse',
      TRUE ~ 'NA'))
  
  spine %>%
    select(c(indicator, areaname, areatype, def_period, numerator, measure,
             lowci, upci, type_definition, topic, measure_comp, comparator, flag)) %>%
    rename(lower_confidence_interval=lowci, upper_confidence_interval=upci,
           latest_period = def_period, definition = type_definition, comparator_measure = measure_comp, difference=flag)
  
})

# Downloading chart  
output$download_spineplot <- downloadHandler(
  filename = 'spine.png',
  content = function(file){
    ggsave(file, plot = plot_spine()
           +ggtitle(label=paste(names(profile_list[unname(profile_list) == input$profile_summary])," profile: ", input$topic_spine,sep=""),
                    subtitle =paste(input$geoname_summary," (",input$geotype_summary,") compared against ",input$geocomp_spine,sep="")),
           device = "png",width=15, limitsize=FALSE)
  })

##END