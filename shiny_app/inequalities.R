##to do

#paf chart needs to cope with indicators like bowel screening where rates highest in least deprived - should we allow neagtive perecntages


#Code for inequalities tab

  ###############################################.        
  #### Help modals ----
  ###############################################.   
  # Inequality help pop-up
  #links to SIMD, deprivation and inequality scotpho and measuring inequalities report
  observeEvent(input$help_simd, {
    showModal(modalDialog(
      title = "Interpretation and methodology",
      p("This tool shows how ",
        tags$a(href="https://www.scotpho.org.uk/life-circumstances/deprivation/key-points/", 
               "inequality and deprivation",  class="externallink"), 
      "affect different indicators of public health. We use different measures to 
      look at various aspects of inequality. The short description on the top of 
      the page provides an overview of these calculations. "),
      p("The 'Trend', 'Gap' and 'Risk' buttons show charts representing these 
        different measures of inequality." ),
      img(src="help_simd1.png"),
      #trend explanation
      h5("Trend", style = "font-weight: bold; color: black; margin-bottom: 0px;"),
      p("The ", tags$b("'Trend'"), " charts show how an indicator varies between the 
         most and least deprived areas over time using rates or percentages."),
      h5("Gap", style = "font-weight: bold; color: black; margin-bottom: 0px;"),
      #gap explanation
      p("The ", tags$b("'Gap'"), " charts show two common measures of inequality - 
        the Slope Index of Inequality (SII), which is used to calculate the absolute 
        inequality gap using a regression model and the Relative Index of Inequality (RII), 
        which is used to quantify the difference between the most deprived group 
        and the overall average value. This means that in some cases absolute 
        inequalities can get better, while relative inequalities get worse. "),
      #risk explanation
      h5("Risk", style = "font-weight: bold; color: black; margin-bottom: 0px;"),
      p("The ", tags$b("'Risk'"), " charts explore the potential for improvement 
        in the overall value of an indicator, if the value of the least deprived 
        group were experienced across the whole population. We use the Population 
        Attributable Risk (PAR) to calculate this. "),
      p("You can read more about the measures used and presented in the",
        tags$a(href="https://www.scotpho.org.uk/comparative-health/measuring-inequalities/", #to change
               "Measuring inequalities section",  class="externallink"), 
                "of the ScotPHO website."),
      #simd explanation
      p("To prepare the data shown in this tab we have divided the Scotland population 
        into five groups (quintiles) based on their deprivation level. This has been done using the ",
        tags$a(href="https://www2.gov.scot/simd",  "Scottish Index of Multiple Deprivation (SIMD).",
               class="externallink")),
      #quintile explanation
      p("You can access both local and Scotland quintile data. Each local quintile 
        represents roughly a fifth of the population of an area. They are better suited to 
        understand the inequality patterns in a local area. "),
      p("Scotland quintiles can be used to make comparisons between different 
        areas on an equal basis. It is important to note, however, that some 
        areas might not have all five quintiles represented and the populations of each 
        quintile can vary vastly between different areas."),
      p("We recommend using the local quintiles to understand inequalities in a specific area 
        and only using Scotland quintiles if you need to compare between areas. Please refer to the ", 
        tags$a(href="http://www.isdscotland.org/Products-and-Services/GPD-Support/Deprivation/SIMD/_docs/PHI-Deprivation-Guidance.pdf",
               "ISD guidance on deprivation analysis"), " for more information on this topic."),
        img(src="help_simd2.png"),
      size = "l", easyClose = TRUE, fade=FALSE, footer = modalButton("Close (Esc)")
    ))
  }) 

## Help on SII
  observeEvent(input$help_sii, {
    showModal(modalDialog(
      title = "Absolute inequality and the Slope Index of Inequality (SII)",
      p("The chart below shows how the absolute inequalty (the gap between the most and least disadvantaged groups) has changed over time."),
      #trend explanation
      p("The values in the chart are known as the ", tags$b("'Slope Index of Inequality (SII)'"), " for each year they are calcuated using a regression model of the rank 
         of the social variable (in this case the SIMD quintiles) and the selected indicator measure (e.g. rate of hospitalisations/deaths/etc)."),br(),
      p("The SII represents the inequality gap across the whole population between the most and the least disadvantaged. For example an SII
        of 127 for the asthma hospitalisation rate means that the difference between the most and the least disadvantaged groups is 127 
        hospitalisations per 100,000 population."),br(),
      p("Ideally there should be no difference between indicator values in the most and least deprived areas, in this situation the SII would be zero. The larger the SII the great the disparity between the most and least deprived areas.
        In this chart an SII that is decreasing over time would suggest that absolute inequality is reducing.
        It is possible for absolute inequality to reduce but relative inequality to increase which is important to consider trends in both the SII and RII."),
      p("You can read more about the measures used and presented in the",
        tags$a(href="https://www.scotpho.org.uk/comparative-health/measuring-inequalities/", #to change
               "Measuring inequalities section",  class="externallink"), 
        "of the ScotPHO website."),
      #simd explanation
      p("To prepare the data shown in this tab we have divided the Scotland population 
        into five groups (quintiles) based on their deprivation level. This has been done using the ",
        tags$a(href="https://www2.gov.scot/simd",  "Scottish Index of Multiple Deprivation (SIMD).",
               class="externallink")),
      size = "l", easyClose = TRUE, fade=FALSE, footer = modalButton("Close (Esc)")
      ))
  }) 
  
  ## Help on RII
    observeEvent(input$help_rii, {
    showModal(modalDialog(
      title = "Relative inequality and the Relative Index of Inequality (RII)",
      p("The chart below shows how relative inequalty (the gap between the least disadvantaged group and the average of all groups) has changed over time."),
      #trend explanation
      p("The values in the chart are known as the ", tags$b("'Relative Index of Inequality (RII)'"), "for each year this is calcuated using the a linear regression model of the social variable (in this case the SIMD quintiles) and the selected indicator measure (e.g. rate of hospitalisations/deaths/etc)."),br(),
      p("The RII represents the inequality gap between the most disadvantaged and the overall average. ScotPHO use a linear regressiong model and have converted the RII
        so that the value in the chart represents the percentage difference of the rate in the most deprived group relative to the rate in the overall population."),br(),
      p("Ideally there should be no gap meaning an RII of zero. RII typically range from between -2 and 2. The larger the RII the greater the inequity between the most deprived areas and the population average.
        It is possible for absolute inequality to reduce but relative inequality to increase which is important to consider trends in both the SII and RII."),
      p("You can read more about the measures used and presented in the",
        tags$a(href="https://www.scotpho.org.uk/comparative-health/measuring-inequalities/", #to change
               "Measuring inequalities section",  class="externallink"), 
        "of the ScotPHO website."),
      #simd explanation
      p("To prepare the data shown in this tab we have divided the Scotland population 
        into five groups (quintiles) based on their deprivation level. This has been done using the ",
        tags$a(href="https://www2.gov.scot/simd",  "Scottish Index of Multiple Deprivation (SIMD).",
               class="externallink")),
      size = "l", easyClose = TRUE, fade=FALSE, footer = modalButton("Close (Esc)")
    ))
  }) 
  
    ## Help on PAF chart 1 (snapshot year bar chart)
    observeEvent(input$help_paf, {
      showModal(modalDialog(
        title = "Estimating proportions attributable to inequality",
        p("The bar chart shows indicator values split by the deprivation quintiles. The area shaded in blue is the same across all 5 quintiles, it shows the rate observed in the least deprived quintile.
        The area shaded in orange represents the additional activity the 4 remaining quintiles have over and above that seen in the least deprived quintile."),
        p("Looking at data in this way illustrates the potential impact of removing deprivation (i.e. in the hypothetical situation that all deprivation quintiles expereinced the same rates)."),
        p("You can read more about the Population Attributable Risk in the",
          tags$a(href="https://www.scotpho.org.uk/comparative-health/measuring-inequalities/", #to change
                 "Measuring inequalities section",  class="externallink"),"of the ScotPHO website."),
        #simd explanation
        p("To prepare the data shown in this tab we have divided the Scotland population 
        into five groups (quintiles) based on their deprivation level. This has been done using the ",
          tags$a(href="https://www2.gov.scot/simd",  "Scottish Index of Multiple Deprivation (SIMD).",
                 class="externallink")),
        size = "l", easyClose = TRUE, fade=FALSE, footer = modalButton("Close (Esc)")
      ))
    }) 
    
    ## Help on PAF chart 2 (timeseries trend line chart)
    observeEvent(input$help_paf2, {
      showModal(modalDialog(
        title = "The Population Attributable Risk (PAR)",
        p("The line chart shows the ",tags$b("Population Attributable Risk (PAR)"), "also known as Population Attributable Fraction (PAF)."),
        #trend explanation
        p("The PAR is presented as a percentage, and describes by how much the overall rate of an indicator would increase or decrease if all areas were to expereince the rates observed in the most favourable area.
         The higher the PAR values the greater the impact of inequality on that indicator and the greater the potential fo rimpact if this inequailty could be removed."),br(),
        p("The PAF describes a hypothetical situation and makes the assumption that all of the association between the risk factor and indicator is causal. In reality there could a number of other factors influencing the trends observed."),
        p("You can read more about the PAR in the",
          tags$a(href="https://www.scotpho.org.uk/comparative-health/measuring-inequalities/", #to change
                 "Measuring inequalities section",  class="externallink"),"of the ScotPHO website."),
        #simd explanation
        p("To prepare the data shown in this tab we have divided the Scotland population 
        into five groups (quintiles) based on their deprivation level. This has been done using the ",
          tags$a(href="https://www2.gov.scot/simd",  "Scottish Index of Multiple Deprivation (SIMD).",
                 class="externallink")),
        size = "l", easyClose = TRUE, fade=FALSE, footer = modalButton("Close (Esc)")
      ))
    }) 
    
    
  ###############################################.
  ## Which measure option to pick help text ----
  ###############################################.
  
    output$inequality_options_help <- renderUI({
      tagList(
        h5("Which option should I look at?",style = "font-weight: bold; color: black; margin-bottom: 0px;"),
        p("There multiple different ways that health inequality can be measured. Understanding inequality has often requires looking at more than one measure. 
          This tool presents 3 different views of data to help users understand different aspects of inequality. 
          We recommend considering the data options in turn as each will build on the understanding of previous."),
        
        #trend explanation
        p("The ", tags$b("'Patterns of inequality'"), " option includes a bar chart showing how the measure for a particular indicator varies according to the relative deprivation of the area people live in.
          A line chart is also available which shows the same information over time. Together these charts illustrate how rates in the most and least deprived areas compare for a single year or over time and also whether
          there is a simple relationship between relative deprivation and a particular indicator."),
        
        #gap explanation
        p("The ", tags$b("'Inequality gap'"), " option includes charts displaying two commonly used measures of inequality - 
          the Slope Index of Inequality (SII), which represents the absolute 
          inequality gap using a regression model and the Relative Index of Inequality (RII), 
          which quantifies the difference between the most deprived group and the overall average value. 
          It is possible for absolute inequalities to reduce, while relative inequalities increase (or vice-versa) which is why it is helpful to consider both the SII and RII."),
        
        #PAR/risk explanation
        p("The ", tags$b("'Potential for impact'"), " option includes charts which indicate the potential for improvement for a given indicator.  
        The measure shown is a theoretical value known as the Population Attributable Risk (PAR). 
        Interpreting this measure requires an appreciation of whether higher indicator values are considered more desirable or less desirable.  
        For example higher values for deaths or hospital admissions would be considered less desirable but higher values for indicators like life expectancy or immunisation update would be more desirable. 
        The PAR produces a summary value that describes the percentage an indicator might reduce, or increase by, if the population as a whole experienced the same rate as that of the most desirable group.
        The PAR enables generation of summary statements such as 'hospitalisations would be 50% lower if the levels of the least deprived areas were experienced across the whole population' which can be powerful in demonstrating the impact that reducing or removing inequalities might be."),

        #Deprivation
        h5("How does ScotPHO define deprivation? ", style = "font-weight: bold; color: black; margin-bottom: 0px;"),
        #p("Indicators within the inequality module of the profiles tool show rates split by the ",
        p("This tool uses the ",
          tags$a(href="https://www.gov.scot/collections/scottish-index-of-multiple-deprivation-2020/", "Scottish Indicies of Multiple Deprivation (SIMD)",
                 class="externallink"),
          " to subdivide geographic areas into 5 groups (also know as quintiles) experiencing different degrees of relavtive deprivation. In this tool those living in quintile 1 tend to experience the most relative deprivation and those living in quintile 5 the least.
          However it is important to note that SIMD is not a perfect measure of deprivation, SIMD is an area-based measure of relative deprivation and individuals living within an area may experience different levels of deprivation."),
        
        #Further information about measuring inequality
        h5("Further information about measuring inequality ", style = "font-weight: bold; color: black; margin-bottom: 0px;"),
        p("Additional information about ",
          tags$a(href="http://www.healthscotland.scot/health-inequalities", "health inequalities",
                 class="externallink"),
          " and ",
          tags$a(href="https://www.scotpho.org.uk/comparative-health/measuring-inequalities/", "measuring health inequalities",
                 class="externallink"),
          " in Scotland."))
    })
  
    
    
  ###############################################.
  ## Indicator definitions ----
  ###############################################.
  #Subsetting by domain and profile. Profile is fiddly as vector uses abbreviations 
  # so needs to be converted to the names to match techdoc.
  output$defs_text_simd <- renderUI({
    
    defs_data <- techdoc %>% subset(indicator_name == input$indic_simd)
    
    HTML(paste(sprintf("<b><u>%s</b></u> <br> %s ", defs_data$indicator_name, 
                       defs_data$indicator_definition), collapse = "<br><br>"))
  })
  
  ###############################################.
  ## Reactive controls ----
  ###############################################.
  
  #Controls for chart. Dynamic selection of area depending on area type.
  output$geoname_ui_simd <- renderUI({
    
    list_areas <- sort(as.vector(subset(geo_lookup$areaname, geo_lookup$areatype == input$geotype_simd)))
    
    selectInput("geoname_simd", "Select the area", 
                choices = list_areas, selected = "Scotland")
  })
  
  #Dynamic selection of year depending on what years are available for each indicator.
  output$year_ui_simd <- renderUI({
    time_period <- sort(unique(depr_data$trend_axis[depr_data$indicator == input$indic_simd]))
    
    selectInput("year_simd", "Step 3 - Select a time period",
                choices = time_period, selected = last(time_period))
  })
  
  #Disabling quintile option for those created for HSC report
  # better approach will be list of these indicators with no scquintile
  # disable but also update so selection is local.!("sc_quin" %in% simd_quint_data()$quint_type)
  # Patients GP are only calculated for Scotland quintiles
  observeEvent(input$indic_simd, {
    if (input$indic_simd %in% ind_hsc_list) {
      disable("quint_type" )
      
      updateSelectizeInput(session, "quint_type", selected = "Local")
    } else if(input$indic_simd == "Patients per general practitioner") {
      disable("quint_type" )
      
      updateSelectizeInput(session, "quint_type", selected = "Scotland")
    } else if (input$geotype_simd == "Scotland") {
      disable("quint_type" )
    } else {
      enable("quint_type" )
    }
  })
  
  observeEvent(input$geotype_simd, {
    if (input$indic_simd %in% ind_hsc_list) {
      disable("quint_type" )
      
      updateSelectizeInput(session, "quint_type", selected = "Local")
    } else if(input$indic_simd == "Patients per general practitioner") {
      disable("quint_type" )
      
      updateSelectizeInput(session, "quint_type", selected = "Scotland")
    } else if (input$geotype_simd == "Scotland") {
      disable("quint_type" )
    } else {
      enable("quint_type" )
    }
  })
  
  ###############################################.
  ## Reactive data ----
  ###############################################.
  
  #Filtering data based on what quintile type user selected
  simd_quint_data <- reactive({
    
    if (input$quint_type == "Scotland" | input$geotype_simd == "Scotland") {
      quint_chosen <- "sc_quin"
    } else if (input$quint_type == "Local" & input$geotype_simd != "Scotland") {
      quint_chosen <- c("hb_quin", "ca_quin")
    }
 
    depr_data %>% subset(quint_type %in% quint_chosen) %>% droplevels()
  
  })
  
  #reactive dataset for the simd bar plot
  simd_bar_data <- reactive({
    simd_quint_data() %>%
      subset(code %in% as.character(geo_lookup$code[geo_lookup$areaname == input$geoname_simd]) &
               areatype == input$geotype_simd &
               indicator == input$indic_simd & trend_axis == input$year_simd) %>%
      mutate(average = measure[quintile == "Total"],
             lowci_diff = measure - lowci,
             upci_diff = upci - measure) %>% 
      filter(quintile != "Total") %>%
      droplevels()
  })
  
  #reactive dataset for the simd trend plot
  simd_trend_data <- reactive({
    simd_quint_data() %>%
      subset(code %in% as.character(geo_lookup$code[geo_lookup$areaname == input$geoname_simd]) &
               areatype == input$geotype_simd &
               indicator == input$indic_simd) %>%
      arrange(quintile) %>% #this is needed to make palette assignments work well
      mutate(across(sii:abs_range,abs)) %>% #absolute values for sii, rii and paf (plus confidence intervals) so charts and dynamic summary text reads better
      mutate(qmax_statement=case_when(qmax=="1 - most deprived" ~ "The most deprived areas",
                               qmax=="5 - least deprived" ~ "The least deprived areas",
                               qmax=="2" ~ "More deprived areas",
                               qmax=="4" ~ "Less deprived areas",
                               qmax=="3" ~ "No particular areas of deprivation",TRUE ~ " "),
      qmin_statement=case_when(qmin=="1 - most deprived" ~ "The most deprived areas",
                               qmin=="5 - least deprived" ~ "The least deprived areas",
                               qmin=="2" ~ "More deprived areas",
                               qmin=="4" ~ "Less deprived areas",
                               qmin=="3" ~ "No particular areas of deprivation",TRUE ~ " ")) %>%
      droplevels()
  })
  
  ###############################################.
  ## Dynamic summary text ----
  ## i.e the summary bullet points that appear above charts 
  ###############################################.
  # Title of summary box
  output$simd_nutshell_title <- renderText(paste0(input$indic_simd, ": ",
                                                  input$geoname_simd, " ", input$year_simd))
  
  output$simd_text <- renderUI({
    
    #Data used to generate dynamic text for summary box
    simd_text_data <- simd_trend_data() %>%
      filter(quintile == "Total") %>%
      #Statement #1 (states which quintile has the least desirable rate)
      #When interpret is H (higher rates better) tell me which quintile has lowest rate
      #When interpret is L (Lower rates better) tell me which quintile has highest rate
      mutate(statement1a = case_when(interpret=="H" ~ qmin_statement, interpret=="L" ~ qmax_statement, interpret=="O" ~ qmax_statement, TRUE ~ "N/A"),
             statement1b = case_when(interpret=="H" ~ "lowest", interpret=="L" ~ "highest", interpret=="O"~ "highest", TRUE ~ "N/A")) %>%
      #buidling components behind statement 2 around for 'gap' charts 
      # first what is max and min sii and rii for time period selected.  
      mutate(sii_y_start=sii[which.min(year)],
             sii_y_end=sii[which.max(year)],
             rii_y_start=rii[which.min(year)],
             rii_y_end=rii[which.max(year)]) %>%
      #has sii has increased or decreased
      mutate(sii_change=case_when(sii_y_start>sii_y_end ~"narrowed",
                                  sii_y_start<sii_y_end ~"widened",
                                  sii_y_start==sii_y_end~"remained unchanged", TRUE ~"other")) %>%
      #has rii has increased or decreased
      mutate(rii_change=case_when(rii_y_start>rii_y_end ~"narrowed",
                                  rii_y_start<rii_y_end ~"widened",
                                  rii_y_start==rii_y_end~"remained unchanged", TRUE ~"other")) %>%
      #statement 2 - what has happened with absolute and relative inequalities.
      mutate(statement2 = case_when(sii_change=="narrowed" & rii_change=="narrowed" ~ "both absolute and relative inequalities have reduced",
                                    sii_change=="widened" & rii_change=="widened" ~ "both absolute and relative inequalities have increased",
                                    sii_change=="remained unchanged" & rii_change=="remained unchanged" ~ "both absolute and relative inequalities have remained unchanged",
                                    sii_change=="narrowed" & rii_change=="widened" ~ "absolute inequalities have decreased but relative inequalities have increased",
                                    sii_change=="narrowed" & rii_change=="remained unchanged" ~ "absolute inequalities have decreased but relative inequalities have remained unchanged",
                                    sii_change=="widened" & rii_change=="narrowed" ~ "absolute inequalities have increased but relative inequalities have decreased",
                                    sii_change=="widened" & rii_change=="remained unchanged" ~ "absolute inequalities have increased but relative inequalities have remained unchanged", TRUE ~"N/A")) %>%
      #statement 4 - describe % by which rates would be higher or lower in most/least deprived quintile if rates were all the same.
      #logic changes depending on if higher rates are better or worse and whether higher or lower values are most desirable.
      mutate(statement4a = case_when(interpret=="H" & par_gradient =="negative" ~ "higher", 
                                   interpret=="H" & par_gradient =="positive" ~ "lower", 
                                   interpret=="L" & par_gradient =="negative" ~ "higher",
                                   interpret=="L" & par_gradient =="positive" ~ "lower", TRUE ~ "na"),
             statement4b = case_when(interpret=="H" & par_gradient =="negative" ~ "least deprived", 
                                     interpret=="H" & par_gradient =="positive" ~ "most deprived", 
                                     interpret=="L" & par_gradient =="negative" ~ "most deprived",
                                     interpret=="L" & par_gradient =="positive" ~ "least deprived", TRUE ~ "na")) %>%
      filter(trend_axis == input$year_simd)
    
    # Statement #3 (part one)- what percentage higher or lower are the levels are in most deprived quintile compared to average 
      more_less <- case_when(
      unique(simd_text_data$rii_gradient) =="positive" ~ "higher ",
      unique(simd_text_data$rii_gradient) == "negative" ~ "lower ",
      unique(simd_text_data$rii_gradient) == "zero" ~ "the same "
    )
    
    #Statement #3 (part two) - getting grammar of sentence correct
    than_as <- case_when(
      unique(simd_text_data$rii_gradient) != 0 ~ " than ",
      unique(simd_text_data$rii_gradient) == 0 ~ " as "
    )

    #To have dynamic text depending on if par is positive or negative
    par_most_least <- case_when(
      unique(simd_bar_data()$interpret) =="H" ~ "most deprived ",
      unique(simd_bar_data()$interpret) =="L" ~ "least deprived ",
      TRUE ~ "different "
    )
    
    #If no data can be calculated for sii, rii, par, just have a no data available message
    if (is.na(simd_text_data$rii_int)) {
      tags$ul( 
        #Link to user guide
        tags$li(class= "li-custom",
                p("Summary not available"))
      )
      
    } else if (!(is.na(simd_text_data$rii_int)) & is.na(simd_text_data$par)) { # andy pulfords hsc indicators no PAR calculated so no statement 4 available
      tags$ul( #if no data available for PAR only first two points
                #statement #1 : identifies which quintile has the least desirable rate (this statement changes according to 'interpret' field which flags if Higher or Lower rates are best)
                tags$li(class= "li-custom",
                        p(paste0(simd_text_data$statement1a," have the ",simd_text_data$statement1b," ",tolower(simd_text_data$label_ineq),". (see 'Patterns of inequality')"))),
                #statement #2 : whe absolute inequality (from sii) has increased or decreased over time
                tags$li(class= "li-custom",
                        p(paste0("Over time ", simd_text_data$statement2,". (see 'Inequality gap')"))),
                #statement #3 based on rii
                tags$li(class= "li-custom",
                        p(paste0("The most deprived areas have ", abs(round(unique(simd_bar_data()$rii_int), 0)),
                                 "% ", more_less, tolower(unique(simd_bar_data()$label_ineq)), than_as," the average. (see 'Inequality gap')" )))
      )
  
    } else { #if the data is available print the following messages
      tags$ul( 
        #statement #1 : identifies which quintile has the least desirable rate (this statement changes according to 'interpret' field which flags if Higher or Lower rates are best)
        tags$li(class= "li-custom",
                p(paste0(simd_text_data$statement1a," have the ",simd_text_data$statement1b," ",tolower(simd_text_data$label_ineq),". (see 'Patterns of inequality')"))),
        #statement #2 : whe absolute inequality (from sii) has increased or decreased over time
        tags$li(class= "li-custom",
                p(paste0("Over time ", simd_text_data$statement2,". (see 'Inequality gap')"))),
        #statement #3 based on rii
        tags$li(class= "li-custom",
                p(paste0("The most deprived areas have ", abs(round(unique(simd_bar_data()$rii_int), 0)),
                         "% ", more_less, tolower(unique(simd_bar_data()$label_ineq)), than_as," ",input$geoname_simd," as a whole. (see 'Inequality gap')" ))),
        #statement #4 based on PAR
        tags$li(class= "li-custom",
                (paste0(unique(simd_bar_data()$label_ineq)," across ",input$geoname_simd," would be ", abs(round(unique(simd_bar_data()$par), 0)),"% ", unique(simd_text_data$statement4a)," if the levels of the ",
                        unique(simd_text_data$statement4b)," areas were experienced across the whole population. (see 'Potential for impact')")))
      )
    }
  })
  
  ###############################################.
  ## Downloading data ----
  ###############################################.
  simd_csv <- reactive({
    simd_trend_data() %>% 
      arrange(year, code, quintile) %>% 
      select(c(indicator, code, quintile, def_period, numerator, measure, 
               lowci, upci, type_definition, rii_int,	lowci_rii_int,	upci_rii_int,	
               sii,	lowci_sii,	upci_sii, par)) %>% 
      #Converting into NA for all but total quintile
      mutate(rii_int = case_when(quintile != "Total" ~ NA_real_,
                                 TRUE ~ rii_int),
             lowci_rii_int = case_when(quintile != "Total" ~ NA_real_,
                                       TRUE ~ lowci_rii_int),
             upci_rii_int = case_when(quintile != "Total" ~ NA_real_,
                                      TRUE ~ upci_rii_int),
             sii = case_when(quintile != "Total" ~ NA_real_,
                             TRUE ~ sii),
             upci_sii = case_when(quintile != "Total" ~ NA_real_,
                                  TRUE ~ upci_sii),
             lowci_sii = case_when(quintile != "Total" ~ NA_real_,
                                   TRUE ~ lowci_sii),
             par = case_when(quintile != "Total" ~ NA_real_,
                             TRUE ~ par)) %>% 
      rename(geography_code = code, indicator_measure = measure,
             lower_confidence_interval=lowci, upper_confidence_interval=upci,
             period = def_period, definition = type_definition, relative_inequality_gap = rii_int,
             lower_confidence_interval_relative_ineq = lowci_rii_int, upper_confidence_interval_relative_ineq = upci_rii_int,
             absolute_inequality_gap = sii, lower_confidence_interval_absolute_ineq = lowci_sii,
             upper_confidence_interval_absolute_ineq = upci_sii,
             population_attributable_risk = par)
  })	
  
  
  output$download_simd <- downloadHandler(
    filename =  'deprivation_data.csv',content = function(file) {
      write.csv(simd_csv(), file, row.names=FALSE)
      })
  
  
  #####################.
  # Downloading charts image----
  output$report_simd <- downloadHandler(
    # file name
    filename <- 'plot.png',
    # content
    content = function(file){
      #select plots to download, depending on measure type selected
      export(p = if(input$measure_simd == "Patterns of inequality"){combined_trend()}
             else if(input$measure_simd == "Inequality gap"){combined_siirii()}
             else if(input$measure_simd == "Potential for improvement"){combined_risk()}, file = 'tempPlot.png')
      # hand over the file
      file.copy('tempPlot.png',file)
    }
  )
  
  ###############################################.
  ## Plots logic----
  ## Plots in inequalities tab are set up as reactive elements which are then rendered in separate command
  ## Pressing 'Save chart' option within dashboard will trigger formation of a subplot containing both 2 charts on dashboard into one image
  ## Combined into a subplot also allows exporting of charts together with legend and titles
  ###############################################.

  ###############################################.
  ## Plots: Trend ----
  ###############################################.
  
  # Title for barplot from trend (appears within dashboard)
  output$simd_barplot_title <- renderUI({
    p(tags$b(paste0(input$indic_simd," by deprivation group for ", input$year_simd)))
  })
  
  # Chart 1. barplot ----
 
   ineq_chart_1 <- reactive({

    #If no data available for that period then plot message saying data is missing
    if (is.data.frame(simd_bar_data()) && nrow(simd_bar_data()) == 0)
    {
      plot_nodata()
    }
    else { #If data is available plot it
      
      #Text for tooltip
      if (input$ci_simd == FALSE) {  

        tooltip_simd <- paste0("Population living within SIMD quintile ", simd_bar_data()$quintile, "<br>",
                               simd_bar_data()$measure,"<br>",
                               simd_bar_data()$type_definition, "<br>",
                               simd_bar_data()$trend_axis)
      } else { 
        tooltip_simd <- paste0("Population living within SIMD quintile ", simd_bar_data()$quintile, "<br>",
                                 simd_bar_data()$trend_axis, ": ", simd_bar_data()$measure, "<br>",
                                 "95% confidence interval: ", simd_bar_data()$lowci, "-", simd_bar_data()$upci, 
                                 "<br>", simd_bar_data()$type_definition)
      }
      
      #Palette for plot 
      pal_simd_bar <- case_when(simd_bar_data()$quintile == "1 - most deprived" ~ '#022031', 
                                simd_bar_data()$quintile == "2" ~ '#313695', 
                                simd_bar_data()$quintile == "3" ~ '#4575b4', 
                                simd_bar_data()$quintile == "4" ~ '#74add1', 
                                simd_bar_data()$quintile == "5 - least deprived" ~ '#abd9e9')
      
      #Modifying standard layout
      yaxis_plots[["title"]] <- unique(simd_bar_data()$type_definition)
      xaxis_plots[["tickangle"]] <- -45
      
      #Creating plot    
      p <- plot_ly(data=simd_bar_data(), x=~quintile,
                   text=tooltip_simd, textposition="none",hoverinfo="text") %>%
        #Comparator line
        add_trace(y = ~average, name = "Average", type = 'scatter', mode = 'lines',
                  line = list(color = '#FF0000'), hoverinfo="skip") %>% 
        layout(bargap = 0.1, margin=list(b = 140), #to avoid labels getting cut out
               #showlegend = FALSE,
               font = font_plots, yaxis = yaxis_plots, xaxis = xaxis_plots) %>%
        config(displayModeBar = F, displaylogo = F, editable =F) # taking out toolbar
    }
    
    if (input$ci_simd == FALSE) {  
      #adding bar layer without confidence intervals
      p %>% add_bars(y = ~measure, color = ~ quintile, marker = list(color = pal_simd_bar), showlegend = TRUE)
    } else { 
      #adding bar layer with error bars
      p %>% add_bars(y = ~measure, color = ~ quintile, marker = list(color = pal_simd_bar), showlegend = TRUE,
                     error_y = list(type = "data",color='#000000',
                                    symmetric = FALSE, array = ~upci_diff, arrayminus = ~lowci_diff)) 
    }
    
  })
  
  # Chart 1 - bar plot (version to display on dashboard)
  output$simd_bar_plot <- renderPlotly({
    
    ineq_chart_1() %>%
      layout(showlegend = F)
    
  })
  

  # Chart 2. trend plot ----
  
  # Title (as appears on dashboard panel)
  output$simd_trendplot_title <- renderUI({
    p(tags$b(paste0(input$indic_simd," over time by deprivation group")))
  })


  ineq_chart_2 <- reactive({

    #If no data available for that period then plot message saying data is missing
    if (is.data.frame(simd_trend_data()) && nrow(simd_trend_data()) == 0)
    {
      plot_nodata()
    } else { #If there is data plot it
      #Text for tooltip
      
      if (input$ci_simd == FALSE) {  
        tooltip_simd <- paste0(simd_trend_data()$quintile, "<br>",
                               simd_trend_data()$trend_axis, ": ", simd_trend_data()$measure,
                               "<br>", simd_trend_data()$type_definition)
      } else { 
        tooltip_simd <- paste0(simd_trend_data()$quintile, "<br>",
                               simd_trend_data()$trend_axis, ": ", 
                               simd_trend_data()$measure, "<br>",
                               "95% confidence interval: ",
                               simd_trend_data()$lowci, "-", simd_trend_data()$upci,
                               "<br>", simd_trend_data()$type_definition)
      }
      
      #Palette for plot 
      pal_simd_trend <- case_when(simd_trend_data()$quintile == "1 - most deprived" ~ '#022031', 
                                  simd_trend_data()$quintile == "2" ~ '#313695', 
                                  simd_trend_data()$quintile == "3" ~ '#4575b4', 
                                  simd_trend_data()$quintile == "4" ~ '#74add1', 
                                  simd_trend_data()$quintile == "5 - least deprived" ~ '#abd9e9',
                                  simd_trend_data()$quintile == "Total" ~ '#FF0000')
      
      #Creating plot
      trend_simd_plot <- plot_ly(data=simd_trend_data(), x=~trend_axis, 
                                 text=tooltip_simd, hoverinfo="text") %>%
        add_lines(y = ~measure, name = "", type = 'scatter', 
                  mode = 'lines', color = ~quintile, colors = pal_simd_trend, showlegend = FALSE) 
      
      #Adding confidence intervals depending on user input
      if (input$ci_simd == TRUE) {
        trend_simd_plot <- trend_simd_plot %>% 
          add_ribbons(data = simd_trend_data(), ymin = ~lowci, ymax = ~upci, showlegend = FALSE,
                      opacity = 0.2, color = ~quintile) 
        
      } else if (input$ci_simd == FALSE) {
        trend_simd_plot <- trend_simd_plot
      }
      
      #Modifying standard layout
      yaxis_plots[["title"]] <- unique(simd_bar_data()$type_definition)
      xaxis_plots[["tickangle"]] <- ifelse(max(nchar(as.character(simd_trend_data()$trend_axis)))>7, -45, 0)
      xaxis_plots[["dtick"]] <- ifelse(length(unique(simd_trend_data()$trend_axis)) >=10, 3, 1)
      
      #Layout
      trend_simd_plot %>%           
        layout(margin = list(b = 140), #to avoid labels getting cut out
               yaxis = yaxis_plots, xaxis = xaxis_plots, font = font_plots) %>%
        
        config(displayModeBar = FALSE, displaylogo = F, editable =F) # taking out toolbar
    }
  })
  
  
  # Chart 2 - trend plot (command to displaying on the dashboard)
  output$simd_trend_plot <- renderPlotly({
    
    ineq_chart_2() 
    
  })
  

  # Combined trend charts ----

  combined_trend <- reactive({
    
    chart_1 <-  ineq_chart_1 () %>%
      layout(annotations = list(
        xanchor = "left",
        yanchor = "top",
        x = 0 ,
        y = 1.05, 
        text = paste0(input$indic_simd," by deprivation group for ", input$year_simd), 
        showarrow = F, 
        align = "left",
        font=list(size=13,face="bold"),
        xref='paper', 
        yref='paper'),
        yaxis = list(title = first(simd_trend_data()$type_definition)))
    
    
    chart_2 <- ineq_chart_2() %>%
      layout(annotations = list(
        xanchor = "left",
        yanchor = "top",
        align = "left",
        x = 0 ,
        y = 1.05, 
        text = paste0(input$indic_simd," over time by deprivation group"), 
        font=list(size=13,face="bold"),
        showarrow = F, 
        xref='paper', 
        yref='paper'),
        yaxis = list(title = first(simd_trend_data()$type_definition)))
    
    
    final <- subplot(chart_1, chart_2, margin = 0.07, titleY = TRUE) %>%
      layout(width = 1200, height = 600,
        legend = list(orientation = "h",
                      xanchor = "center", 
                      x = 0.5,
                      y = -0.3))
    
  })
  

  
###############################################.
## Plots: RII/SII ----
###############################################.
  
  #text for title sii (as appears on dashboard)
  output$title_sii <- renderUI({
    div(p(tags$b("Inequalities over time: absolute differences")),
        p(paste0("The chart below shows the difference between most and least deprived areas 
                 (expressed as ", tolower(unique(simd_trend_data()$type_definition)), ")")),
        br(),
        p("An increasing trend suggests the gap between the most and least deprived areas is growing."))
  })
  

  # Chart 3.SII plot ----
  ineq_chart_3 <- reactive({

    simd_index <- simd_trend_data() %>% filter((quintile == "Total"))
    
    #If no SII for that period then plot message saying data is missing
    if (is.na(simd_index$sii))
    {
      plot_nodata()
    } else { #If data is available plot it
    
    # #Text for tooltips
      if (input$ci_simd == FALSE) {  
        tooltip_sii <- paste0("Difference between most and least deprived areas,","<br>",
                              "also known as Slope Index of Inequality (SII)", "<br>",
          simd_index$trend_axis, ": ", abs(simd_index$sii), "<br>", 
                               simd_index$type_definition)
      } else { 
        tooltip_sii <- paste0(simd_index$trend_axis, ": ", simd_index$sii, "<br>",
                                "95% confidence interval: ",
                                simd_index$lowci_sii, "-", simd_index$upci_sii,
                                "<br>", simd_index$type_definition, "<br>", 
                              "Also known as Slope Index of Inequality")
      }

    
    #Modifying standard layout
    yaxis_plots[["title"]] <- paste0(unique(simd_index$type_definition))
    xaxis_plots[["autotick"]] <- F
    xaxis_plots[["tickangle"]] <- ifelse(max(nchar(as.character(simd_index$trend_axis)))>7, -45, 0)
    xaxis_plots[["dtick"]] <- ifelse(length(unique(simd_index$trend_axis)) >=10, 3, 1)
    
    
    #Create plot SII
    sii_plot <- plot_ly(data=simd_index, x=~trend_axis,
                        text=tooltip_sii, hoverinfo="text",height = 500) %>%
      add_lines(y = ~sii, name = "Absolute inequality (SII)", type = 'scatter', mode = 'lines',
                line = list(color = '#74add1'))  %>%
      #Layout
      layout(yaxis= yaxis_plots, showlegend = FALSE,
             margin = list(b = 140), #to avoid labels getting cut out
             xaxis = xaxis_plots, font = font_plots) %>% 
      config(displayModeBar = FALSE, displaylogo = F,  editable =F) # taking out toolbar
    
    #Adding confidence intervals depending on user input
    if (input$ci_simd == TRUE) {
      sii_plot <- sii_plot %>% 
        add_ribbons(data = simd_index, ymin = ~lowci_sii, ymax = ~upci_sii, showlegend = F,
                    opacity = 0.2, color = I("#74add1")) 
      
    } else if (input$ci_simd == FALSE) {
      sii_plot <- sii_plot
    }
    
    }
  })
  
  # ssi plot (chart 3) to display on dashboard
  output$simd_sii_plot <- renderPlotly({
    
    ineq_chart_3() 
    
  })
  
  
  #  Chart 4.RII plot ----
  
  # RII chart title for dashboard panel
  output$title_rii <- renderUI({
    div(p(tags$b("Inequalities over time: relative differences")),
        p(paste0("The chart below shows the differences between the least deprived area
            and the overall average for ",input$geoname_simd," (expressed as a percentage).")),
        br(),
        p("An increasing trend suggests that the gap between the least deprived areas and the average is growing."))
  })
  
  
  # rri plot
  ineq_chart_4 <- reactive({

    simd_index <- simd_trend_data() %>% filter((quintile == "Total"))
    
    #If no SII for that period then plot message saying data is missing
    if (is.na(simd_index$sii))
    {
      plot_nodata()
    } else { #If data is available plot it
      
      # #Text for tooltips
      if (input$ci_simd == FALSE) {  
        tooltip_rii <- c(paste0(simd_index$trend_axis, ": ", simd_index$rii_int, 
                                "% difference with average","<br>", 
                                "Also known as Relative Index of Inequality"))
      } else { 
        tooltip_rii <- c(paste0(simd_index$trend_axis, ": ", simd_index$rii_int, 
                                "% difference with average", "<br>",
                                "95% confidence interval: ",
                                simd_index$lowci_rii_int, "-", simd_index$upci_rii_int, "<br>", 
                                "Also known as Relative Index of Inequality"))
      }
      
     # To have dynamic rii chart axis title depending on if rii is positive or negative
      more_less_axis <- case_when(
        unique(simd_index$rii_gradient) =="positive" ~ "more ",
        unique(simd_index$rii_gradient) == "negative" ~ "less ",
        unique(simd_index$rii_gradient) == "zero" ~ "more/less "
      )
      
      yaxis_title <- paste0("Percentage ",more_less_axis," than average")
      
      #Modifying standard layout
      yaxis_plots[["title"]] <- yaxis_title 
      xaxis_plots[["autotick"]] <- F
      xaxis_plots[["tickangle"]] <- ifelse(max(nchar(as.character(simd_index$trend_axis)))>7, -45, 0)
      xaxis_plots[["dtick"]] <- ifelse(length(unique(simd_index$trend_axis)) >=10, 3, 1)
      
      #Create plot RII
      rii_plot <- plot_ly(data=simd_index, x=~trend_axis,
                          text=tooltip_rii, hoverinfo="text",height = 500) %>%
        add_lines(y = ~rii_int, name = "Relative gap", type = 'scatter', mode = 'lines',
                  line = list(color = '#313695')) %>% 
        #Layout
        layout(yaxis= yaxis_plots, showlegend = FALSE,
               margin = list(b = 140), #to avoid labels getting cut out
               xaxis = xaxis_plots, font = font_plots) %>% 
        config(displayModeBar = FALSE, displaylogo = F, editable =F) # taking out toolbar
      
      #Adding confidence intervals depending on user input
      if (input$ci_simd == TRUE) {
        rii_plot <- rii_plot %>% 
          add_ribbons(data = simd_index, ymin = ~lowci_rii_int, ymax = ~upci_rii_int, showlegend = F,
                      opacity = 0.2, color = I("#313695")) 
        
      } else if (input$ci_simd == FALSE) {
        rii_plot <- rii_plot
      }

    }
  })
  
  # rri plot (to display on dashboard)
  output$simd_rii_plot <- renderPlotly({
  
    ineq_chart_4() 
    
  })
  
  
# Combined SII/RII plot ----

  combined_siirii <- reactive({
  
    chart_3 <-  ineq_chart_3 () %>%
      layout(annotations = list(
        xanchor = "left",
        yanchor = "top",
        x = 0 ,
        y = 1.05, 
        text = paste0("Absolute differences between the most and least deprived areas\nexpressed as ", tolower(unique(simd_trend_data()$type_definition))),
        showarrow = F, 
        align = "left",
        xref='paper', 
        yref='paper',
        font=list(size=13,face="bold")),
        xaxis = list(autorange = TRUE),
        yaxis = list(autorange = TRUE))
    
    chart_4 <-  ineq_chart_4 () %>%
      layout(showlegend = T,
             annotations = list(
               xanchor = "left",
               yanchor = "top",
               align = "left",
               x = 0 ,
               y = 1.05, 
               text = "Relative differences between the least deprived area\nand the overall average for the area.",
               showarrow = F, 
               xref='paper', 
               yref='paper',
               font=list(size=13,face="bold")),
             xaxis = list(autorange = TRUE),
             yaxis = list(autorange = TRUE),
             yaxis = list(title = "% more/less than average"))
    
    final <- subplot(chart_3, chart_4, margin = 0.07, titleY = TRUE) %>%
      layout(legend = list(orientation = "h",
                           xanchor = "center", 
                           x = 0.5,
                           y = -0.2),
             width = 1200, height = 600)
  })
  
  
  ###############################################.
  ## Plots: PAR ----
  ###############################################.
  
  # Chart 5. Bar plot for PAR ----

  #Title (as displayed within dashboard panel)
  output$simd_par_barplot_title <- renderUI({
    div(p(tags$b(paste0("Attributable to inequality, ", input$year_simd))),
        p("What percentage of ", tolower(input$indic_simd), " can be attributed to socioeconomic inequalities."))
  })
  
  #Bar plot for PAR
  ineq_chart_5 <- reactive({
    
    #If no PAR for that period then plot message saying data is missing
    if (is.na(simd_bar_data()$par))
    {
      plot_nodata()
      
    } else { #If data is available plot it
    
    #preparing data needed, creates two dummy variables for stacked bar chart
    simd_parbar_data <- simd_bar_data() %>%
      mutate(baseline = measure[quintile == "5 - least deprived"],
             diff_baseline = measure - measure[quintile == "5 - least deprived"]) %>% 
      droplevels()
    
    tooltip_parbar <- paste0(simd_parbar_data$trend_axis, "<br>",
                            "Total: ", simd_parbar_data$measure, "<br>",
                            "Attributable: ", simd_parbar_data$diff_baseline, "<br>",
                            simd_parbar_data$type_definition)
    
    
    #Modifying standard layout
    yaxis_plots[["title"]] <- unique(simd_parbar_data$type_definition)
    xaxis_plots[["tickangle"]] <- -45
    
    
    par_bar_plot <- plot_ly(data = simd_parbar_data, x = ~quintile, 
                    text=tooltip_parbar,textposition="none", hoverinfo="text") %>%
      add_bars(y = ~baseline, name= "baseline", marker = list(color = "#4da6ff"), showlegend = TRUE) %>%   
      add_bars(y = ~diff_baseline, name = "Attributable to deprivation", marker = list(color = "#ffa64d"), showlegend = TRUE) %>% 
      layout(bargap = 0.1, barmode = 'stack', showlegend = T, 
             legend = list(x = 0.9, y = 0.9),
             margin = list(b = 140), #to avoid labels getting cut out
             yaxis = yaxis_plots, 
             xaxis = xaxis_plots, font = font_plots) %>%
      config(displayModeBar = FALSE, displaylogo = F, editable =F) # taking out toolbar
    
    }
  })
  
  # bar plot for PAR (for displaying on the dashboard)
  output$simd_par_barplot <- renderPlotly({
      ineq_chart_5()  %>%
      layout(showlegend = F)
  })
  
  # Chart 6. Line plot for PAR ----
  
  # Title (as appears in dashboard)
  output$simd_par_trendplot_title <- renderUI({
    div(p(tags$b("Potential for improvement in ", tolower(input$indic_simd))),
    p(" If the levels of the least deprived area were experienced across the whole population."))
  })
  
  ineq_chart_6 <- reactive({ 
    
    #preparing data needed
    simd_partrend_data <- simd_quint_data() %>%
      subset(code == as.character(geo_lookup$code[geo_lookup$areaname == input$geoname_simd]) &
               indicator == input$indic_simd & quintile == "Total") %>% 
      droplevels()
    
    #If no SII for that period then plot message saying data is missing
    if (is.na(simd_partrend_data$par))
    {
      plot_nodata()
    } else { #If data is available plot it
      
    #Tooltip
    tooltip_partrend <- paste0(simd_partrend_data$trend_axis, "<br>",
                                 simd_partrend_data$par, "%")
      
    #Modifying standard layout
 
    yaxis_plots[["title"]] <- "Percentage attributable to deprivation"
    xaxis_plots[["dtick"]] <- ifelse(length(unique(simd_partrend_data$trend_axis)) >=10, 3, 1)
    xaxis_plots[["tickangle"]] <- ifelse(max(nchar(as.character(simd_partrend_data$trend_axis)))>7, -45, 0)
    
    
    par_trend_plot <- plot_ly(data=simd_partrend_data, x=~trend_axis,
                              text=tooltip_partrend, textposition="none",hoverinfo="text") %>%
      add_lines(y = ~abs(par), type = 'scatter', mode = 'lines', line = list(color = "#4575b4"),name = "% attributable to deprivation") %>%
      layout(yaxis = yaxis_plots, xaxis = xaxis_plots, font = font_plots,
             margin = list(b = 140)) %>% #to avoid labels getting cut out
      config(displayModeBar = FALSE, displaylogo = F, editable =F) # taking out toolbar
    
    }
  })
  
  # line plot PAR (to display on dashboard)
  output$simd_par_trendplot <- renderPlotly({
        ineq_chart_6() 
      })
  
  
  # Combine PAR charts ----
  
  combined_risk <- reactive({

    chart_5 <- ineq_chart_5() %>%
      layout(annotations = list(
        xanchor = "left",
        yanchor = "top",
        x = 0.07 ,
        y = 1.05, 
        align = "left",
        font=list(size=13,face="bold"),
        text = paste0("What percantage of ", tolower(input$indic_simd), "\n can be attributed to socioeconomic inequalities\n, ",input$year_simd),
        yref='paper'),
        xaxis = list(autorange = TRUE),
        yaxis = list(autorange = TRUE))

    chart_6 <-  ineq_chart_6() %>%
      layout(annotations = list(
        xanchor = "left",
        yanchor = "top",
        x = 0.07 ,
        y = 1.05, 
        align = "left",
        font=list(size=13,face="bold"),
        text = paste0("How much ",first(simd_bar_data()$indicator)," could be reduced if the \nlevels of the least deprived area were experienced across\n the whole population."),
        yref='paper'),
        xaxis = list(autorange = TRUE),
        yaxis = list(autorange = TRUE))
    
      final <- subplot(chart_5, chart_6, margin = 0.07, titleY = TRUE) %>%
      layout(width = 1200, height = 600,
            legend = list(orientation = "h",
                           xanchor = "center", 
                           x = 0.5,
                           y = -0.3),
             margin = list(l = 50, r = 50, b = 100, t = 100, pad = 4))
  })
  
  
  ##Summary text object----
  ## Summary text an output object so that it can be shown/hidden depending on which conditional panel is being shown
  output$inequality_summary_text <- renderUI({
    div(class= "depr-text-box",
        div(class= "title", textOutput("simd_nutshell_title")),
        div(class= "content", htmlOutput("simd_text")))
  })
  
  
  ##Main panel objects show/hide----
  observeEvent(input$measure_simd,{
    
    if (input$measure_simd == "About these options") {
      #objects to hide when displaying meta data about 
      shinyjs::hide("inequality_summary_text") 
      shinyjs::hide("download_simd") 
      shinyjs::hide("report_simd")
    }
    
    if (input$measure_simd %in% c("Patterns of inequality", "Inequality gap", "Potential for improvement")) {
      #objects to show when conditional panel is one of the data panels
      shinyjs::show("inequality_summary_text")
      shinyjs::show("download_simd") 
      shinyjs::show("report_simd")
    }
    
  })
  
  
  
##END
