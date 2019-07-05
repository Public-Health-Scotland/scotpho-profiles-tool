library(shiny)
library(dplyr)
library(flextable)
#library(stringr)
library(readr)


#might need webshot to render flextable as image in shiny?

library(webshot) #to download plotly charts
# As well as webshot phantomjs is needed l to download Plotly charts
# https://github.com/rstudio/shinyapps-package-dependencies/pull/180
if (is.null(suppressMessages(webshot:::find_phantom()))) {
  webshot::install_phantomjs()
}


###############################################.
## Data ----
###############################################.    

techdoc <- readRDS("data/techdoc.rds") #original techdoc in shiny
techdoc_ve <- read_csv("data/techdoc_backup_ve.csv") #slightly adjusted techdo - corrected capitalisation of mental health plus a few tidying formatting tests




###############################################.
## User Input ----
###############################################.  

profile_list <- setNames(c('HWB','CYP','ALC','DRG','MEN', "TOB", "POP"),
                         c('Health & wellbeing','Children & young people','Alcohol',
                           'Drugs','Mental Health', "Tobacco control", "Population"))

#try one input selection at a time
input_profile_picked <- "CYP"
#input_profile_picked <- "HWB"
#input_profile_picked <- "Show all"



## filter tech doc dataset based on ui selections ----


#filter techdoc for either a single profile or all indicators
if (input_profile_picked != "Show all"){ #if single profile selected
  
  techdoc_filter <- techdoc_ve %>%
    subset(grepl(names(profile_list[unname(profile_list) == input_profile_picked]),profile)) %>% #filter on selected profile
    mutate(test=regexpr((names(profile_list[unname(profile_list) == input_profile_picked])), domain), #find start position of profile name in domain column
           test2=substr(domain,test, nchar(domain)),  #generate column that starts with filtered profile
           findcomma=regexpr(",",test2), 
           findhyp=regexpr("-",test2), 
           domain1= case_when(findcomma<0 ~ substr(test2,findhyp+1,nchar(test2)), 
                              findcomma>0 ~ substr(test2,findhyp+1,findcomma-1), 
                              TRUE ~ "")) %>%
    mutate (profilename=input_profile_picked) %>%  #sort on profile name since some indicators in multiple profiles
    arrange(profilename, domain1, indicator_name) %>%
    rownames_to_column(var="ind_number") %>% #add numbering 
    select (domain1, ind_number,indicator_name, indicator_definition)
  
} else { #else show all profile indicators
  techdoc_filter <- techdoc_ve %>%
    arrange(profile, domain) %>%
    rownames_to_column(var="ind_number") %>%
    select (profile, domain, ind_number,indicator_name, indicator_definition)
}



##set up flex table

#set up flextable for eithe single profile or all indicators
if (input_profile_picked != "Show all"){
  
    flextable(techdoc_filter) %>%
    add_header_lines(paste0(input_profile_picked," Profile")) %>%
    set_header_labels (domain1="Domain",ind_number= "",indicator_name="Indicator",indicator_definition="Indicator Definition") %>% 
    theme_box() %>%
    # bg(bg = fillcolours, part = "header") %>%
    merge_v(j = ~ domain1) %>%
    align_text_col(align = "left") %>%
    autofit()
} else {
  
  flextable(techdoc_filter) %>%
    set_header_labels (profile="Profile(s)",domain="Domain(s)",ind_number= "",indicator_name="Indicator",indicator_definition="Indicator Definition") %>% 
    theme_box() %>%
    merge_v(j = ~ profile) %>%
    merge_v(j = ~ domain) %>%
    align_text_col(align = "left") %>%
    autofit()
  }

