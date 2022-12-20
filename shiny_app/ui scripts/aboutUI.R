###############################################
#
# UI for about us tab
#
###############################################



aboutTab <- 

tabPanel("About", value = "about",
         sidebarPanel(width=1),
         mainPanel(width=8,
                   h3("About this tool", style = "color:black; font-weight: 600"),
                   p("ScotPHO's profiles tool allows users to explore the various different profiles 
                     produced by the ", tags$a(href="http://www.scotpho.org.uk/about-us/about-scotpho/", "ScotPHO collaboration.", 
                                               class="externallink")),
                   p("The profiles are intended to increase understanding of local health issues 
                     and to prompt further investigation, rather than to be used as a performance 
                     management tool. The information needs to be interpreted within a local 
                     framework; an indicator may be higher or lower in one area compared to another, 
                     but local knowledge is needed to understand and interpret differences."),
                   h3("About ScotPHO", style = "color:black; font-weight: 600"),
                   p("The Scottish Public Health Observatory (ScotPHO) collaboration is led 
                     by Public Health Scotland, and includes Glasgow Centre for Population Health, National Records of Scotland, 
                     the MRC/CSO Social and Public Health Sciences Unit and the Scottish Learning Disabilities Observatory."),
                              p("We aim to provide a clear picture of the health of the Scottish population and the factors 
                                that affect it. We contribute to improved collection and use of routine data on health, 
                                risk factors, behaviours and wider health determinants. We take a lead in determining 
                                Scotland's future public health information needs, develop innovations in public health 
                                information and provide a focus for new routine public health information development 
                                where gaps exist."),
                              h3("Referencing our work", style = "color:black; font-weight: 600"),
                              p("Organisations may cite material included within the ScotPHO profiles tool subject to the following conditions:",
                                tags$li("Quote the source as “Scottish Public Health Observatory”"),
                                tags$li("Include the following URL -", 
                                        tags$a(href ="https://scotland.shinyapps.io/ScotPHO_profiles_tool/", "https://scotland.shinyapps.io/ScotPHO_profiles_tool/", class = "externallink"))),
                              h3("Contact us", style = "color:black; font-weight: 600"),
                              p("If you have any trouble accessing any information on this site or have
                                any further questions or feedback relating to the data or the tool, then please contact us at: ",
                                tags$b(tags$a(href="mailto:phs.scotpho@phs.scot", "phs.scotpho@phs.scot", class="externallink")),
                                "and we will be happy to help.")),
                    br()
           )#Tab panel