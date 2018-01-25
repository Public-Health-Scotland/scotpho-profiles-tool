## Testing things ----
##################################################.
###########################.
## Dynamic time period selection rank----
test <- as.factor(c(unique(subset(optdata, indicator == 'Deaths all ages', 
                                  select= c("trend_axis")))))
###########################.
## Plotly rank chart----

# Comparator data rank plot
rank_compar_test <- optdata %>% filter(year == '2013' &
                                  areaname == 'Scotland' & indicator == 'Deaths all ages')

#Rank plot data
rank_bar_data_test <- optdata %>% 
  filter(areatype=='Locality' &  year == '2013' & indicator == 'Deaths all ages') %>% 
  mutate(comparator = rank_compar_test$measure) %>% 
  mutate(comp_name = rank_compar_test$areaname) %>% 
  arrange(desc(measure)) # for ranking by value

plot_ly(data = rank_bar_data_test, x = ~areaname) %>% 
  #adding bar layer
  add_bars(y = ~measure, name=~areaname, showlegend = FALSE) %>% #changing bar color
  #Comparator line
  add_trace(y = ~comparator, name = ~unique(comp_name), type = 'scatter', mode = 'lines',
            line = list(color = '#FF0000')) %>% #changing line color
  #Layout
  layout(annotations = list(), #It needs this because of a buggy behaviour
         yaxis = list(title = ~type_definition),
         xaxis = list(title = ~def_period, tickangle = 270, tickfont =list(size=10), #axis parameters
                      categoryorder="array", #order of plotting
                      categoryarray = ~measure),
         margin=list(b = 160),
         hovermode = 'false') %>% # to get hover compare mode as default
  config(displaylogo = F, collaborate=F, editable =F) 

  -rank_bar_data_test[,measure]
margin=list( l = 70, r = 50, b = 150, t = 50, pad = 4 ), #margin-paddings
# Create Rank plot


  ggplot(data=rank_bar_data(), aes(reorder(areaname, -measure), measure) ) +
    geom_bar_interactive(stat = "identity", fill="steelblue", 
                         aes(tooltip= paste("<font size=2><u>", areaname, "</u>", "<br>",  "Measure: ", "<b>", measure, "</b>",  "<br>",  "Numerator: ",  
                                            "<b>", numerator, "</b>", "<br>",  "CI: ", "<b>", lowci, " - ", upci, "</b>", "</font>"), 
                             data_id=areaname)) +
    geom_errorbar(aes(ymax=upci, ymin=lowci), width=0.5)+
    geom_hline(data = rank_compar(), aes(yintercept=measure,  col = areaname)) + #comparator
    labs(title = paste(input$indic_rank),
         subtitle = paste(unique(rank_bar_data()$def_period)),
         y = "Measure") + #title and subtitle
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), #rotating labels
          axis.title.x=element_blank(), #Taking out x axis title
          axis.title.y=element_blank(), #Taking out y axis title
          axis.line = element_line(colour = "black"), # Creating axis line
          panel.background = element_blank(),#Blanking background
          panel.grid.major = element_blank(), #taking out grid lines
          panel.grid.minor = element_blank(),
          legend.position="none" #taking out legends
    )



#Downloading data
output$download_rank <- downloadHandler(
  filename =  'rankplot_data.csv',
  content = function(file) {
    write.csv(rank_bar_data(), file) 
  }
)

###########################.
## Other download way----

# Use observeEvent to tell Shiny what action to take
# when input$save is clicked.
observeEvent(input$save, {
  write.csv(df(), "data.csv")
})

###########################.
## Plotly trend line----
trend_pal <-  c('#08519c','#bdd7e7','#3182bd', '#6baed6', '#eff3ff')
#Time trend data. Filtering based on user input values.
trend_data_test <- optdata %>% filter(areaname %in% c("Scotland", "Ayrshire & Arran") 
                                      & indicator == "Deaths all ages") %>% 
  droplevels()

trend_data_test <- trend_data_test[order(trend_data_test$year),]

#This is needed not sure why
trend_data_test <- as.data.frame(trend_data_test)

#   | areaname %in% input$laname_trend
#   | areaname %in% input$scotname_trend   

#   | areaname %in% input$locname_trend)

# Create time trend plot

#   plot_plotly <- plot_ly(data=data, x=data[,xvar], y = round(data[,yvar],1),
#                          type = 'scatter', mode = 'lines',
#                          color=as.factor(data[,group]), colors = pal_chose[1:cat_length],
#                          width = 650, height = 500) %>%
#     #Layout
#     layout(title = paste(title, "<br>", "<sup><i>Source: ", sourc, sep=""), #title
#            titlefont = list(size=15), #title size
#            annotations = list(), #It needs this because of a buggy behaviour
#            yaxis = list(title = yaxtitle, rangemode="tozero"),
#            xaxis = list(title = xaxtitle, tickangle = 270, tickfont =list(size=10)), #axis parameter
#            margin=list( l = 70, r = 50, b = 150, t = 50, pad = 4 ), #margin-paddings
#            hovermode = 'false', # to get hover compare mode as default
#            images = scotpho_logo) %>%
#     config(displaylogo = F, collaborate=F, editable =F) # taking out plotly logo and collaborate button

#Plotting 
      plot_ly(data=trend_data_test, x=~trend_axis,  y = ~measure, 
              type = 'scatter', mode = 'lines',
              color = ~code , colors = trend_pal) %>% 
        #Layout
        layout(annotations = list(), #It needs this because of a buggy behaviour
               yaxis = list(title = "Measure", rangemode="tozero"), 
               xaxis = list(title = "Time period"),  #axis parameter
               hovermode = 'false') %>%  # to get hover compare mode as default
        config(displaylogo = F, collaborate=F, editable =F) # taking out plotly logo and collaborate button



##################################################.
#Reading data quickly ---- 

optdata_test<- fread("./data/locality_OPTdata.csv") # Cannot use it because of bug that only affects this version of R

system.time(read.csv("./data/all_OPTdata.csv", na.strings=c(""," ","NA")))
# user  system elapsed 
# 42.23    1.09  151.90 
system.time(fread("./data/all_OPTdata.csv", na.strings=c(""," ","NA")))
# user  system elapsed 
# 6.65    2.12  145.41 
system.time(read_csv("./data/all_OPTdata.csv"))
#   user  system elapsed 
# 6.59    1.78  160.34 

##################################################.
#Spine chart dynamic size ---- 
hb_test <- optdata %>% subset(areaname=="Tayside" & year==2013)

paste(nrow(hb_test)*20, "px", sep="")

###########################.
## Frequencies----

data.frame(table(optdata$year)) # year frequencies

###########################.
## Size objects----
sort(sapply(ls(),function(x){object.size(get(x))})) #all

object.size(CA_bound) #one

###########################.
## Trying change buttons format----
# A bit of CSS to modify buttons
#For some obscure reason the background is covered by another layer, so it only
#changes the frame
tags$head(tags$style(".butt{background-color:#262626; color: white;} 
                     .butt:hover{  background-color: #4CAF50;}"))

###########################.
## Deploy on cloud ----
install.packages('rsconnect')

library(rsconnect)

#This is just to hide your username and password, you could include in the script
pass <-source('H:/proxy_password.R') #the file only contains this "username:password". I used my NSS login details
pass <- pass$value
#Proxy options
options(RCurlOptions = list(proxy = "PROXY.NSS.SCOT.NHS.UK:3128",
                            proxyuserpwd="pass", 
                            verbose = TRUE))
#Connecting
rsconnect::setAccountInfo(name='scotphotest',
                          token='355C2FC7F795D5E46DB91FF77BDE8ECB',
                          secret='Wc3YY4jV/wIUBxi0ZyV64FKYJiiTrJkOtZFL6aYd')


rsconnect::deployApp('//stats/phip/Projects/Profiles/R Shiny/App-JV')

?rsconnectProxies
###########################.
## Ggiraph ----
hb_test <- optdata %>% subset(areatype=="Health Board" & year==2012 
                              & indicator=="lungcancer_deaths") %>% 
  droplevels() %>% #dropping missing factor levels to allow merging
  rename(HBCode=Code) 

ggiraph(code = print(
 ggplot(data=hb_test, aes(reorder(areaname, -rate), rate) ) +
  geom_bar_interactive(stat = "identity", fill="steelblue", 
                       aes(tooltip= paste("<font size=2><u>", areaname, "</u>", "<br>",  "Rate: ", "<b>", rate, "</b>",  "<br>",  "Numerator: ",  
                                          "<b>", numerator, "</b>", "<br>",  "CI: ", "<b>", lowci, " - ", upci, "</b>", "</font>"), 
                           data_id=areaname)) +
  geom_errorbar(aes(ymax=upci, ymin=lowci), width=0.5)+
  #geom_hline(data = compar_bar(), aes(yintercept=rate,  col = areaname)) + #comparator
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), #rotating labels
        axis.title.x=element_blank(), #Taking out x axis title
        axis.title.y=element_blank(), #Taking out y axis title
        axis.line = element_line(colour = "black"), # Creating axis line
        panel.background = element_blank(),#Blanking background
        panel.grid.major = element_blank(), #taking out grid lines
        panel.grid.minor = element_blank() 
  )
), 
width = .7, 
hover_css = "cursor:pointer;fill:lightsteelblue;" , 
tooltip_extra_css = "background-color:#99BF9E; 
  border: 2px #45B563 solid; border-radius: 5px; font-family:Calibri;")


  theme(plot.title = element_text(lineheight=.8, face="bold"))

  ###########################.
## Rbokeh ----
test_data <- data.frame(fact=c("a","b","c", "a","b","c"),nume=1:3, max=2:4, min=0:2, year=c(2012,2012,2012,2013,2013,2013))
test_data2 <- data.frame(fact=c("a","b","c"),nume=1:3, max=2:4, min=0:2, year=c(2012,2012,2012), comp=2)

rownames(hb_test) <- hb_test$areaname #used for the dotplots
#data=hb_test,ylim=levels(with(hb_test, reorder(areaname, rate))), 

figure(data=hb_test, xlab=NULL, ylab=NULL) %>%
  ly_bar(data=hb_test, x=areaname, y=rate, hover=list(rate)) %>% 
  ly_abline(h=test_data2$comp, type= 2, color="red") %>% 
#   ly_segments(lowci, areaname, upci, areaname, data=hb_test) #%>%
#   ly_points(rate, areaname, glyph = 16, data = hb_test)
str(iris)
  # ly_multi_line(xs=test_data2$fact, ys=c(test_data2$max, test_data2$min))

figure() %>%
  ly_points(Sepal.Length, Sepal.Width, data = iris,
            color = Species, glyph = Species,
            hover = list(Sepal.Length, Sepal.Width))

###########################.
## Plotly ----

  ggplot(data=hb_test, aes(reorder(areaname, -rate), rate) ) +
    geom_bar(stat = "identity", fill="steelblue") +
    geom_errorbar(aes(ymax=upci, ymin=lowci))+
    #geom_hline(data = compar_bar(), aes(yintercept=rate,  col = areaname)) + #comparator
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), #rotating labels
          axis.title.x=element_blank(), #Taking out x axis title
          axis.line = element_line(colour = "black"), # Creating axis line
          panel.background = element_blank(),#Blanking background
          panel.grid.major = element_blank(), #taking out grid lines
          panel.grid.minor = element_blank()
    )

#old bar plot.
output$barPlot <- renderPlotly({
  ggplot(data=barpl(), aes(reorder(areaname, -rate), rate) ) +
    geom_bar(stat = "identity", fill="steelblue") +
    geom_errorbar(aes(ymax=upci, ymin=lowci))+
    geom_hline(data = compar_bar(), aes(yintercept=rate,  col = areaname)) + #comparator
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), #rotating labels
          axis.title.x=element_blank(), #Taking out x axis title
          axis.line = element_line(colour = "black"), # Creating axis line
          panel.background = element_blank(),#Blanking background
          panel.grid.major = element_blank(), #taking out grid lines
          panel.grid.minor = element_blank()
    )
})

## Highchart ----
test_data <- data.frame(fact=c("a","b","c", "a","b","c"),nume=1:3, max=2:4, min=0:2, year=c(2012,2012,2012,2013,2013,2013))
test_data2 <- data.frame(fact=c("a","b","c"),nume=1:3, max=2:4, min=0:2, year=c(2012,2012,2012), comp=2)


highchart() %>% 
  hc_yAxis(plotLines = list(
    list(label = list(text = "Comparator"),
         color = "#FF0000",
         width = 2, 
         value = mean(test_data2$comp)))) %>% 
  hc_add_series(test_data2, "column", name="Rate", hcaes(x = fact, y = nume), color="steelblue", pointPadding= 0, groupPadding= 0) %>% 
  hc_add_series(test_data2, "errorbar", name="95%", hcaes(low = min, high = max), whiskerLength=20) %>% 
  #hc_add_series(test_data2, "line", hcaes(y =comp), color="red", marker=FALSE) %>% 
  hc_tooltip(shared = TRUE, borderWidth = 2) %>% #modifying tooltip
  hc_legend(enabled=FALSE) %>% #taking out legend
  hc_exporting(enabled = TRUE) #enabling exporting of image

??plotLines

output$barPlot <- renderHighchart({
  highchart() %>% 
    hc_add_series(barpl(), "column", name="Rate", hcaes(x = areaname, y = rate), color="steelblue", pointPadding= 0, groupPadding= 0) %>% 
    hc_add_series(barpl(),"errorbar", name="95%", hcaes(low = lowci, high = upci), whiskerLength=20) %>% 
    hc_add_series(compar_bar(),"line", hcaes(y =rate), color="red", marker=FALSE) %>% 
    hc_tooltip(shared = TRUE, borderWidth = 2) %>% #modifying tooltip
    hc_xAxis(categories = barpl()$areaname) %>% #Modifying labels x axis
    hc_title(text = paste(input$indic_bar),
             margin = 20, align = "left",
             style = list(color = "#90ed7d", useHTML = TRUE)) %>% #title
    hc_subtitle(text = paste(unique(barpl()$def_period)),
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>% #subtitle
    hc_legend(enabled=FALSE) %>% #taking out legend
    hc_exporting(enabled = TRUE) #enabling exporting of image
})


###########################.
##to organize dropdowns ----
selectInput("country", "Select country", list(
  "Europe" = c("Germany", "Spain"),
  "North America" = c("Canada", "United States" = "USA")
))

###########################.
## Dygraph-time series ----
test_time <- optdata %>% subset(indicator=="Babies exclusively breastfed at 6-8 weeks" & areatype=="Health Board", 
         select= c("areaname","rate", "year","indicator")) %>% 
  dcast(year+indicator ~ areaname, value.var="rate")


dygraph(test_time, main=paste(unique(test_time$indicator))) %>% 
  dyRangeSelector() %>% 
  dyLegend(width = 400) %>%  
  dyOptions(axisLineWidth = 1.5, drawGrid = FALSE, drawPoints = TRUE, pointSize = 2)



#making it reactive
test_time <- optdata %>% subset(indicator=="lungcancer_deaths" & areatype=="Health Board", 
                                select= c("areaname","rate", "year")) %>% 
  dcast(year ~ areaname, value.var="rate")

timetrend <- optdata %>% subset(select= c("areaname","rate", "year")) %>% 
  dcast(year ~ areaname, value.var="rate")

test1 <- optdata %>% subset( 
      (areaname %in% "Scotland"
      | areaname %in% "Fife"
      | areaname %in% "Glasgow City"                      
      | areaname %in% "Borders")
      & indicator == "lungcancer_deaths",
    select= c("areaname","rate", "year")) %>% 
  dcast(year ~ areaname, value.var="rate")


###old style with ggplot/plotly
  timetrend <- reactive({filter(optdata, 
                           (areaname %in% input$hbname_trend
                           | areaname %in% input$laname_trend
                           | areaname %in% input$scotname_trend                       
                           | areaname %in% input$locname_trend)
                            & indicator == input$indic_trend)
  })

  output$timetrendPlot <- renderPlotly({
    ggplot(data=timetrend(), aes(trend_axis, rate, group=areaname, color=areaname))+
      geom_line(stat = "identity")+
      geom_point()+
      geom_errorbar(aes(ymax=upci, ymin=lowci), width=0.2, size=0.3) +
      #scale_x_discrete(labels=as.character(timetrend()$trend_axis)) + # changing labels for x axis
      scale_y_continuous(limits = c(0, NA)) + #Setting minimum value of y axis to 0.
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), #rotating labels
            axis.title.x=element_blank(), #Taking out x axis title
            axis.line = element_line(colour = "black"), # Creating axis line
            panel.background = element_blank(),#Blanking background
            panel.grid.major = element_blank(), #taking out grid lines
            panel.grid.minor = element_blank(),
            legend.title = element_blank() # Taking out title legend
      ) 
  })

  ###########################.
## Map with leaflet ----

# Maybe use this provider:addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite")

###
#Trying to add councils
#Reading file with council shapefiles
#making it small 29mb to 2.5. Sometimes it fails, due to lack of memory (use memory.limits and close things).
CA_bound_orig<-readOGR("./shapefiles","CA_2011_EoR_Scotland") %>% 
  rmapshaper::ms_simplify(keep=0.0025)

object.size(CA_bound_orig)

#Transforming coordinate system to the one leaflet needs
CA_bound_orig <- spTransform(CA_bound_orig,  CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))

#Saving the simplified shapefile to avoid the calculations.
writeOGR(CA_bound_orig, dsn="./shapefiles", "CA_simpl", driver="ESRI Shapefile", overwrite_layer=TRUE)

#Reading file with council shapefiles simplified
CA_bound<-readOGR("./shapefiles","CA_simpl")

#Test data and renaming variable for merging
ca_test <- optdata %>% subset(areatype=="Local Authority" & year==2012 
                              & indicator=="lungcancer_deaths") %>% 
  droplevels() %>% #dropping missing factor levels to allow merging
  rename(GSS_COD=Code) 

# merge on common variable
CA_bound <- merge(CA_bound, ca_test, by='GSS_COD')

##########################.
###Trying to add health board
#making it small 29mb to 2.5. Sometimes it fails, due to lack of memory (use memory.limits and close things).
HB_bound_orig<-readOGR("./shapefiles","SG_NHS_HealthBoards_2014") %>% 
  rmapshaper::ms_simplify(keep=0.0025)

object.size(HB_bound_orig)

#Transforming coordinate system to the one leaflet needs
HB_bound_orig <- spTransform(HB_bound_orig,  CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))

#Saving the simplified shapefile to avoid the calculations.
writeOGR(HB_bound_orig, dsn="./shapefiles", "HB_simpl", driver="ESRI Shapefile", overwrite_layer=TRUE)

#Reading file with health board shapefiles simplified
HB_bound<-readOGR("./shapefiles","HB_simpl")

#Test data and renaming variable for merging
hb_test <- optdata %>% subset(areatype=="Health Board" & year==2012 
                              & indicator=="lungcancer_deaths") %>% 
  droplevels() %>% #dropping missing factor levels to allow merging
  rename(HBCode=Code) 

# merge on common variable
HB_bound <- merge(HB_bound, hb_test, by='HBCode')



####
    leaflet() %>% 
      setView(-4.6519999, 56.33148888, zoom = 8) %>% # setting initial view point
      fitBounds(-10, 60, 0, 54)  %>%
      addProviderTiles(providers$OpenMapSurfer) %>%
      #Adding polygons with HB
      addPolygons(data=HB_bound, group="Health Board",
                  color = "#444444", weight = 2, smoothFactor = 0.5, 
                  label = (sprintf(
                    "<strong>%s</strong><br/>Total: %g<br/>Rate: %g",
                    HB_bound$HBName, HB_bound$numerator, HB_bound$rate) 
                    %>% lapply(htmltools::HTML)),
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~colorQuantile("YlOrRd", rate)(rate),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)
      ) %>% 
      #Adding polygons with CA
      addPolygons(data=CA_bound, group="Local Authority",
                  color = "#444444", weight = 1, smoothFactor = 0.5, 
                  label = (sprintf(
                    "<strong>%s</strong><br/>Total: %g<br/>Rate: %g",
                    CA_bound$NAME, CA_bound$numerator, CA_bound$rate) 
                    %>% lapply(htmltools::HTML)),
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~colorQuantile("YlOrRd", rate)(rate),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)
      ) %>% 
      #Adding layer control
      addLayersControl( 
        overlayGroups = c("Health Board", "Local Authority"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      hideGroup(c("Health Board", "Local Authority"))  

###trying to add legend
    #adding legend
    addLegend("bottomright", pal = "default", values = colorQuantile("YlOrRd", CA_pol()$rate),
              title = "Rate",
              opacity = 1
    )

###old popups for map
popup = paste("<b>", HB_bound$HBName, "</b>", "<br>", 
              "Total:", HB_bound$numerator, "<br>", 
              "Rate:", HB_bound$rate)

##################################################.
#Spine chart attempt ---- 
#scaling variables approach 
#Maybe use geom_crossbar
hb_test <- subset(optdata, areatype=="Health Board" & year==2013)

#This would have to be done properly
hb_test$lowci_sc <- hb_test$rate_sc - 0.2
hb_test$upci_sc <- hb_test$rate_sc + 0.2

#Works too
optdata$rate_sc <- ave(optdata$rate, optdata$indicator, optdata$year, optdata$areatype, FUN=rescale)

#quantiles by indicator
# hb_test$quant5 <- tapply(hb_test$rate_sc, hb_test$indicator, FUN=quantile, probs=0.05)
# hb_test$quant25 <- tapply(hb_test$rate_sc, hb_test$indicator, FUN=quantile, probs=0.25)
# hb_test$quant75 <- tapply(hb_test$rate_sc, hb_test$indicator, FUN=quantile, probs=0.75)
# hb_test$quant95 <- tapply(hb_test$rate_sc, hb_test$indicator, FUN=quantile, probs=0.95)

#Only one area df
borders_test <- optdata %>% subset(areaname=="Ayrshire & Arran" & year==2006) 
hb_test <- subset(optdata, areatype=="Health Board" & year==2006 & indicator %in% borders_test$indicator)

comp_test <- subset(optdata, areaname=="Scotland" & year==2006 & indicator %in% borders_test$indicator)

# ggplot() +
#   geom_point(data=hb_test, aes(indicator, rate_sc), stat="identity" ) +
#   geom_point(data=borders_test, aes(indicator, rate_sc), stat="identity", color="red" ) +
#   coord_flip() 
  
ggplot() +
  geom_boxplot(data=hb_test, aes(indicator, rate_sc), fill="steelblue", width=0.4) +
  geom_dotplot(data=hb_test, aes(indicator, rate_sc), binaxis='y', stackdir='center', fill="lightsteelblue", dotsize=0.5) +
  geom_point(data=borders_test, aes(indicator, rate_sc),  size=4, pch=21, color="black",
             #colors based on overlap of CI with comparator rate
             fill=ifelse(borders_test$lowci < comp_test$rate & borders_test$upci > comp_test$rate,'white',
                          ifelse(borders_test$lowci > comp_test$rate, 'red',
                                 ifelse(borders_test$upci < comp_test$rate, 'green', "lightblue")))) +
  #geom_point(data=comp_test2, aes(indicator, rate_sc),  size=4, color="black")+
  coord_flip() +
  theme(axis.title.x=element_blank(), #Taking out x axis title
        axis.text.x=element_blank(), # taking out x axis labels
        axis.ticks.x=element_blank(), # taking out x axis tick marks
        axis.title.y=element_blank(), #Taking out y axis title
        axis.line.y = element_line(colour = "black"), # Creating axis line
        panel.background = element_blank(),#Blanking background
        panel.border = element_rect(fill=NA),
        legend.position="none" #taking out legends
  ) 

#Trying to add table to plot. There are other options, such as covert table in graph,
#include tooltip/plotly for info on values, using labels for period, sparktables.

test_table <- subset(borders_test, select = c("indicator","trend_axis", "numerator", "rate"))

grid.arrange(tableGrob(test_table), ggplotGrob(test_plot), 
             ncol = 2, widths = unit(c(0.40, 0.60), "npc"))

##################################################.
#Bar plot data ---- 
barpl <- filter(optdata, areatype=="Health Board" &
                            year == "2013" & indicator == "under75_Cancerdeaths")
# Comparator data bar plot
compar_bar <- filter(optdata, year == "2013" &
                                 areaname == "Scotland" & indicator == "under75_Cancerdeaths")

ggplot( ) +
  geom_bar(data=barpl, aes(reorder(areaname, -rate), rate),stat = "identity", fill="steelblue") +
  geom_hline(data = compar_bar, aes(yintercept=rate,  col = "red")) + #comparator
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), #rotating labels
        axis.title.x=element_blank(), #Taking out x axis title
        axis.line = element_line(colour = "black"), # Creating axis line
        panel.background = element_blank(),#Blanking background
        panel.grid.major = element_blank(), #taking out grid lines
        panel.grid.minor = element_blank() 
  ) 

##################################################.
#For interactive hovering -----
  output$hover_info <- renderPrint({
    cat("input$plot_hover:\n")
    str(input$plot_hover)
  })

fluidRow(column(width = 12, verbatimTextOutput("hover_info"))) , hover = hoverOpts(id = "plot_hover") #ui part

%>% config(displaylogo = FALSE, collaborate = FALSE)#to try to delete icons from bar in plotly, not working

##################################################.
######### Dynamic dropdown box ----

# Look-up codes and lists with geography names and geography types organised into list
# Make geography a labelled vector
geog_lkp <- optdata$areaname
names(geog_lkp) <- optdata$Code

# Split the geography labelled vector by the geography type (as a code)
geog_by_type <- tapply(geog_lkp, optdata$areatype, unique)

# Conserve memory by removing unnecessary data
geog_lkp <- unique(geog_lkp)

# Create another labelled vector this time for geography type
geog_type <- c(Scotland = "Scotland", lo = "Locality", hb = "Health Board", la = "Local authority") 
geog_type_rev <- names(geog_type)
names(geog_type_rev) <- geog_type

# Type of geography
output$geotypeControl <- renderUI({
  selectInput("geotype", "Type of geography", geog_type_rev, selected = "Scotland",
              multiple=TRUE, selectize=TRUE)
})

geogs <- reactive({geog_by_type[[input$geotype]]})

# Chosing specific geography, main
output$geonameControl <- renderUI({
  selectInput("geoname", "Choose Geography", geogs(),
              multiple=TRUE, selectize=TRUE, selected = "Scotland")
})
# Chosing specific geography, comparator
output$geocompControl <- renderUI({
  selectInput("geocomp", "Choose Comparator geography", geogs(),
              selectize=TRUE, selected = "Scotland")
})

output$geogControls = renderUI({
  selectInput('geoname', 'Area name', choices=unique(areatype_select$areaname),
              multiple=TRUE, selectize=TRUE, selected = "Scotland")
})

##################################################.
###Other layout ----


dashboardPage(
  dashboardHeader(title = "ScotPHO Profiles"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("th")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Projection", tabName = "projection", icon = icon("line-chart"))
    ),
    hr(),    
    selectInput("geoname", "Health Board:", 
                choices=unique(hb$areaname) ),
    hr(),
    selectInput("year", "Year:", 
                choices=unique(optdata$year) ),
    hr(),
    selectInput("indicator", "Indicator:", 
                choices=unique(optdata$indicator)),
    hr(),
    submitButton("Apply filters"),
    #For downloading the data
    downloadButton('downloadData', 'Download')
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "intro",
              h2("ScotPHO Online Profile Tool"),
              fluidRow(
                box(p("The tabs above can be used to draw spine charts and, 
                      to give a better (albeit still incomplete) idea of the functionality
                      of Shiny, a plot showing current and future trends"), height = 250))
                ),
      # Second tab content
      tabItem(tabName = "dashboard",
              # Boxes need to be put in a row (or column)
              fluidRow(
                box(plotOutput("timetrendPlot", height = 250
                ))
              ),
              fluidRow(
                box(plotOutput("barPlot", height = 250,
                               hover = hoverOpts(
                                 id = "plot_hover"
                               )
                )
                )
              ),
              fluidRow(
                column(width = 3,
                       verbatimTextOutput("hover_info")
                )
              )
      )
                )
    ))
