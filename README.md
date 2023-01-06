# ScotPHO profiles tool
In this respository you can find the code used to produce [ScotPHO's Online Profiles Tool](https://scotland.shinyapps.io/ScotPHO_profiles_tool/).
## Code
- data_preparation: reads the data prepared for each indicator and produces a file for the shiny tool. It also prepares a geography lookup, indicator metadata and shapefiles. It's not necessary to run the app.
- ui: visual interface, what the user can see in the app (main ui script sources separate ui scripts for different tabs within dashboard)
- server: deals with the server side: produces charts, creates reactive objects, etc. (main server script sources separate server scripts for different tabs within dashboard)
- global: datasets, functions, objects that are read both by server and ui
- google-analytics: to allow tracking of visits to the app.
- gtag: to allow tracking of visits with new Google Analytics 4
- rsessioninfo: list that includes the R versions used to develop the app as well as the packages used and their versions.
-landing-page.hmtl : html script controlling layout of profiles tool landing page
##Folders
- data: data used in the app, including shapefiles.
- functions: collection of functions that can be called to generate similar content (eg charts/tables) within the dashboard. Use fo functions reduces duplication of code across different scripts. 
- ui scripts: collection of scripts controlling the user interface for various different tabs within the tool.For ease of navigation and code editing each tab of the dashboard has separate ui scripts
- server scripts: collection of scripts controlling server side actions of various tabs.For ease of navigation and code editing each tab of the dashboard has separate server scripts
- www: includes all the images and media used in the app.
