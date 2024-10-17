# Historic ScotPHO profiles tool
OCTOBER 2024: A major refactoring exercise was carried out on the code within this repository.
The active ScotPHO online profiles tool continues to be available at the same url however the repo containing code behind the app is in a new location
[ScotPHO's Online Profiles Tool](https://scotland.shinyapps.io/ScotPHO_profiles_tool/)
[New code repo behind shiny app ](https://github.com/Public-Health-Scotland/scotpho-profiles-app)

The hisotric version of the shiny app (which is no longer being updated with new indicator data) is available
[Historic ScotPHO's Online Profiles Tool](https://scotland.shinyapps.io/Historic_ScotPHO_profiles/)

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
