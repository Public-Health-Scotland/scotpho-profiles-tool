# ScotPHO profiles tool
In this respository you can find the code used to produce [ScotPHO's Online Profiles Tool](https://scotland.shinyapps.io/ScotPHO_profiles_tool/).
## Code
- data_preparation: reads the data prepared for each indicator and produces a file for the shiny tool. It also prepares a geography lookup, indicator metadata and shapefiles. It's not necessary to run the app.
- ui: visual interface, what the user can see in the app
- server: deals with the server side: produces charts, creates reactive objects, etc.
- global: datasets, functions, objects that are read both by server and ui
- google-analytics: to allow tracking of visits to the app.
- rsessioninfo: list that includes the R versions used to develop the app as well as the packages used and their versions.
##Folders
- data: data used in the app, including shapefiles.
- www: includes all the images and media used in the app.
