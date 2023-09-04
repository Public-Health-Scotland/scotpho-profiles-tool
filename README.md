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

##RENV
-The Project makes use of the RENV package to help create a reproducible environment for this repository
-The version of R and all current packages needed to run this project are recorded in the renv lockfile
-The lockfile, renv.lock, records the metadata about every package used for the project so that they can be re-installed in their correct versions as required by the project and also the repos to install from.
-Packages can still be installed as normal but would have quicker install times by using the source argument in the install.packages() function e.g install.packages("package name", type="source"). this will copy binaries from PHS posit package manager directly into your project package library
-Please remember to call renv::snapshot() after installing or updating packages and modifying code to use that new package. this will update the lock file to add data for this new package.Changes will be reflected when pushed to github 
-Other users will be made aware of the changes after refreshing their local repo from github. they will have to run renv::restore() or install the packages manually.
