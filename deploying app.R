#Syntax to deploy the data explorer app to the shiny.io server

##################.
#Connection to server ----
##################.
library(rsconnect)
#Proxy options, needed to carry out the connection.
#It will prompt a screen where you have to include your NSS login details
#separated by an : (e.g. user01:hola1234)
options(RCurlOptions = list(proxy = "PROXY.NSS.SCOT.NHS.UK:3128",
                            proxyuserpwd=.rs.askForPassword("Enter username:password"), 
                            verbose = TRUE)) #this provides all details from process (useful if there are issues)

#Connecting to the shyny.io account.
#Enter the token and secret provided by one of the account managers
rsconnect::setAccountInfo(name='scotland',
                          token='2CA75B26108872D3EA1FDB0665CF1B80',
                          secret='o8mkNkleSR0N4lPezhEjJrs7eZmcCXRzAc8kNl2b')

##################.
#Live app ----
##################.
rsconnect::deployApp('//stats/phip/Projects/Profiles/R Shiny/ScotPHO_profiles',
                     appName="ScotPHO_profiles_tool")

##################.
#Test app ----
##################.
rsconnect::deployApp('//stats/phip/Projects/Profiles/R Shiny/ScotPHO_profiles',
                     appName="ScotPHO_profiles_tool_test")


###########################################################.
##END


