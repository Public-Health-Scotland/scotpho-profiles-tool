###############################################
#
# UI for homepage tab
#
###############################################

homepageTab <- tabPanel(
  div(
    div(class="fa fa-home", role = "navigation"), "Home"), # wrap in div for screenreader / accessibility purposes 
  value = "home", # tab ID
  use_cicerone(), # guided tour
  htmlTemplate("landing-page.html", # html file containing landing page contents
               # variables defined in landing-page.html file to be built in Rshiny.
               latest_updates_button = actionButton('btn_indicator_updates', "View recent updates", class = "button") #,
               #see_more_button = actionButton("jump_to_life_exp", "View life expectancy by SIMD", class = "button"),
               #guide_button = actionButton("guide", "Need help? Take a guided tour", class = "hero-button")
  ))
