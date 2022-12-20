###############################################
#
# Server logic for homepage tab
#
###############################################


### 1. guided tour button ----
# Start guided tour when 'take a tour' button is clicked
guide$init()

observeEvent(input$guide, {
  guide$start()
})


### 2. recent updates button ----
# show table of latest indicator updates when 'recent updates' button is clicked
# updates_modal can be found in global script as it's not reactive
observeEvent(input$btn_indicator_updates, {
  showModal(updates_modal)
})


### 3. what's new button(s) ----
# these buttons will change depending on what we are showcasing in the 'what's new' section

# a. life expectancy ---
# when 'view life expectancy by SIMD' button is clicked: - 
# (i) navigate to inequalities tab
# (ii) change indicator dropdown filter to life expectancy indicator

observeEvent(
  input$jump_to_life_exp,
  {
    updateTabsetPanel(session, "intabset", selected = "ineq")
    updateSelectInput(session,
                      "indic_simd",
                      selected = "Cancer registrations")
  })

