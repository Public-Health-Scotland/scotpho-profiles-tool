###############################################
#
# UI for data tab
#
###############################################


dataTab <-
 tabPanel(
  title = div(
  div(class="fa fa-table", role = "navigation"),
  "Data"),
  value = "table",
  layout_sidebar(padding = 20, gap = 20,
    sidebar = sidebar(width = 300, padding = 20,

        h2("Filters"),

        # clear filters button
        actionButton("clear_table_filters",
                     label = "Clear all filters",
                     icon ("eraser"),
                     class = "down"),

        # dataset selection filter
        radioGroupButtons(
          inputId = "dataset_selector",
          label = "Choose a dataset",
          choices = c("Main Dataset",
                      "Inequalities Dataset"),
          selected = "Main Dataset" # default on main opt dataset
        ),

        # quintile type filter (only if inequalities dataset is selected)
        conditionalPanel(
          condition = "input['dataset_selector'] === 'Inequalities Dataset'",
          radioGroupButtons(
            inputId = "quint_type_selector",
            label = "Select quintile type",
            choices = c("Local", "Scotland"),
            selected = "Scotland" # default on max year for each indicator
          )
        ),

        # Geography filters
        jstreeOutput("geography_selector"),

        # profile filters
        virtualSelectInput(inputId = "profile_selector",
                           label = "Select profile(s)",
                           choices = profile_list,
                           disableSelectAll = FALSE,
                           multiple = TRUE,
                           search = TRUE,
                           searchByStartsWith = TRUE,
                           width = '100%',
                           zIndex = 10),

        # indicator filters
        virtualSelectInput(inputId = "indicator_selector",
                           label = "Select indicator(s)",
                           noOptionsText = "Select atleast one geography to see what indicators are available",
                           choices = NULL,
                           disableSelectAll = FALSE,
                           multiple = TRUE,
                           search = TRUE,
                           searchByStartsWith = TRUE,
                           width = '100%',
                           zIndex = 10),
 

        # time period filter
        radioGroupButtons(
          inputId = "time_period_selector",
          label = "Select time period:",
          choices = c("Latest available year", "Trends"),
          selected = "Latest available year"
        )

      ), # close sidebar

    h1("Data table"),
    p("Use the filters to build a data table, which can then be downloaded in various
    formats using the button below. Please note that the table below is a preview. The downloaded dataset will contain more columns containing metadata than are presented here."),
    
    # download data button
    downloadDataButtonsUI(id = "datatable_downloads"),
    # data table
    DTOutput("data_tab_table")

  ) # close layout
) # close tab panel


### END
  