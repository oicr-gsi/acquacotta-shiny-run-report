#' The ui object to pass to \link[shiny]{shinyApp}
#'
#' @export
createAcquacottaUI <- function() {
  dashboardPage(
    dashboardHeader(
      title = "Acquacotta", titleWidth = 280,
      dropdownMenuOutput("notificationMenu")
    ),

    # Most User Input Widgets are initialized dynamically by the server
    dashboardSidebar(
      width = 280,

      # https://stackoverflow.com/questions/31253351/r-shiny-dashboard-how-to-add-vertical-scrollbar-to-dashboard-sidebar
      # This adds a scroll bar to the sidebar. The plot will stay centered on the screen even as sidebar elements are added
      tags$style(type = "text/css", ".sidebar {height: 90vh; overflow-y: auto;}"),

      # Select Run Report
      # Note that this assumes the oldest to newest ordering and displays the newest Run Report
      selectInput("run", "Select Run Report", c()),

      # Select one or more studies in the Run Report (PCSI, CYT)
      selectInput("study", "Select Study", c(), multiple = TRUE),

      # Select one or more lanes in the Run Report
      selectInput("lane", "Select Lane", c(), multiple = TRUE),

      # Selection of the menues will display different options
      sidebarMenu(
        id = 'menu',
        menuItem("Library", tabName = "library", icon = icon("book")),
        menuItem("Selection", tabName = "selection", icon = icon("clone"))
      ),

      # Sorting is only available at Library level display
      conditionalPanel(
        condition = "input.menu == 'library'",
        # Order by which metrics
        selectInput("order.by", "Order By", c()),

        # Reverse the metrics order
        checkboxInput("order.rev", "Reverse Order")
      ),

      # The options are only displayed if the appropriate menu item is selected
      # Note that the options can still be accessed (and have values) even when not displayed
      conditionalPanel(
        condition = "input.menu == 'library'",
        # Select which metrics (Map %, Coverage, % of Target) to plots
        checkboxGroupInput("check.type", "Select Plots"),
        # Select Coverage cutoffs
        sliderInput("slider.coverage", "Coverage Filter", 0, 0, value = c(0, 0))
      )
    ),

    #Displays the plots and any errors
    dashboardBody(
      # Enables the use of shinyjs
      shinyjs::useShinyjs(),

      # https://stackoverflow.com/questions/44412382/clicking-same-plotly-marker-twice-does-not-trigger-events-twice
      # Clicking on the same library bar twice does not trigger event twice. This custom JS resets the event to null
      # It is called in the reactive expression that deals with the click event
      shinyjs::extendShinyjs(text = "shinyjs.resetLibClick = function() { Shiny.onInputChange('.clientValue-plotly_click-library_plot', 'null'); }"),

      # The css selector allows for the plot to be full height:
      # https://stackoverflow.com/questions/36469631/how-to-get-leaflet-for-r-use-100-of-shiny-dashboard-height
      tags$style(type = "text/css", "#plot {height: calc(100vh - 80px) !important;}"),

      # Shows any errors
      # By default has no content and has no dimensions
      verbatimTextOutput("error_run"),

      # Only one plot will be displayed at a time given which menu is selected
      # If a menu is selected that does not match any tabName, the last selected tab stays open
      tabItems(
        tabItem(
          tabName = "library",
          # The css selector and the id of plotlyOutput must match
          plotlyOutput("plot", height = "100%")
        ),
        tabItem(
          tabName = "selection",
          tableOutput("text_selection")
        )
      )
    )
  )
}
