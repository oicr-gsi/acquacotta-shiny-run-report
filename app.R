source("./utility.R")
source("./list_reports.R")
source("./read_config.R")

library(plotly)
library(data.table)
library(shiny)
library(shinydashboard)

# The UI
ui <- dashboardPage(
  dashboardHeader(),
  
  # Most User Input Widgets are initialized dynamically by the server
  dashboardSidebar(
    # Select Run Report
    # Note that this assumes the oldest to newest ordering and displays the newest Run Report
    selectInput("run", "Select Run Report", c()),
    
    # Select the specific study within the Run Report (PCSI, CYT)
    selectInput("study", "Select Study", c()),
    
    # Select Coverage cutoffs
    sliderInput("slider.coverage", "Coverage", 0, 0, value = c(0, 0)),
    
    # Select which metrics (Map %, Coverage, % of Target) to plots
    checkboxGroupInput(
      "check.type", "Select Plots"
    )
  ),
  
  #Displays the plots and any errors
  dashboardBody(
    # The css selector allows for the plot to be full height: https://stackoverflow.com/questions/36469631/how-to-get-leaflet-for-r-use-100-of-shiny-dashboard-height
    tags$style(type = "text/css", "#plot {height: calc(100vh - 80px) !important;}"),
    
    # Shows any errors
    # By default has no content and has no dimensions
    verbatimTextOutput("error_run"),
    
    # The css selector and the id of plotlyOutput must match
    plotlyOutput("plot", height = "100%")
  )
)

# Server function that makes everything alive
server <- function(session, input, output) {
  # A data table that contains the paths and display names of the available Run Reports
  # The Run Reports should be ordered from oldest to newest
  all.runs.dt <- reactiveVal(NULL)
  tryCatch({
    loaded.dt <-listRunReports() 
    updateSelectInput(session, "run", choices = sort(loaded.dt$name, decreasing = TRUE))
    all.runs.dt(loaded.dt)
  }, error = function(err) {
    err.msg <- paste("Failed to load Run Report database:", conditionMessage(err), sep = "\n")
    output$error_run <- renderText(err.msg)
  })
  
  # The current Run Report that is being displayed
  # This Reactive Variable allows different watchers to access/modify the same information
  # Can be set to NULL to display nothing. Useful if selected Run Report cannot be read
  current.run <- reactiveVal(NULL)
  
  # Changing the Run Report updates the current.run and the Studies that can be selected
  # If any error occurs in loading report, current.run is set to NULL (nothing will be plotted) and error message is set
  observeEvent(input$run, {
    req(input$run)
    
    tryCatch({
      # Remove any previous error messages
      output$error_run <- renderPrint(invisible())
      current.run(createAppDT(all.runs.dt()[name == input$run, path]))
      all.studies <- names(current.run())
      updateSelectInput(session, "study", choices = all.studies)
    }, error = function(err) {
      current.run(NULL)
      err.msg <- paste("Failed to read Run Report TSV file for the following reason:", conditionMessage(err), sep = "\n")
      output$error_run <- renderText(err.msg)
    })
  })
  
  # Once a Run Report or Study is changed, update Coverage slider 
  observeEvent(c(input$run, input$study), {
    selected.study <- current.run()[[input$study]]
    req(selected.study)
    
    coverage.max <- max(selected.study[, "Coverage (collapsed)"])
    updateSliderInput(session, "slider.coverage", max = coverage.max, value = c(0, coverage.max))
    
    if (is.null(input$check.type)) {
      study.types <- unique(selected.study$Type)
      updateCheckboxGroupInput(
        session, 'check.type', choices = study.types,
        selected = CONFIG.DEFAULTPLOTS
      )
    }
  })
  
  # Recalculates the data table to plot every time any variable within this expression is changed
  dt.to.plot <- reactive({
    req(input$run)
    req(input$check.type)

    selected.study <- current.run()[[input$study]]
    req(selected.study)
    
    lane.levels <- sort(unique(selected.study$Lane))
    
    # Make Lanes a factor rather than number and sort by lane and then by coverage
    set(selected.study, j = "Lane", value = factor(selected.study$Lane, levels = lane.levels, ordered = TRUE))
    setorder(selected.study, Lane, -`Coverage (collapsed)`)
    
    # Libraries should also be factors rather than strings
    set(selected.study, j = "Library", value = factor(selected.study$Library, levels = unique(selected.study$Library, ordered = TRUE)))
    
    # Filter by Coverage
    selected.coverage <- input$slider.coverage
    selected.study <- selected.study[`Coverage (collapsed)` >= selected.coverage[1] & `Coverage (collapsed)` <= selected.coverage[2],]
    
    # Keep only the metrics that will be plotted 
    selected.study <- split(selected.study, by = "Type")
    selected.type <- input$check.type
    type.to.keep <- names(selected.study) %in% selected.type
    return(selected.study[type.to.keep])
  })
  
  # Create the plot
  output$plot <- renderPlotly({
    req(input$check.type)
    
    temp.to.plot <- dt.to.plot()
    req(temp.to.plot)

    # Removes the cryptic error message if the parameters are such that there are no data points to print
    # This is not caught by the req call because the temp.to.plot has elements, but all the elements are empty
    anything.to.print <- all(sapply(temp.to.plot, nrow))
    if(!anything.to.print) {
      return(NULL)
    }
    
    legend <- TRUE
    plots.all <- lapply(temp.to.plot, function (x) {
      stat.type <- as.character(x[["Type"]][1])
      temp.plot <- x %>% 
        plot_ly(x = ~Library, color = paste0("Lane ", x$Lane)) %>% 
        add_bars(y = ~Value, showlegend = legend, legendgroup = paste0("Lane ", x$Lane)) %>%
        layout(
          xaxis = list(visible = FALSE), 
          yaxis = list(title = stat.type, titlefont = list(size = 8)), 
          legend = list(orientation = "h")
        )
      
      legend <<- FALSE
      return(temp.plot)
    })
    
    # If there are more than 10 plots, split them into two columns
    two.columns <- ceiling(length(input$check.type) / 2)
    nrows <- ifelse(length(temp.to.plot) > 10, two.columns, length(temp.to.plot))
    subplot(plots.all, nrows = nrows, titleY = TRUE)
  })  
}

shinyApp(ui, server, options = list(port = CONFIG.PORT))