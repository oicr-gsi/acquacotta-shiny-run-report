source("./utility.R")
source("./list_reports.R")
source("./read_config.R")

library(plotly)
library(data.table)
library(shiny)
library(shinydashboard)

# The UI
ui <- dashboardPage(
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
      menuItem("Plot", tabName = "plot", icon = icon("image")),
      menuItem("Filter", tabName = "filter", icon = icon("filter"))
    ),
    
    tabItems(
      tabItem(
        tabName = "plot",
        
        # Order by which metrics
        selectInput("order.by", "Order By", c()),
        
        # Reverse the metrics order
        checkboxInput("order.rev", "Reverse Order"),
        
        # Select which metrics (Map %, Coverage, % of Target) to plots
        checkboxGroupInput("check.type", "Select Plots")
      ),
      tabItem(
        tabName = "filter",
        # Select Coverage cutoffs
        sliderInput("slider.coverage", "Coverage", 0, 0, value = c(0, 0))
      )
    )
  ),
  
  #Displays the plots and any errors
  dashboardBody(
    # The css selector allows for the plot to be full height:
    # https://stackoverflow.com/questions/36469631/how-to-get-leaflet-for-r-use-100-of-shiny-dashboard-height
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
  # If an error string is supplied, error string will be displayed instead of a plot
  error.msg <- reactiveVal(NULL)
  
  # A data table that contains the paths and display names of the available Run Reports
  # The Run Reports should be ordered from oldest to newest
  all.runs.dt <- reactiveVal(NULL)
  
  # The current Run Report that is being displayed
  # This Reactive Variable allows different watchers to access/modify the same information
  # Can be set to NULL to display nothing. Useful if selected Run Report cannot be read
  current.run <- reactiveVal(NULL)
  
  # Try loading Run Report names and file locations. Error will prevent the app from working, with error message displayed.
  tryCatch({
    loaded.dt <- listRunReports(CONFIG.RUNPATH)
    all.runs.dt(loaded.dt)
    
    # Populate the available metric plots (assumption is that all Run Reports have the same)
    updateCheckboxGroupInput(session,
                             'check.type',
                             choices = CONFIG.ALLPLOTS,
                             selected = CONFIG.DEFAULTPLOTS)
    
    # Populate the ordering metric drop down (assumption is that all the Run Reports have the same)
    updateSelectInput(session,
                      "order.by",
                      choices = CONFIG.ALLPLOTS,
                      selected = CONFIG.DEFAULTORDER)
    
    # Set the default order direction
    updateCheckboxInput(session,
                        "order.rev",
                        value = CONFIG.DEFAULTORDERREV)
    
  }, error = function(err) {
    err.msg <-
      paste("Failed to load Run Report database:",
            conditionMessage(err),
            sep = "\n")
    error.msg(err.msg)
  })
  
  # Set error.msg to NULL to hide it, otherwise plot will be hidden and error message diplayed
  observeEvent(error.msg(), {
    if (is.null(error.msg())) {
      output$error_run <- renderPrint(invisible())
    } else {
      current.run(NULL)
      output$error_run <- renderText(error.msg())
    }
  })
  
  # The Run Report data table is loaded (happens once when client connects)
  # Check if a GET request is specified and modify the app accordingly
  observeEvent(all.runs.dt(), {
    get.req <- parseQueryString(session$clientData$url_search)
    get.run <- get.req$run
    
    # If GET specifying "run" is not given, it is NULL
    updateSelectInput(
      session,
      "run",
      choices = sort(loaded.dt$name, decreasing = TRUE),
      selected = get.run
    )
    
    # If GET "run" does not exist, app will be blank. Display error message to inform user as to why.
    if (!is.null(get.run)) {
      if (!(get.run %in% all.runs.dt()$name)) {
        error.msg(
          paste(
            "Run Name specified in GET request does not exist: ",
            get.run,
            ". Select a valid run to continue.",
            sep = ""
          )
        )
      }
    }
    
  })
  
  # Changing the Run Report updates the current.run and the Studies that can be selected
  # If any error occurs in loading report, current.run is set to NULL (nothing will be plotted) and error message is set
  observeEvent(input$run, {
    req(input$run)
    
    tryCatch({
      # Remove any previous error messages
      error.msg(NULL)
      current.run(createAppDT(all.runs.dt()[name == input$run, path]))

      all.studies.count <- current.run()[, .N, by = Study]
      all.studies.list <- as.list(all.studies.count$Study)
      names(all.studies.list) <- paste(all.studies.count$Study, "(", as.character(all.studies.count$N), ")", sep = "")
      updateSelectInput(session, "study", choices = all.studies.list, selected = all.studies.count$Study)
      
      all.lane.count <- current.run()[, .N, by = Lane]
      all.lane.list <- as.list(all.lane.count$Lane)
      names(all.lane.list) <- paste(all.lane.count$Lane, "(", as.character(all.lane.count$N), ")", sep = "")
      updateSelectInput(session, "lane", choices = all.lane.list, selected = all.lane.count$Lane)
      
      # Add links that lead to useful places
      output$notificationMenu <- renderMenu({
        dropdownMenu(
          notificationItem("SeqWare Run Report", status = "info", icon = icon("link", lib = "glyphicon"), href = generateRunReportURL(input$run)),
          notificationItem("MISO Run Report", status = "info", icon = icon("link", lib = "glyphicon"), href = generateMisoRunURL(input$run)),
          type= "notification", headerText = "Useful Links", icon = icon("link", lib = "glyphicon")
        )
      })
    }, error = function(err) {
      err.msg <-
        paste(
          "Failed to read Run Report TSV file for the following reason:",
          conditionMessage(err),
          sep = "\n"
        )
      error.msg(err.msg)
    })
  })
  
  # Once a Run Report or Study is changed, update Coverage slider
  observeEvent(c(input$run, input$study), {
    req(current.run())
    
    selected.study <- current.run()[Study %in% input$study,]
    req(nrow(selected.study) > 0)
    
    coverage.max <- max(selected.study[, "Coverage (collapsed)"])
    updateSliderInput(session,
                      "slider.coverage",
                      max = coverage.max,
                      value = c(0, coverage.max))
  })
  
  # Recalculates the data table to plot every time any variable within this expression is changed
  dt.to.plot <- reactive({
    req(current.run())
    
    selected.study <- current.run()[Study %in% input$study & Lane %in% input$lane,]
    req(nrow(selected.study) > 0)
    
    lane.levels <- sort(unique(selected.study$Lane))
    
    # Make Lanes a factor rather than number
    set(
      selected.study,
      j = "Lane",
      value = factor(selected.study$Lane,
                     levels = lane.levels)
    )
    
    # Order by Lane first and then by selected metric
    setorderv(selected.study,
              c("Lane", input$order.by),
              order = c(1, ifelse(input$order.rev,-1, 1)))
    
    # Libraries should also be factors rather than strings
    set(
      selected.study,
      j = "Library",
      value = factor(
        selected.study$Library,
        levels = unique(selected.study$Library, ordered = TRUE)
      )
    )
    
    # Filter by Coverage
    selected.coverage <- input$slider.coverage
    selected.study <-
      selected.study[`Coverage (collapsed)` >= selected.coverage[1] &
                       `Coverage (collapsed)` <= selected.coverage[2],]
    
    # Keep only the metrics that will be plotted
    # As the data table contains info fields not part of input$check.type, take away metrics that will not be plotted
    return(selected.study[, !setdiff(CONFIG.ALLPLOTS, input$check.type), with = FALSE])
  })
  
  # Create the plot
  output$plot <- renderPlotly({
    req(input$check.type, nrow(dt.to.plot()) > 0)
    
    temp.to.plot <- createLong(dt.to.plot())
    temp.to.plot <- split(temp.to.plot, by = "Type")
    
    legend <- TRUE
    plots.all <- lapply(temp.to.plot, function (x) {
      stat.type <- as.character(x[["Type"]][1])
      temp.plot <- x %>%
        plot_ly(x = ~ Library, color = paste0("Lane ", x$Lane)) %>%
        add_bars(
          y = ~ Value,
          showlegend = legend,
          legendgroup = paste0("Lane ", x$Lane)
        ) %>%
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
    nrows <-
      ifelse(length(temp.to.plot) > 10,
             two.columns,
             length(temp.to.plot))
    subplot(plots.all, nrows = nrows, titleY = TRUE)
  })
}

shinyApp(ui, server, options = list(port = CONFIG.PORT))