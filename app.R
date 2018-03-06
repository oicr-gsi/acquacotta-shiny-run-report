source("./utility.R")
source("./list_reports.R")
library(ggplot2)
library(plotly)
library(data.table)
library(shiny)
library(shinydashboard)





all.runs.dt <- listRunReports()
current.dt <- all.runs.dt[nrow(all.runs.dt), ]

current.run <- createAppDT(current.dt$path)

all.studies <- names(current.run)
initial.study <- all.studies[[1]]
initial.dt <- current.run[[initial.study]]
initial.coverage.max <- max(initial.dt[, "Coverage"])
initial.plot.types <- unique(initial.dt$Type)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    selectInput("run", "Select Run Report", all.runs.dt$name, selected = current.dt$name),
    
    selectInput("study", "Select Study", all.studies, selected = initial.study),
    
    sliderInput("slider.coverage", "Coverage", 0, initial.coverage.max, value = c(0, initial.coverage.max)),
    
    checkboxGroupInput(
      "check.type", "Select Plots", initial.plot.types, 
      selected = c("Coverage", "PF Reads", "Map Percent", "Reads/SP", "Percent mapped on Target", "Insert Size")
    )
  ),
  
  dashboardBody(
    # The css selector allows for the plot to be full height: https://stackoverflow.com/questions/36469631/how-to-get-leaflet-for-r-use-100-of-shiny-dashboard-height
    tags$style(type = "text/css", "#plot {height: calc(100vh - 80px) !important;}"),
    
    verbatimTextOutput("error_run"),
    
    # The css selector and the id of plotlyOutput must match
    plotlyOutput("plot", height = "100%")
  )
)

server <- function(session, input, output) {
  observeEvent(input$run, {
    tryCatch({
      output$error_run <- renderPrint(invisible())
      current.run <<- createAppDT(all.runs.dt[name == input$run, path])
      all.studies <- names(current.run)
      updateSelectInput(session, "study", choices = all.studies)
    }, error = function(err) {
      current.run <<- NULL
      err.msg <- paste("Failed to read Run Report TSV file for the following reason:", conditionMessage(err), sep = "\n")
      output$error_run <- renderText(err.msg)
    })
  })
  
  observeEvent(c(input$run, input$study), {
    selected.study <- current.run[[input$study]]
    req(selected.study)
    
    single.study.type <- split(selected.study, by = "Type")
    
    coverage.max <- max(selected.study[, "Coverage"])
    updateSliderInput(session, "slider.coverage", max = coverage.max, value = c(0, coverage.max))
  })
  
  dt.to.plot <- reactive({
    req(input$run)

    selected.study <- current.run[[input$study]]
    req(selected.study)
    
    lane.levels <- sort(unique(selected.study$Lane))
    
    # Make Lanes a factor rather than number and sort by lane and then by coverage
    set(selected.study, j = "Lane", value = factor(selected.study$Lane, levels = lane.levels, ordered = TRUE))
    setorder(selected.study, Lane, -Coverage)
    
    # Libraries should also be factors rather than strings
    set(selected.study, j = "Library", value = factor(selected.study$Library, levels = unique(selected.study$Library, ordered = TRUE)))
    
    selected.coverage <- input$slider.coverage
    selected.study <- selected.study[Coverage >= selected.coverage[1] & Coverage <= selected.coverage[2],]
    
    selected.study <- split(selected.study, by = "Type")
    selected.type <- input$check.type
    type.to.keep <- names(selected.study) %in% selected.type
    return(selected.study[type.to.keep])
  })
  
  output$plot <- renderPlotly({
    temp.to.plot <- dt.to.plot()
    req(temp.to.plot)

    # Removes the cryptic error message if the parameters are such that there are no data points to print
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
    
    two.columns <- ceiling(length(input$check.type) / 2)
    nrows <- ifelse(length(input$check.type) > 10, two.columns, length(input$check.type))
    subplot(plots.all, nrows = nrows, titleY = TRUE)
  })  
}

shinyApp(ui, server)

