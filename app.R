source("./utility.R")
source("./list_reports.R")
library(ggplot2)
library(plotly)
library(data.table)
library(shiny)
library(shinydashboard)





all.runs.dt <- listRunReports()

current.run <- createAppDT(all.runs.dt[nrow(all.runs.dt), path])

all.studies <- names(current.run)
initial.study <- all.studies[[1]]
initial.dt <- current.run[[initial.study]]
initial.coverage.max <- max(initial.dt[, "Coverage"])
initial.plot.types <- unique(initial.dt$Type)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
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
    # The css selector and the id of plotlyOutput must match
    plotlyOutput("plot", height = "100%")
  )
)

server <- function(session, input, output) { 
  observeEvent(input$study, {
    selected.study <- current.run[[input$study]]
    single.study.type <- split(selected.study, by = "Type")
    
    coverage.max <- max(selected.study[, "Coverage"])
    updateSliderInput(session, "slider.coverage", max = coverage.max, value = c(0, coverage.max))
  })
  
  dt.to.plot <- reactive({
    selected.study <- current.run[[input$study]]
    selected.coverage <- input$slider.coverage
    selected.study <- selected.study[Coverage >= selected.coverage[1] & Coverage <= selected.coverage[2],]
    
    selected.study <- split(selected.study, by = "Type")
    selected.type <- input$check.type
    type.to.keep <- names(selected.study) %in% selected.type
    return(selected.study[type.to.keep])
  })
  
  output$plot <- renderPlotly({
    temp.to.plot <- dt.to.plot()
    
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

