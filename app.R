source("./utility.R")
library(ggplot2)
library(plotly)
library(data.table)
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

single.run <- readSeqWareTSV("./170203_D00343_0160_ACACFLANXX_report.tsv")
single.run <- fixSeqWareTSV(single.run)

single.run.split <- createLong(single.run)
set(single.run.split, j = "Lane", value = factor(single.run.split$Lane, levels = 1:8, ordered = TRUE))
setorder(single.run.split, Lane, -Coverage)
set(single.run.split, j = "Library", value = factor(single.run.split$Library, levels = unique(single.run.split$Library, ordered = TRUE)))

single.run.study <- split(single.run.split, by = "Study")

all.studies <- unique(single.run$Study)
initial.study <- all.studies[[1]]
initial.lib <- single.run[Study == initial.study, "Library"][[1]]
initial.coverage.max <- max(single.run[Study == initial.study, "Coverage"])
initial.plot.types <- unique(single.run.split$Type)

# Define UI for application that draws the plot
ui <- fillPage(
  fillCol(
    # sidebarLayout(
    #  sidebarPanel(
    #    selectInput("study", "Select Study", unique(single.run$Study)),
    #    checkboxGroupInput("library", "Select Library", single.run$Library)
    #  ),
    #  mainPanel(
    #    div(style='min-height:100px; overflow-y: scroll',
    #        plotlyOutput("gecco", height = "100%")
    #    )
    #    )
    # )
    fillRow(
      fillCol(
        selectInput("study", "Select Study", unique(single.run$Study), selected = initial.study)
        #actionButton("selectAllLib", "All Libraries"),
        #actionButton("removeAllLib", "No Libraries"),
        #actionButton("updateLib", "Update Libraries")
      ),
      #div(style='height:200px; overflow-y: scroll',
      #checkboxGroupInput("library", "Select Library", initial.lib, selected = initial.lib)
      #)
      sliderInput("slider.coverage", "Coverage", 0, initial.coverage.max, value = c(0, initial.coverage.max)),
      div(
        style='height:200px; overflow-y: scroll',
        checkboxGroupInput(
          "check.type", "Select Plots", initial.plot.types, 
          selected = c("Coverage", "PF Reads", "Map Percent", "Reads/SP", "Percent mapped on Target", "Insert Size")
        )
      )
    ),
    plotlyOutput("gecco", height = "100%"),
    
    flex = c(2, 8)
  )
) 

# Define server logic required to draw plot
server <- function(input, output, session) {
  observeEvent(input$study, {
    selected.study <- single.run.study[[input$study]]
    single.study.type <- split(selected.study, by = "Type")
    
    coverage.max <- max(selected.study[, "Coverage"])
    updateSliderInput(session, "slider.coverage", max = coverage.max, value = c(0, coverage.max))
  })
  
  dt.to.plot <- reactive({
    selected.study <- single.run.study[[input$study]]
    selected.coverage <- input$slider.coverage
    selected.study <- selected.study[Coverage >= selected.coverage[1] & Coverage <= selected.coverage[2],]
    
    selected.study <- split(selected.study, by = "Type")
    selected.type <- input$check.type
    type.to.keep <- names(selected.study) %in% selected.type
    return(selected.study[type.to.keep])
  })
  
  output$gecco <- renderPlotly({
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

# Run the application 
shinyApp(ui = ui, server = server)

