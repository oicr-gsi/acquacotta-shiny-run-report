library(shinydashboard)
library(shiny)
library(data.table)
library(plotly)
library(stringr)
library(shinyjs)

source("./R/read_config.R")
source("./R/utility.R")
source("./R/list_reports.R")
source("./R/create_ui.R")
source("./R/create_server.R")

shinyApp(createAcquacottaUI(), createAcquacottaServer(), options = list(port = CONFIG.PORT))
