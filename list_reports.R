# Expose a function that delivers a data table linking Run Report name to file path

library(data.table)
library(stringr)

listRunReports <- function() {
  path <- list.files("./runreport_files/", full.names = TRUE)
  file <- sapply(strsplit(path, "/"), function(x) x[4])
  name <- str_match(file, "(.*?)_report.tsv")[,2]
  dt <- data.table(path = path, file = file, name = name)
  
  if (nrow(dt) != length(unique(dt$name))) {
    stop("Duplicated run report names were found")
  }
  
  return(dt)
}
