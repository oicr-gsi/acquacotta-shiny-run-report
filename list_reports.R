# Expose a function that delivers a data table linking Run Report name to file path

library(data.table)
library(stringr)

source("./read_config.R")

# file: The filename of the SeqWare TSV Run Report
# path: The path to the SeqWare TSV file
# dir_path: The path to the directory holding the SeqWare TSV file
# name: The human readable name that is exposer to user
listRunReports <- function() {
  path <- list.files(normalizePath(CONFIG.RUNPATH), pattern = ".*tsv$", full.names = TRUE, recursive = TRUE)
  file <- basename(path)
  name <- str_match(file, "(.*?)_report.tsv")[, 2]
  dt <- data.table(path = path,
                   dir_path = dirname(path),
                   file = file,
                   name = name)
  
  # This is temporary code to exclude the TSV files in the base directory from eariler app version
  valid_dir <- list.dirs(path = normalizePath(CONFIG.RUNPATH), full.names = TRUE)
  valid_dir <- valid_dir[2:length(valid_dir)]
  dt <- dt[dt$dir_path %in% valid_dir, ]
  
  if (nrow(dt) != length(unique(dt$name))) {
    stop(simpleError("Duplicated run report names were found"))
  }
  
  return(dt)
}