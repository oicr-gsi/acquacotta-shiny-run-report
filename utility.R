source("./read_config.R")

library(data.table)

# This file contains information about the expected column names, name conversions, and column types
COLUMN.NAME <- fread(CONFIG.COLUMN)

fixSeqWareTSV <- function(t.df) {
  t.df.colnames <- colnames(t.df)
  
  # Library column is duplicated for some reason in Run Report TSV files. This removes is.
  # Note that if duplicated column names with different data exist, the second one will be ignored.
  t.df.colnames <- unique(t.df.colnames)
  
  # Create a data table for each column
  dt.list <- lapply(t.df.colnames, function(x) {
    conversion <- COLUMN.NAME[file.name == x, ]
    data <- t.df[[x]]
    
    if (nrow(conversion) != 1) {
      stop(
        paste(
          "Field name",
          x,
          "cannot be unambiguously parsed. It is found",
          nrow(conversion),
          "times in the annotation file. Expected 1."
        )
      )
    }
    
    # Split Read Length from a character column into two numeric columns
    if (conversion$app.name == "#Split_Read1_Read2") {
      read.length <- sapply(data, function(x)
        strsplit(x, ",")[[1]])
      data <-
        as.data.table(list(
          `R1 Length` = as.numeric(read.length[1, ]),
          `R2 Length` = as.numeric(read.length[2, ])
        ))
      return(data)
    }
    
    # Split insert size and SD from a character column into two numeric columns
    if (conversion$app.name == "#Split_Mean_SD") {
      in.sd <- gsub("[\\(\\)]", "", data)
      in.sd <- sapply(in.sd, function(x)
        strsplit(x, " ")[[1]])
      data <-
        as.data.table(list(
          `Insert Mean` = as.numeric(in.sd[1, ]),
          `Insert Stddev` = as.numeric(in.sd[2, ])
        ))
      return(data)
    }
    
    # If it is a numeric type, only leave numbers and periods
    if (conversion$type == "Numeric") {
      data <- gsub("[^0-9\\.]", "", data)
      data <- as.numeric(data)
    }
    
    result <- list()
    
    # The column is renamed to canonical name
    result[[conversion$app.name]] <- data
    return(as.data.table(result))
  })
  
  result.dt <- as.data.table(dt.list)
  
  # Add Study name
  set(result.dt,
      j = "Study",
      value = sapply(strsplit(result.dt$Library, "_"), function(x)
        x[[1]]))
  
  return(result.dt)
}

createLong <- function(t.df) {
  epic.dt.long <-
    melt(
      t.df,
      id.vars = intersect(CONFIG.INFO.COLUMN, colnames(t.df)),
      variable.name = "Type",
      value.name = "Value"
    )
  
  # Order the different metrics, so that they will always be displayed in the same order
  # Only present metrics can be included in factor levels, as data.table::split bugs out otherwise
  set(
    epic.dt.long,
    j = "Type",
    value = factor(
      epic.dt.long$Type,
      levels = intersect(CONFIG.ALLPLOTS, epic.dt.long$Type)
    )
  )
  setorder(epic.dt.long, Type)
  
  return(epic.dt.long)
}

# This function loads the file and adds a column with the path
# This is necessary as the TSV file itself does not tell you which run it came from
# Any warning is thrown as an error, as fread tries its best to read even nonstandard files
readSeqWareTSV <- function(path) {
  tryCatch({
    dt <-
      fread(
        path,
        sep = "\t",
        header = TRUE,
        na.strings = c("NA", "na")
      )
    set(dt, j = "Run Name", value = factor(rep(path, nrow(dt))))
    
  }, warning = function(w) {
    stop(conditionMessage(w))
  })
}

# The data table that will be used throughout the app, starting from the path to the Run Report
createAppDT <- function(path) {
  current.run <- readSeqWareTSV(path)
  current.run <- fixSeqWareTSV(current.run)
  return(current.run)
}

generateRunReportURL <- function(run_alias) {
  paste("https://www.hpc.oicr.on.ca/archive/web/runReports/", run_alias, "/", run_alias, "_report.html", sep = "")
}

generateMisoRunURL <- function(run_alias) {
  paste("https://miso.oicr.on.ca/miso/run/alias/", run_alias, sep = "")
}