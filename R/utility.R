#' @import stringr

fixSeqWareTSV <- function(t.df) {
  t.df.colnames <- colnames(t.df)

  # Library column is duplicated for some reason in Run Report TSV files. This removes is.
  # Note that if duplicated column names with different data exist, the second one will be ignored.
  t.df.colnames <- unique(t.df.colnames)

  # Create a data table for each column
  dt.list <- lapply(t.df.colnames, function(x) {
    conversion <- loadValidFields()[file.name == x, ]
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
  result.dt <- addCustomTSVMetrics(
    result.dt,
    "Study",
    sapply(strsplit(result.dt$Library, "_"), function(x) x[[1]])
  )

  # Add On Target Percentage
  result.dt <- addCustomTSVMetrics(
    result.dt,
    "On Target Percentage",
    result.dt$`Percent Mapped on Target` * result.dt$`Map Percent` / 100
  )

  # Field keeps track if a library has been selected by user
  result.dt <- addCustomTSVMetrics(
    result.dt,
    "plotly_library_selected",
    FALSE
  )

  # Field that must be unique for each library. For illumina that is: Library name + lane + barcode
  result.dt <- addCustomTSVMetrics(
    result.dt,
    "plotly_unique_key",
    paste(result.dt$Library, result.dt$Lane, result.dt$Barcode, sep = "_")
  )

  return(result.dt)
}

# Add fields not present in the original TSV file
addCustomTSVMetrics <- function(dt, field_name, field_values) {
  if (field_name %in% colnames(dt)) {
    stop(
      paste(
        "Custom field name cannot be added as it already exists:",
        field_name
      )
    )
  }

  if (!(field_name %in% loadValidFields()$app.name)) {
    stop(
      paste(
        "Custom field not specified in annotation file:",
        field_name
      )
    )
  }

  set(dt, j = field_name, value = field_values)
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

    if ("Run Name" %in% colnames(dt)) {
      stop("Column 'Run Name' is used internally by the App and cannot be present in Run Report")
    }
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

loadValidFields <- function() {
  valid_fields <- fread(normalizePath(CONFIG.VALID.FIELD))
  return(valid_fields)
}

generateRunReportURL <- function(run_alias) {
  paste("https://www.hpc.oicr.on.ca/archive/web/runReports/", run_alias, "/", run_alias, "_report.html", sep = "")
}

generateMisoRunURL <- function(run_alias) {
  paste("https://miso.oicr.on.ca/miso/run/alias/", run_alias, sep = "")
}
