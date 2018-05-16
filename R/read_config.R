# CONFIG <- read_yaml("./config.yml")
# CONFIG.RUNPATH <- CONFIG$app_instance$run_report_dir
# CONFIG.ALLPLOTS <- CONFIG$app_instance$plots
# CONFIG.DEFAULTPLOTS <- CONFIG$default$plots
# CONFIG.PORT <- CONFIG$app_instance$port
# CONFIG.COLUMN <- CONFIG$app_instance$column_naming
# CONFIG.INFO.COLUMN <- CONFIG$app_instance$info_column
# CONFIG.DEFAULTORDER <- CONFIG$default$order$plot
# CONFIG.DEFAULTORDERREV <- CONFIG$default$order$reverse

# Name of source variable for library plots that allows user interaction
CONFIG.ID.LIBPLOT.SOURCE = "library_plot"

CONFIG.RUNPATH <- "./runreport_files"
CONFIG.ALLPLOTS <- c(
  "Coverage (collapsed)", "On Target Percentage", "Map Percent", "PF Reads", "Reads/SP", "Insert Mean",
  "Insert Stddev", "R1 Mismatch Percent", "R2 Mismatch Percent", "R1 Indel Percent", "R2 Indel Percent",
  "R1 Soft Clip Percent", "R2 Soft Clip Percent", "R1 Length", "R2 Length", "Percent Mapped on Target",
  "Estimated Yield (collapsed)", "PF Yield", "Target Size (bp)", "Number of Targets"
)

CONFIG.DEFAULTPLOTS <- c("Coverage (collapsed)", "On Target Percentage", "Map Percent", "PF Reads", "Reads/SP", "Insert Mean")

CONFIG.VALID.FIELD <- fread(normalizePath("./config/column_naming.csv"))

CONFIG.INFO.COLUMN <- c(
  "Library", "Barcode", "Lane", "Run Name", "Study", "Group ID", "External Name", "Target File",
  "plotly_library_selected", "plotly_unique_key"
)

CONFIG.DEFAULTORDER <- "Coverage (collapsed)"
CONFIG.DEFAULTORDERREV <- TRUE
