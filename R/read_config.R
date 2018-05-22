#' Default values for Acquacotta.
#'
#' @export
createConfig <- function() {
  list(
    runpath = "./runreport_files",
    all_plots = c(
      "Coverage (collapsed)", "On Target Percentage", "Map Percent", "PF Reads", "Reads/SP", "Insert Mean",
      "Insert Stddev", "R1 Mismatch Percent", "R2 Mismatch Percent", "R1 Indel Percent", "R2 Indel Percent",
      "R1 Soft Clip Percent", "R2 Soft Clip Percent", "R1 Length", "R2 Length", "Percent Mapped on Target",
      "Estimated Yield (collapsed)", "PF Yield", "Target Size (bp)", "Number of Targets"
    ),
    default_plots = c("Coverage (collapsed)", "On Target Percentage", "Map Percent", "PF Reads", "Reads/SP", "Insert Mean"),
    default_order = "Coverage (collapsed)",
    default_order_reversed = TRUE,
    info_columns = c(
      "Library", "Barcode", "Lane", "Run Name", "Study", "Group ID", "External Name", "Target File",
      "plotly_library_selected", "plotly_unique_key"
    ),
    valid_column_path = "./config/column_naming.csv",
    # Name of source variable for library plots that allows user interaction
    plotly_id_lib_source = "library_plot"
  )
}
