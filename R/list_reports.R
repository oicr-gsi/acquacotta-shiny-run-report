# Expose a function that delivers a data table linking Run Report name to file path

# This assumes a base folder full of run folders that contain the TSV and JSON files
# file: The filename of the SeqWare TSV Run Report
# path: The path to the SeqWare TSV file
# dir_path: The path to the directory holding the SeqWare TSV file
# name: The human readable name that is exposer to user
listRunReports <- function(base_dir) {
  full_path <- normalizePath(base_dir)

  path <- list.files(full_path, pattern = ".*\\.tsv$", full.names = TRUE, recursive = TRUE)
  file <- basename(path)
  name <- stringr::str_match(file, "(.*?)_report.tsv")[, 2]
  dt <- data.table(path = path,
                   dir_path = dirname(path),
                   file = file,
                   name = name)

  # If two or more TSV files share the same name, their parent folder name is prepanded to remove ambiguity
  dup_name <- duplicated(dt$name) | duplicated(dt$name, fromLast = TRUE)
  unambiguous_name <- paste(
    basename(dt[dup_name, dir_path]),
    dt[dup_name, name],
    sep = "/"
  )
  dt$name[dup_name] <- unambiguous_name

  return(dt)
}
