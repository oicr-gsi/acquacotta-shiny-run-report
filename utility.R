library(data.table)

fixSeqWareTSV <- function(t.df) {
  # Remove commas from numeric values
  set_numeric <- c("PF Reads", "PF Yield", "Estimated Yield", "Target Size (bp)", "Number of Targets")
  sapply(set_numeric, function(x) {
    set(t.df, j = x, value = as.numeric(gsub(",", "", t.df[, x, with = FALSE][[1]])))
  })
  
  # Remove percentages from numeric values
  remove_percent <- c(
    "Map Percent", "R1 Mismatch Percent", "R2 Mismatch Percent", "R1 Indel Percent", 
    "R2 Indel Percent", "R1 Soft Clip Percent", "R2 Soft Clip Percent", "Percent mapped on Target"
  )
  sapply(remove_percent, function(x) {
    set(t.df, j = x, value = as.numeric(gsub("%", "", t.df[, x, with = FALSE][[1]])))
  })
  
  # Remove the x unicode sign from converage numeric values
  remove_x_unicode <- "Coverage"
  set(t.df, j = remove_x_unicode, value = as.numeric(gsub("×", "", t.df[, remove_x_unicode, with = FALSE][[1]])))
  
  # Split insert size and SD from a character column into two numeric columns
  in.sd <-gsub("[\\(\\)]", "", t.df$`Insert Mean (SD)`)
  in.sd<-sapply(in.sd, function(x) strsplit(x, " ")[[1]])
  set(t.df, j = "Insert Size", value = as.numeric(in.sd[1,]))
  set(t.df, j = "Insert SD", value = as.numeric(in.sd[2,]))
  set(t.df, j = "Insert Mean (SD)", value = NULL)
  
  # Split Read Length from a character column into two numeric columns
  read.length<-sapply(t.df$`Read Length`, function(x) strsplit(x, ",")[[1]])
  set(t.df, j = "R1 Length", value = as.numeric(read.length[1,]))
  set(t.df, j = "R2 Length", value = as.numeric(read.length[2,]))
  set(t.df, j = "Read Length", value = NULL)
  
  # Remove duplicated columns
  t.df <- t.df[,unique(names(t.df)),with=FALSE]
  
  # Add Study name
  set(t.df, j = "Study", value = sapply(strsplit(t.df$Library, "_"), function(x) x[[1]]))
  
  return(t.df)
}

createLong <- function(t.df) {
  epic.dt.long <- melt(t.df[, !c("Target File")], id.vars = c("Library", "Barcode", "Lane", "Run Name", "Coverage", "Study"), variable.name = "Type", value.name = "Value")
  
  # Add back the coverage metrics to it generates its own graph
  epic.dt.coverage <- epic.dt.long[Type == epic.dt.long[1,]$Type, ]
  set(epic.dt.coverage, j = "Value", value = epic.dt.coverage$Coverage)
  set(epic.dt.coverage, j = "Type", value = rep("Coverage", nrow(epic.dt.coverage)))
  
  epic.dt.long <- rbindlist(list(epic.dt.coverage, epic.dt.long))
  
  # Make Lanes a factor rather than number and sort by lane and then by coverage
  set(epic.dt.long, j = "Lane", value = factor(epic.dt.long$Lane, levels = 1:8, ordered = TRUE))
  setorder(epic.dt.long, Lane, -Coverage)
  
  # Libraries should also be factors rather than strings
  set(epic.dt.long, j = "Library", value = factor(epic.dt.long$Library, levels = unique(epic.dt.long$Library, ordered = TRUE)))
  return(epic.dt.long)
}

# This function loads the file and adds a column with the path
# This is necessary as the TSV file itself does not tell you which run it came from
readSeqWareTSV <- function(path) {
  dt <- fread(path)
  set(dt, j = "Run Name", value = factor(rep(path, nrow(dt))))
}

# The data table that will be used throughout the app, starting from the path to the Run Report
createAppDT <- function(path) {
  current.run <- readSeqWareTSV(path)
  current.run <- fixSeqWareTSV(current.run)
  current.run <- createLong(current.run)
  current.run <- split(current.run, by = "Study")
  return(current.run)
}
