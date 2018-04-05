library(yaml)

CONFIG <- read_yaml("./config.yaml")
CONFIG.RUNPATH <- CONFIG$app_instance$run_report_dir
CONFIG.ALLPLOTS <- CONFIG$app_instance$plots
CONFIG.DEFAULTPLOTS <- CONFIG$default$plots
CONFIG.PORT <- CONFIG$app_instance$port
CONFIG.COLUMN <- CONFIG$app_instance$column_naming
CONFIG.INFO.COLUMN <- CONFIG$app_instance$info_column
CONFIG.DEFAULTORDER <- CONFIG$default$order$plot
CONFIG.DEFAULTORDERREV <- CONFIG$default$order$reverse