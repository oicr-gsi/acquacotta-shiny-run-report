library(yaml)

CONFIG <- read_yaml("./config.yaml")
CONFIG.RUNPATH <- CONFIG$app_instance$run_report_dir
CONFIG.ALLPLOTS <- CONFIG$app_instance$all_plots
CONFIG.DEFAULTPLOTS <- CONFIG$app_instance$default_plots
CONFIG.PORT <- CONFIG$app_instance$port
CONFIG.COLUMN <- CONFIG$app_instance$column_naming
CONFIG.INFO.COLUMN <- CONFIG$app_instance$info_column