import glob
import shutil

glob_path = "/mnt/HPC/oicr/data/archive/web/runReports/**/*_report.tsv"

for filename in glob.iglob(glob_path):
    shutil.copy2(filename, "./runreport_files")

