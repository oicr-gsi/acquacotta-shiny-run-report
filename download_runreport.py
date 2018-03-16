#!/usr/bin/python3

import glob
import shutil
import argparse
import os.path
import sys
import os

HPC_PATH = "/oicr/data/archive/web/runReports"
HPC_GLOB = "**/*_report.tsv"

parser = argparse.ArgumentParser(
    description = """Obtain information about the Run Report TSV files
    produced by SeqWare and hosted on the OICR HPC. By default prints all Run
    Reports to stdout.""",
    # Show the defaults if supplied
    formatter_class = argparse.ArgumentDefaultsHelpFormatter
)

parser.add_argument(
    "-p", "--path", default = HPC_PATH
    help = "Path to HPC Run Report folder"
)

parser.add_argument(
    "-g", "--glob", default = HPC_GLOB,
    help = """The pathname pattern expansion used to list all Run Report files
    using Python glob library"""
)

parser.add_argument(
    "-s", "--save",
    help = """Run Reports will be saved if path is specified"""
)

parser.add_argument(
    "-f", "--force", action="store_true",
    help = """Existing files and folders will be overwritten"""
)

arg_input = parser.parse_args()

if arg_input.save is not None:
    if os.path.exists(arg_input.save):
        if arg_input.force:
            shutil.rmtree(arg_input.save)
        else:
            print(
                """{} cannot be created. If it already exists, use the -f flag
                to overwrite the folder.""".format(arg_input.save),
                file=sys.stderr
            )
            exit(1)

    os.makedirs(arg_input.save)


for filename in glob.iglob(os.path.join(arg_input.path, arg_input.glob)):
    print(filename)
    if arg_input.save is not None:
        shutil.copy2(filename, arg_input.save)
