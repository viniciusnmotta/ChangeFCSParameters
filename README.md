# Change Parameter Names in .FCS file
This repository contains script to change names and parameters in .fcs files.
You can run this script locally in your computer by type the following commands in Rstudio:
1. install.packages(shiny)
2. library(shiny)
3. runGitHub("ChangeFCSParameters","viniciusnmotta")

The script will open an interface on your web browser.

Changes can be made in an excel-like table in your browser, i.e select cell type your changes or copy and paste values from an excel file

The script will change only parameters name. All metada associated with your .fcs files will be preserved and not altered.
For instance, information about the date of acquisition, instrument, operator, CST baseline and so forth will remain untouched in the new files.

The script creates a new folder ("NEW_files") within the folder containing the original files where the modified .fcs files with the new parameters names are written.

Even though original files are not altered, you should consider back up your original files before running this script.

I hope you find it helpful if you ever need to change parameter names in .fcs file.

Please see the PDF file for a step-by-step tutorial how to use the application.

All the best,

