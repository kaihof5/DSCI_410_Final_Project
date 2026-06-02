# DSCI 410 Final Project - Kai Hoffman (WORK IN PROGRESS)
DSCI 410's Final Project involving CAHOOTS, EPD, SPD, and MCSLC data.

This repository contains the R-scripts for cleaning, preparation, analysis, and visualization creation of the raw data collected by Dr. Rohlfs for the DSCI 410 class at the University of Oregon.  
Each of the scripts have instructions on how to run them within the file, as well as descriptions of what the code is doing.

The **R Packages** needed to run these scripts are:
- tidyverse  
- readxl

## All scripts should be put in a folder containing:
- The "2015-2025 SPD Calls for Service.xlsx" file - Downloaded from Canvas files
- The "MCSLC.xlsc" file - Downloaded from Canvas files
- The "Eugene_CAD_data_noloc" folder - .zip downloaded from Canvas, and extracted into the folder
- The `Data_Prep.r`, `All_Data_Sync.r` and `Analysis+Visuals.r`files - Downloaded from this repo

Before you run the scripts the folder should look something like this:
<img width="717" height="261" alt="Screenshot 2026-05-05 235705" src="https://github.com/user-attachments/assets/b2e35277-ee27-485a-a349-0fbbde130987" />

## For those unfamiliar with R-Studio:  
- First download R-studio and R by following these instructions: https://rstudio-education.github.io/hopr/starting.html
- Once R-Studio is installed, double-click the `Data_Prep.r` file and it should open.
- Make sure to install the required packages from above, following instructions from this link: https://libguides.chapman.edu/R/packages#:~:text=Installing%20Packages&text=The%20default%20option%20is%20to,the%20Script%20Editor%2C%20type%20install.
- Navigate to the Session dropdown in the top bar, then Set Working Directory, then Choose Directory..., and then navigate to your folder containing all the data.
  <img width="691" height="508" alt="Screenshot 2026-05-06 000248" src="https://github.com/user-attachments/assets/48b74d49-bf69-4ac9-b6eb-7340aa304724" />
- To run the code within R-Studio, either press Ctrl+Enter to run on the selected lines of code, or Alt+Ctrl+R to run the whole script at once.

# The files should be ran in the following order:  
- [`Data_Prep.r`](<https://github.com/kaihof5/DSCI_410_Final_Project/blob/main/Data_Prep.R>) <- Combines and prepares all raw data.
  - Output: "CompiledData" folder with compiled versions of raw data
- [`All_Data_Sync.r`](<https://github.com/kaihof5/DSCI_410_Final_Project/blob/main/All_Data_Sync.R>) <- Identifies CAHOOTS data and combines all datasets into one.
  - Output: "AllData.csv" file with all 2m+ data entries prepared for analysis
- [`Analysis+Visuals.r`](<https://github.com/kaihof5/DSCI_410_Final_Project/blob/main/Analysis+Visuals.R>) <- Runs Analysis and creates accompanying visuals.
  - Output: Plots and tables that decribe the data in "AllData.csv"
