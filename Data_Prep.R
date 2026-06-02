#Kai Hoffman DSCI 410 data prep

#README
#Run this script in a folder containing all the raw data downloaded from the course "files" page
#This means that in this directory there should be:
#MCSLC.xlsx
#2015-2025 SPD Calls for Service.xlsx
#Eugene_CAD_data_noloc folder


###Loading necessary libraries
library("tidyverse")
library("readxl")

###Make sure above libraries are loaded and environment is empty before running script
rm(list=ls())


###Loading the data
MCSLC = read_excel("MCSLC.xlsx")


#Iterating through years to make importing EPD and SPD Data Easier
yearStart = 2015
yearEnd = 2025

for (i in yearStart:yearEnd) {
  assign(paste0("SPD",i), read_excel("2015-2025 SPD Calls for Service.xlsx", sheet = as.character(i)))
  assign(paste0("EPD",i), read_csv(str_glue("Eugene_CAD_data_noloc/EugeneCAD{as.character(i)}noloc.csv")))
  
}
rm(i)
##Putting all EPD and SPD data together

#Fix SPD2019$Priority character type problem so bind_rows() will work
SPD2019 = SPD2019 %>%
  mutate(Priority = as.numeric(Priority))
#Same for SPD 2021
SPD2021 = SPD2021 %>%
  mutate(Priority = as.numeric(Priority))
#Same for SPD 2025
SPD2025 = SPD2025 %>%
  mutate(Priority = as.numeric(Priority))

#Assign to first year data so we bind_rows() will work
EugeneAll = get(paste0("EPD",yearStart))
SpringfieldAll = get(paste0("SPD",yearStart))

for (i in (yearStart+1):yearEnd) {
  #iteratively add year i data to datasets
  EugeneAll = bind_rows(EugeneAll, get(paste0("EPD",i)))
  SpringfieldAll = bind_rows(SpringfieldAll, get(paste0("SPD",i)))
  #print(i)
  
}
rm(i)

###Get rid of singular year variables to free up space
rm(list = ls(pattern = "^EPD"))
rm(list = ls(pattern = "^SPD"))


###Select only variables of interest
SpringfieldAll = SpringfieldAll %>%
  select(`Call Creation Time`, `Primary Responding Unit`, `Final Call Type`)

EugeneAll = EugeneAll %>% 
  select(calltime, primeunit, agency, nature)

MCSLC = MCSLC %>% 
  select(`Dispatch Request Date & Time`, City, `Reason for Dispatch #1`)

###Harmonizing column types and names across datasets
#Harmonzing names
SpringfieldAll = SpringfieldAll %>%
  rename(Time = `Call Creation Time`, CAHOOTSBinary = `Primary Responding Unit`, CallType = `Final Call Type`)

EugeneAll = EugeneAll %>%
  rename(Time = calltime, CAHOOTSBinary = primeunit, CallType = nature)

MCSLC = MCSLC %>%
  rename(Time = `Dispatch Request Date & Time`, CallType = `Reason for Dispatch #1`)

#Dropping NA's in Time column for MCSLC
MCSLC = MCSLC %>%
  drop_na(Time)
  

#Remove Duplicates
EugeneAll = EugeneAll %>% 
  distinct(Time, .keep_all = TRUE) %>% 
  drop_na(CAHOOTSBinary)

SpringfieldAll = SpringfieldAll %>%
  distinct(Time, .keep_all = TRUE) %>% 
  drop_na(CAHOOTSBinary)

MCSLC = MCSLC %>% 
  distinct(Time, .keep_all = TRUE)

###Writing compiled .csv files to a new folder in the current directory
dir.create("CompiledData")
write_csv(EugeneAll, "CompiledData/EugeneAll.csv")
write_csv(SpringfieldAll, "CompiledData/SpringfieldAll.csv")
write_csv(MCSLC, "CompiledData/MCSLC_Trimed.csv")

