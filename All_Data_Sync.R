#Kai Hoffman DSCI 410 data combination
#The goal of this script is to identify all CAHOOTS calls from the PD data,
#and end up with a finished .csv with columns: Time   Organization   Type
#where Organization will either be: EPD, SPD, E-MCSLC, S-MCSLC, E-CHT, S-CHT, or O-MCSLC

#READ
#This script should be run in the same folder Data_Prep.r was run in,
#the same one containing the "CompiledData" folder

###Required packages
library("tidyverse")

rm(list = ls())

###Import files
EugeneData = read_csv("CompiledData/EugeneAll.csv")
SpringfieldData = read_csv("CompiledData/SpringfieldAll.csv")
MCSLCData = read_csv("CompiledData/MCSLC_Trimed.csv")

##Make CAHOOTSBinary column work for EPD and SPD
# Ways CAHOOTS is identified: 
# CAHE in Agency column in EPD (Corresponds with "J" in unit #, and "...CAHOOTS" in nature)
# "J" in unit # in SPD data

#For Eugene
EugeneData = EugeneData %>% 
  mutate(
    CAHOOTSBinary = ifelse(
      #Make CAHOOTSBinary a 1 for the following cases
      agency == "CAHE" |
      CAHOOTSBinary == "_CAHOT" |
      str_detect(CAHOOTSBinary, "J") |
      str_detect(CallType, "CAHOOTS"),
      1,0
    )
  ) %>% select(-agency)
  
#For Springfield
SpringfieldData = SpringfieldData %>% 
  mutate(
    CAHOOTSBinary =ifelse(
      str_detect(CAHOOTSBinary, "J"),
      1,0
    )
  )

###Create Organization column for each dataset

EugeneData = EugeneData %>% 
  mutate(
    Organization = ifelse(
      #If CAHOOTSBinary is a 1, that means that the organization that took the call was CAHOOTS,
      #therefore we assign organization to the respective E/S-CAHOOTS
      CAHOOTSBinary == 1 ,
      "E-CAHOOTS", "EPD"
    )
  ) %>%  select(Time, Organization, CallType)

SpringfieldData = SpringfieldData %>%  
  mutate(
    Organization = ifelse(
      CAHOOTSBinary == 1 |
      is.na(CAHOOTSBinary) == TRUE,
      "S-CAHOOTS", "SPD"
    )
  ) %>%  select(Time, Organization, CallType)

MCSLCData = MCSLCData %>% 
  mutate(
    Organization = case_when(
      #Splitting MCSLC in E/S/O-MCSLC depending on city variable
      City == "Eugene" ~ "E-MCSLC",
      City == "Springfield" ~ "S-MCSLC",
      TRUE ~ "O-MCSLC"
    )
  )%>%  select(Time, Organization, CallType)

###Combine all into same dataframe sorted by time
AllData = EugeneData %>% 
  bind_rows(SpringfieldData) %>% 
  bind_rows(MCSLCData) %>% 
  arrange(Time)

#Remove individual Eugene, Springfield, and MCSLC datasets for space
rm(EugeneData, SpringfieldData, MCSLCData)

###Write AllData to a .csv
write_csv(AllData, "AllData.csv")

