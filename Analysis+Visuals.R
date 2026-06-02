#Kai Hoffman DSCI 410 Data Analysis and Visuals
#This script will run all the numerical analysis, visualizations, and statistical tests 
#outlined in my Project Methods and Project Description assignments

#README
#This file should be ran in the same folder the other two files were, and only after the 
#"AllData.csv" file has been writen by All_Data_Sync.r

#Install Packages
library("tidyverse")
library("ggplot2")
library("plotly")
library("scales")

rm(list=ls())

##Read in data
AllData = read_csv("AllData.csv") #Might take a little


 
#Colors for each Org. - Synced across all plots and charts
org_colors = c(
  "CAHOOTS" = "#F6D8AE",    #
  "E-CAHOOTS" = "#F3DE8A",
  "S-CAHOOTS" = "#6AB547",
  "Police" = "#D5573B",     #
  "EPD" ="#885053",         #
  "SPD" ="#777DA7",        #
  "MCSLC" = "#C09BD8",     #
  "E-MCSLC" = "#000000",
  "S-MCSLC" = "#000000"
)


# For bug fixing
# a1 = Daily_Calls_ValidTypes %>% 
#   filter(between(Date, as.Date('2022-05-01'), as.Date('2022-07-01')))
# 
# a2 = AllData %>% 
#   filter(between(Time, as.Date('2022-05-01'), as.Date('2024-07-01')))



# Finding Frequencies of Common CallTypes ---------------------------------

#HOW TO FIND CALL TYPES THAT BOTH CAHOOTS AND POLICE COULD ANSWER IN A MATHEMATICALLY SOUND WAY
#The plan is to create frequency charts for both CAHOOTS CallTypes and PD CallTypes
#Then, by plotting those two frequency charts on a scatterplot (Y - PD Freq, X - CAHOOTS Freq), with each point representing a call Type
#The line y = x would represent CallTypes that PD and CAHOOTS both answer equally, 
#and therefore CallTypes that would be applicable when doing fair comparisons
#We would then plot a range around the y = x line, almost line an error/variation range, and any
#CallType in that range we would use for fair comparisons

##Start with CAHOOTS
#Filtering for only CAHOOTS calls
CAHOOTS_Only = AllData %>% 
  filter(Organization == "E-CAHOOTS" | Organization == "S-CAHOOTS") %>% 
  drop_na(CallType)

#Frequency table for each type
CAHOOTS_Type_Freq = as.data.frame(table(CAHOOTS_Only$CallType)) %>% rename(CallType = Var1)

#Sets up and plots Frequency of CAHOOTS CallTypes in descending order (Have to use Plotly to scroll and zoom)
CAHOOTS_Freq_Plot = ggplot(CAHOOTS_Type_Freq, aes(x = CallType, y = Freq)) + geom_col()   # reorder(CallType, -Freq)
ggplotly(CAHOOTS_Freq_Plot)



##Repeat with Police

Police_Only = AllData %>% 
  filter(Organization == "EPD" | Organization == "SPD") %>% 
  drop_na(CallType)

Police_Type_Freq = as.data.frame(table(Police_Only$CallType)) %>% rename(CallType = Var1)

Police_Freq_Plot = ggplot(Police_Type_Freq, aes(x = CallType, y = Freq)) + geom_col()
ggplotly(Police_Freq_Plot)


##Plot both frequency tables as scatter plot
#First merge into one dataset (Inner join to keep only categories that are in both)
CAHOOTS_Police_Freq_Merged = CAHOOTS_Type_Freq %>% 
  inner_join(Police_Type_Freq, by = "CallType") %>% 
  rename(CAHOOTS_Freq = Freq.x, Police_Freq = Freq.y)

CAHOOTS_Police_Freq_Merged = CAHOOTS_Police_Freq_Merged %>%     # Manually add in CallTypes containing "CAHOOTS" because after 2022 EPD CAD recorded all CAHOOTS diversions seperate from police
  bind_rows(
    CAHOOTS_Type_Freq %>% filter(str_detect(CallType, "CAHOOTS")) %>% rename(CAHOOTS_Freq = Freq),
    CAHOOTS_Police_Freq_Merged
    ) %>% 
  mutate(Police_Freq = replace_na(Police_Freq, 1))


# Building Frequency Scatterplot ------------------------------------------

# Build the scatterplot and add the upper and lower bounds
CallType_scatter = plot_ly(
  CAHOOTS_Police_Freq_Merged,
  x = ~CAHOOTS_Freq,
  y = ~Police_Freq,
  type = "scatter",
  mode = "markers",
  text = ~CallType
) %>% layout(
  title = "Frequencies of Common Call Types between CAHOOTS and Police",
  xaxis = list(title = "CAHOOTS Frequencies", type = "log"),  # <- Change type from "log" to "linear" for linear axies
  yaxis = list(title = "Police Frequencies", type = "log")
) %>% 
  add_segments(x = 0, xend = 100000, y = 0, yend = 100000, name = "Y = X", # Y = X reference Line
               line = list(
                 color = "red",
                 dash = "dash",
                 width = 1
               )
) %>%
  add_segments(x = 0, xend = 100000, y = 0, yend = 666667, name = "Upper", # Y = X reference Line
               line = list(
                 color = "red",
                 dash = "line",
                 width = 1
               )
) %>%
  add_segments(x = 0, xend = 100000, y = 0, yend = 15000, name = "Lower", # Y = X reference Line
               line = list(
                 color = "red",
                 dash = "line",
                 width = 1
               )
) %>% layout(showlegend = FALSE)

CallType_scatter


## Defining upper and lower bounds
upper_slope = 666667 / 100000
lower_slope = 15000 / 100000


CallTypes_Within_Bounds = CAHOOTS_Police_Freq_Merged %>%
  filter(
    (
      Police_Freq >= lower_slope * CAHOOTS_Freq &
        Police_Freq <= upper_slope * CAHOOTS_Freq
    ) | # "PUBLIC ASSIST, CAHOOTS" Inherently is part of this category, b/c it is calls that are diverted by CAHOOTS recorded properly
      CallType == "PUBLIC ASSIST, CAHOOTS" |
      str_detect(CallType, "CAHOOTS")
    ) 
  

#Make list of only valid CallTypes
CallTypes_Within_Bounds_List = CallTypes_Within_Bounds %>%
  pull(CallType)



# Line Charts -------------------------------------------------------------

# Plot CAHOOTS vs PD without filtering CallTypes
Daily_Calls_AllTypes = AllData %>%
  mutate(
    Date = as.Date(Time),
    Org_Group = case_when(
      Organization %in% c("E-CAHOOTS", "S-CAHOOTS") ~ "CAHOOTS",
      Organization %in% c("EPD", "SPD") ~ "Police",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Org_Group)) %>%
  group_by(Date, Org_Group) %>%
  summarise(Calls = n(), .groups = "drop") %>%
  complete(
    Date = seq(min(Date), max(Date), by = "day"),
    Org_Group = c("CAHOOTS", "Police"),
    fill = list(Calls = 0)
  )

Daily_Calls_AllTypes_Smoothed = Daily_Calls_AllTypes %>%
  arrange(Org_Group, Date) %>%
  group_by(Org_Group) %>%
  mutate(
    Calls_7day_avg = zoo::rollmean(Calls, k = 7, fill = NA, align = "right")
  ) %>%
  ungroup()

Daily_Calls_AllTypes_Smoothed_Line = plot_ly(
  Daily_Calls_AllTypes_Smoothed,
  x = ~Date,
  y = ~Calls_7day_avg,
  color = ~Org_Group,
  colors = org_colors,
  type = "scatter",
  mode = "lines"
) %>%
  layout(
    title = "7-Day Average Daily Calls for All Call Types: Police vs CAHOOTS",
    xaxis = list(title = "Date"),
    yaxis = list(title = "7-Day Average Number of Calls"),
    legend = list(title = list(text = "Organization Group")),
    shapes = list(
      list(
        type = "line",
        x0 = as.Date('2025-04-07'),
        x1 = as.Date('2025-04-07'),
        y0 = 0,
        y1 = 1,
        xref = "x",
        yref = "paper",
        line = list(
          color = "black",
          width = 1,
          dash = "dash"
        )
      )
    ),
    annotations = list(
      list(
        x = as.Date('2025-04-07')-200,
        y = 1,
        xref = "x",
        yref = "paper",
        text = "CAHOOTS Shutdown",
        showarrow = FALSE,
        font = list(color = "black", size = 15)
      )
    )
  )

Daily_Calls_AllTypes_Smoothed_Line

# Plot call volume filtering for valid CallTypes
Daily_Calls_ValidTypes = AllData %>%
  filter(CallType %in% CallTypes_Within_Bounds_List) %>%
  mutate(
    Date = as.Date(Time),
    Org_Group = case_when(
      Organization %in% c("E-CAHOOTS", "S-CAHOOTS") ~ "CAHOOTS",
      Organization %in% c("EPD", "SPD") ~ "Police",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Org_Group)) %>%
  group_by(Date, Org_Group) %>%
  summarise(Calls = n(), .groups = "drop")

Daily_Calls_ValidTypes = Daily_Calls_ValidTypes %>%
  arrange(Org_Group, Date) %>%
  group_by(Org_Group) %>%
  mutate(
    Calls_7day_avg = zoo::rollmean(Calls, k = 7, fill = NA, align = "right")
  ) %>%
  ungroup()

Daily_Calls_Smoothed = plot_ly(
  Daily_Calls_ValidTypes,
  x = ~Date,
  y = ~Calls_7day_avg,
  color = ~Org_Group,
  colors = org_colors,
  type = "scatter",
  mode = "lines"
) %>%
  layout(
    title = "7-Day Average Daily Calls for Valid Call Types: Police vs CAHOOTS",
    xaxis = list(title = "Date"),
    yaxis = list(title = "7-Day Average Number of Calls"),
    legend = list(title = list(text = "Organization Group")),
    shapes = list(
      list(
        type = "line",
        x0 = as.Date('2025-04-07'),
        x1 = as.Date('2025-04-07'),
        y0 = 0,
        y1 = 1,
        xref = "x",
        yref = "paper",
        line = list(
          color = "black",
          width = 1,
          dash = "dash"
        )
      )
    ),
    annotations = list(
      list(
        x = as.Date('2025-04-07')-200,
        y = 1,
        xref = "x",
        yref = "paper",
        text = "CAHOOTS Shutdown",
        showarrow = FALSE,
        font = list(color = "black", size = 15)
      )
    )
  )

Daily_Calls_Smoothed

# All Orgs
Daily_Calls_ByOrg = AllData %>%
  filter(
    Organization %in% c(
      "E-CAHOOTS", "S-CAHOOTS",
      "EPD", "SPD",
      "E-MCSLC", "S-MCSLC", "O-MCSLC"
    )
  ) %>%
  mutate(
    Date = as.Date(Time),
    Org_Plot = case_when(
      Organization %in% c("E-MCSLC", "S-MCSLC", "O-MCSLC") ~ "MCSLC",
      TRUE ~ Organization
    )
  ) %>%
  group_by(Date, Org_Plot) %>%
  summarise(Calls = n(), .groups = "drop") %>%
  complete(
    Date = seq(min(Date), max(Date), by = "day"),
    Org_Plot = c(
      "E-CAHOOTS", "S-CAHOOTS",
      "EPD", "SPD",
      "MCSLC"
    ),
    fill = list(Calls = 0)
  )

MCSLC_start_date = as.Date("2024-08-18")

Daily_Calls_ByOrg = Daily_Calls_ByOrg %>%
  mutate(
    Calls = if_else(
      Org_Plot == "MCSLC" & Date < MCSLC_start_date,
      NA_integer_,
      Calls
    )
  )

Daily_Calls_ByOrg_Smoothed = Daily_Calls_ByOrg %>%
  arrange(Org_Plot, Date) %>%
  group_by(Org_Plot) %>%
  mutate(
    Calls_7day_avg = zoo::rollmean(Calls, k = 7, fill = NA, align = "right"),
    Calls_30day_avg = zoo::rollmean(Calls, k = 30, fill = NA, align = "right")
  ) %>%
  ungroup()

#7 Day average
Daily_Calls_ByOrg_7Day_Line = plot_ly(
  Daily_Calls_ByOrg_Smoothed,
  x = ~Date,
  y = ~Calls_7day_avg,
  color = ~Org_Plot,
  colors = org_colors,
  type = "scatter",
  mode = "lines"
) %>%
  layout(
    title = "7-Day Average Daily Calls by Organization",
    xaxis = list(title = "Date"),
    yaxis = list(title = "7-Day Average Number of Calls"),
    legend = list(title = list(text = "Organization")),
    shapes = list(
      list(
        type = "line",
        x0 = as.Date('2025-04-07'),
        x1 = as.Date('2025-04-07'),
        y0 = 0,
        y1 = 1,
        xref = "x",
        yref = "paper",
        line = list(
          color = "black",
          width = 1,
          dash = "dash"
        )
      )
    ),
    annotations = list(
      list(
        x = as.Date('2025-04-07')-200,
        y = 1,
        xref = "x",
        yref = "paper",
        text = "CAHOOTS Shutdown",
        showarrow = FALSE,
        font = list(color = "black", size = 15)
      )
    )
  )

Daily_Calls_ByOrg_7Day_Line

#30 Day Average
Daily_Calls_ByOrg_30Day_Line = plot_ly(
  Daily_Calls_ByOrg_Smoothed,
  x = ~Date,
  y = ~Calls_30day_avg,
  color = ~Org_Plot,
  colors = org_colors,
  type = "scatter",
  mode = "lines"
) %>%
  layout(
    title = "30-Day Average Daily Calls by Organization",
    xaxis = list(title = "Date"),
    yaxis = list(title = "30-Day Average Number of Calls"),
    legend = list(title = list(text = "Organization"))
  )

#Daily_Calls_ByOrg_30Day_Line

#No Average
Daily_Calls_ByOrg_Line = plot_ly(
  Daily_Calls_ByOrg,
  x = ~Date,
  y = ~Calls,
  color = ~Org_Plot,
  colors = org_colors,
  type = "scatter",
  mode = "lines"
) %>%
  layout(
    title = "Daily Calls by Organization",
    xaxis = list(title = "Date"),
    yaxis = list(title = "Number of Calls"),
    legend = list(title = list(text = "Organization"))
  )

#Daily_Calls_ByOrg_Line


# Bar Charts --------------------------------------------------------------

# Find the most recent date in your dataset
max_date = max(as.Date(AllData$Time), na.rm = TRUE)

# Last 3 years of data
three_year_cutoff = max_date - years(3)      # <--- If you wanted all years min(as.Date(AllData$Time), na.rm = TRUE)

# Optional: manually define MCSLC start date
MCSLC_start_date = as.Date("2024-08-18")

Hourly_Avg_Calls = AllData %>%
  mutate(
    Date = as.Date(Time),
    Hour = hour(Time),
    Org_Plot = case_when(
      Organization %in% c("E-CAHOOTS", "S-CAHOOTS") ~ "CAHOOTS",
      Organization == "EPD" ~ "EPD",
      Organization == "SPD" ~ "SPD",
      Organization %in% c("E-MCSLC", "S-MCSLC", "O-MCSLC") ~ "MCSLC",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Org_Plot)) %>%
  filter(
    # CAHOOTS, EPD, and SPD use last 3 years
    (Org_Plot %in% c("CAHOOTS", "EPD", "SPD") & Date >= three_year_cutoff) |
      
      # MCSLC uses all data since it started
      (Org_Plot == "MCSLC" & Date >= MCSLC_start_date)
  ) %>%
  group_by(Org_Plot, Hour) %>%
  summarise(
    Total_Calls = n(),
    .groups = "drop"
  ) %>%
  complete(
    Org_Plot = c("CAHOOTS", "EPD", "SPD", "MCSLC"),
    Hour = 0:23,
    fill = list(Total_Calls = 0)
  ) %>%
  mutate(
    Days_In_Period = case_when(
      Org_Plot %in% c("CAHOOTS", "EPD", "SPD") ~ as.numeric(max_date - three_year_cutoff) + 1,
      Org_Plot == "MCSLC" ~ as.numeric(max_date - MCSLC_start_date) + 1
    ),
    Avg_Calls_Per_Day = Total_Calls / Days_In_Period,
    Hour_Label = sprintf("%02d:00", Hour)
  )

# title_style = list(
#   family = "verdana",
#   size = 15,
#   weight = 500
# )

#CAHOOTS Chart
CAHOOTS_Hourly_Bar = Hourly_Avg_Calls %>%
  filter(Org_Plot == "CAHOOTS") %>%
  plot_ly(
    x = ~Hour_Label,
    y = ~Avg_Calls_Per_Day,
    type = "bar",
    marker = list(color = org_colors["CAHOOTS"])
  ) %>%
  layout(
    title = list(text = "Average CAHOOTS Calls per Day by Hour"),
    xaxis = list(title = ""),
    yaxis = list(title = "Average Calls per Day"), 
    shapes = list(
      list(
        type = "line",
        x0 = "12:00",
        x1 = "12:00",
        y0 = 0,
        y1 = 1,
        xref = "x",
        yref = "paper",
        line = list(
          color = "darkgray",
          width = 3,
          dash = "dash"
        )
      )
    ),
    annotations = list(
      list(
        x = "12:00",
        y = 1.03,
        xref = "x",
        yref = "paper",
        text = "Noon",
        showarrow = FALSE,
        font = list(color = "darkgray", size = 15)
      )
  ))

CAHOOTS_Hourly_Bar

#EPD Chart
EPD_Hourly_Bar = Hourly_Avg_Calls %>%
  filter(Org_Plot == "EPD") %>%
  plot_ly(
    x = ~Hour_Label,
    y = ~Avg_Calls_Per_Day,
    type = "bar",
    marker = list(color = org_colors["EPD"])
  ) %>%
  layout(
    title = "Average EPD Calls per Day by Hour",
    xaxis = list(title = ""),
    yaxis = list(title = "Average Calls per Day"),
    shapes = list(
      list(
        type = "line",
        x0 = "12:00",
        x1 = "12:00",
        y0 = 0,
        y1 = 1,
        xref = "x",
        yref = "paper",
        line = list(
          color = "darkgray",
          width = 3,
          dash = "dash"
        )
      )
    ),
    annotations = list(
      list(
        x = "12:00",
        y = 1.03,
        xref = "x",
        yref = "paper",
        text = "Noon",
        showarrow = FALSE,
        font = list(color = "darkgray", size = 15)
      )
    ))

EPD_Hourly_Bar

#SPD Chart
SPD_Hourly_Bar = Hourly_Avg_Calls %>%
  filter(Org_Plot == "SPD") %>%
  plot_ly(
    x = ~Hour_Label,
    y = ~Avg_Calls_Per_Day,
    type = "bar",
    marker = list(color = org_colors["SPD"])
  ) %>%
  layout(
    title = "Average SPD Calls per Day by Hour",
    xaxis = list(title = ""),
    yaxis = list(title = "Average Calls per Day"),
    shapes = list(
      list(
        type = "line",
        x0 = "12:00",
        x1 = "12:00",
        y0 = 0,
        y1 = 1,
        xref = "x",
        yref = "paper",
        line = list(
          color = "darkgray",
          width = 3,
          dash = "dash"
        )
      )
    ),
    annotations = list(
      list(
        x = "12:00",
        y = 1.03,
        xref = "x",
        yref = "paper",
        text = "Noon",
        showarrow = FALSE,
        font = list(color = "darkgray", size = 15)
      )
    ))

SPD_Hourly_Bar

#MCSLC Chart
MCSLC_Hourly_Bar = Hourly_Avg_Calls %>%
  filter(Org_Plot == "MCSLC") %>%
  plot_ly(
    x = ~Hour_Label,
    y = ~Avg_Calls_Per_Day,
    type = "bar",
    marker = list(color = org_colors["MCSLC"])
  ) %>%
  layout(
    title = "Average MCSLC Calls per Day by Hour",
    xaxis = list(title = ""),
    yaxis = list(title = "Average Calls per Day"),
    shapes = list(
      list(
        type = "line",
        x0 = "12:00",
        x1 = "12:00",
        y0 = 0,
        y1 = 1,
        xref = "x",
        yref = "paper",
        line = list(
          color = "darkgray",
          width = 3,
          dash = "dash"
        )
      )
    ),
    annotations = list(
      list(
        x = "12:00",
        y = 1.03,
        xref = "x",
        yref = "paper",
        text = "Noon",
        showarrow = FALSE,
        font = list(color = "darkgray", size = 15)
      )
    ))

MCSLC_Hourly_Bar


# Combined Bar Charts ------------------------

# Hourly Bar Chart
# Some variable names might say "overlapping" because I changed from an overlapping Bar chart to a simpler one
Hourly_Overlap_Data = Hourly_Avg_Calls %>%
  filter(Org_Plot %in% c("CAHOOTS", "EPD", "SPD", "MCSLC")) %>%
  mutate(
    Org_Plot = factor(
      Org_Plot,
      levels = c("EPD", "SPD", "CAHOOTS", "MCSLC")
    )
  ) %>%
  arrange(Org_Plot, Hour)

Hourly_Grouped_Data = Hourly_Avg_Calls %>%
  filter(Org_Plot %in% c("CAHOOTS", "EPD", "SPD", "MCSLC")) %>%
  mutate(
    Org_Plot = factor(
      Org_Plot,
      levels = c("EPD", "SPD", "CAHOOTS", "MCSLC")
    )
  ) %>%
  arrange(Hour, Org_Plot)

Hourly_Grouped_Bar = Hourly_Grouped_Data %>%
  plot_ly(
    x = ~Hour_Label,
    y = ~Avg_Calls_Per_Day,
    color = ~Org_Plot,
    colors = org_colors,
    type = "bar",
    marker = list(
      line = list(
        color = "rgba(0,0,0,0)",
        width = 0
      )
    )
  ) %>%
  layout(
    title = list(text = "Average Calls per Day by Hour"),
    xaxis = list(title = ""),
    yaxis = list(title = "Average Calls per Day"),
    barmode = "group",
    bargap = 0.01,
    bargroupgap = 0,
    legend = list(title = list(text = "Organization")),
    shapes = list(
      list(
        type = "line",
        x0 = "12:00",
        x1 = "12:00",
        y0 = 0,
        y1 = 1,
        xref = "x",
        yref = "paper",
        line = list(
          color = "darkgray",
          width = 3,
          dash = "dash"
        )
      )
    ),
    annotations = list(
      list(
        x = "12:00",
        y = 1.03,
        xref = "x",
        yref = "paper",
        text = "Noon",
        showarrow = FALSE,
        font = list(color = "darkgray", size = 15)
      )
    )
  )

Hourly_Grouped_Bar


## Day-of-the-Week Bar Chart ##
# Some variable names might say "overlapping" because I changed from an overlapping Bar chart to a simpler one

max_date = max(as.Date(AllData$Time), na.rm = TRUE)

# Last 3 years
three_year_cutoff = max_date - years(3)         # <--- If you wanted all years min(as.Date(AllData$Time), na.rm = TRUE)

# MCSLC start date
MCSLC_start_date = as.Date("2024-08-18")

Weekly_Avg_Calls = AllData %>%
  mutate(
    Date = as.Date(Time),
    Weekday = wday(Date, label = TRUE, abbr = FALSE, week_start = 7),
    Org_Plot = case_when(
      Organization %in% c("E-CAHOOTS", "S-CAHOOTS") ~ "CAHOOTS",
      Organization == "EPD" ~ "EPD",
      Organization == "SPD" ~ "SPD",
      Organization %in% c("E-MCSLC", "S-MCSLC", "O-MCSLC") ~ "MCSLC",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Org_Plot)) %>%
  filter(
    # CAHOOTS, EPD, and SPD use last 3 years
    (Org_Plot %in% c("CAHOOTS", "EPD", "SPD") & Date >= three_year_cutoff) |
      
      # MCSLC uses all data since it started
      (Org_Plot == "MCSLC" & Date >= MCSLC_start_date)
  ) %>%
  group_by(Org_Plot, Weekday) %>%
  summarise(
    Total_Calls = n(),
    .groups = "drop"
  ) %>%
  complete(
    Org_Plot = c("CAHOOTS", "EPD", "SPD", "MCSLC"),
    Weekday = factor(
      c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
      levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
      ordered = TRUE
    ),
    fill = list(Total_Calls = 0)
  )

Weekday_Denominators = tibble(
  Date = seq(min(three_year_cutoff, MCSLC_start_date), max_date, by = "day")
) %>%
  crossing(
    Org_Plot = c("CAHOOTS", "EPD", "SPD", "MCSLC")
  ) %>%
  mutate(
    Include_Date = case_when(
      Org_Plot %in% c("CAHOOTS", "EPD", "SPD") ~ Date >= three_year_cutoff,
      Org_Plot == "MCSLC" ~ Date >= MCSLC_start_date
    ),
    Weekday = wday(Date, label = TRUE, abbr = FALSE, week_start = 7)
  ) %>%
  filter(Include_Date) %>%
  group_by(Org_Plot, Weekday) %>%
  summarise(
    Days_In_Period = n(),
    .groups = "drop"
  )

Weekly_Avg_Calls = Weekly_Avg_Calls %>%
  left_join(
    Weekday_Denominators,
    by = c("Org_Plot", "Weekday")
  ) %>%
  mutate(
    Avg_Calls_Per_Day = Total_Calls / Days_In_Period,
    Weekday = factor(
      Weekday,
      levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
      ordered = TRUE
    )
  ) %>%
  arrange(Org_Plot, Weekday)

Weekly_Grouped_Bar = Weekly_Avg_Calls %>%
  mutate(
    Org_Plot = factor(
      Org_Plot,
      levels = c("EPD", "SPD", "CAHOOTS", "MCSLC")
    )
  ) %>%
  plot_ly(
    x = ~Weekday,
    y = ~Avg_Calls_Per_Day,
    color = ~Org_Plot,
    colors = org_colors,
    type = "bar",
    marker = list(
      line = list(
        color = "rgba(0,0,0,0)",
        width = 0
      )
    )
  ) %>%
  layout(
    title = list(text = "Average Calls per Day by Day of Week"),
    xaxis = list(title = ""),
    yaxis = list(title = "Average Calls per Day"),
    barmode = "group",
    bargap = 0.15,
    bargroupgap = 0.01,
    legend = list(title = list(text = "Organization"))
  )

Weekly_Grouped_Bar


# Heatmaps -----------------------------------------------------------------

#Year X Month

# Create a full calendar over the span of your dataset
min_date = min(as.Date(AllData$Time), na.rm = TRUE)
max_date = max(as.Date(AllData$Time), na.rm = TRUE)

Calendar_Months = tibble(
  Date = seq(min_date, max_date, by = "day")
) %>%
  mutate(
    Year = year(Date),
    MonthNum = month(Date),
    Month = month(Date, label = TRUE, abbr = FALSE)
  ) %>%
  group_by(Year, MonthNum, Month) %>%
  summarise(
    Days_In_Data = n(),
    .groups = "drop"
  )

# Count total calls by month-year across all organizations
Monthly_Heatmap_Data = AllData %>%
  mutate(
    Date = as.Date(Time),
    Year = year(Date),
    MonthNum = month(Date),
    Month = month(Date, label = TRUE, abbr = FALSE)
  ) %>%
  group_by(Year, MonthNum, Month) %>%
  summarise(
    Total_Calls = n(),
    .groups = "drop"
  ) %>%
  right_join(Calendar_Months, by = c("Year", "MonthNum", "Month")) %>%
  mutate(
    Total_Calls = replace_na(Total_Calls, 0),
    Avg_Calls_Per_Day = Total_Calls / Days_In_Data,
    Month = factor(Month, levels = month.name, ordered = TRUE)
  ) %>%
  arrange(Year, MonthNum)

Monthly_Call_Heatmap = plot_ly(
  data = Monthly_Heatmap_Data,
  x = ~Month,
  y = ~Year,
  z = ~Avg_Calls_Per_Day,
  type = "heatmap",
  colorscale = "YlOrRd",
  text = ~paste(
    "Year:", Year,
    "<br>Month:", Month,
    "<br>Total Calls:", Total_Calls,
    "<br>Days in Data:", Days_In_Data,
    "<br>Avg Calls/Day:", round(Avg_Calls_Per_Day, 2)
  ),
  hoverinfo = "text"
) %>%
  layout(
    title = list(text = "Average Daily Call Volume by Month and Year"),
    xaxis = list(title = "Month"),
    yaxis = list(title = "Year")
  )

Monthly_Call_Heatmap


## Year X Week ##
years_to_plot = 2015:2025

# Build weekly average call-volume data
Weekly_Heatmap_Data = AllData %>%
  mutate(
    Date = as.Date(Time),
    Year = year(Date),
    Week = week(Date),
    Month = month(Date)
  ) %>%
  filter(Year %in% years_to_plot) %>%
  group_by(Year, Week) %>%
  summarise(
    Total_Calls = n(),
    Days_In_Week = n_distinct(Date),
    Month = max(Month),
    .groups = "drop"
  ) %>%
  complete(
    Year = years_to_plot,
    Week = 1:53,
    fill = list(
      Total_Calls = 0,
      Days_In_Week = 7
    )
  ) %>%
  mutate(
    Avg_Calls_Per_Day = Total_Calls / Days_In_Week
  ) %>%
  filter(!(Week >= 52))


Month_Positions = tibble(
  Date = seq(as.Date("2021-01-01"), as.Date("2021-12-31"), by = "day")
) %>%
  mutate(
    Week = week(Date),
    Month = month(Date, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(Month) %>%
  summarise(
    Month_Start = min(Week),
    Month_End = max(Week),
    Month_Mid = (Month_Start + Month_End) / 2,
    .groups = "drop"
  )


Weekly_Call_Heatmap_No_Lines = ggplot(
  Weekly_Heatmap_Data,
  aes(x = Week, y = Year, fill = Avg_Calls_Per_Day)
) +
  geom_tile(
    width = 1,
    height = 1
  ) +
  scale_x_continuous(
    breaks = Month_Positions$Month_Mid,
    labels = Month_Positions$Month,
    position = "top",
    expand = c(0, 0)
  ) +
  scale_y_reverse(
    breaks = 2015:2025,
    expand = c(0, 0)
  ) +
  scale_fill_gradientn(
    colors = c(
      "#2D004B",
      "#6A00A8",
      "#B12A90",
      "#E34A33",
      "#FD8D3C",
      "#FEE08B",
      "#FFFFF5"
    ),
    name = "Avg Calls\nper Day"
  ) +
  labs(
    title = "Weekly Average Call Volume by Month and Year",
    subtitle = "All organizations combined",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 11, face = "bold"),
    axis.text.y = element_text(size = 11),
    axis.ticks = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )

Weekly_Call_Heatmap_No_Lines




  



# Numbers -----------------------------------------------------------------


# Finding Averages of Yearly and weekly call volume, Average min, max, and range
# Set this to the date CAHOOTS shut down
CAHOOTS_shutdown_date = as.Date("2025-04-07")

# Create daily total call volume before CAHOOTS shutdown
Daily_Total_Calls_PreShutdown = AllData %>%
  mutate(
    Date = as.Date(Time)
  ) %>%
  filter(Date < CAHOOTS_shutdown_date) %>%
  group_by(Date) %>%
  summarise(
    Total_Calls = n(),
    .groups = "drop"
  ) %>%
  complete(
    Date = seq(min(Date), max(Date), by = "day"),
    fill = list(Total_Calls = 0)
  ) %>%
  arrange(Date) %>%
  mutate(
    Year = year(Date),
    Calls_7day_avg = zoo::rollmean(
      Total_Calls,
      k = 7,
      fill = NA,
      align = "right"
    )
  )

Daily_Total_Calls_PreShutdown = Daily_Total_Calls_PreShutdown %>%
  filter(Year < year(CAHOOTS_shutdown_date))

Yearly_7Day_Summary = Daily_Total_Calls_PreShutdown %>%
  filter(!is.na(Calls_7day_avg)) %>%
  group_by(Year) %>%
  summarise(
    Mean_7Day_Avg = mean(Calls_7day_avg, na.rm = TRUE),
    Max_7Day_Avg = max(Calls_7day_avg, na.rm = TRUE),
    Min_7Day_Avg = min(Calls_7day_avg, na.rm = TRUE),
    
    Yearly_Range = Max_7Day_Avg - Min_7Day_Avg,
    Peak_Above_Mean = Max_7Day_Avg - Mean_7Day_Avg,
    Valley_Below_Mean = Mean_7Day_Avg - Min_7Day_Avg,
    
    .groups = "drop"
  )

Average_Total_Calls_Per_Year = Daily_Total_Calls_PreShutdown %>%
  group_by(Year) %>%
  summarise(
    Total_Calls_Year = sum(Total_Calls, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  summarise(
    Average_Total_Calls_Per_Year = mean(Total_Calls_Year, na.rm = TRUE)
  )

Average_Total_Calls_Per_Year

Total_Calls_By_Year = Daily_Total_Calls_PreShutdown %>%
  group_by(Year) %>%
  summarise(
    Total_Calls_Year = sum(Total_Calls, na.rm = TRUE),
    .groups = "drop"
  )

Total_Calls_By_Year


Yearly_7Day_Summary

Final_7Day_Call_Volume_Metrics = Yearly_7Day_Summary %>%
  summarise(
    Average_Amount_7Day_Avg_Calls = mean(Mean_7Day_Avg, na.rm = TRUE),
    Average_Yearly_Range = mean(Yearly_Range, na.rm = TRUE),
    Average_Peak_Above_Mean = mean(Peak_Above_Mean, na.rm = TRUE),
    Average_Valley_Below_Mean = mean(Valley_Below_Mean, na.rm = TRUE),
    Average_Total_Calls_Per_Year = mean(
      Daily_Total_Calls_PreShutdown %>%
        group_by(Year) %>%
        summarise(Total_Calls_Year = sum(Total_Calls), .groups = "drop") %>%
        pull(Total_Calls_Year),
      na.rm = TRUE
    )
  )

Final_7Day_Call_Volume_Metrics

Averges_Table = Final_7Day_Call_Volume_Metrics %>%
  pivot_longer(
    cols = everything(),
    names_to = "Metric",
    values_to = "Value"
  ) %>% 
  mutate(Value = round(Value))
