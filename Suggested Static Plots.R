## ----------------------------------------------------------------
## Suggested Static Plots
##
## Date: January 23rd, 2025

## ----------------------------------------------------------------
## SET UP THE ENVIRONMENT

library("readr")
library("tidyr")
library("dplyr")
library("scales")
library("ggplot2")
library("lubridate")
library("usmap")
library("zoo")
library("mapview")
library("cowplot")
library("tigris")
library("leaflet")
library("sf")
library("RColorBrewer")
library("gridExtra")
library("waffle")

"%!in%" <- function(x,y)!("%in%"(x,y))




## ----------------------------------------------------------------
## LOAD THE DATA

opioid_od <- read_csv("Opioid OD Data/Harmonized Opioid Overdose Datasets_02.05.2025.csv") %>%
  as.data.frame()

rsv <- read_csv("RSV Infections Data/Harmonized RSV Infections Datasets_01.30.2025.csv") %>%
  as.data.frame()




## ----------------------------------------------------------------
## OPIOIDS OVERDOSE DATA


# The following three columns of data could have any of these three numerical
# placeholder values. These only indicate a different form of missing data, and
# need to be filtered out. When plotting, only use the code line that filters
# these placeholders for the values that will be plotted.
opioid_od_not_suppressed <- opioid_od %>% 
  filter(Count %!in% 7777 & Count %!in% 8888 & Count %!in% 9999) %>%
  filter(`Crude Rate` %!in% 7777 & `Crude Rate` %!in% 8888 & `Crude Rate` %!in% 9999) %>%
  filter(`Age Adjusted Rate` %!in% 7777 & `Age Adjusted Rate` %!in% 8888 & `Age Adjusted Rate` %!in% 9999)




# -----------------------------
# Basic time-series line plot.

opioid_od %>%
  # Filter the placeholder numerical values.
  filter(`Crude Rate` %!in% 7777 & `Crude Rate` %!in% 8888 & `Crude Rate` %!in% 9999) %>%
  
  # Filter the metadata settings.
  filter(State %in% "US", Quarter %in% NA, Drug %in% "All Opioids",
         Setting %in% "Medical Facility - Inpatient",
         `Underlying Cause of Death` %in% "All",
         Characteristic %in% "Not Stratified", Level %in% "N/A") %>%
  
  # Plot settings and features.
  ggplot(data = ., aes(x = Year, y = `Crude Rate`)) +
  geom_line(aes(color = Dataset)) +
  labs(title = "National Opioid Overdose Rate for All Types of Opioids",
       subtitle = "Underlying Cause of Death: All. Setting: Medical Facility - Inpatient.",
       x = "Year", y = "Crude Rate (per 100,000)") +
  theme_minimal()




# -----------------------------
# Bar graph showing overdose rates by drug type and polysubstance.

# Only the SUDORS or CDC WONDER datasets have information about anything other
# than all opioid overdose ICD-10 codes compiles as "All Opioids". SUDORS only
# has "Setting = All", "Underlying Cause of Death = Unintentional", and 
# age-adjusted rates for these settings.
# 
# Plot different years of information: 2020, 2021, and 2022.

opioid_od %>%
  # Filter the placeholder numerical values.
  filter(`Age Adjusted Rate` %!in% 7777 & `Age Adjusted Rate` %!in% 8888 & `Age Adjusted Rate` %!in% 9999) %>%
  
  # Change the date named in the plot title.
  filter(Year %in% 2020) %>%

  # Filter the metadata settings.
  filter(State %in% "US", Quarter %in% NA, Setting %in% "All",
         `Underlying Cause of Death` %in% "Unintentional",
         Characteristic %in% "Not Stratified", Level %in% "N/A") %>%
  
  # Plot settings and features.
  ggplot(data = ., aes(x = Drug, y = `Age Adjusted Rate`)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = Dataset, color = Dataset)) +
  labs(title = "National Opioid Overdose Rate by Types of Opioid and Polysubstance in 2020",
       subtitle = "Underlying Cause of Death: Unintentional Setting: All.",
       x = "", y = "Age-Adjusted Rate (per 100,000)") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 55,  hjust = 1))




# -----------------------------
# Alternative to the bar graph, the waffle plot.

# First subset step to clear the "Count" column and isolate the specific
# stratification being plotted.
subset <-  opioid_od %>%
  # Filter the placeholder numerical values.
  #filter(`Age Adjusted Rate` %!in% 7777 & `Age Adjusted Rate` %!in% 8888 & `Age Adjusted Rate` %!in% 9999) %>%
  filter(Count %!in% 7777 & Count %!in% 8888 & Count %!in% 9999) %>%
  
  # Removing the extraneous Drug and Year classes that are not common to the
  # SUDORS and CDC WONDER dataset.
  filter(Drug %!in% c("Naloxone"), Year %in% c(2020, 2021, 2022)) %>%
  
  # Filter the metadata settings.
  filter(State %in% "US", Quarter %in% NA, Setting %in% "All",
         `Underlying Cause of Death` %in% "Unintentional",
         Characteristic %in% "Not Stratified", Level %in% "N/A") %>%
  
  # Some random duplicates were detected. Not sure where these might have
  # been introduced or if the dataset came this way. This script is meant to
  # quickly deal with these.
  group_by(Dataset, Year, Drug) %>%
  mutate(Count = sum(Count)) %>%
  ungroup() %>%
  
  select(-`Crude Rate`, -`Age Adjusted Rate`, -Population) %>%
  distinct() %>%
  
  # Calculate the relative proportion of overdose events for a given Dataset
  # and Year.
  group_by(Dataset, Year) %>%
  mutate(Percentage = round(Count/sum(Count)*100, digits = 0)) %>%
  ungroup() %>%
  
  # Organize the final table.
  .[with(., order(Dataset, Year, Percentage)), ] %>% `rownames<-`(NULL) %>%
  as.data.frame()



# Toggling the "Year \in 2020, 2021, and 2022", not all the same drugs are
# present in each dataset. With scale_fill_manual() this is proving problematic,
# as it overwrites the categories.
subset[subset$Dataset %in% "CDC WONDER" & subset$Year %in% 2022, "Drug"] %>% .[. %!in% subset[subset$Dataset %in% "SUDORS" & subset$Year %in% 2022, "Drug"]]
subset[subset$Dataset %in% "SUDORS" & subset$Year %in% 2022, "Drug"] %>% .[. %!in% subset[subset$Dataset %in% "CDC WONDER" & subset$Year %in% 2022, "Drug"]]

# Expand the table to include the missing values as NA.
subset <- subset %>%
  group_by(Dataset, Year) %>%
  complete(data.frame("Drug" = sort(unique(subset$Drug))) ) %>%
  fill(State, Setting, `Underlying Cause of Death`, Characteristic, Level, .direction = "down") %>%
  ungroup() %>%
  
  na_omit(., cols = c("Count", "Percentage")) %>%
  #mutate_at(c("Count", "Percentage"), ~replace_na(., 0)) %>%
  
  # Organize the final table.
  .[with(., order(Dataset, Year, Percentage)), ] %>% `rownames<-`(NULL) %>%
  as.data.frame()



# Two options for coloring from ColorBrewer.
coloring <- data.frame("Drug"  = sort(unique(subset$Drug)),
                       "Color" = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99', '#e31a1c', '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99')
                       #"Color" = c('#8dd3c7','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5','#d9d9d9','#bc80bd','#ccebc5','#ffffb3')
                       )

# Subset the data in preparation to plot by Dataset. 
# These are done separated from the ggplot() function so that the color scheme
# line can call the correct HEX code for the drugs present in the subset.
waffle_SUDORS <- subset %>%
  filter(Dataset %in% "SUDORS")

waffle_CDC_WONDER <- subset %>%
  filter(Dataset %in% "CDC WONDER")


# Generate the waffle plots.
w1 <- waffle_SUDORS %>%
  ggplot(aes(fill = Drug, values = Percentage)) +
    geom_waffle(n_rows = 5, size = 0.33, colour = "white", na.rm = TRUE) +
    facet_wrap(~Dataset+Year) +
    # Add the color scheme specific for the drugs present in the SUDORS subset.
    scale_fill_manual(name = NULL, 
                      labels = coloring[coloring$Drug %in% unique(waffle_SUDORS$Drug) , "Drug"],
                      values = coloring[coloring$Drug %in% unique(waffle_SUDORS$Drug) , "Color"]) +
    coord_equal() + theme_void() + 
    theme(legend.position = "bottom")


w2 <- waffle_CDC_WONDER %>%
  ggplot(aes(fill = Drug, values = Percentage) ) +
    geom_waffle(n_rows = 5, size = 0.33, colour = "white") +
    facet_wrap(~Dataset+Year) +
    # Add the color scheme specific for the drugs present in the CDC WONDER subset.
    scale_fill_manual(name = NULL, 
                      labels = coloring[coloring$Drug %in% unique(waffle_CDC_WONDER$Drug) , "Drug"],
                      values = coloring[coloring$Drug %in% unique(waffle_CDC_WONDER$Drug) , "Color"]) +
    coord_equal() + theme_void() + 
    theme(legend.position = "bottom", legend.title = element_blank())


iron(w1, w2)



# Subset the data in preparation to plot by Year. 
# These are done separated from the ggplot() function so that the color scheme
# line can call the correct HEX code for the drugs present in the subset.
waffle_2020 <- subset %>%
  filter(Year %in% 2020)

waffle_2021 <- subset %>%
  filter(Year %in% 2021)

waffle_2022 <- subset %>%
  filter(Year %in% 2022)


# Generate the waffle plots.
w1 <- waffle_2020 %>%
  ggplot(aes(fill = Drug, values = Percentage)) +
  geom_waffle(n_rows = 5, size = 0.33, colour = "white") +
  facet_wrap(~Dataset+Year) +
  # Add the color scheme specific for the drugs present in the SUDORS subset.
  scale_fill_manual(name = NULL, 
                    labels = coloring[coloring$Drug %in% unique(waffle_2020$Drug), "Drug"],
                    values = coloring[coloring$Drug %in% unique(waffle_2020$Drug), "Color"]) +
  coord_equal() + theme_void() + 
  theme(legend.position = "bottom")


w2 <- waffle_2021 %>%
  ggplot(aes(fill = Drug, values = Percentage)) +
  geom_waffle(n_rows = 5, size = 0.33, colour = "white") +
  facet_wrap(~Dataset+Year) +
  # Add the color scheme specific for the drugs present in the CDC WONDER subset.
  scale_fill_manual(name = NULL, 
                    labels = coloring[coloring$Drug %in% unique(waffle_2021$Drug), "Drug"],
                    values = coloring[coloring$Drug %in% unique(waffle_2021$Drug), "Color"]) +
  coord_equal() + theme_void() + 
  theme(legend.position = "bottom")


w3 <- waffle_2022 %>%
  ggplot(aes(fill = Drug, values = Percentage)) +
  geom_waffle(n_rows = 5, size = 0.33, colour = "white") +
  facet_wrap(~Dataset+Year) +
  # Add the color scheme specific for the drugs present in the CDC WONDER subset.
  scale_fill_manual(name = NULL, 
                    labels = coloring[coloring$Drug %in% unique(waffle_2022$Drug), "Drug"],
                    values = coloring[coloring$Drug %in% unique(waffle_2022$Drug), "Color"]) +
  coord_equal() + theme_void() + 
  theme(legend.position = "bottom", legend.title = element_blank())


iron(w1, w2, w3)




# -----------------------------
# US Map plot.

# Code to find matches between AHRQ and CDC WONDER
#opioid_od[opioid_od$Dataset %in% "AHRQ", "Setting"] %>% unique()

# Generate a side-by-side set of plots showing "Drug = All Opioids" counts
# by state in 2022. Toggle the setting to show either inpatient or ER.


# Counts in AHRQ dataset.
ahrq_map_plot <- opioid_od %>%
  # Filter the placeholder numerical values.
  filter(Count %!in% 7777 & Count %!in% 8888 & Count %!in% 9999) %>%
  
  # Switch between the two settings, and change the subtitle name.
  filter(Setting %in% "Medical Facility - Inpatient") %>%
  #filter(Setting %in% "Medical Facility - Outpatient or ER") %>%
  
  # Filter the metadata settings.
  filter(Dataset %in% "AHRQ", State %in% datasets::state.name, 
         Year %in% 2022, Quarter %in% NA,
         `Underlying Cause of Death` %in% "All", Drug %in% "All Opioids",
         Characteristic %in% "Not Stratified", Level %in% "N/A") %>%
  
  # plot_usmap() requires specific nomenclature for the column with states.
  rename(state = State) %>%
    
  # Plot settings and features.
  plot_usmap(data = ., values = "Count", color = "red") + 
    scale_fill_continuous(
      low = "white", high = "red", name = "Count (2022)", label = scales::comma
    ) + 
  labs(title = "AHRQ") +
  theme(legend.position = "")



# Counts in CDC WONDER dataset.
wonder_map_plot <- opioid_od %>%
  # Filter the placeholder numerical values.
  filter(Count %!in% 7777 & Count %!in% 8888 & Count %!in% 9999) %>%
  
  # Switch between the two settings, and change the subtitle name.
  filter(Setting %in% "Medical Facility - Inpatient") %>%
  #filter(Setting %in% "Medical Facility - Outpatient or ER") %>%
  
  # Filter the metadata settings.
  filter(Dataset %in% "CDC WONDER", State %in% datasets::state.name, 
         Year %in% 2022, Quarter %in% NA,
         `Underlying Cause of Death` %in% "All", Drug %in% "All Opioids",
         Characteristic %in% "Not Stratified", Level %in% "N/A") %>%
  
  # plot_usmap() requires specific nomenclature for the column with states.
  rename(state = State) %>%
  
  # Plot settings and features.
  plot_usmap(data = ., values = "Count", color = "red") + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Count (2022)", label = scales::comma
  ) + 
  labs(title = "\ \ \ \ \ \ CDC WONDER") +
  theme(legend.position = "right")



# Compile plots to display side-by-side.
plot_together <- plot_grid(ahrq_map_plot, wonder_map_plot, labels = "AUTO")

# Generate the main title.
title <- ggdraw() + 
  draw_label("National Opioid Overdose Counts for All Opioid Types in 2022", x = 0, y = 0.2, hjust = 0, vjust =1) +
  draw_label("Underlying Cause of Death: All Setting: Medical Facility - Inpatient.", x = 0, y = 0.1, hjust = 0, size = 12) +
  theme(plot.margin = margin(0, 0, 0, 7))

# Display the plot with title.
plot_grid(title, plot_together, ncol = 1, rel_heights = c(1, 1))




# -----------------------------
# US Map plot. Interactive.

us_geo <- tigris::states(cb = TRUE, resolution = '20m') %>%
  shift_geometry()

geo_data_wonder <- opioid_od %>%
  # Filter the placeholder numerical values.
  filter(Count %!in% 7777 & Count %!in% 8888 & Count %!in% 9999) %>%
  
  # Switch between the two settings, and change the subtitle name.
  filter(Setting %in% "Medical Facility - Inpatient") %>%
  #filter(Setting %in% "Medical Facility - Outpatient or ER") %>%
  
  # Filter the metadata settings.
  filter(Dataset %in% "CDC WONDER", State %in% datasets::state.name, 
         Year %in% 2022, Quarter %in% NA,
         `Underlying Cause of Death` %in% "All", Drug %in% "All Opioids",
         Characteristic %in% "Not Stratified", Level %in% "N/A") %>%
  rename(NAME = State) %>%
  left_join(., us_geo, by = "NAME")


# Counts in AHRQ dataset.
geo_data_ahrq <- opioid_od %>%
  # Filter the placeholder numerical values.
  filter(Count %!in% 7777 & Count %!in% 8888 & Count %!in% 9999) %>%
  
  # Switch between the two settings, and change the subtitle name.
  filter(Setting %in% "Medical Facility - Inpatient") %>%
  #filter(Setting %in% "Medical Facility - Outpatient or ER") %>%
  
  # Filter the metadata settings.
  filter(Dataset %in% "AHRQ", State %in% datasets::state.name, 
         Year %in% 2022, Quarter %in% NA,
         `Underlying Cause of Death` %in% "All", Drug %in% "All Opioids",
         Characteristic %in% "Not Stratified", Level %in% "N/A") %>%
  rename(NAME = State) %>%
  left_join(., us_geo, by = "NAME")


# Different maps.
# https://leaflet-extras.github.io/leaflet-providers/preview/

map_wonder <- mapview(st_as_sf(geo_data_wonder), zcol = "Count", layer.name = "CDC WONDER", map.types = c("TomTom.Basic", "OpenAIP"))
map_ahrq <- mapview(st_as_sf(geo_data_ahrq), zcol = "Count", layer.name = "AHRQ", map.types = c("TomTom.Basic", "OpenAIP"))


map_wonder|map_ahrq





library(leaflet)
library(leaflet.extras2)
library(sf)
library(yyjsonr)

data <- sf::st_as_sf(leaflet::atlStorms2005[1, ])
data <- st_cast(data, "POINT")
data$time <- as.POSIXct(
  seq.POSIXt(Sys.time() - 1000, Sys.time(), length.out = nrow(data))
)

leaflet() %>%
  addTiles() %>%
  addTimeslider(
    data = data,
    options = timesliderOptions(
      position = "topright",
      timeAttribute = "time",
      range = TRUE
    )
  ) %>%
  setView(-72, 22, 4)




## ----------------------------------------------------------------
## RSV INFECTIONS DATA


# -----------------------------
# Basic time-series line plot.


# Define the start of the infection season.
season_start = week("01-10-2020")

rsv %>%
  # Drop down to choose the HHS region: Region 1 through 10.
  filter(Region %in% "Region 10") %>% 
  
  
  # Filter the metadata settings.
  filter(`Region Type` %in% "HHS", `Week Observed` %!in% NA, 
         Dataset %in% "NREVSS Lab") %>%
  # Shift the week number so that week = 0 at the beginning of the start of the
  # infection season.
  mutate(week = ifelse(week(`Week Observed`) - season_start < 0, 
                       week(`Week Observed`) - season_start + 53, 
                       week(`Week Observed`) - season_start) ) %>%
  
  # Plot settings and features.
  ggplot(data = ., aes(x = week, y = `Crude Rate`)) +
  geom_line(aes(color = Season)) +
  facet_grid(cols = vars(`Diagnostic Test Type`)) +
  labs(title = "Respiratory Syncytial Virus Infection (RSV) Rate",
       subtitle = "Surveillance Program: NREVSS Lab.",
       x = "Weeks Since October", y = "Crude Rate (per 100,000)") +
  theme_minimal()





# -----------------------------
# US Map plot.

# Code to find mathes between AHRQ and CDC WONDER
#opioid_od[opioid_od$Dataset %in% "AHRQ", "Setting"] %>% unique()

# Generate a side-by-side set of plots showing "Drug = All Opioids" counts
# by state in 2022. Toggle the setting to show either inpatient or ER.


# Counts in AHRQ dataset.
ahrq_map_plot <- opioid_od %>%
  # Filter the placeholder numerical values.
  filter(Count %!in% 7777 & Count %!in% 8888 & Count %!in% 9999) %>%
  
  # Switch between the two settings, and change the subtitle name.
  filter(Setting %in% "Medical Facility - Inpatient") %>%
  #filter(Setting %in% "Medical Facility - Outpatient or ER") %>%
  
  # Filter the metadata settings.
  filter(Dataset %in% "AHRQ", State %in% datasets::state.name, 
         Year %in% 2022, Quarter %in% NA,
         `Underlying Cause of Death` %in% "All", Drug %in% "All Opioids",
         Characteristic %in% "Not Stratified", Level %in% "N/A") %>%
  
  # plot_usmap() requires specific nomenclature for the column with states.
  rename(state = State) %>%
  
  # Plot settings and features.
  plot_usmap(data = ., values = "Count", color = "red") + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Count (2022)", label = scales::comma
  ) + 
  labs(title = "AHRQ") +
  theme(legend.position = "")













