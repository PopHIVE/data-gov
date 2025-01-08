## ----------------------------------------------------------------
## Harmonizing Opioid OD Data
##
## Date: December 23rd, 2024
## Source:
##    - CDC SUDORS and DOSE
##    - Agency for Healthcare Research and Quality (AHRQ)
##    - CDC WONDER

## ----------------------------------------------------------------
## SET UP THE ENVIRONMENT

library("readxl")
library("tidyr")
library("dplyr")
library("stringr")




## ----------------------------------------------------------------
## LOAD THE DATA

sudors_raw <- read_excel("Opioid OD Data/Raw Download/CDC SUDORS_Dashboard Output_Download 12.23.2024.xlsx", 
                         sheet = "Data") %>% as.data.frame()

## HCUP data sets
hcup_raw_rates  <- read_excel("Opioid OD Data/Raw Download/Healthcare Cost and Utilization Project (HCUP)_Opioid Related Hospital Use_Downloaded 12.23.2024.xlsx", 
                              sheet = "Quarterly Rates", skip = 1) %>% as.data.frame()

hcup_raw_annual <- read_excel("Opioid OD Data/Raw Download/Healthcare Cost and Utilization Project (HCUP)_Opioid Related Hospital Use_Downloaded 12.23.2024.xlsx", 
                              sheet = "Rates Annual", skip = 1) %>% as.data.frame()

hcup_raw_counts <- read_excel("Opioid OD Data/Raw Download/Healthcare Cost and Utilization Project (HCUP)_Opioid Related Hospital Use_Downloaded 12.23.2024.xlsx", 
                              sheet = "Quarterly Counts", skip = 1) %>% as.data.frame()


## WONDER
wonder_raw <- read.delim("/Users/sg2736/Desktop/DSDE/Projects/Repos and Codespaces/data-gov/Opioid OD Data/Raw Download/CDC Wonder_Multiple Cause of Death 2018-2022_Not Age Adjusted Rates_Downloaded 01.06.2024.txt")



## ----------------------------------------------------------------
## HARMONIZE

## The format used in the CDC's WONDER data set is the target cross format
## for all three data sets with the added columns for "Characteristic" and
## "Characteristic Level" as in HCUP.

head(wonder_raw)
head(hcup_raw_rates)[, 1:6]


## ----------------------------------------------------------------
## HARMONIZE - SUDORS

# "Rate of overdose deaths by jurisdiction and select drug or drug class" by ICD-10 codes

keep_columns <- c("Jurisdiction", "year", "opioids_deaths", "opioids_rate",
                  "heroin_deaths", "heroin_rate",
                  "imfs_deaths", "imfs_rate",
                  "rxopioids_deaths", "rxopioids_rate",
                  "cocaine_deaths", "cocaine_rate",
                  "benzodiazepines_deaths", "benzodiazepines_rate",
                  "opioids_stim_deaths", "opioids_nostim_deaths",
                  "male_deaths", "male_rate", "female_deaths", "female_rate",
                  "aian_nh_deaths", "aian_nh_rate", "asian_nh_deaths", 
                  "asian_nh_rate", "black_nh_deaths", "black_nh_rate",
                  "multi_nh_deaths", "multi_nh_rate", "nhpi_nh_deaths", 
                  "nhpi_nh_rate", "white_nh_deaths", "white_nh_rate", 
                  "hisp_deaths", "hisp_rate", "age_under15_deaths", 
                  "age_under15_rate", "age_15_24_deaths", "age_15_24_rate",
                  "age_25_34_deaths", "age_25_34_rate", "age_35_44_deaths",
                  "age_35_44_rate", "age_45_54_deaths", "age_45_54_rate",
                  "age_55_64_deaths", "age_55_64_rate", "age_65plus_deaths",
                  "age_65plus_rate", "naloxone_deaths", 
                  "ed_deaths", "hospital_deaths")

sudors_subset <- sudors_raw[, colnames(sudors_raw) %in% keep_columns]




## ----------------------------------------------------------------
## HARMONIZE - HCUP

head(hcup_raw_rates)

## Currently, this is in a mix of long and wide formats. While the column for
## "Characteristic" is currently long, in contrast to the same kinds of information
## in the CDC's WONDER data set which is wide, this orientation is preferable.
## This data set does not fully stratify across all unique combinations of the 
## subcategories, but are instead aggregated summary stats for one characteristic
## stratification. We do, however, want to elongate the quarterly updates.

hcup_rates  <- pivot_longer(hcup_raw_rates, "2005Q1":"2022Q4", names_to = "Dates", values_to = "Rates") %>% as.data.frame()
hcup_annual <- pivot_longer(hcup_raw_annual, "2005":"2022", names_to = "Dates", values_to = "Rates") %>% as.data.frame()
hcup_counts <- pivot_longer(hcup_raw_counts, "2005Q1":"2022Q4", names_to = "Dates", values_to = "Rates") %>% as.data.frame()


# Join the rates and counts separated by quarter.
hcup <- right_join(hcup_rates, hcup_counts, 
                   by = c("STATE", "Hospital Setting", "Characteristic", 
                          "Characteristic Level", "Dates"))

# Adjust the column names.
colnames(hcup)[c(1:2, 4, 6:7)] <- c("State", "Setting", "Level", "Rate", "Count")

# Separate out the year and quarter for the value.
hcup$Year    <- str_sub(hcup$Dates, start = 1, end = 4)
hcup$Quarter <- str_sub(hcup$Dates, start = 5, end = 6)

# Reorder the columns and remove the unnecessary ones.
hcup <- select(hcup, c("State", "Setting", "Characteristic", "Level", "Year", 
                       "Quarter", "Rate", "Count"))


# Adjust the column names.
colnames(hcup_annual)[c(2, 4:5)] <- c("Setting", "Level", "Year")

# Calculate the counts by year.
hcup_annual_counts <- hcup %>% 
  group_by(State, Setting, Characteristic, Level, Year)  %>% 
  summarise(Count_year = sum(Count)) %>%
  ungroup() %>%
  as.data.frame()

# Join the rates and counts separated by year only.
hcup_annual <- right_join(hcup_annual, hcup_annual_counts, 
                         by = c("State", "Setting", "Characteristic", 
                                "Level", "Year"))

# Adjust the column names.
colnames(hcup_annual)[c(6:7)] <- c("Rate", "Count")


# Add quarterly and annual rates/counts by row.
hcup <- bind_rows(hcup, hcup_annual) %>% `rownames<-`(NULL)


# Remove the unnecessary "Characteristic" outcomes.
hcup <- hcup[-str_which(hcup$Characteristic, "Community-Level Income|Patient Location"), ]

# Remove the years where ICD-9 codes were used instead of ICD-10.
hcup <- hcup[-str_which(hcup$Year, "2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015"), ]



## There are two types of hospital setting reflected in this data set. The others
## do not differentiate them, or in some cases only include occurrences from either
## Emergency Departments (ED) or In-Patient (IP).





## ----------------------------------------------------------------
## HARMONIZE - WONDER

# Cause of death: all or X40-44/Y10-14
# Dates by month and quarter
# Ages to HCUP ranges
# Place of death: all or 









