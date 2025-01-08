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
                         sheet = "Data")

wonder_raw <- read.delim("/Users/sg2736/Desktop/DSDE/Projects/Repos and Codespaces/data-gov/Opioid OD Data/Raw Download/CDC Wonder_Multiple Cause of Death 2018-2022_Not Age Adjusted Rates_Downloaded 01.06.2024.txt")

hcup_raw   <- read_excel("Opioid OD Data/Raw Download/Healthcare Cost and Utilization Project (HCUP)_Opioid Related Hospital Use_Downloaded 12.23.2024.xlsx", 
                         sheet = "Quarterly Rates", skip = 1) %>% as.data.frame()




## ----------------------------------------------------------------
## HARMONIZE

## The format used in the CDC's WONDER data set is the target cross format
## for all three data sets.

head(wonder_raw)



## ----------------------------------------------------------------
## HARMONIZE - SUDORS




## ----------------------------------------------------------------
## HARMONIZE - HCUP

head(hcup_raw)

## Currently, this is in a mix of long and wide formats. While the column for
## "Characteristic" is currently long, in contrast to the same kinds of information
## in the CDC's WONDER data set which is wide, this orientation is preferable.
## This data set does not fully stratify across all unique combinations of the 
## subcategories, but are instead aggregated summary stats for one characteristic
## stratification. We do, however, want to elongate the quarterly updates.


hcup <- pivot_longer(hcup_raw, "2005Q1":"2022Q4", names_to = "Dates", values_to = "Rates") %>% as.data.frame()


hcup$Year    <- str_sub(hcup$Dates, start = 1, end = 4)
hcup$Quarter <- str_sub(hcup$Dates, start = 5, end = 6)

hcup <- select(hcup, c("STATE", "Hospital Setting", "Characteristic",
                       "Characteristic Level", "Year", "Quarter", "Rates"))

colnames(hcup)[1] <- "State"


## There are two types of hospital setting reflected in this data set. The others
## do not differentiate them, or in some cases only include occurrences from either
## Emergency Departments (ED) or In-Patient (IP).

hcup$`Hospital Setting` %>% unique()



## ----------------------------------------------------------------
## HARMONIZE - WONDER











