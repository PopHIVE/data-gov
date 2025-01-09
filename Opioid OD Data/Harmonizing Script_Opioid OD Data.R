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

"%!in%" <- function(x,y)!("%in%"(x,y))




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

## Stratification by sex, race, and age group, where available. The specific
## drug and drug combination with clinically significant polysubstance overdoses
## is noted as well.
## 
## Opioids: ICD-10 T40.0 (Opium), T40.1 (Heroin), T40.2 (Other opioids),
##          T40.3 (Methadone), T40.4 (Other synthetic narcotics),
##          T40.6 (Other and unspecified narcotics), F11.0 (Mental and
##          behavioural disorders due to use of opioids, acute intoxication).
##          For the SUDORS data sets, ICD codes were not required. Data could
##          reflect coronary toxicology results not explicitly categorized the
##          same as ICD.
## 
## Stimulants: T40.5 (Cocaine) and general stimulants (as defined by SUDORS).
## 
## Depressants: T42 (Benzodiazepines) and general depressants (as defined by SUDORS).
##
## Polysubstance: combinations of opioids with Cocaine or Benzodiazepines.

## Settings are defined to be all based on WONDER's definition.


## ----------------------------------------------------------------
## HARMONIZE - SUDORS

## Submitted a request with the CDC 1/8/2024 to ask for deaths by class of
## drugs for the stratification classes. Right now it only reports all drug
## deaths together.
## Ask how they developed the SUDORS dashboard.
## CASE ID: CDC-3344821-C5Q4L2

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

# Select the necessary columns of information.
sudors_subset <- sudors_raw[, colnames(sudors_raw) %in% keep_columns]


# Separate out the columns that report the incidence rate.
sudors_rate_only <- sudors_subset[, c(1:2, str_which(colnames(sudors_subset), "rate"))]

# Further separate out the columns for drug-specific overdose rates and characteristics.
sudors_rate_characteristic <- sudors_rate_only %>% pivot_longer("male_rate":"age_65plus_rate", names_to = "Level", values_to = "Rate") %>% 
  as.data.frame() %>% .[, c("Jurisdiction", "year", "Level", "Rate")]

sudors_rate_drug <- sudors_rate_only %>% pivot_longer("opioids_rate":"benzodiazepines_rate", names_to = "Drug", values_to = "Rate") %>% 
  as.data.frame() %>% .[, c("Jurisdiction", "year", "Drug", "Rate")]


# Prepare a "Characteristic" column for filling with broad characteristic classes.
sudors_rate_characteristic$Characteristic <- "NA"

# Fill those "Characteristic" entries.
sudors_rate_characteristic$Level <- str_replace(sudors_rate_characteristic$Level, "_rate", "")
sudors_rate_characteristic[str_detect(sudors_rate_characteristic$Level, "aian_nh|asian_nh|black_nh|multi_nh|nhpi_nh|white_nh|hisp"), "Characteristic"] <- "Race/Ethnicity"
sudors_rate_characteristic[str_detect(sudors_rate_characteristic$Level, "age"), "Characteristic"]  <- "Age"
sudors_rate_characteristic[str_detect(sudors_rate_characteristic$Level, "male"), "Characteristic"] <- "Sex"

# Add the death setting and drug classification to the rate by characteristic subset.
sudors_rate_characteristic$Drug    <- "All"
sudors_rate_characteristic$Setting <- "All"

# Remove the note of "_rate" from the "Drug" name.
sudors_rate_drug$Drug <- str_replace(sudors_rate_drug$Drug, "_rate", "")

# Add the death setting, characteristic, and level to the drug rate subset.
sudors_rate_drug$Characteristic <- "Not Stratified"
sudors_rate_drug$Level          <- "N/A"
sudors_rate_drug$Setting        <- "All"


# Add rates stratified by drug classification and drug type by row.
sudors_rate <- bind_rows(sudors_rate_characteristic, sudors_rate_drug) %>% `rownames<-`(NULL)


# Separate out the columns that report the incidence count, reported as "deaths".
sudors_count_only <- sudors_subset[, c(1:2, str_which(colnames(sudors_subset), "deaths"))] 

# Further separate out the columns for drug-specific overdose rates and characteristics.
sudors_count_characteristic <- sudors_count_only %>% pivot_longer("male_deaths":"age_65plus_deaths", names_to = "Level", values_to = "Count") %>% 
  as.data.frame() %>% .[, c("Jurisdiction", "year", "Level", "Count")]

sudors_count_drug <- sudors_count_only %>% pivot_longer(c("opioids_deaths":"opioids_nostim_deaths", "naloxone_deaths"), names_to = "Drug", values_to = "Count") %>% 
  as.data.frame() %>% .[, c("Jurisdiction", "year", "Drug", "Count")]

sudors_count_setting <- sudors_count_only %>% pivot_longer("ed_deaths":"hospital_deaths", names_to = "Setting", values_to = "Count") %>% 
  as.data.frame() %>% .[, c("Jurisdiction", "year", "Setting", "Count")]


# Prepare a "Characteristic" column for filling with broad characteristic classes.
sudors_count_characteristic$Characteristic <- "NA"

# Fill those "Characteristic" entries.
sudors_count_characteristic$Level <- str_replace(sudors_count_characteristic$Level, "_deaths", "")
sudors_count_characteristic[str_detect(sudors_count_characteristic$Level, "aian_nh|asian_nh|black_nh|multi_nh|nhpi_nh|white_nh|hisp"), "Characteristic"] <- "Race/Ethnicity"
sudors_count_characteristic[str_detect(sudors_count_characteristic$Level, "age"), "Characteristic"]  <- "Age"
sudors_count_characteristic[str_detect(sudors_count_characteristic$Level, "male"), "Characteristic"] <- "Sex"

# Add the death setting and drug classification to the characteristic count subset.
sudors_count_characteristic$Drug    <- "All"
sudors_count_characteristic$Setting <- "All"

# Remove the note of "_deaths" from the "Drug" name.
sudors_count_drug$Drug <- str_replace(sudors_count_drug$Drug, "_deaths", "")

# Add the death setting, characteristic, and level to the drug count subset.
sudors_count_drug$Characteristic <- "Not Stratified"
sudors_count_drug$Level          <- "N/A"
sudors_count_drug$Setting        <- "All"


# Adjust the setting nomenclature to match HCUP, with the exception that
# emergency department visits are ones that resulted in a death for SUDORS.
sudors_count_setting$Setting <- str_replace(sudors_count_setting$Setting, "ed_deaths", "ED")
sudors_count_setting$Setting <- str_replace(sudors_count_setting$Setting, "hospital_deaths", "IP")

# Add the death characteristic, level, and type of drug to the setting count subset.
sudors_count_setting$Characteristic <- "Not Stratified"
sudors_count_setting$Level          <- "N/A"
sudors_count_setting$Drug           <- "All"


# Add counts stratified by drug classification, drug type, and setting by row.
sudors_count <- bind_rows(sudors_count_characteristic, sudors_count_drug, sudors_count_setting) %>% `rownames<-`(NULL)


# Join the rates and counts.
sudors <- right_join(sudors_rate, sudors_count, 
                     by = c("Jurisdiction", "year", "Characteristic", 
                            "Level", "Drug", "Setting"))

# Reorder the columns.
sudors <- sudors %>% select(Jurisdiction, year, Setting, Drug, Characteristic, Level, Rate, Count)

# Adjust the column names.
colnames(sudors)[1:2] <- c("State", "Year")


# Adjust the drug nomenclature.
sudors$Drug <- str_to_title(sudors$Drug)
sudors$Drug <- str_replace(sudors$Drug, "Rxopioids", "RX Opioids")
sudors$Drug <- str_replace(sudors$Drug, "Imfs", "Illegally-Made Fentanyls")
sudors$Drug <- str_replace(sudors$Drug, "Opioids_stim", "Opioids + Stimulant")
sudors$Drug <- str_replace(sudors$Drug, "Opioids_nostim", "Opioids + Depressant")

# Adjust the race/ethnicity nomenclature.
sudors[str_detect(sudors$Level, "aian_nh"), "Level"]  <- "American Indian/Alaska Natives"
sudors[str_detect(sudors$Level, "asian_nh"), "Level"] <- "Asian"
sudors[str_detect(sudors$Level, "black_nh"), "Level"] <- "Black"
sudors[str_detect(sudors$Level, "multi_nh"), "Level"] <- "Multi-Race, non-Hispanic"
sudors[str_detect(sudors$Level, "nhpi_nh"), "Level"]  <- "Native Hawaiian/Pacific Islander"
sudors[str_detect(sudors$Level, "white_nh"), "Level"] <- "White"
sudors[str_detect(sudors$Level, "hisp"), "Level"]     <- "Hispanic"

# Adjust the sex nomenclature.
sudors[str_detect(sudors$Level, "^male"), "Level"]  <- "Males"
sudors[str_detect(sudors$Level, "female"), "Level"] <- "Females"

# Adjust the age nomenclature.
sudors[str_detect(sudors$Level, "age_under15"), "Level"] <- "<15 Year"
sudors[str_detect(sudors$Level, "age_15_24"), "Level"]   <- "15-24 Years"
sudors[str_detect(sudors$Level, "age_25_34"), "Level"]   <- "25-34 Years"
sudors[str_detect(sudors$Level, "age_35_44"), "Level"]   <- "35-44 Years"
sudors[str_detect(sudors$Level, "age_45_54"), "Level"]   <- "45-54 Years"
sudors[str_detect(sudors$Level, "age_55_64"), "Level"]   <- "55-64 Years"
sudors[str_detect(sudors$Level, "age_65plus"), "Level"]  <- "65+ Years"



# Suppress the low counts
# Combine age groups to match hcup?
# "Rate of overdose deaths by jurisdiction and select drug or drug class" by ICD-10 codes



## ----------------------------------------------------------------
## HARMONIZE - HCUP

head(hcup_raw_rates)

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
hcup <- select(hcup, c("State", "Year", "Quarter", "Setting", "Characteristic",
                       "Level", "Rate", "Count"))


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
hcup <- hcup[-str_which(hcup$Year, "2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015"), ] %>% `rownames<-`(NULL)


# Clean up the characteristic level of age.
hcup$Level <- str_replace(hcup$Level, "Age ", "")


## There are two types of hospital setting reflected in this data set. Only the
## WONDER data set differentiates them. 


# add column for drug type






## ----------------------------------------------------------------
## HARMONIZE - WONDER

# Cause of death: all or X40-44/Y10-14
# Dates by month and quarter
# Ages to HCUP ranges
# Place of death: all or 









