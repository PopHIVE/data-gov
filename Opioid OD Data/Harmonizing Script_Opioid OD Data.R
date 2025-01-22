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
## 
## Settings are defined to be all based on WONDER's definition. Otherwise,
## events are stratified based on "Inpatient" or "Emergency Department" status.


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


# -----------------------------
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


# -----------------------------
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


# -----------------------------
# Add missing columns.

# All of the SUDORS overdose events are classified as either ICD-10 X40–X44 or 
# Y10–Y14, which are unintentional deaths.
sudors$Manner_of_Death <- "Unintentional"

# While quarterly rates and counts are available on the dashboard, the values
# could not be easily exported. They required manual transcription for
# every "State" and "Year" combination, and so it was skipped.
sudors$Quarter <- NA


# -----------------------------
# Identify and correct non-value numerical codings to match the other datasets.

# Rates based on < 20 drug overdose deaths are suppressed. These numerical
# placeholders will be changed to adhere to the nomeclature used in the CDC
# WONDER data cleaning steps.
#     - Not Applicable = NA
#     - Unreliable = 0.888
#     - Suppressed = 0.999
#     - Incomplete = 0.777 (for generating the quarters)

# Confirm that "Counts < 20" are marked as suppressed.
sudors[sudors$Count < 20, "Rate"] %>% unique()

# We see that some of these values are unexpectedly labeled as NA instead.
sudors[sudors$Count < 20 & sudors$Rate %in% NA, ]

# All of the Counts where "Rate = NA" meet the criteria to be classified as
# a suppressed value, and so we'll adjust those.
(sudors[sudors$Count < 20 & sudors$Rate %in% NA, "Count"] > 0) %>% all()
(sudors[sudors$Count < 20 & sudors$Rate %in% NA, "Count"] < 20) %>% all()

# Convert "Rate = NA" to "Rate = 9999" for these rows.
sudors[sudors$Count < 20 & sudors$Rate %in% NA, "Rate"] <- 9999


# Confirm that no values with "Count >= 20" are marked as suppressed.
any(sudors[sudors$Count >= 20, "Rate"] %in% 9999) == FALSE

# Rates are age-adjusted for  drug-, sex-, and race/ethnicity-specific rates 
# using the 2010 U.S. Census population. NOTE: the numerator represents the
# occurrent population with the residency population as the denominator. To
# clarify these rate differences, they will be split between two columns.
# Rates are multiplied by the constant per 100,000 persons.

# Rates < 20 are suppressed. In the other two datasets, a suppressed value
# indicates one that is suppressed for confidentiality as opposed for limiting
# the reporting of unstable rates. Because the SUDORS data export only reports
# the annual counts, it is unlikely that confidentiality is of concern for them.
# To conform with the other two datasets nomenclature, these will be all changed 
# to the numerical coding for Unstable: 8888.

sudors[sudors$Rate %in% 9999, "Rate"] <- 8888

# Counts that are zero do not have "Rate = 0", as would be expected. These will
# be changed.
sudors[sudors$Count == 0, "Rate"] %>% unique()
sudors[sudors$Count == 0, "Rate"] <- 0

# There are some row entries where the "Rate = NA" when a valid "Count" is present.
# This could be because the population for that group was unknown, but notably
# this only occurs for "Characteristic = Not Stratified" entries.
sudors[sudors$Rate %in% NA, ]
sudors[sudors$Rate %in% NA, "Characteristic"] %>% unique()

# There may be opportunities to calculate the "Crude Rate" using the age-grouped
# population counts. It is not, however, readily available for us to calculate
# the age-adjusted counts without delving into the 2010 U.S. Census population
# counts. For the sake of time, this is skipped.


# -----------------------------
# Harmonize the age-groups to match HCUP.

# For each HCUP group, sum the following age groupings:
#     "<1 Year":  *Included with the "<15 Year" group.
#  "1-24 Years":  "<15 Year", "15-24 Years"
# "25-44 Years":  "25-34 Years", "35-44 Years"
# "45-64 Years":  "45-54 Years" "55-64 Years"
#   "65+ Years":  "65+ Years"

# Subset the dataset to only reflect the entries for "Age"
sudors_age <- sudors[sudors$Characteristic %in% "Age", ] %>%
  mutate(`Crude Rate` = Rate, `Age Adjusted Rate` = NA) %>%
  select(State, Year, Quarter, Setting, Manner_of_Death, Drug, Characteristic, Level, Count, `Crude Rate`, `Age Adjusted Rate`) %>%
  `rownames<-`(NULL)


# When aggregating these age groups, we'll need to recalculate the rate. As
# described above, rates for entries stratified by age are not age-adjusted.
# Therefore, we can use the simple formula to back calculate the population
# used to normalize counts:
#     (counts of events in the occurrent population / residency population) * 100,000 = crude rate
# 
# NOTE: Each rate will be using a numerator and denominator value relevant only
#       to that age-group's population numbers.


# Confirm that there are no NA's in the "Crude Rate" column.
(sudors_age$`Crude Rate` %in% NA) %>% any() == FALSE

# All "Counts" are a count and not a numerical placeholder denoting something
# like a suppressed or unreliable value. We therefore do not need to be concerned
# about anything other than incomplete representation.
sudors_age$Count %>% table()

# We do have some "Counts = 0", and these are all associated with a "Crude Rate = 0".
sudors_age[sudors_age$Count == 0, "Crude Rate"]

# All the numerically coded rates qualify as unstable ("Counts < 20").
(sudors_age[sudors_age$`Crude Rate` == 8888, "Count"] < 20) %>% all()

# When back-calculating the population, we need to exclude rows where the
# "Crude Rate = 0 or 8888".
sudors_age$Population <- NA

# Back-calculate the population used to generate the "Crude Rate".
sudors_age[sudors_age$`Crude Rate` %!in% 0 & sudors_age$`Crude Rate` %!in% 8888, "Population"] <- 
  sudors_age[sudors_age$`Crude Rate` %!in% 0 & sudors_age$`Crude Rate` %!in% 8888, ] %>% 
  (\(x) { floor((x$Count * 100000) / x$`Crude Rate`) }) ()

# Now we can regenerate the "Crude Rate" for some aggregated age groups.

# Based on the HCUP dataset, we'll want to group prospective age ranges so
# the algorithm knows what gets batched together.
sudors_age$Group <- NA

sudors_age[sudors_age$Level %in% c("<15 Year", "15-24 Years"), "Group"]    <- 2
sudors_age[sudors_age$Level %in% c("25-34 Years", "35-44 Years"), "Group"] <- 3
sudors_age[sudors_age$Level %in% c("45-54 Years", "55-64 Years"), "Group"] <- 4
sudors_age[sudors_age$Level %in% c("65+ Years"), "Group"]                  <- 5


# Generate a table for all of the possible combinations. Instead of using
# "Characteristic" or "Level" the temporary vector "Group" is used. This allows
# subsetting by the desired age groups for aggregation.
entries_to_group <- table(sudors_age$State, sudors_age$Year, sudors_age$Setting, sudors_age$Manner_of_Death, sudors_age$Drug, sudors_age$Group, useNA = "ifany") %>% 
  as.data.frame() %>% `colnames<-`(c("State", "Year", "Setting", "Manner_of_Death", "Drug", "Group", "Freq"))

# Some possible combinations are not even represented once. We will remove these 
# possible combinations to reduce the search space to only those that are expected.
entries_to_group <- entries_to_group[entries_to_group$Freq != 0, ] %>% `rownames<-`(NULL)

# Confirm that the number of times a combination shows up does not exceed
# the maximum number of age groups that are being aggregated together (2).
entries_to_group$Freq %>% max() == 2


# Now we need some reference tables for the algorithm:
# Label which months are associated with which group.
target_groups <- list("2" = c("<15 Year", "15-24 Years"), 
                      "3" = c("25-34 Years", "35-44 Years"),
                      "4" = c("45-54 Years", "55-64 Years"),
                      "5" = c("65+ Years"))

# Denote the new label to be applied to that group.
new_age_groups <- list("2" = c("<24 Years"), "3" = c("25-44 Years"), 
                       "4" = c("45-64 Years"), "5" = c("65+ Years"))


# The following function takes a few seconds. The progress bar has been
# added to show where the function is in the for loop.
pb = txtProgressBar(min = 0, max = nrow(entries_to_group), initial = 0)

final <- list()
#for(i in 1:nrow(entries_to_group)) {
  # Extract the current sub-stratification.
  combination = entries_to_group[i, ] %>% droplevels()
  
  # Subset the dataset based on this individual stratification.
  subset_df <- sudors_age[sudors_age$State %in% combination$State &
                            sudors_age$Year %in% combination$Year &
                            sudors_age$Setting %in% combination$Setting &
                            sudors_age$Manner_of_Death %in% combination$Manner_of_Death &
                            sudors_age$Drug %in% combination$Drug &
                            sudors_age$Group %in% combination$Group, ]
  
  
  # -----------------------------
  # Prepare the dataset with conditions for labeling incomplete information,
  # suppressed counts, and incomplete counts.
  
  # Remove entries where "Count = NA". Keep the other numeric codings for
  # interpretation later. If all entries are NA then do not remove the whole
  # subsetted dataset.
  if(all(subset_df$Count %in% NA)){
    subset_df
    
  } else if(all(subset_df$Count %!in% NA)) {
    subset_df <- subset_df[subset_df$Count %!in% NA, ] %>% `rownames<-`(NULL)
    
  }
  
  # Extract out the reference age groupings and the target one that the final
  # dataset will have.
  reference <- target_groups[names(target_groups) %in% unique(subset_df$Group)][[1]]
  target    <- new_age_groups[names(new_age_groups) %in% unique(subset_df$Group)][[1]]
  
  # Aggregate subset. Because these are simple counts, they result is simply 
  # a sum, unless there is missing or incomplete information.
  if(all(subset_df$Count %in% NA)) {
    grouped_counts <- NA
    
  } else if(all(subset_df$Count %!in% NA)) {
    grouped_counts <- sum(subset_df$Count)
    
    # Check if all expected months are present.
    dates_present  <- lapply(reference, function(x) all(x %in% subset_df$Level)) %>% unlist() %>% `names<-`(c(reference))

    # If some age groups are missing, then replace the counts with the placeholder
    # numeric 7777 to denote 
    if(all(dates_present) == FALSE) {
      # Change the count to a placeholder numeric.
      grouped_counts <- 7777
      
    }
    
  }
  
  # Store the total population or code is as NA/incomplete depending on the
  # presence/absence of a population value for each age-group subcategory.
  if( all(is.na(subset_df$Population)) ) {
    pop <- NA
      
  } else if( any(is.na(subset_df$Population)) ) {
    pop <- 7777
    
  } else if( all(is.na(subset_df$Population) == FALSE) ){
    pop <- sum(subset_df$Population)
    
  }
  
  # Similar series of if statements as for population. Only calculate the new
  # rate if both the summed counts and population counts are numeric != 7777.
  if( is.na(grouped_counts) & is.na(pop) ) {
    new_rate  <- NA
    
  } else if( (is.numeric(grouped_counts) & is.na(pop)) | (is.na(grouped_counts) & is.numeric(pop)) | grouped_counts == 7777 | pop == 7777 ) {
    # NOTE: To avoid the chances (while very small) that a rate will equal 7777
    #       the 0.7777 placeholder is used instead to quality check that "Crude
    #       Rate = 7777" only when information is incomplete.
    new_rate <- 0.7777
    
  } else if( is.numeric(grouped_counts) & is.numeric(pop) ) {
    new_rate <- round((grouped_counts * 100000) / pop, digits = 1)
    
  }
  
  
  # Print the for loop's progress.
  setTxtProgressBar(pb, i)
  
  # Column-merge the quartered results with the metadata. Add back in the
  # "Characteristic", "Level", and "Population" columns.
  final[[i]] <- combination %>% 
    mutate(Characteristic = "Age", Level = target, Quarter = NA,
           Count = grouped_counts, Population = pop, 
           `Crude Rate` = new_rate, `Age Adjusted Rate` = NA) %>%
    select(State, Year, Quarter, Setting, Manner_of_Death, Drug, Characteristic, Level, Count, Population, `Crude Rate`, `Age Adjusted Rate`)
  
}


# Compile all quartered stratifications to the final data frame.
sudors_age_grouped <- do.call(rbind, final) %>% `rownames<-`(NULL)

# Inspect the "Counts" and "Population" columns to confirm that the numerical
# placeholder value 0.7777 only shows up when information is incomplete.
sudors_age_grouped[sudors_age_grouped$`Crude Rate` %in% 0.7777, ]

# Before reattributing the placeholder value to 0.7777, check that none of the
# rates calculated happened to be 7777.
sudors_age_grouped[sudors_age_grouped$`Crude Rate` %in% 7777, ]

# Because both of these tests show that 7777 is only present when there is
# incomplete information to calculate the rate, we'll rename those placeholder
# numerics.
sudors_age_grouped[sudors_age_grouped$`Crude Rate` %in% 0.7777, "Crude Rate"] <- 7777



# Recall that some non-stratified entries lacked a rate. We wanted to see if
# the age-stratified population counts could be used to supplement a "Crude Rate"
# value at least.
no_rate <- sudors[sudors$Rate %in% NA, ] 

# First we'll check if any values match in their metadata.
entries_to_add_pop <- table(no_rate$State, no_rate$Year, no_rate$Setting, no_rate$Manner_of_Death, no_rate$Drug, useNA = "ifany") %>% 
  as.data.frame() %>% `colnames<-`(c("State", "Year", "Setting", "Manner_of_Death", "Drug", "Freq"))

entries_to_add_pop <- entries_to_add_pop[entries_to_add_pop$Freq %!in% 0, ] %>% `rownames<-`(NULL)


final <- list()
for(i in 1:nrow(entries_to_add_pop)) {
  # Extract the current sub-stratification.
  combination = entries_to_add_pop[i, ] %>% droplevels()
  
  # Subset the dataset based on this individual stratification.
  subset_df <- sudors_age_grouped[sudors_age_grouped$State %in% combination$State &
                                    sudors_age_grouped$Year %in% combination$Year &
                                    sudors_age_grouped$Setting %in% combination$Setting &
                                    sudors_age_grouped$Manner_of_Death %in% combination$Manner_of_Death &
                                    sudors_age_grouped$Drug %in% combination$Drug, ]
  
  final[[i]] <- nrow(subset_df)
}

# Looks like none of the entries have similar metadata. Therefore, we cannot
# supplement these rates.
unlist(final)[unlist(final) != 0]




sudors_no_age <- sudors[sudors$Characteristic %!in% "Age", ] %>%
  mutate(`Crude Rate` = NA, `Age Adjusted Rate` = Rate, Population = NA) %>%
  select(State, Year, Quarter, Setting, Manner_of_Death, Drug, Characteristic, Level, Count, Population, `Crude Rate`, `Age Adjusted Rate`) %>%
  `rownames<-`(NULL)

# Confirm 
all(colnames(sudors_age_grouped) %in% colnames(sudors_no_age)) &
  all(colnames(sudors_no_age) %in% colnames(sudors_age_grouped))

#write a function to order all rows

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

# Add a "Drug" variable. The HCUP documentation indicates that all overdoses
# include all kinds of opioids as defined by their ICD-10 codes.
hcup$Drug <- "All Opioids"


# HCUP states that the drug overdose codes include underlying cause of death
# equal to "accidental (unintentional) poisoning, intentional self-harm, assault, 
# undetermined, and adverse effect (except heroin) - with a seventh digit 
# indicating initial, subsequent encounter, or sequela)". This is interpreted
# to imply all underlying causes of deaths are counted together, which includes
# those specific for "Drug-induced causes".

hcup$Manner_of_Death <- "ALL"


# HCUP states that "annualized quarterly rates are calculated as the quarterly 
# count of inpatient stays or ED visits divided by one-fourth the annual 
# population, times 100,000. Rates are suppressed for confidentiality when numerator 
# counts are less than or equal to 25." No mention is made that the rates
# have been age-adjusted, and so they will all be assumed to be the crude rate.

hcup <- hcup %>%
  mutate(`Crude Rate` = Rate, `Age Adjusted Rate` = NA) %>%
  select(State, Year, Quarter, Setting, Manner_of_Death, Drug, Characteristic, Level, Count, `Crude Rate`, `Age Adjusted Rate`)


# The HCUP age groups are used as reference for the other two datasets. The
# only exception is ages < 1 years of age, which is included in the first
# age-group for the SUDORS dataset. This group is kept separate for HCUP and
# CDC WONDER.

# Rates are suppressed when counts are <= 25. It appears that no counts with
# less than 25 are reported.
hcup[hcup$Count <= 25, ]

# It's possible that suppressed values are reported as NA, though this is not noted
# in the HCUP data documentation.
hcup[hcup$`Crude Rate` %in% NA, ]


##
##
##
## There are two types of hospital setting reflected in this data set. Only the
## WONDER data set differentiates them. 




# Correct the class for each variable.
hcup[, c("State", "Quarter", "Setting", "Manner_of_Death", "Drug", "Characteristic", "Level")] <- sapply(hcup[, c("State", "Quarter", "Setting", "Manner_of_Death", "Drug", "Characteristic", "Level")], as.character)
hcup[, c("Count", "Crude Rate", "Age Adjusted Rate")] <- sapply(hcup[, c("Count", "Crude Rate", "Age Adjusted Rate")], as.numeric)
hcup[, c("Year")] <- sapply(hcup[, c("Year")], as.integer)






combined <- bind_rows(cbind("Dataset" = rep("AHRQ", nrow(hcup)), hcup), 
                      cbind("Dataset" = rep("SUDORS", nrow(sudors)), sudors))

#write.csv(combined, "SUDORS and AHRQ_01.09.2024.csv", row.names=FALSE)



## ----------------------------------------------------------------
## HARMONIZE - WONDER

# Cause of death: all or X40-44/Y10-14
# Dates by month and quarter
# Ages to HCUP ranges
# Place of death: all or 
# Naloxone?



#     - Not Applicable = NA
#     - Unreliable = 0.888
#     - Suppressed = 0.999
#     - Incomplete = 0.777 (for generating the quarters)









