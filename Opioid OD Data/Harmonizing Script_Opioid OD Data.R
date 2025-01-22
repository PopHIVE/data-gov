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
wonder_raw <- read_csv("CDC WONDER_Cleaned_01.21.2024.csv") %>% as.data.frame()



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


#     - Not Applicable = NA
#     - Unreliable = 0.888
#     - Suppressed = 0.999
#     - Incomplete = 0.777 (for generating the quarters)




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
#     - Unreliable = 0.888 or 8888
#     - Suppressed = 0.999 or 9999
#     - Incomplete = 0.777 (for generating the quarters) or 7777

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


# Correct the class for each variable.
sudors_age_grouped[, c("State", "Year", "Quarter", "Setting", "Manner_of_Death", "Drug", "Characteristic", "Level")] <- sapply(sudors_age_grouped[, c("State", "Year", "Quarter", "Setting", "Manner_of_Death", "Drug", "Characteristic", "Level")], as.character)
sudors_age_grouped[, c("Count", "Crude Rate", "Age Adjusted Rate")] <- sapply(sudors_age_grouped[, c("Count", "Crude Rate", "Age Adjusted Rate")], as.numeric)
sudors_age_grouped[, c("Year", "Population")] <- sapply(sudors_age_grouped[, c("Year", "Population")], as.integer)


# Recombine the newly grouped age stratified data to the other data.
# NOTE: "Population" was only needed to regenerate the "Crude Rate".
#        it will not be kept in the final dataset with all three combined.
sudors_no_age <- sudors[sudors$Characteristic %!in% "Age", ] %>%
  mutate(`Crude Rate` = NA, `Age Adjusted Rate` = Rate, Population = NA) %>%
  select(State, Year, Quarter, Setting, Manner_of_Death, Drug, Characteristic, Level, Count, Population, `Crude Rate`, `Age Adjusted Rate`) %>%
  `rownames<-`(NULL)

# Confirm that all of the columns are the same. Don't want to accidentaly induce
# NA's.
all(colnames(sudors_age_grouped) %in% colnames(sudors_no_age)) &
  all(colnames(sudors_no_age) %in% colnames(sudors_age_grouped))

# Correct the class for each variable.
sudors_no_age[, c("State", "Year", "Quarter", "Setting", "Manner_of_Death", "Drug", "Characteristic", "Level")] <- sapply(sudors_no_age[, c("State", "Year", "Quarter", "Setting", "Manner_of_Death", "Drug", "Characteristic", "Level")], as.character)
sudors_no_age[, c("Count", "Crude Rate", "Age Adjusted Rate")] <- sapply(sudors_no_age[, c("Count", "Crude Rate", "Age Adjusted Rate")], as.numeric)
sudors_no_age[, c("Year", "Population")] <- sapply(sudors_no_age[, c("Year", "Population")], as.integer)


# Recombine to attain the final resulting table.
sudors_final <- bind_rows(sudors_no_age, sudors_age_grouped)




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

# None of the other typical numerical placeholders are present. Therefore, we
# cannot differentiate between NA for "Not Available or Not Applicable" from an
# intentionally suppressed or incomplete value.
sapply(hcup[, c("Count", "Crude Rate", "Age Adjusted Rate")], function(x){any(x %in% 999)})
sapply(hcup[, c("Count", "Crude Rate", "Age Adjusted Rate")], function(x){any(x %in% 9999)})

# Rates in the other datasets are reported to the tenths place.
hcup$`Crude Rate`        <- round(hcup$`Crude Rate`, digits = 1)
hcup$`Age Adjusted Rate` <- round(hcup$`Age Adjusted Rate`, digits = 1)


# -----------------------------
# Adjust the age groupings.

# Most groups for the other datasets are conformed to HCUP, except for the
# "<1 Year" and "1-24 Years" entries. These need to be combined.

# Subset based on the values that need to be summed.
hcup_age <- hcup[hcup$Characteristic %in% "Age" & hcup$Level %in% "<1 Year" | hcup$Level %in% "1-24 Years", ]

# Confirm that the only rates available for these entries are "Crude Rate".
hcup_age[, "Age Adjusted Rate"] %>% unique()


# There are counts labeled as NA that have a reported rate. These likely are 
# suppressed values, but without documentation that is only a guess.
hcup_age[hcup_age$Count %in% NA, "Crude Rate"] %>% unique()

# For all "Crude Rate = NA" the "Count" is also all NA.
hcup_age[hcup_age$`Crude Rate` %in% NA, "Count"] %>% unique()


# Generate a table for all of the possible combinations. Instead of using
# "Characteristic" or "Level" the temporary vector "Group" is used. This allows
# subsetting by the desired age groups for aggregation.
entries_to_group <- table(hcup_age$State, hcup_age$Year, hcup_age$Quarter, hcup_age$Setting, hcup_age$Manner_of_Death, hcup_age$Drug, useNA = "ifany") %>% 
  as.data.frame() %>% `colnames<-`(c("State", "Year", "Quarter", "Setting", "Manner_of_Death", "Drug", "Freq"))

# Some possible combinations are not even represented once. We will remove these 
# possible combinations to reduce the search space to only those that are expected.
entries_to_group <- entries_to_group[entries_to_group$Freq != 0, ] %>% `rownames<-`(NULL)

# Confirm that the number of times a combination shows up does not exceed
# the maximum number of age groups that are being aggregated together (2).
entries_to_group$Freq %>% max() == 2


final <- list()
for(i in 1:nrow(entries_to_group)) {
  # Extract the current sub-stratification.
  combination = entries_to_group[i, ] %>% droplevels()
  
  # Subset the dataset based on this individual stratification.
  subset_df <- hcup_age[hcup_age$State %in% combination$State &
                          hcup_age$Year %in% combination$Year &
                          hcup_age$Quarter %in% combination$Quarter &
                          hcup_age$Setting %in% combination$Setting &
                          hcup_age$Manner_of_Death %in% combination$Manner_of_Death &
                          hcup_age$Drug %in% combination$Drug, ]
  
  counts <- subset_df$Count
  rates  <- subset_df$`Crude Rate`
  
  # Store the new counts.
  if( all(is.numeric(counts)) ) {
    new_count <- sum(counts)
    
  } else if( all(is.na(counts)) ) {
    new_count <- NA
    
  } else if(nrow(subset_df) < 2 | (any(is.na(counts)) == TRUE & any(is.na(counts)) == FALSE) ) {
    new_count <- 7777
    
  }
  
  # Calculate the population from the provided counts and crude rates.
  if( all(is.numeric(rates)) ) {
    pop <- floor((counts * 100000) / rates) %>% sum()
    
  } else if( all(is.na(rates)) ) {
    pop <- NA
    
  } else if(nrow(subset_df) < 2 | (any(is.na(rates)) == TRUE & any(is.na(rates)) == FALSE) ) {
    pop <- 7777
    
  }
  
  # Calculate the new rate.
  if( is.numeric(new_count) & is.numeric(pop) ) {
    new_rate <- round((new_count * 100000) / pop, digits = 1)
    
  } else if( is.na(new_count) & is.na(pop) ) {
    new_rate <- NA
    
  } else if( new_count == 7777 | pop == 7777 | is.na(new_count) | is.na(pop) ) {
    new_rate <- 7777
    
  }
  
  # Column-merge the quartered results with the metadata. Add back in the
  # "Characteristic", "Level", and "Population" columns.
  final[[i]] <- combination %>% 
    mutate(Characteristic = "Age", Level = "<24 Years",
           Count = new_count, Population = pop, 
           `Crude Rate` = new_rate, `Age Adjusted Rate` = NA) %>%
    select(State, Year, Quarter, Setting, Manner_of_Death, Drug, Characteristic, Level, Count, Population, `Crude Rate`, `Age Adjusted Rate`)

}

hcup_grouped <- do.call(rbind, final)

# Correct the class for each variable.
hcup_grouped[, c("State", "Year", "Quarter", "Setting", "Manner_of_Death", "Drug", "Characteristic", "Level")] <- sapply(hcup_grouped[, c("State", "Year", "Quarter", "Setting", "Manner_of_Death", "Drug", "Characteristic", "Level")], as.character)
hcup_grouped[, c("Count", "Crude Rate", "Age Adjusted Rate")] <- sapply(hcup_grouped[, c("Count", "Crude Rate", "Age Adjusted Rate")], as.numeric)
hcup_grouped[, c("Year", "Population")] <- sapply(hcup_grouped[, c("Year", "Population")], as.integer)



# NOTE: "Population" was only needed to regenerate the "Crude Rate".
#        it will not be kept in the final dataset with all three combined.
hcup_not_age <- hcup[hcup$Level %!in% "<1 Year" & hcup$Level %!in% "1-24 Years", ] %>%
  mutate(Population = NA)

# Correct the class for each variable.
hcup_not_age[, c("State", "Year", "Quarter", "Setting", "Manner_of_Death", "Drug", "Characteristic", "Level")] <- sapply(hcup_not_age[, c("State", "Year", "Quarter", "Setting", "Manner_of_Death", "Drug", "Characteristic", "Level")], as.character)
hcup_not_age[, c("Count", "Crude Rate", "Age Adjusted Rate")] <- sapply(hcup_not_age[, c("Count", "Crude Rate", "Age Adjusted Rate")], as.numeric)
hcup_not_age[, c("Year", "Population")] <- sapply(hcup_not_age[, c("Year", "Population")], as.integer)


# Recombine to attain the final resulting table.
hcup_final <- bind_rows(hcup_not_age, hcup_grouped)




## ----------------------------------------------------------------
## HARMONIZE - WONDER

# The CDC WONDER dataset was cleaned in file "Compile CDC Wonder TXT Output.R".

head(wonder_raw)




## ----------------------------------------------------------------
## COMBINE ALL THREE

# -----------------------------
# Align "State" nomenclature.
unique(hcup_final$State)[unique(hcup_final$State) %in% c("US", datasets::state.name, "District of Columbia") == FALSE]
unique(wonder_raw$State)[unique(wonder_raw$State) %in% c("US", datasets::state.name, "District of Columbia") == FALSE]
unique(sudors_final$State)[unique(sudors_final$State) %in% c("US", datasets::state.name, "District of Columbia") == FALSE]

# The sets differ with how nation-wide values are noted. We'll adhere to noting
# this level of information as "US".

hcup_final[hcup_final$State %in% "National", "State"]    <- "US"
sudors_final[sudors_final$State %in% "Overall", "State"] <- "US"


# -----------------------------
# Align "Quarter" nomenclature.
unique(hcup_final$Quarter)[unique(hcup_final$Quarter) %in% c(NA, str_c("Q", 1:4)) == FALSE]
unique(wonder_raw$Quarter)[unique(wonder_raw$Quarter) %in% c(NA, str_c("Q", 1:4)) == FALSE]
unique(sudors_final$Quarter)[unique(sudors_final$Quarter) %in% c(NA, str_c("Q", 1:4)) == FALSE]

# All is good.


# -----------------------------
# Align "Setting" nomenclature.
unique(hcup_final$Setting)
unique(wonder_raw$Setting)
unique(sudors_final$Setting)

# The only portion that needs to be adjusted is how emergency department (ED)
# or inpatient (IP) overdose events are noted. Will adhere to the CDC WONDER's
# nomenclature.

hcup_final[hcup_final$Setting %in% "ED Treat-and-Release", "Setting"] <- "Medical Facility - Outpatient or ER"
sudors_final[sudors_final$Setting %in% "ED", "Setting"] <- "Medical Facility - Outpatient or ER"

hcup_final[hcup_final$Setting %in% "IP", "Setting"] <- "Medical Facility - Inpatient"
sudors_final[sudors_final$Setting %in% "IP", "Setting"] <- "Medical Facility - Inpatient"



# -----------------------------
# Align "Manner of Death" nomenclature.
unique(hcup_final$Manner_of_Death)
unique(wonder_raw$Manner_of_Death)
unique(sudors_final$Manner_of_Death)

# All nomenclature is the same between the sets. One thing to review is
# the interpretation of the AHRQ dataset for underlying causes of deaths.
# Its documentation states that deaths as a result of "Drug-induced causes"
# are "encompassed" but does not state that this is the only underlying cause of
# death. Therefore, it is labeled with "Manner of Death = All" by the CDC WONDER's
# definitions for UCD - Drug/Alcohol Induced Causes ICD-10 codes.

# Underlying cause of death that is "Unintentional" is defined by SUDORS, where
# only the ICD-10 codes X40-X44 and Y10-Y14 are included.

hcup_final <- rename(hcup_final, `Underlying Cause of Death` = Manner_of_Death)
wonder_raw <- rename(wonder_raw, `Underlying Cause of Death` = Manner_of_Death)
sudors_final <- rename(sudors_final, `Underlying Cause of Death` = Manner_of_Death)



# -----------------------------
# Align "Drug" nomenclature.
unique(hcup_final$Drug)
unique(wonder_raw$Drug)
unique(sudors_final$Drug)

# In SUDORS "Drug = Opioids" denotes deaths where any form of opioid is listed.
sudors_final[sudors_final$Drug %in% "Opioids", "Drug"] <- "All Opioids"

# Without further investigating further, it is not readily apparent  if 
# "Illegally-Made" or "RX" opioids can be easily defined by discrete ICD-10 codes.
# SUDORS did not require ICD-10 codings for opioid poisoning events; it is
# therefore not expected that these types of classifications can be cross-compared.
sudors_final[sudors_final$Drug %in% "RX Opioids", "Drug"] <- "Prescription Opioids"

# "All Opioids + Benzodiazepines/Cocaine" and "Opioids + Depressant/Stimulant"
# are loosely asssociated. SUDORS combines multiple kinds of depressants, including
# Benzodiazepines, and stimulants, including Cocaine. Without an exhaustive
# search of ICD-10 codes that could be batched with "All Opioids" in the CDC
# WONDER tool, these polysubstance were restricted to the most commonly used
# drugs in tandem with opioids: Benzodiazepines and Cocaine.
#
# They will be kept labeled differently, and prompted for plotting together on
# the Shiny app, or else labeled together with a footnote.
sudors_final[sudors_final$Drug %in% "Opioids + Stimulant", "Drug"] <- "All Opioids + Stimulant"
sudors_final[sudors_final$Drug %in% "Opioids + Depressant", "Drug"] <- "All Opioids + Depressant"



# -----------------------------
# Align "Characteristic" nomenclature.
unique(hcup_final$Characteristic)
unique(wonder_raw$Characteristic)
unique(sudors_final$Characteristic)

# "Characteristic = Total" implies not stratifed.
hcup_final[hcup_final$Characteristic %in% "Total", "Characteristic"] <- "Not Stratified"



# -----------------------------
# Align "Characteristic = Age" nomenclature.
hcup_final[hcup_final$Characteristic %in% "Age", "Level"] %>% unique()
wonder_raw[wonder_raw$Characteristic %in% "Age", "Level"] %>% unique()
sudors_final[sudors_final$Characteristic %in% "Age", "Level"] %>% unique()

# Confirm the strings all match exactly.
unique(hcup_final[hcup_final$Characteristic %in% "Age", "Level"])[unique(hcup_final[hcup_final$Characteristic %in% "Age", "Level"]) %in% c("<24 Years", "25-44 Years", "45-64 Years", "65+ Years") == FALSE]
unique(wonder_raw[wonder_raw$Characteristic %in% "Age", "Level"])[unique(wonder_raw[wonder_raw$Characteristic %in% "Age", "Level"]) %in% c("<24 Years", "25-44 Years", "45-64 Years", "65+ Years") == FALSE]
unique(sudors_final[sudors_final$Characteristic %in% "Age", "Level"])[unique(sudors_final[sudors_final$Characteristic %in% "Age", "Level"]) %in% c("<24 Years", "25-44 Years", "45-64 Years", "65+ Years") == FALSE]



# -----------------------------
# Align "Characteristic = Sex" nomenclature.
hcup_final[hcup_final$Characteristic %in% "Sex", "Level"] %>% unique()
wonder_raw[wonder_raw$Characteristic %in% "Sex", "Level"] %>% unique()
sudors_final[sudors_final$Characteristic %in% "Sex", "Level"] %>% unique()

# Change to "Sex = Male/Female" nomenclature.
hcup_final[hcup_final$Characteristic %in% "Sex" & hcup_final$Level %in% "Males", "Level"]   <- "Male"
hcup_final[hcup_final$Characteristic %in% "Sex" & hcup_final$Level %in% "Females", "Level"] <- "Female"

sudors_final[sudors_final$Characteristic %in% "Sex" & sudors_final$Level %in% "Males", "Level"]   <- "Male"
sudors_final[sudors_final$Characteristic %in% "Sex" & sudors_final$Level %in% "Females", "Level"] <- "Female"



# -----------------------------
# Align "Characteristic = Not Stratified" nomenclature.
hcup_final[hcup_final$Characteristic %in% "Not Stratified", "Level"] %>% unique()
wonder_raw[wonder_raw$Characteristic %in% "Not Stratified", "Level"] %>% unique()
sudors_final[sudors_final$Characteristic %in% "Not Stratified", "Level"] %>% unique()

# The location information was moved to the "Setting" variable. Confirm that
# when "Not Stratified = All ED Visits/All Inpatient Stays" the "Setting" outcome
# is unique and corresponds.
hcup_final[hcup_final$Characteristic %in% "Not Stratified" & hcup_final$Level %in% "All ED Visits", "Setting"] %>% unique()
hcup_final[hcup_final$Characteristic %in% "Not Stratified" & hcup_final$Level %in% "All Inpatient Stays", "Setting"] %>% unique()

# Because they do, we'll change these to N/A.
hcup_final[hcup_final$Characteristic %in% "Not Stratified" & hcup_final$Level %in% "All ED Visits", "Level"]       <- "N/A"
hcup_final[hcup_final$Characteristic %in% "Not Stratified" & hcup_final$Level %in% "All Inpatient Stays", "Level"] <- "N/A"



# -----------------------------
# Align "Characteristic = Race/Ethnicity" nomenclature.
wonder_raw[wonder_raw$Characteristic %in% "Race/Ethnicity", "Level"] %>% unique()
sudors_final[sudors_final$Characteristic %in% "Race/Ethnicity", "Level"] %>% unique()

# When stratifying by race in the CDC WONDER tool, the following "Single Race 6"
# categories were specified to have no Hispanic origin:
#     - American Indian or Alaska Native
#     - Asian
#     - Black or African American
#     - Native Hawaiian or Other Pacific Islander
#     - White
#     - More than one race
#     - Not Available
# 
# The CDC WONDER documentation does not explain what "Not Available" under
# "Single Race 6" means exactly. This could be interpreted simply as entries
# where the field was left blank. It is not clear how this variable relates to
# Hispanic origin, and so it will be removed; it does not have an adequate
# definition or comparison in SUDORS.
#
# When stratifying the CDC WONDER dataset for Hispanic peoples, the "Single Race 6"
# was set to "All" and "Hispanic Origin" to is/is not "Hispanic or Latino". This
# included one outcome that was "Not Stated", which is not well defined. Therefore
# this entry was removed.

# Confirm nomenclature between the two sets.
sudors_final[sudors_final$Characteristic %in% "Race/Ethnicity" & sudors_final$Level %in% "Black", "Level"] <- "Black or African American"
sudors_final[sudors_final$Characteristic %in% "Race/Ethnicity" & sudors_final$Level %in% "American Indian/Alaska Natives", "Level"] <- "American Indian or Alaska Native"
sudors_final[sudors_final$Characteristic %in% "Race/Ethnicity" & sudors_final$Level %in% "Native Hawaiian/Pacific Islander", "Level"] <- "Native Hawaiian or Other Pacific Islander"
wonder_raw[wonder_raw$Characteristic %in% "Race/Ethnicity" & wonder_raw$Level %in% "More than one race", "Level"] <- "Multi-Race, non-Hispanic"

# We'll assume that "Hispanic" in the SUDORS set also implies "Hispanic or Latino".
sudors_final[sudors_final$Characteristic %in% "Race/Ethnicity" & sudors_final$Level %in% "Hispanic", "Level"] <- "Hispanic or Latino"

# Remove the "Not Available" entries.
wonder_raw <- wonder_raw[!str_detect(wonder_raw$Level, "Not Available"), ] %>% `rownames<-`(NULL)



# -----------------------------
# Compile all datasets.

combined <- bind_rows(cbind("Dataset" = rep("AHRQ", nrow(hcup_final)), hcup_final),
                      cbind("Dataset" = rep("CDC WONDER", nrow(wonder_raw)), wonder_raw), 
                      cbind("Dataset" = rep("SUDORS", nrow(sudors_final)), sudors_final))

#write.csv(combined, "Harmonized Opioid Overdose Datasets_01.22.2025.csv", row.names = FALSE)









