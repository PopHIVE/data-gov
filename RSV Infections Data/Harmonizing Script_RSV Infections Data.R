## ----------------------------------------------------------------
## Harmonizing RSV Infections Data
##
## Date: December 23rd, 2024
## Source:
##    - CDC;s RSV-NET (connected with NREVSS data)
##    - CDC's NREVSS
##    - EPIC COSMOS

## ----------------------------------------------------------------
## SET UP THE ENVIRONMENT

library("readxl")
library("readr")
library("data.table")
library("R.utils")
library("tidyr")
library("dplyr")
library("stringr")
library("lubridate")
library("glue")

"%!in%" <- function(x,y)!("%in%"(x,y))




## ----------------------------------------------------------------
## LOAD THE DATA

rsv_net <- read_csv("RSV Infections Data/Raw Download/RSV-NET All Seasons_Interactive Dashboard Output_Downloaded 12.23.20224.csv") %>%
  as.data.frame()


nrevss_lab <- read_csv("RSV Infections Data/Raw Download/NREVSS Laboratory Data_Respiratory Syncytial Virus (RSV) Surveillance Archive_Downloaded 01.23.2025.csv") %>%
  as.data.frame()

nrevss_hhs <- read_csv("RSV Infections Data/Raw Download/NREVSS by HHS Region_Respiratory Syncytial Virus (RSV) Surveillance Archive_Downloaded 01.23.2025.csv") %>%
  as.data.frame()


epic <- read_csv("RSV Infections Data/Raw Download/EpicCosmos_Communicable Diseases by State_Between 12.22.2024 and 01.04.2025_ Downloaded 01.23.2025.csv") %>%
  as.data.frame()




## ----------------------------------------------------------------
## HARMONIZE

## The format used in RSV Net and NREVSS Lab have the most relevant format,
## including the region and season.

head(rsv_net)
head(nrevss_lab)


## Stratification by sex, race, and age group, where available. The specific
## season, count of tests administered, and rate positive rate is reported as
## well where available.
## 
## States will be aggregated by HHS region:
## https://www.hhs.gov/about/agencies/iea/regional-offices/index.html


HHS_regions <- data.frame(
  "State" = c(c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", 
                "Vermont"), 
              c("New Jersey", "New York", "Puerto Rico", "U.S. Virgin Islands"),
              c("Delaware", "District of Columbia", "Maryland", "Pennsylvania", 
                "Virginia", "West Virginia"), 
              c("Alabama", "Florida", "Georgia", "Kentucky", "Mississippi", 
                "North Carolina", "South Carolina", "Tennessee"),
              c("Illinois", "Indiana", "Michigan", "Minnesota", "Ohio", "Wisconsin"),
              c("Arkansas", "Louisiana", "New Mexico", "Oklahoma", "Texas"),
              c("Iowa", "Kansas", "Missouri", "Nebraska"),
              c("Colorado", "Montana", "North Dakota", "South Dakota", "Utah", "Wyoming"),
              c("Arizona", "California", "Hawaii", "Nevada", "American Samoa", 
                "Commonwealth of the Northern Mariana Islands", 
                "Federated States of Micronesia", "Guam", "Marshall Islands", 
                "Republic of Palau"),
              c("Alaska", "Idaho", "Oregon", "Washington")),
  "Region" = c(rep("Region 1", 6), rep("Region 2", 4), rep("Region 3", 6), 
               rep("Region 4", 8), rep("Region 5", 6), rep("Region 6", 5), 
               rep("Region 7", 4), rep("Region 8", 6), rep("Region 9", 10), 
               rep("Region 10", 4)) )


## The RSV-NET file came only with rates and no counts or population numbers.
## Its documentation states that rates as the crude rate appropriate for that
## demographic (age range, race, etc.) using the bridged-race population before
## the 2020-21 season and unbridged census population estimates (U.S. Census Bureau, 
## Population Division, Vintage 2020–2022 Special Tabulation) starting that
## season. To generate as much cross-over as possible, these population rates
## are imported for new crude rate calculations.
## 
## Source: https://www.cdc.gov/nchs/nvss/bridged_race/data_documentation.htm#vintage2020
##         https://www.census.gov/programs-surveys/popest/data/special-tab/content.html


bridged_pop <- read_csv("RSV Infections Data/CDC_Vintage 2020 Bridged-Race Postcensal Population Estimates_v2020_y1020_Prepared.csv") %>%
  as.data.frame()


## Notes from Jan. 22nd, 2025 meeting:
## Google Trends is not expected to have any other stratification than by state.
## Ideal searching key words: "RSV" and "bronchiolitis", the later of which is
## only a condition present in infants.




## ----------------------------------------------------------------
## HARMONIZE - RSV NET

head(rsv_net)

# About: https://www.cdc.gov/rsv/php/surveillance/rsv-net.html
# A case is defined as laboratory-confirmed RSV in a person who:
#     - Lives in a defined RSV-NET surveillance area AND
#     - Tests positive for RSV (using a laboratory-based molecular, antigen, 
#       serology, or antibody test) within 14 days before or during hospitalization.

# -----------------------------
# Conform stratification nomenclature and data structure.

# Only require the following age-groups: "<1 Years", "1-4 Years", "5-17 Years",
# "18-49 Years", "50-64 Years", "65-74 Years", and "75+ Years".
remove_ages <- unique(rsv_net$`Age Category`) %>% .[. %!in% c("0-<1 year", "1-4 years", "5-17 years", "18-49 years", "50-64 years", "65-74 years", "≥75 years", "All")]

# Select out the values stratified by age-groups we do not need.
rsv_net <- rsv_net[rsv_net$`Age Category` %!in% remove_ages, ] %>% `rownames<-`(NULL)

# Adjust the age-groups nomenclature.
rsv_net[rsv_net$`Age Category` %in% "0-<1 year", "Age Category"]   <- "<1 Years"
rsv_net[rsv_net$`Age Category` %in% "1-4 years", "Age Category"]   <- "1-4 Years"
rsv_net[rsv_net$`Age Category` %in% "5-17 years", "Age Category"]  <- "5-17 Years"
rsv_net[rsv_net$`Age Category` %in% "18-49 years", "Age Category"] <- "18-49 Years"
rsv_net[rsv_net$`Age Category` %in% "50-64 years", "Age Category"] <- "50-64 Years"
rsv_net[rsv_net$`Age Category` %in% "65-74 years", "Age Category"] <- "65-74 Years"
rsv_net[rsv_net$`Age Category` %in% "≥75 years", "Age Category"]   <- "75+ Years"

# The nomenclature for sex is correct.
rsv_net$Sex %>% unique()

# Adjust the race/ethnicity nomenclature.
rsv_net[rsv_net$Race %in% "Hispanic", "Race"]            <- "Hispanic or Latino"
rsv_net[rsv_net$Race %in% "A/PI, non-Hispanic", "Race"]  <- "Asian or Pacific Islander"
rsv_net[rsv_net$Race %in% "AI/AN, non-Hispanic", "Race"] <- "American Indian or Alaska Native"
rsv_net[rsv_net$Race %in% "Black, non-Hispanic", "Race"] <- "Black or African American"
rsv_net[rsv_net$Race %in% "White, non-Hispanic", "Race"] <- "White"


# Confirm that each Race, Age, and Sex stratification is independent.
rsv_net[rsv_net$Sex %!in% "All", ] %>% (\(x) { table(x[, "Age Category"], x[, "Sex"], x[, "Race"]) }) ()
rsv_net[rsv_net$Race %!in% "All", ] %>% (\(x) { table(x[, "Race"], x[, "Age Category"], x[, "Sex"]) }) ()
rsv_net[rsv_net$`Age Category` %!in% "All", ] %>% (\(x) { table(x[, "Age Category"], x[, "Race"], x[, "Sex"]) }) ()

# We see that indeed each stratification is independent of the others. We will
# now separate them.

rsv_net_sex <- rsv_net[rsv_net$Sex %!in% "All", ] %>%
  mutate(Characteristic = "Sex") %>% 
  rename(Level = Sex) %>%
  select("State", "Season", "Week ending date", "Characteristic", "Level", colnames(rsv_net)[c(7:9)])

rsv_net_race <- rsv_net[rsv_net$Race %!in% "All", ] %>%
  mutate(Characteristic = "Race/Ethnicity") %>% 
  rename(Level = Race) %>%
  select("State", "Season", "Week ending date", "Characteristic", "Level", colnames(rsv_net)[c(7:9)])

rsv_net_age  <- rsv_net[rsv_net$`Age Category` %!in% "All", ] %>%
  mutate(Characteristic = "Age") %>% 
  rename(Level = `Age Category`) %>%
  select("State", "Season", "Week ending date", "Characteristic", "Level", colnames(rsv_net)[c(7:9)])

rsv_net_not_strat <- rsv_net[rsv_net$Sex %in% "All" & rsv_net$Race %in% "All" & rsv_net$`Age Category` %in% "All", ] %>%
  mutate(Characteristic = "Not Stratified", Level = "N/A") %>% 
  select("State", "Season", "Week ending date", "Characteristic", "Level", colnames(rsv_net)[c(7:9)])


# Confirm that all rows have been accounted for.
nrow(rsv_net_sex) + nrow(rsv_net_race) + nrow(rsv_net_age) + nrow(rsv_net_not_strat) == nrow(rsv_net)


# Generate the final table.
rsv_net <- bind_rows(rsv_net_sex, rsv_net_race, rsv_net_age, rsv_net_not_strat) %>%
  as.data.frame() %>% `rownames<-`(NULL)




# -----------------------------
# Conform seasons and rate nomenclature and data structure.

# There are duplicated seasons, some specifying rates for specific "Characteristics".
rsv_net$Season %>% unique() %>% sort()

# Notice that these differ in how the rate is processed: crude or age-adjusted.
rsv_net[!str_detect(rsv_net$Season, "\\("), "Type"] %>% unique()
rsv_net[str_detect(rsv_net$Season, "\\("), "Type"] %>% unique()

# To keep these values, we'll simply strip away the part of the string in "Season"
# that differentiates the rate types.
rsv_net$Season <- str_replace(rsv_net$Season, " \\(.+?\\)", "")

# Expand out these rates to a wide format.
rsv_net <- pivot_wider(rsv_net, names_from = Type, values_from = c(Rate, `Cumulative Rate`)) %>% as.data.frame()




# -----------------------------
# Check for other outcomes not needed in the variable columns.

# Confirm that only states are reflected.
unique(rsv_net$State) %>% .[. %!in% c("US", "District of Columbia", datasets::state.name)]

# But notice also that these are the only entries that reflect the age-adjusted
# rate. This variable denotes counts and rates accross all participating sites.
# It will therefor be kept, but renamed to more clearly denote this.
rsv_net[rsv_net$State %in% "RSV-NET", "Rate_Age adjusted Rate"] %>% unique() %>% head()
rsv_net[rsv_net$State %!in% "RSV-NET", "Rate_Age adjusted Rate"] %>% unique() %>% head()

rsv_net[rsv_net$State %in% "RSV-NET", "Cumulative Rate_Age adjusted Rate"] %>% unique() %>% head()
rsv_net[rsv_net$State %!in% "RSV-NET", "Cumulative Rate_Age adjusted Rate"] %>% unique() %>% head()


# Rename "State = RSV-NET" to "State = All Sites".
rsv_net[rsv_net$State %in% "RSV-NET", "State"] <- "All Sites"


# Confirm no weeks are reported as an NA.
anyNA(rsv_net$`Week ending date`)




# -----------------------------
# Aggregate to get HHS regions.

# Without the counts the population used to calculate the crude rate cannot
# be regenerated. Therefore, the bridged-race population counts are needed.
# Because not every county within a state participates in the RSV-NET surveillance
# program, each baseline population will be multiplied by the percentage of
# that states population is represented.

# Sourced from the "About Our Data" tab.
# https://www.cdc.gov/rsv/php/surveillance/rsv-net.html
percent_rep <- data.frame(
  "State" = c( c("California", "Georgia", "Maryland", "Minnesota", "New York", 
                 "Oregon", "Tennessee"),
               c("Tennessee", "Oregon", "New York", "New Mexico", "Minnesota", 
                 "Maryland", "Georgia", "California"),
               rep(c("Utah", "Tennessee", "Oregon", "New York", "New Mexico", 
                     "Minnesota", "Michigan", "Maryland", "Georgia", "Connecticut", 
                     "Colorado", "California"), 4), 
               rep(c("Utah", "Tennessee", "Oregon", "New York", "New Mexico", 
                     "Minnesota", "Michigan", "Maryland", "Georgia", "Connecticut", 
                     "Colorado", "California"), 2),
               c("Utah", "Tennessee", "Oregon", "New York", "New Mexico", 
                 "North Carolina", "Minnesota", "Michigan", "Maryland", "Georgia", 
                 "Connecticut", "Colorado", "California") ),
  "Percent" = c( c(0.20, 0.44, 0.44, 0.66, 0.20, 0.44, 0.20),
                 c(0.20, 0.44, 0.20, 0.44, 0.66, 0.44, 0.44, 0.20),
                 rep(c(0.44, 0.44, 0.44, 0.20, 0.44, 0.66, 0.20, 0.88, 0.44, 
                       0.20, 0.44, 0.20), 4),
                 rep(c(0.44, 0.20, 0.44, 0.20, 0.44, 0.66, 0.20, 0.44, 0.44, 0.20, 
                       0.44, 0.20), 2),
                 c(0.44, 0.20, 0.44, 0.20, 0.44, 0.20, 0.88, 0.20, 0.44, 0.44, 
                   0.20, 0.44, 0.20) ),
  "Season" = c( rep("2016-17", 7),  rep("2017-18", 8), 
                c(rep("2018-19", 12), rep("2019-20", 12), rep("2020-21", 12), 
                  rep("2021-22", 12)), c(rep("2022-23", 12), rep("2023-24", 12)),
                rep("2024-25", 13) )
)


# The population estimates will be based on that weeks year of observation, as
# the season spans over two postcensal year's estimates.

# Add a column for the HHS region to be filled in.
rsv_net <- rsv_net %>% 
  mutate("HHS Region" = NA, "Population" = NA, "Percent Represented" = NA)

# The "State = All Sites" variable will need to be handled separately.
all_sites   <- rsv_net[rsv_net$State %in% "All Sites", ] %>% `rownames<-`(NULL)
states_only <- rsv_net[rsv_net$State %!in% "All Sites", ] %>% `rownames<-`(NULL)

# Fill in the population count based on the year of the RSV detection and
# the entry metadata. The population used is based on the year recorded
# by the "Week ending date" column.
for(i in 1:nrow(states_only)){
  year_observed    <- year(states_only$`Week ending date`[i])
  years_bridge_pop <- str_extract(colnames(bridged_pop)[-c(1:4)], "[0-9]{4}")
  
  if( any(year_observed %in% years_bridge_pop) ){
    # Match the appropriate population by metadata and year of observation.
    states_only[i, "Population"] <- 
      bridged_pop[bridged_pop$State %in% states_only$State[i] & 
                  bridged_pop$Characteristic %in% states_only$Characteristic[i] &
                  bridged_pop$Level %in% states_only$Level[i], 
                str_detect(colnames(bridged_pop), "Postcensal Population")] %>%
      (\(x) { x[, str_detect(colnames(x), as.character(year_observed) )] }) ()
    
  } else {
    # If the years observed are out of range, then apply only the max available
    # bridge-race population count.
    states_only[i, "Population"] <- 
      bridged_pop[bridged_pop$State %in% states_only$State[i] & 
                  bridged_pop$Characteristic %in% states_only$Characteristic[i] &
                  bridged_pop$Level %in% states_only$Level[i], 
                str_detect(colnames(bridged_pop), "Postcensal Population")] %>%
      (\(x) { x[, str_detect(colnames(x), as.character(max(years_bridge_pop) )) ] }) ()
    
  }
}

# Fill in the percent of total state population that was represented that season.
# This will be used in place of aggregating population counts only by the
# counties that were participating.
for(i in 1:nrow(percent_rep)){
  states_only[states_only$State %in% percent_rep[i, 1] & states_only$Season %in% percent_rep[i, 3], "Percent Represented"] <- percent_rep[i, 2]
}

# Produce the approximate denominator used to generate the rates for
# participating counties.
states_only$Denominator <- floor(states_only$Population * states_only$`Percent Represented`)


# For the "State = All Sites" outcome, the sum of the denominator from participating
# sites needs to be summed. For earlier seasons, not all stratifications were
# included at the state-level. These will be given a placeholder now and
# handled separately.

# Generate the search space for subsetting.
search_space <- all_sites %>% 
  (\(x) { table(x$`Week ending date`, x$Characteristic, x$Level) }) () %>% 
  as.data.frame() %>%
  (\(x) { x[x$Freq %!in% 0, ] }) () %>% 
  `rownames<-`(NULL)

# The following function takes a few minutes. This progress bar has been
# added to show where the function is in the for loop.
pb = txtProgressBar(min = 0, max = nrow(search_space), initial = 0)

for(i in 1:nrow(search_space) ){
  # Subset the states only RSV-NET subset by the metadata that matches the all
  # sites aggregated subset.
  subset <- states_only[states_only$`Week ending date` %in% search_space[i, 1] &
   states_only$Characteristic %in% search_space[i, 2] &
   states_only$Level %in% search_space[i, 3], ]
  
  # If there is a match, simply sum the scaled population used for the rate denominator.
  if(nrow(subset) > 0){
    denominator<- sum(subset$Denominator)
    
  # If there is no match, generalize using the participating state for that infection
  # season and the stratified postcensal population counts to recalculate the
  # scaled rate denominator.
  } else if(nrow(subset) == 0){
    # Target metadata associated with this stratification in the all sites subset.
    target_subset <- all_sites[all_sites$`Week ending date` %in% search_space[i, 1] &
      all_sites$Characteristic %in% search_space[i, 2] &
      all_sites$Level %in% search_space[i, 3], ]
    
    # Save the postcensal population date range.
    pop_date <- colnames(bridged_pop)[str_detect(colnames(bridged_pop), str_glue("Postcensal Population_", {year(target_subset$`Week ending date`)[i]}) )]
    # Save the season in the target.
    season   <- percent_rep[percent_rep$Season %in% target_subset$Season, ]
    
    # Extract the postcensal population estimates for a specific combination of
    # metadata and the states participating in the RSV-NET season.
    season$Population <- bridged_pop[bridged_pop$Characteristic %in% target_subset$Characteristic[i] & 
      bridged_pop$Level %in% target_subset$Level[i] &
      bridged_pop$State %in% season$State, pop_date]
    
    # Calculate the scaled population rate denominator.
    denominator <- sum(season$Percent * season$Population) %>% floor()
    
  }
  
  # Commit the denominator.
  all_sites[all_sites$`Week ending date` %in% search_space[i, 1] &
   all_sites$Characteristic %in% search_space[i, 2] &
   all_sites$Level %in% search_space[i, 3], "Denominator"] <- denominator
  
  # Print the for loop's progress.
  setTxtProgressBar(pb, i)
}


# Recombine the all sites and states only subsets.
rsv_net <- bind_rows(all_sites, states_only)

# Counts should be an integer. At this state, we are approximating the count
# in order to regenerate the rates. Therefore, a few extra significant figures
# will be retained.
rsv_net$Count <- round( (rsv_net$`Rate_Crude Rate` * rsv_net$Denominator) / 100000, digits = 2)


# Fill in the HHS Region that the state is in.
for(i in 1:nrow(HHS_regions)){
  rsv_net[rsv_net$State %in% HHS_regions[i, 1], "HHS Region"] <- HHS_regions[i, 2]
}

# Aggregate by HHS Region.
rsv_hhs <- rsv_net[, c(2:5, 10, 13:14)] %>%
  group_by(`HHS Region`, Season, `Week ending date`, Characteristic, Level) %>% 
  summarise_at(vars(Denominator, Count), sum, na.rm = FALSE) %>%
  ungroup()

# Produce the new crude rates.
rsv_hhs$`Rate_Crude Rate` <- round((rsv_hhs$Count/rsv_hhs$Denominator) * 100000, 1)


# Order the columns and adjust the variable nomenclature.
rsv_net <- rsv_net %>%
  mutate(`Region Type` = "State") %>%
  rename(Region = State, `Week Observed` = `Week ending date`, `Crude Rate` = `Rate_Crude Rate`, 
         `Age-Adjusted Rate` = `Rate_Age adjusted Rate`, 
         `Cumulative Crude Rate` = `Cumulative Rate_Crude Rate`,
         `Cumulative Age-Adjusted Rate` = `Cumulative Rate_Age adjusted Rate`) %>%
  select(-c(`HHS Region`, Population, `Percent Represented`, Denominator))


rsv_hhs <- rsv_hhs %>%
  mutate(`Region Type` = "HHS") %>%
  rename(Region = `HHS Region`, `Week Observed` = `Week ending date`, `Crude Rate` = `Rate_Crude Rate`) %>%
  select(-Denominator)


# Rebind the HHS Regions.
rsv_net <- bind_rows(rsv_net, rsv_hhs) %>%
  select(Region, `Region Type`, colnames(rsv_net)[2:5], Count, colnames(rsv_net)[6:9]) %>%
  `rownames<-`(NULL)



# Organize the rows.
rsv_net <- rsv_net[with(rsv_net, order(`Region Type`, Region, Season, Characteristic, Level, `Week Observed`)), ] %>% 
  `rownames<-`(NULL)


# Now we're going to calculate the "Cumulative Crude Rate" values.

# Generate the search space for subsetting.
search_space <- rsv_net[rsv_net$`Region Type` %in% "HHS", ] %>% 
  (\(x) { table(x$Region, x$Characteristic, x$Level) }) () %>% 
  as.data.frame() %>%
  (\(x) { x[x$Freq %!in% 0, ] }) () %>% 
  `rownames<-`(NULL)


for(i in 1:nrow(search_space) ){
  # Subset by the metadata.
  subset <- rsv_net[rsv_net$Region %in% search_space[i, 1] &
    rsv_net$Characteristic %in% search_space[i, 2] &
    rsv_net$Level %in% search_space[i, 3], ]
  
  for(j in 1:length(unique(subset$Season)) ){
    # Save the crude rate.
    rate <- subset[subset$Season %in% unique(subset$Season)[j], "Crude Rate"]
    
    # Calculate and save the cumulative of that rate.
    subset[subset$Season %in% unique(subset$Season)[j], "Cumulative Crude Rate"] <- cumsum(rate)
  }
  
  # Commit the changes.
  rsv_net[rsv_net$Region %in% search_space[i, 1] &
            rsv_net$Characteristic %in% search_space[i, 2] &
            rsv_net$Level %in% search_space[i, 3], ] <- subset
}


# Add diagnostic test type to conform with the NREVSS dataset.
rsv_net <- rsv_net %>%
  mutate(`Diagnostic Test Type` = "All") %>%
  select(c(colnames(rsv_net)[1:4], `Diagnostic Test Type` , colnames(rsv_net)[5:11]))
  

# Round the counts to ceiling.
rsv_net$Count <- ceiling(rsv_net$Count)




## ----------------------------------------------------------------
## HARMONIZE - NREVSS

head(nrevss_lab)
head(nrevss_hhs)


# The RSV-NET surveillance season begins week 40 (approximately Oct. 1st) and
# ends week 39 of the following year (approximately Sep. 30th).

nrevss_lab$`Week ending Date` <- as.Date(nrevss_lab$`Week ending Date`, format = "%d%B%Y")


available_years <- year(nrevss_lab$`Week ending Date`) %>% unique()

##
##
## fix the "Season" to add the initial date too
season_ranges <- data.frame("Season" = str_c(min(available_years) - 1, 10:21, sep = "-"),
  "Start" = str_c(c(min(available_years) - 1, available_years), "-10-01"),
  "End" = str_c(c(available_years, max(available_years) + 1), "-09-30"))


ifelse("2010-07-10" >= season_ranges[, "Start"] & "2010-07-10" <= season_ranges[, "End"], 1, 0)


# for lab:
# manipulate the date to get season and week ending
# Calculate crude rate and cumulative rate

# for HHS:
# remove the percentage columns and exclude the 3- and 5-week averages
# add season and week ending
# add diagnostic test type "PCR"



## ----------------------------------------------------------------
## HARMONIZE - EPIC COSMOS


head(epic)

# About link: https://www.epicresearch.org/data-tracker/communicable-diseases
# This page comes with additional tables for Office Visits and ER visits
# nationally, but this cannot be readily downloaded. It is therefore only
# left to the state-wide rates that have a data download button.

# At the time of download, the season reflected is: Between 12/22/2024 and 1/4/2025.
# Rates are noted to be age-adjusted, and so they are assumed to be crude rates.


# calculate the population, sum to get the total visits and crude rates by HHS region.




## ----------------------------------------------------------------
## COMBINE ALL THREE


# -----------------------------
# Compile all datasets.

combined <- bind_rows(cbind("Dataset" = rep("AHRQ", nrow(hcup_final)), hcup_final),
                      cbind("Dataset" = rep("CDC WONDER", nrow(wonder_raw)), wonder_raw), 
                      cbind("Dataset" = rep("SUDORS", nrow(sudors_final)), sudors_final))

write.csv(rsv_net, "RSV Infections Data/Harmonized RSV-NET_01.28.2025.csv", row.names = FALSE)







