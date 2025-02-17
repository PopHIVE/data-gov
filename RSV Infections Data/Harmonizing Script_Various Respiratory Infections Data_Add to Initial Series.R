## ----------------------------------------------------------------
## Harmonizing RSV Infections Data - Adding to Initial Compilation
##
## Date: February 12rd, 2025
## Description: Quick pull and harmonization of additional RSV, COVID,
##              and Flu datasets to be added to the initial RSV compilation.
##              Continual pull for prototype building.

## ----------------------------------------------------------------
## SET UP THE ENVIRONMENT

suppressPackageStartupMessages({
  library("readxl")
  library("readr")
  library("data.table")
  library("R.utils")
  library("tidyr")
  library("dplyr")
  library("stringr")
  library("lubridate")
  library("glue")
})

"%!in%" <- function(x,y)!("%in%"(x,y))




## ----------------------------------------------------------------
## LOAD THE CURRENT COMPILATION

rsv <- read_csv("RSV Infections Data/Harmonized RSV Infections Datasets_02.11.2025.csv") %>%
  as.data.frame()




## ----------------------------------------------------------------
## GOOGLE TRENDS

google_raw <- read_csv("RSV Infections Data/Raw Download/Google Trends_RSV and Bronchiolitis Query_Downloaded 01.30.2025.csv", skip = 2, col_types = cols(`rsv: (United States)` = col_character())) %>%
  as.data.frame()

# Bronchiolitis is a respiratory condition specific to children ages 2 or younger.
# It can be caused by multiple different respiratory infections, including RSV.
# In this process, we will be compiling additional respiratory diseases, and
# so an additional column denoting "Desease" will be included to distinguish this.
# Source: https://my.clevelandclinic.org/health/diseases/8272-bronchiolitis

google <- google_raw %>%
  rename(RSV = `rsv: (United States)`, 
         Bronchiolitis = `bronchiolitis: (United States)`,
         `Week Observed` = Week)

# NOTE: a count for the Google Trends dataset is not necessarily a positive case,
#       but it will be considered as such here.
google <- pivot_longer(google, cols = RSV:Bronchiolitis,
                       names_to = "Disease", values_to = "Positives Detected")


# Add "Disease" column to the RSV dataset if it is not there.
if(any(colnames(rsv) %in% "Disease") == FALSE){
  rsv <- rsv %>% mutate(Disease = "RSV") %>%
    select(Dataset, Disease, colnames(rsv)[-1])
}


# Add most of the missing columns and format.
google <- google %>%
  mutate(Dataset = "Google Trends", Region = "National", `Region Type` = NA,
         Characteristic = "Not Stratified", Level = "N/A", `Tests Administered` = NA,
         `Crude Rate` = NA, `Age-Adjusted Rate` = NA, 
         `Cumulative Crude Rate` = NA, `Cumulative Age-Adjusted Rate` = NA) %>%
  mutate(`Week Observed` = as.Date(google$`Week Observed`, format = "%m/%d/%y"))


# To label the season where the tests results were recorded, we need to define
# the span of years available and the boundaries of that season.
available_years <- year(nwss$`Week Observed`) %>% unique() %>% sort()
years_range_end <- format(nwss$`Week Observed`, "%y") %>% unique() %>% sort() %>% as.numeric() %>%
  (\(x) { c(x[1], x+1) }) ()

# Associate a seasons week's boundaries over the span of years available with
# the new season name that conforms to the RSV-NET nomenclature.
season_ranges <- data.frame("Season" = str_c((min(available_years)-1):(max(available_years)), years_range_end, sep = "-"),
                            "Start" = str_c(c(min(available_years) - 1, available_years), "-07-01"),
                            "End" = str_c(c(available_years, max(available_years) + 1), "-06-30"))

# Add the appropriate season label as a new variable.
google$Season <- sapply(google$`Week Observed`, function(x) {
  ifelse(x >= season_ranges[, "Start"] & x <= season_ranges[, "End"], 1, 0) %>% 
    (\(y) { season_ranges[as.logical(y), "Season"] }) 
})


google <- google %>% 
  select(colnames(rsv)) %>% as.data.frame()


# We know that bronchiolitis is only a condition in children less than 2 years of
# age. We can reflect this using the "Characteristic" and "Level" variables.
google[str_which(google$Disease, "Bronchiolitis"), c("Characteristic", "Level")] <- data.frame("Characteristic" = "Age", "Level" = "<2 Years")




## ----------------------------------------------------------------
## WASTEWATER DATA

nwss_flu   <- read_csv("RSV Infections Data/Raw Download/NWSS Wastewater State and Territory Trends_Influenza A_State Level Download 02.10.2025.csv") %>%
  as.data.frame()

nwss_rsv   <- read_csv("RSV Infections Data/Raw Download/NWSS Wastewater State and Territory Trends_RSV_State Level Download 02.10.2025.csv") %>%
  as.data.frame()

nwss_covid <- read_csv("RSV Infections Data/Raw Download/NWSS Wastewater State and Territory Trends_SARS-CoV-2_State Level Download 02.10.2025.csv") %>%
  as.data.frame()


# Combine the diseases.
nwss <- bind_rows(cbind("Disease" = rep("Influenza A", nrow(nwss_flu)), nwss_flu),
                  cbind("Disease" = rep("RSV", nrow(nwss_rsv)), nwss_rsv), 
                  cbind("Disease" = rep("SARS-CoV-2", nrow(nwss_covid)), nwss_covid))


# "Data_Collection_Period" specifies entries that cover over specific spans of time.
nwss$Data_Collection_Period %>% unique()

# To confirm, these are overlapping categories:
one_year <- nwss[nwss$`State/Territory` %in% "Alabama" & nwss$Data_Collection_Period %in% "1 Year" & nwss$Disease %in% "Influenza A", ]
last6    <- nwss[nwss$`State/Territory` %in% "Alabama" & nwss$Data_Collection_Period %in% "6 Months" & nwss$Disease %in% "Influenza A", ]

# We see that indeed smaller spans of time are totally included in larger ones.
# Therefore, we can reduce the dataset to what is unique, 
# "Data_Collection_Period = All Results"
last6$Week_Ending_Date[last6$Week_Ending_Date %in% one_year$Week_Ending_Date] %>% length() == nrow(last6)

rm(list = c("one_year", "last6"))


# Only keep the "All Results" entries, and remove unnecessary replicates.
nwss <- nwss[nwss$Data_Collection_Period %in% "All Results", ] %>% `rownames<-`(NULL)


# We'll replace the "Data_Collection_Period" instead with the infection season
# applied to the testing surveillance datasets.

# To label the season where the tests results were recorded, we need to define
# the span of years available and the boundaries of that season.
available_years <- year(nwss$`Week_Ending_Date`) %>% unique() %>% sort()
years_range_end <- format(nwss$`Week_Ending_Date`, "%y") %>% unique() %>% sort() %>% as.numeric() %>%
  (\(x) { c(x[1], x+1) }) ()

# Associate a seasons week's boundaries over the span of years available with
# the new season name that conforms to the RSV-NET nomenclature.
season_ranges <- data.frame("Season" = str_c((min(available_years)-1):(max(available_years)), years_range_end, sep = "-"),
                            "Start" = str_c(c(min(available_years) - 1, available_years), "-07-01"),
                            "End" = str_c(c(available_years, max(available_years) + 1), "-06-30"))

# Add the appropriate season label as a new variable.
nwss$Season <- sapply(nwss$`Week_Ending_Date`, function(x) {
  ifelse(x >= season_ranges[, "Start"] & x <= season_ranges[, "End"], 1, 0) %>% 
    (\(y) { season_ranges[as.logical(y), "Season"] }) 
})


# Metrics for waste water detection are fundamentally different from the other
# testing surveillance programs. Therefore, it will not be completely combined
# with the running table. Other columns of commonality, will, however be produced.

# Check that only states and the District of Columbia are represented.
nwss$`State/Territory` %>% unique() %in% c(datasets::state.name, "District of Columbia") %>% all()

nwss <- nwss %>%
  select(-Data_Collection_Period) %>%
  rename(Region = `State/Territory`, `Week Observed` = Week_Ending_Date,
         `State WVAL` = `State/Territory_WVAL`, `National WVAL` = `National_WVAL`,
         `Regional WVAL` = `Regional_WVAL`, `WVAL Category` = `WVAL_Category`) %>%
  mutate(Dataset = "NWSS", `Region Type` = "State", 
         Characteristic = "Not Stratified", Level = "N/A") %>%
  # Wastewater Viral Activity Level (WVAL) is determined to the nearest 10th. We'll
  # round to the nearest 100th place to add some flexibility.
  mutate(`National WVAL` = round(`National WVAL`, digits = 2),
         `State WVAL` = round(`State WVAL`, digits = 2),
         `Regional WVAL` = round(`Regional WVAL`, digits = 2)) %>%
  select(colnames(rsv)[1:8], `WVAL Category`, `National WVAL`, `State WVAL`, `Regional WVAL`, Coverage)


# Coverage by "Disease" and "Season".
table(nwss$Region, nwss$Season, nwss$Disease)


# Organize the rows.
nwss <- nwss[with(nwss, order(Disease, Region, `Week Observed`)), ] %>% 
  `rownames<-`(NULL)



write.csv(nwss, "RSV Infections Data/Wastewater_02.14.2025.csv", row.names = FALSE)




## ----------------------------------------------------------------
## RESP-NET

# Initially, RSV-NET was used, but the RESP-NET dataset contains all programs
# for FluSurv-NET and COVID-NET. It also includes a combined category tallying
# total infections.

resp_net_raw <- read_csv("RSV Infections Data/Raw Download/RESP-NET All Seasons_Interactive Dashboard Output_Downloaded 02.16.2025.csv") %>%
  as.data.frame()


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

bridged_pop <- read_csv("RSV Infections Data/Reference Files/CDC_Vintage 2020 Bridged-Race Postcensal Population Estimates_v2020_y1020_All Counties.csv") %>%
  as.data.frame()

# Special subset for counties of hospitals that were listed as participants in the
# 2024-25 season. Other seasons were not noted.
bridged_pop_nrevss <- read_csv("RSV Infections Data/Reference Files/CDC_Vintage 2020 Bridged-Race Postcensal Population Estimates_v2020_y1020_NREVSS Subset.csv") %>%
  as.data.frame()


# About: https://www.cdc.gov/rsv/php/surveillance/rsv-net.html
# A case is defined as laboratory-confirmed RSV in a person who:
#     - Lives in a defined RSV-NET surveillance area AND
#     - Tests positive for RSV (using a laboratory-based molecular, antigen, 
#       serology, or antibody test) within 14 days before or during hospitalization.

# -----------------------------
# Conform stratification nomenclature and data structure.

# Only require the following age-groups: "<1 Years", "1-4 Years", "5-17 Years",
# "18-49 Years", "50-64 Years", "65-74 Years", and "75+ Years".
remove_ages <- unique(resp_net_raw$`Age group`) %>% .[. %!in% c("0-<1 yr", "1-4 yr", "5-17 yr", "18-49 yr", "50-64 yr", "65-74 yr", "75+ yr", "Overall")]

# Select out the values stratified by age-groups we do not need.
resp_net_raw <- resp_net_raw[resp_net_raw$`Age group` %!in% remove_ages, ] %>% `rownames<-`(NULL)

# Adjust the age-groups nomenclature.
resp_net_raw[resp_net_raw$`Age group` %in% "0-<1 yr", "Age group"]  <- "<1 Years"
resp_net_raw[resp_net_raw$`Age group` %in% "1-4 yr", "Age group"]   <- "1-4 Years"
resp_net_raw[resp_net_raw$`Age group` %in% "5-17 yr", "Age group"]  <- "5-17 Years"
resp_net_raw[resp_net_raw$`Age group` %in% "18-49 yr", "Age group"] <- "18-49 Years"
resp_net_raw[resp_net_raw$`Age group` %in% "50-64 yr", "Age group"] <- "50-64 Years"
resp_net_raw[resp_net_raw$`Age group` %in% "65-74 yr", "Age group"] <- "65-74 Years"
resp_net_raw[resp_net_raw$`Age group` %in% "75+ yr", "Age group"]   <- "75+ Years"

# The nomenclature for sex is correct.
resp_net_raw$Sex %>% unique()

# Adjust the race/ethnicity nomenclature.
resp_net_raw[resp_net_raw$`Race/Ethnicity` %in% "Hispanic", "Race/Ethnicity"] <- "Hispanic or Latino"
resp_net_raw[resp_net_raw$`Race/Ethnicity` %in% "Asian/Pacific Islander, NH", "Race/Ethnicity"] <- "Asian or Pacific Islander"
resp_net_raw[resp_net_raw$`Race/Ethnicity` %in% "A/PI, non-Hispanic", "Race/Ethnicity"]         <- "Asian or Pacific Islander"
resp_net_raw[resp_net_raw$`Race/Ethnicity` %in% "American Indian/Alaska Native, NH", "Race/Ethnicity"] <- "American Indian or Alaska Native"
resp_net_raw[resp_net_raw$`Race/Ethnicity` %in% "AI/AN, non-Hispanic", "Race/Ethnicity"]               <- "American Indian or Alaska Native"
resp_net_raw[resp_net_raw$`Race/Ethnicity` %in% "Black, NH", "Race/Ethnicity"]           <- "Black or African American"
resp_net_raw[resp_net_raw$`Race/Ethnicity` %in% "Black, non-Hispanic", "Race/Ethnicity"] <- "Black or African American"
resp_net_raw[resp_net_raw$`Race/Ethnicity` %in% "White, NH", "Race/Ethnicity"]           <- "White"
resp_net_raw[resp_net_raw$`Race/Ethnicity` %in% "White, non-Hispanic", "Race/Ethnicity"] <- "White"


# Confirm that each Race, Age, and Sex stratification is independent.
resp_net_raw[resp_net_raw$Sex %!in% "Overall", ] %>% (\(x) { table(x[, "Age group"], x[, "Sex"], x[, "Race/Ethnicity"]) }) ()
resp_net_raw[resp_net_raw$`Race/Ethnicity` %!in% "Overall", ] %>% (\(x) { table(x[, "Race/Ethnicity"], x[, "Age group"], x[, "Sex"]) }) ()
resp_net_raw[resp_net_raw$`Age group` %!in% "Overall", ] %>% (\(x) { table(x[, "Age group"], x[, "Race/Ethnicity"], x[, "Sex"]) }) ()


# We see that indeed each stratification is independent of the others. We will
# now separate them.

resp_net_sex <- resp_net_raw[resp_net_raw$Sex %!in% "Overall", ] %>%
  mutate(Characteristic = "Sex") %>% 
  rename(Level = Sex) %>%
  select("Surveillance Network", "Site", "Season", "Week Ending Date", "Characteristic", "Level", colnames(resp_net_raw)[c(9:10, 12)])

resp_net_race <- resp_net_raw[resp_net_raw$`Race/Ethnicity` %!in% "Overall", ] %>%
  mutate(Characteristic = "Race/Ethnicity") %>% 
  rename(Level = `Race/Ethnicity`) %>%
  select("Surveillance Network", "Site", "Season", "Week Ending Date", "Characteristic", "Level", colnames(resp_net_raw)[c(9:10, 12)])

resp_net_age  <- resp_net_raw[resp_net_raw$`Age group` %!in% "Overall", ] %>%
  mutate(Characteristic = "Age") %>% 
  rename(Level = `Age group`) %>%
  select("Surveillance Network", "Site", "Season", "Week Ending Date", "Characteristic", "Level", colnames(resp_net_raw)[c(9:10, 12)])

resp_net_not_strat <- resp_net_raw[resp_net_raw$Sex %in% "Overall" & resp_net_raw$`Race/Ethnicity` %in% "Overall" & resp_net_raw$`Age group` %in% "Overall", ] %>%
  mutate(Characteristic = "Not Stratified", Level = "N/A") %>% 
  select("Surveillance Network", "Site", "Season", "Week Ending Date", "Characteristic", "Level", colnames(resp_net_raw)[c(9:10, 12)])


# Confirm that all rows have been accounted for.
nrow(resp_net_sex) + nrow(resp_net_race) + nrow(resp_net_age) + nrow(resp_net_not_strat) == nrow(resp_net_raw)


# Generate the final table.
resp_net <- bind_rows(resp_net_sex, resp_net_race, resp_net_age, resp_net_not_strat) %>%
  as.data.frame() %>% `rownames<-`(NULL)




# -----------------------------
# Conform seasons and rate nomenclature and data structure.

# Confirm that the seasons are unique. In the RSV-NET specific export there were
# duplicates, where some entries included notation of the associated characteristic.
resp_net$Season %>% unique() %>% sort()

# Expand out these rates to a wide format.
resp_net <- pivot_wider(resp_net, names_from = Type, values_from = c(`Weekly Rate`, `Cumulative Rate`)) %>% as.data.frame()


# The RESP-NET surveillance season begins week 40 (approximately Oct. 1st) and
# ends week 39 of the following year (approximately Sep. 30th). We desire the
# range to start in July and end in June of the following year.

# To label the season where the tests results were recorded, we need to define
# the span of years available and the boundaries of that season.
available_years <- year(resp_net$`Week Ending Date`) %>% unique() %>% sort()
years_range_end <- format(resp_net$`Week Ending Date`, "%y") %>% unique() %>% sort() %>% as.numeric() %>%
  (\(x) { c(x[1], x+1) }) ()

# Associate a seasons week's boundaries over the span of years available with
# the new season name that conforms to the RSV-NET nomenclature.
season_ranges <- data.frame("Season" = str_c((min(available_years)-1):(max(available_years)), years_range_end, sep = "-"),
                            "Start" = str_c(c(min(available_years) - 1, available_years), "-07-01"),
                            "End" = str_c(c(available_years, max(available_years) + 1), "-06-30"))

# Add the appropriate season label as a new variable.
resp_net$Season <- sapply(resp_net$`Week Ending Date`, function(x) {
  ifelse(x >= season_ranges[, "Start"] & x <= season_ranges[, "End"], 1, 0) %>% 
    (\(y) { season_ranges[as.logical(y), "Season"] }) 
})

# Confirm there are no NA's introduced in the conversion process.
resp_net[resp_net$Season %in% NA, ]



# -----------------------------
# Check for other outcomes not needed in the variable columns.

# Confirm that only states are reflected.
unique(resp_net$Site) %>% .[. %!in% c("US", "District of Columbia", datasets::state.name)]

# But notice also that these are the only entries that reflect the age-adjusted
# rate. This variable denotes counts and rates across all participating sites.
# It will therefor be kept, but renamed to more clearly denote this.
resp_net[resp_net$Site %in% "Overall", "Weekly Rate_Age-Adjusted Rate"] %>% unique() %>% head()
resp_net[resp_net$Site %!in% "Overall", "Weekly Rate_Age-Adjusted Rate"] %>% unique() %>% head()

resp_net[resp_net$Site %in% "Overall", "Cumulative Rate_Age-Adjusted Rate"] %>% unique() %>% head()
resp_net[resp_net$Site %!in% "Overall", "Cumulative Rate_Age-Adjusted Rate"] %>% unique() %>% head()


# Rename "Site = Overall" to "Site = All Sites".
resp_net[resp_net$Site %in% "Overall", "Site"] <- "All Sites"


# Confirm no weeks are reported as an NA.
anyNA(resp_net$`Week Ending Date`)




# -----------------------------
# Aggregate to get HHS regions.

# Without the counts the population used to calculate the crude rate cannot
# be regenerated. Therefore, the bridged-race population counts are needed.
# Because not every county within a state participates in the REST-NET surveillance
# programs, each baseline population will be multiplied by the percentage of
# that states population is represented. 
#
# For simplicity, the reported coverage for RSV-NET is reflected here, however,
# a more accurate representation will report each program individually as they
# are not always the same.

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
resp_net <- resp_net %>% 
  mutate("HHS Region" = NA, "Population" = NA, "Percent Represented" = NA) %>%
  rename(State = Site)

# The "State = All Sites" variable will need to be handled separately.
all_sites   <- resp_net[resp_net$State %in% "All Sites", ] %>% `rownames<-`(NULL)
states_only <- resp_net[resp_net$State %!in% "All Sites", ] %>% `rownames<-`(NULL)

# Fill in the population count based on the year of the RSV detection and
# the entry metadata. The population used is based on the year recorded
# by the "Week Ending Date" column.
for(i in 1:nrow(states_only)){
  year_observed    <- year(states_only$`Week Ending Date`[i])
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
# sites needs to be added. For earlier seasons, not all stratifications were
# included at the state-level. These will be given a placeholder now and
# handled separately.

# Generate the search space for subsetting.
search_space <- all_sites %>% 
  (\(x) { table(x$`Week Ending Date`, x$Characteristic, x$Level) }) () %>% 
  as.data.frame() %>%
  (\(x) { x[x$Freq %!in% 0, ] }) () %>% 
  `rownames<-`(NULL)

# The following function takes a few minutes. This progress bar has been
# added to show where the function is in the for loop.
pb = txtProgressBar(min = 0, max = nrow(search_space), initial = 0)

for(i in 1:nrow(search_space) ){
  # Subset the states only RSV-NET subset by the metadata that matches the all
  # sites aggregated subset.
  subset <- states_only[as.character(states_only$`Week Ending Date`) %in% search_space[i, 1] &
                          states_only$Characteristic %in% search_space[i, 2] &
                          states_only$Level %in% search_space[i, 3], ]
  
  # If there is a match, simply sum the scaled population used for the rate denominator.
  if(nrow(subset) > 0){
    denominator <- sum(subset$Denominator)
    
    # If there is no match, generalize using the participating state for that infection
    # season and the stratified postcensal population counts to recalculate the
    # scaled rate denominator.
  } else if(nrow(subset) == 0){
    # Target metadata associated with this stratification in the all sites subset.
    target_subset <- all_sites[all_sites$`Week Ending Date` %in% search_space[i, 1] &
                                 all_sites$Characteristic %in% search_space[i, 2] &
                                 all_sites$Level %in% search_space[i, 3], ]
    
    # Save the postcensal population date range.
    pop_date <- colnames(bridged_pop)[str_detect(colnames(bridged_pop), str_glue("Postcensal Population_", {year(target_subset$`Week Ending Date`)} ) )]
    # Save the season in the target.
    season   <- percent_rep[percent_rep$Season %in% target_subset$Season, ]
    
    # Extract the postcensal population estimates for a specific combination of
    # metadata and the states participating in the RSV-NET season.
    season$Population <- bridged_pop[bridged_pop$Characteristic %in% target_subset$Characteristic & 
                                       bridged_pop$Level %in% target_subset$Level &
                                       bridged_pop$State %in% season$State, pop_date]
    
    # Calculate the scaled population rate denominator.
    denominator <- sum(season$Percent * season$Population) %>% floor()
    
  }
  
  # Commit the denominator.
  all_sites[all_sites$`Week Ending Date` %in% search_space[i, 1] &
              all_sites$Characteristic %in% search_space[i, 2] &
              all_sites$Level %in% search_space[i, 3], "Denominator"] <- denominator
  
  # Print the for loop's progress.
  setTxtProgressBar(pb, i)
}


# Recombine the all sites and states only subsets.
rsv_net <- bind_rows(all_sites, states_only)
















