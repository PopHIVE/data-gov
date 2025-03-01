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
  library("arrow")
  library("readxl")
  library("readr")
  library("data.table")
  library("R.utils")
  library("tidyr")
  library("dplyr")
  library("stringr")
  library("lubridate")
  library("MMWRweek")
  library("glue")
})

"%!in%" <- function(x,y)!("%in%"(x,y))




## ----------------------------------------------------------------
## LOAD THE CURRENT COMPILATION

rsv <- read_csv("RSV Infections Data/Harmonized RSV Infections Datasets_02.11.2025.csv") %>%
  as.data.frame()




## ----------------------------------------------------------------
## GOOGLE TRENDS

google_raw <- read_csv("RSV Infections Data/Raw Download/Google Trends_RSV, Bronchiolitis, COVID, and Influenza Query_Downloaded 02.17.2025.csv", skip = 2, col_types = cols(`rsv: (United States)` = col_character())) %>%
  as.data.frame()

# Bronchiolitis is a respiratory condition specific to children ages 2 or younger.
# It can be caused by multiple different respiratory infections, including RSV.
# In this process, we will be compiling additional respiratory diseases, and
# so an additional column denoting "Disease" will be included to distinguish this.
# Source: https://my.clevelandclinic.org/health/diseases/8272-bronchiolitis

google <- google_raw %>%
  rename(COVID = `covid-19: (United States)`,
         Flu = `Influenza: (United States)`,
         RSV = `rsv: (United States)`, 
         Bronchiolitis = `bronchiolitis: (United States)`,
         `Week Observed` = Week)


# Use NA to represent values that are less than 1.
google[, -c(1, 3)] <- sapply(google[, -c(1, 3)], function(x) { x[x == "<1"] <- NA; x} )

# Adjust the column class to numeric.
google[, -1] <- sapply(google[, -1], as.numeric)


# NOTE: a count for the Google Trends dataset is not necessarily a positive case,
#       but it will be considered as such here.
google <- pivot_longer(google, cols = COVID:Bronchiolitis,
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
available_years <- year(google$`Week Observed`) %>% unique() %>% sort()
years_range_end <- format(google$`Week Observed`, "%y") %>% unique() %>% sort() %>% as.numeric() %>%
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


write_parquet(nwss, "RSV Infections Data/Harmonized NWSS Datasets_02.17.2025.gz.parquet", compression = "gzip", compression_level = 5)




## ----------------------------------------------------------------
## RESP-NET

# Initially, RSV-NET was used, but the RESP-NET dataset contains all programs
# for FluSurv-NET and COVID-NET. It also includes a combined category tallying
# total infections.

resp_net_raw <- read_csv("RSV Infections Data/Raw Download/RESP-NET All Seasons_Interactive Dashboard Output_Downloaded 02.16.2025.csv") %>%
  as.data.frame()

# Unfortunately, we see that RESP-NET does not provide stratification at the 
# state-level, but only at the all sites level. Therefore, we will need to
# combine all of the FluSurv-NET and COVID-NET programs with RSV-NET individually
# in order to maintain this granularity.

resp_net_raw[resp_net_raw$Site %in% "Overall", c("Sex", "Race/Ethnicity", "Age group")] %>%
  (\(x) { sapply(x, function(y) unique(y)) }) ()

resp_net_raw[resp_net_raw$Site %!in% "Overall", c("Sex", "Race/Ethnicity", "Age group")] %>%
  (\(x) { sapply(x, function(y) unique(y)) }) ()




# -----------------------------
# IMPORT AND PROCESS COVID-NET
covid_net_raw <- read_csv("RSV Infections Data/Raw Download/COVID-NET All Seasons_Interactive Dashboard Output_Downloaded 02.17.2025.csv") %>%
  as.data.frame()

# Add most of the missing columns and format.
covid_net_raw <- covid_net_raw %>%
  mutate(Dataset = "COVID-NET", Disease = "COVID-19", `Region Type` = NA,
         `Tests Administered` = NA, `Positives Detected` = NA) %>%
  rename(Region = State, `Week Observed` = `_WeekendDate`,
         `Rate` = WeeklyRate, `Cumulative Rate` = CumulativeRate,
         `Age Category` = AgeCategory_Legend, Sex = Sex_Label,
         Race = Race_Label)

# Confirm that the seasons are unique. In the RSV-NET specific export there were
# duplicates, where some entries included notation of the associated characteristic.
covid_net_raw$Season %>% unique() %>% sort()

# Expand out these rates to a wide format.
covid_net_raw <- pivot_wider(covid_net_raw, names_from = Type, values_from = c(`Rate`, `Cumulative Rate`)) %>% as.data.frame()

# Final adjustment of the column names and organization.
covid_net <- covid_net_raw %>%
  rename(`Crude Rate` = `Rate_Crude Rate`, `Age-Adjusted Rate` = `Rate_Age adjusted Rate`,
         `Cumulative Crude Rate` = `Cumulative Rate_Crude Rate`,
         `Cumulative Age-Adjusted Rate` = `Cumulative Rate_Age adjusted Rate`) %>%
  # "Program" is included in the FluSurv-NET dataset, but not the other two
  # NET datasets. Further description justifying why this is added is included
  # in the FluSurv-NET processing section below.
  mutate(Program = "EIP")




# -----------------------------
# IMPORT AND PROCESS FluSurv-NET

flu_net_raw <- read_csv("RSV Infections Data/Raw Download/FluSurv-NET All Seasons_Interactive Dashboard Output_Downloaded 02.17.2025.csv", skip = 2) %>%
  as.data.frame()

# FluSurv-NET included ending notes, which filled one column. We can remove
# these by excluding the ending rows where the second column is NA.
flu_net_raw <- flu_net_raw[flu_net_raw$NETWORK %!in% NA, ]

# Add most of the missing columns and format.
flu_net_raw <- flu_net_raw %>%
  mutate(Dataset = "FluSurv-NET", `Region Type` = NA,
         `Tests Administered` = NA, `Positives Detected` = NA) %>%
  rename(Region = CATCHMENT, Disease = `VIRUS TYPE CATEGORY`, Program = NETWORK,
         Rate = `WEEKLY RATE`, `Cumulative Rate` = `CUMULATIVE RATE`,
         `Age-Adjusted Rate` = `AGE ADJUSTED WEEKLY RATE`, 
         `Cumulative Age-Adjusted Rate` = `AGE ADJUSTED CUMULATIVE RATE`,
         `Age Category` = `AGE CATEGORY`, Sex = `SEX CATEGORY`,
         Race = `RACE CATEGORY`, Season = YEAR...3, Year = YEAR...4)


# Notice that two years include an invalid week for the lubridate() package, but
# a week that is significant in the epidemiological MMWRweek format.
flu_net_raw[flu_net_raw$WEEK %in% "53", "Year"] %>% unique()

# The following code works but is incompatible with the 1-53 week span.
#as.Date(str_c(flu_net_raw$Year, "-", flu_net_raw$WEEK, "-1"), format = "%Y-%U-%u") %>% floor_date("weeks", week_start = 1) %>% head()

flu_net_raw$`Week Observed` <- MMWRweek2Date(flu_net_raw$Year, flu_net_raw$WEEK, MMWRday = NULL) %>% floor_date("weeks", week_start = 1)

# FluSurv-NET reflects results from different surveillance programs. From online
# documentation, it appears that FluSurv-NET is the only program in RESP-NET that
# includes results from other surveillance programs, while RSV- and COVID-NET
# only include EIP sites.
# https://www.cdc.gov/emerging-infections-program/php/network-activities/index.html

# We do see that sites specifically designated as being in the two surveillance
# programs are reported int the COVID-NET program. It is possible that EIP
# masks IHSP, but in FluSurv-NET they are kept separate. Without further confirmation
# it is difficult to know exactly which program is feeding the RSV- and COVID-NET
# results.
flu_net_raw[flu_net_raw$Region %in% unique(covid_net_raw[, "Region"]), "Program"] %>% unique()


# It's not clear if "Region = Entire Network" is the for each respective program,
# or if it is only the whole FluSurv-NET surveillance series.
table(flu_net_raw[, c("Program", "Region")])

# We further see that the only "Region" the two programs have in common is the
# "Region = Entire Network" entry.
unique(flu_net_raw[flu_net_raw$Program %in% "EIP", "Region"]) %>% .[. %in% unique(flu_net_raw[flu_net_raw$Program %in% "IHSP", "Region"])]
unique(flu_net_raw[flu_net_raw$Program %in% "IHSP", "Region"]) %>% .[. %in% unique(flu_net_raw[flu_net_raw$Program %in% "EIP", "Region"])]

# It appears that each programs "Region = Entire Network" is mostly unique from
# the others. Likely, detected duplicates are incidental. Therefore we will
# keep them.
flu_net_raw[flu_net_raw$Region %in% "Entire Network", -c(2)] %>% nrow()/14280
flu_net_raw[flu_net_raw$Region %in% "Entire Network", -c(2)] %>% distinct() %>% nrow()/14280


# Remove unnecessary columns and adjust column names for matching with covid_net.
flu_net <- flu_net_raw %>%
  select(-Year, -WEEK) %>%
  rename(`Crude Rate` = Rate, `Cumulative Crude Rate` = `Cumulative Rate`)

# Confirm that the column names are the same between both sets.
all(colnames(flu_net) %in% colnames(covid_net)) & all(colnames(covid_net) %in% colnames(flu_net))


# Adjust the column classes.
covid_net <- covid_net %>%
  mutate_at(vars(`Tests Administered`, `Positives Detected`, `Crude Rate`, 
                 `Age-Adjusted Rate`, `Cumulative Crude Rate`, `Cumulative Age-Adjusted Rate`), 
            as.numeric)

flu_net <- flu_net %>%
  mutate_at(vars(`Tests Administered`, `Positives Detected`, `Crude Rate`, 
                 `Age-Adjusted Rate`, `Cumulative Crude Rate`, `Cumulative Age-Adjusted Rate`), 
            as.character) %>%
  mutate_at(vars(`Tests Administered`, `Positives Detected`, `Crude Rate`, 
                 `Age-Adjusted Rate`, `Cumulative Crude Rate`, `Cumulative Age-Adjusted Rate`), 
            as.numeric)

# Combine both for co-processing.
resp_net <- bind_rows(flu_net, covid_net)




# -----------------------------
# COMPLETE NOMENCLATURE ADJUSTMENTS AND STANDARDIZATION

# Change "Disease = Overall" to "Influenza". This only applies to the 
# FluSurv-NET dataset. We loose "Influenza A/B" when the unecessary age groups
# were removed a few lines back. It's more important to keep just "Influenza"
# for now.
resp_net[resp_net$Dataset %in% "FluSurv-NET", "Disease"] %>% unique()
resp_net[resp_net$Dataset %in% "FluSurv-NET" & resp_net$Disease %in% "Overall", "Disease"] <- "Influenza"

# We see that there are two representation of New York. We can rename "Region =
# Entire Network/COVID-NET" to "Region = All Sites".
unique(resp_net$Region) %>% .[. %!in% c("District of Columbia", datasets::state.name)]

resp_net[str_detect(resp_net$Region, "Entire Network|COVID-NET"), "Region"] <- "All Sites"

# To simplify the workflow, we are not going to spend time combining the different
# New York regions, and instead choose one. We'll randomly choose to remove
# Rochester.
resp_net <- resp_net[!str_detect(resp_net$Region, "New York - Rochester"), ]

resp_net[str_detect(resp_net$Region, "New York - Albany"), "Region"] <- "New York"

# The two datasets merged label non-stratified entries differently: "Overall" or
# "All". Change these to match.
resp_net <- resp_net %>%
  mutate(across(`Age Category`:Race, \(x) replace(x, x == "Overall", "All")))


## The NET datasets came only with rates and no counts or population numbers.
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
remove_ages <- unique(resp_net$`Age Category`) %>% .[. %!in% c("0-<1 year", "0-< 1 yr", "1-4 years", "1-4 yr", "5-17 years", "5-17 yr", "18-49 yr", "18-49 years", "50-64 years", "50-64 yr", "65-74 years", "65-74 yr", "≥75 years", ">= 75", "All")]

# Select out the values stratified by age-groups we do not need.
resp_net <- resp_net[resp_net$`Age Category` %!in% remove_ages, ] %>% `rownames<-`(NULL)

# Adjust the age-groups nomenclature.
resp_net[resp_net$`Age Category` %in% c("0-<1 year", "0-< 1 yr"), "Age Category"]   <- "<1 Years"
resp_net[resp_net$`Age Category` %in% c("1-4 years", "1-4 yr"), "Age Category"]     <- "1-4 Years"
resp_net[resp_net$`Age Category` %in% c("5-17 years", "5-17 yr"), "Age Category"]   <- "5-17 Years"
resp_net[resp_net$`Age Category` %in% c("18-49 yr", "18-49 years"), "Age Category"] <- "18-49 Years"
resp_net[resp_net$`Age Category` %in% c("50-64 years", "50-64 yr"), "Age Category"] <- "50-64 Years"
resp_net[resp_net$`Age Category` %in% c("65-74 years", "65-74 yr"), "Age Category"] <- "65-74 Years"
resp_net[resp_net$`Age Category` %in% c("≥75 years", ">= 75"), "Age Category"]      <- "75+ Years"

# The nomenclature for sex is correct.
resp_net$Sex %>% unique()

# Adjust the Race nomenclature.
resp_net[resp_net$Race %in% c("Hispanic/Latino", "Hispanic"), "Race"] <- "Hispanic or Latino"
resp_net[resp_net$Race %in% c("Asian/Pacific Islander", "A/PI, non-Hispanic"), "Race"]         <- "Asian or Pacific Islander"
resp_net[resp_net$Race %in% c("American Indian/Alaska Native", "AI/AN, non-Hispanic"), "Race"] <- "American Indian or Alaska Native"
resp_net[resp_net$Race %in% c("Black", "Black, non-Hispanic"), "Race"] <- "Black or African American"
resp_net[resp_net$Race %in% c("White", "White, non-Hispanic"), "Race"] <- "White"


# Confirm that each Race, Age, and Sex stratification is independent.
resp_net[resp_net$Sex %!in% "All", ] %>% (\(x) { table(x[, "Age Category"], x[, "Sex"], x[, "Race"]) }) ()
resp_net[resp_net$Race %!in% "All", ] %>% (\(x) { table(x[, "Race"], x[, "Age Category"], x[, "Sex"]) }) ()
resp_net[resp_net$`Age Category` %!in% "All", ] %>% (\(x) { table(x[, "Age Category"], x[, "Race"], x[, "Sex"]) }) ()


# We see that indeed each stratification is independent of the others, except for
# specific age groups which we do not plan to maintain. First we'll select out the 

resp_net_sex <- resp_net[resp_net$Sex %!in% "All", ] %>%
  mutate(Characteristic = "Sex") %>% 
  rename(Level = Sex) %>%
  select(colnames(resp_net)[-c(4:6)], "Characteristic", "Level")

resp_net_race <- resp_net[resp_net$Race %!in% "All", ] %>%
  mutate(Characteristic = "Race") %>% 
  rename(Level = Race) %>%
  select(colnames(resp_net)[-c(4:6)], "Characteristic", "Level")

resp_net_age  <- resp_net[resp_net$`Age Category` %!in% "All", ] %>%
  mutate(Characteristic = "Age") %>% 
  rename(Level = `Age Category`) %>%
  select(colnames(resp_net)[-c(4:6)], "Characteristic", "Level")

resp_net_not_strat <- resp_net[resp_net$Sex %in% "All" & resp_net$Race %in% "All" & resp_net$`Age Category` %in% "All", ] %>%
  mutate(Characteristic = "Not Stratified", Level = "N/A") %>% 
  select(colnames(resp_net)[-c(4:6)], "Characteristic", "Level")


# Confirm that all rows have been accounted for.
nrow(resp_net_sex) + nrow(resp_net_race) + nrow(resp_net_age) + nrow(resp_net_not_strat) == nrow(resp_net)


# Generate the final table.
resp_net <- bind_rows(resp_net_sex, resp_net_race, resp_net_age, resp_net_not_strat) %>%
  as.data.frame() %>% `rownames<-`(NULL)




# -----------------------------
# Conform seasons and rate nomenclature and data structure.

# The RESP-NET surveillance season begins week 40 (approximately Oct. 1st) and
# ends week 39 of the following year (approximately Sep. 30th). We desire the
# range to start in July and end in June of the following year.

# To label the season where the tests results were recorded, we need to define
# the span of years available and the boundaries of that season.
available_years <- year(resp_net$`Week Observed`) %>% unique() %>% sort()
years_range_end <- format(resp_net$`Week Observed`, "%y") %>% unique() %>% sort() %>% as.numeric() %>%
  (\(x) { c(x[1], x+1) }) ()

# Associate a seasons week's boundaries over the span of years available with
# the new season name that conforms to the RSV-NET nomenclature.
season_ranges <- data.frame("Season" = str_c((min(available_years)-1):(max(available_years)), years_range_end, sep = "-"),
                            "Start" = str_c(c(min(available_years) - 1, available_years), "-07-01"),
                            "End" = str_c(c(available_years, max(available_years) + 1), "-06-30"))

# Add the appropriate season label as a new variable.
resp_net$Season <- sapply(resp_net$`Week Observed`, function(x) {
  ifelse(x >= season_ranges[, "Start"] & x <= season_ranges[, "End"], 1, 0) %>% 
    (\(y) { season_ranges[as.logical(y), "Season"] }) 
})

# Confirm there are no NA's introduced in the conversion process.
resp_net[resp_net$Season %in% NA, ]


# Check the "Characteristic/Level" nomenclature.
unique(rsv$Characteristic) %>% .[. %!in% unique(resp_net$Characteristic)]
unique(rsv$Level) %>% .[. %!in% unique(resp_net$Level)]

# Align the "Characteristic/Level" nomenclature.
resp_net[resp_net$Characteristic %in% "Race", "Characteristic"] <- "Race/Ethnicity"

# Check the "Region/Region Level" nomenclature.
unique(rsv$Region) %>% .[. %!in% unique(resp_net$Region)] %>% sort()
unique(rsv$`Region Type`) %>% .[. %!in% unique(resp_net$`Region Type`)]

# There are multiple states that are not present in both sets. We really want to
# confirm that the state name is consistent and will be detected together with
# other entries with the same region name. HHS regions will be added in the next
# section.
unique(resp_net$Region) %>% .[. %!in% c("District of Columbia", datasets::state.name)]

resp_net[resp_net$Region %!in% "All Sites", "Region Type"] <- "State"


# The current data conglomerate does not contain a column for "Program" or "Disease".
colnames(rsv)[colnames(rsv) %!in% colnames(resp_net)]
colnames(resp_net)[colnames(resp_net) %!in% colnames(rsv)]

# Add "Program" or "Disease" column to the RSV dataset if it is not there.
if(any(colnames(rsv) %in% "Disease") == FALSE){
  rsv <- rsv %>% mutate(Disease = "RSV") %>%
    select(Dataset, Disease, colnames(rsv)[-1])
}

# As explained earlier, it is assumed that the RSV- and COVID-NET programs
# are assumed to be apart of the EIP surveillance program only. This could
# change down the road.
if(any(colnames(rsv) %in% "Program") == FALSE){
  rsv <- rsv %>% mutate(Program = NA) %>%
    select(Dataset, Program, colnames(rsv)[-1])
  
  rsv[str_detect(rsv$Dataset, "RSV-NET|COVID-NET"), "Program"] <- "EIP"
}




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
  mutate("HHS Region" = NA, "Population" = NA, "Percent Represented" = NA)

# The "Region = All Sites" variable will need to be handled separately.
all_sites   <- resp_net[resp_net$Region %in% "All Sites", ] %>% `rownames<-`(NULL) %>%
  rename(State = Region)

states_only <- resp_net[resp_net$Region %!in% "All Sites", ] %>% `rownames<-`(NULL) #%>%
  #rename(State = Region)

# Fill in the population count based on the year of the RSV detection and
# the entry metadata. The population used is based on the year recorded
# by the "Week Observed column.
for(i in 1:nrow(states_only)){
  year_observed    <- year(states_only$`Week Observed`[i])
  years_bridge_pop <- str_extract(colnames(bridged_pop)[-c(1:4)], "[0-9]{4}")
  
  if( any(year_observed %in% years_bridge_pop) ){
    # Match the appropriate population by metadata and year of observation.
    states_only[i, "Population"] <- 
      bridged_pop[bridgzed_pop$Region %in% states_only$Region[i] & 
                    bridged_pop$Characteristic %in% states_only$Characteristic[i] &
                    bridged_pop$Level %in% states_only$Level[i], 
                  str_detect(colnames(bridged_pop), "Postcensal Population")] %>%
      (\(x) { x[, str_detect(colnames(x), as.character(year_observed) )] }) ()
    
  } else {
    # If the years observed are out of range, then apply only the max available
    # bridge-race population count.
    states_only[i, "Population"] <- 
      bridged_pop[bridged_pop$Region %in% states_only$Region[i] & 
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
  states_only[states_only$Region %in% percent_rep[i, 1] & states_only$Season %in% percent_rep[i, 3], "Percent Represented"] <- percent_rep[i, 2]
}

# Produce the approximate denominator used to generate the rates for
# participating counties.
states_only$Denominator <- floor(states_only$Population * states_only$`Percent Represented`)


# For the "Region = All Sites" outcome, the sum of the denominator from participating
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
                                       bridged_pop$Region %in% season$Region, pop_date]
    
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




## ----------------------------------------------------------------
# COMPILE ALL DATASETS

combined <- bind_rows(rsv, google)
                      #cbind("Dataset" = rep("Google Trends", nrow(google)), google))

write_parquet(combined, "RSV Infections Data/Harmonized RSV Infections Datasets_02.11.2025.gz.parquet", compression = "gzip", compression_level = 5)

#write.csv(combined, "RSV Infections Data/Harmonized RSV Infections Datasets_02.11.2025.csv", row.names = FALSE)










