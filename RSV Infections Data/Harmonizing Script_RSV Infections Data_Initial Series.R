## ----------------------------------------------------------------
## Harmonizing RSV Infections Data
##
## Date: December 23rd, 2024
## Source:
##    - CDC's RSV-NET (connected with NREVSS data)
##    - CDC's NREVSS
##    - EPIC COSMOS

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


bridged_pop <- read_csv("RSV Infections Data/Reference Files/CDC_Vintage 2020 Bridged-Race Postcensal Population Estimates_v2020_y1020_All Counties.csv") %>%
  as.data.frame()

# Special subset for counties of hospitals that were listed as participants in the
# 2024-25 season. Other seasons were not noted.
bridged_pop_nrevss <- read_csv("RSV Infections Data/Reference Files/CDC_Vintage 2020 Bridged-Race Postcensal Population Estimates_v2020_y1020_NREVSS Subset.csv") %>%
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


# The RSV-NET surveillance season begins week 40 (approximately Oct. 1st) and
# ends week 39 of the following year (approximately Sep. 30th). We desire the
# range to start in July and end in June of the following year.

# To label the season where the tests results were recorded, we need to define
# the span of years available and the boundaries of that season.
available_years <- year(google$`Week Ending Date`) %>% unique() %>% sort()

# Associate a seasons week's boundaries over the span of years available with
# the new season name that conforms to the RSV-NET nomenclature.
season_ranges <- data.frame("Season" = str_c((min(available_years)-1):(max(available_years)), 20:26, sep = "-"),
                            "Start" = str_c(c(min(available_years) - 1, available_years), "-07-01"),
                            "End" = str_c(c(available_years, max(available_years) + 1), "-06-30"))

# Add the appropriate season label as a new variable.
google$Season2 <- sapply(google$`Week Observed`, function(x) {
  ifelse(x >= season_ranges[, "Start"] & x <= season_ranges[, "End"], 1, 0) %>% 
    (\(y) { season_ranges[as.logical(y), "Season"] }) 
})

# Confirm there are no NA's introduced in the conversion process.
rsv_net[rsv_net$Season2 %in% NA, ]

# Commit the season changes.
rsv_net <- rsv_net %>% 
  select(State, Season2, colnames(rsv_net)[3:9]) %>%
  rename(Season = Season2)




# -----------------------------
# Check for other outcomes not needed in the variable columns.

# Confirm that only states are reflected.
unique(rsv_net$State) %>% .[. %!in% c("US", "District of Columbia", datasets::state.name)]

# But notice also that these are the only entries that reflect the age-adjusted
# rate. This variable denotes counts and rates across all participating sites.
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
    denominator <- sum(subset$Denominator)
    
  # If there is no match, generalize using the participating state for that infection
  # season and the stratified postcensal population counts to recalculate the
  # scaled rate denominator.
  } else if(nrow(subset) == 0){
    # Target metadata associated with this stratification in the all sites subset.
    target_subset <- all_sites[all_sites$`Week ending date` %in% search_space[i, 1] &
      all_sites$Characteristic %in% search_space[i, 2] &
      all_sites$Level %in% search_space[i, 3], ]
    
    # Save the postcensal population date range.
    pop_date <- colnames(bridged_pop)[str_detect(colnames(bridged_pop), str_glue("Postcensal Population_", {year(target_subset$`Week ending date`)} ) )]
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
  all_sites[all_sites$`Week ending date` %in% search_space[i, 1] &
   all_sites$Characteristic %in% search_space[i, 2] &
   all_sites$Level %in% search_space[i, 3], "Denominator"] <- denominator
  
  # Print the for loop's progress.
  setTxtProgressBar(pb, i)
}


# Recombine the all sites and states only subsets.
rsv_net <- bind_rows(all_sites, states_only)


# -----------------------------
# Fill in the missing values.

# Counts should be an integer. At this state, we are approximating the count
# in order to regenerate the rates. Therefore, a few extra significant figures
# will be retained.
rsv_net$Count <- round( (rsv_net$`Rate_Crude Rate` * rsv_net$Denominator) / 100000, digits = 2)


# Fill in the HHS Region that the state is in.
for(i in 1:nrow(HHS_regions)){
  rsv_net[rsv_net$State %in% HHS_regions[i, 1], "HHS Region"] <- HHS_regions[i, 2]
}

# Aggregate by HHS Region.
rsv_hhs <- rsv_net[rsv_net$`HHS Region` %!in% NA, c(2:5, 10, 13:14)] %>%
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


# -----------------------------
# Aesthetics and structure.

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


#########
# NEW CUMSUM METHOD
new_rows %>%
  filter(type == "Crude Rate") %>%
  group_by(season, state, age_category, sex, race) %>%
  mutate("new_cumSum" = cumsum(rate)) %>% 
  ungroup() %>%
  as.data.frame()






# Add diagnostic test type to conform with the NREVSS dataset.
rsv_net <- rsv_net %>%
  mutate(`Diagnostic Test Type` = "All") %>%
  select(c(colnames(rsv_net)[1:4], `Diagnostic Test Type` , colnames(rsv_net)[5:11]))
  

# Round the counts to ceiling.
rsv_net$Count <- ceiling(rsv_net$Count)

# Adhere column nomenclature and organization to NREVSS.
rsv_net <- rsv_net %>%
  mutate(`Tests Administered` = NA) %>%
  select(colnames(rsv_net)[1:7], `Tests Administered`, colnames(rsv_net)[8:12]) %>%
  rename(`Positives Detected` = Count)
  

# "Region = All Sites" has "Region Type = NA".
rsv_net[rsv_net$Region %in% "All Sites", "Region Type"] <- NA


# Organize the rows.
rsv_net <- rsv_net[with(rsv_net, order(`Region Type`, Region, `Diagnostic Test Type`, Season, Characteristic, Level, `Week Observed`)), ] %>% 
  `rownames<-`(NULL)




## ----------------------------------------------------------------
## HARMONIZE - NREVSS

head(nrevss_lab)
head(nrevss_hhs)


# -----------------------------
# NREVSS Lab Data

# The RSV-NET surveillance season begins week 40 (approximately Oct. 1st) and
# ends week 39 of the following year (approximately Sep. 30th). We desire to
# have the season start July 1st through June 30th.

# Format the dates so that it is easier for the program to read.
nrevss_lab$`Week ending Date` <- as.Date(nrevss_lab$`Week ending Date`, format = "%d%B%Y")

# To label the season where the tests results were recorded, we need to define
# the span of years available and the boundaries of that season.
available_years <- year(nrevss_lab$`Week ending Date`) %>% unique()

# Associate a seasons week's boundaries over the span of years available with
# the new season name that conforms to the RSV-NET nomenclature.
season_ranges <- data.frame("Season" = str_c((min(available_years)-1):(max(available_years)), 10:21, sep = "-"),
                            "Start" = str_c(c(min(available_years) - 1, available_years), "-07-01"),
                            "End" = str_c(c(available_years, max(available_years) + 1), "-06-30"))

# Add the appropriate season label as a new variable.
nrevss_lab$Season <- sapply(nrevss_lab$`Week ending Date`, function(x) {
  ifelse(x >= season_ranges[, "Start"] & x <= season_ranges[, "End"], 1, 0) %>% 
         (\(y) { season_ranges[as.logical(y), "Season"] }) 
  })


# Adhere to the RSV-NET dataset formatting and columns.
nrevss_lab <- nrevss_lab %>%
  select(-c(`Surveillance Year`, `Week ending Code`)) %>%
  rename(`Positives Detected` = `RSV Detections`, `Tests Administered` = `RSV Tests`,
         Region = `HHS region`, `Week Observed` = `Week ending Date`) %>%
  mutate(`Region Type` = "HHS", Characteristic = "Not Stratified", Level = "N/A",
         `Crude Rate` = NA, `Age-Adjusted Rate` = NA, 
         `Cumulative Crude Rate` = NA, `Cumulative Age-Adjusted Rate` = NA) %>%
  mutate(Region = str_c("Region ", nrevss_lab$`HHS region`)) %>%
  select(colnames(rsv_net))

# Organize the rows.
nrevss_lab <- nrevss_lab[with(nrevss_lab, order(`Region Type`, Region, `Diagnostic Test Type`, Season, Characteristic, Level, `Week Observed`)), ] %>% 
  `rownames<-`(NULL)



# In this scenario, "State = US" represents the total population of all
# participating counties.
pop_not_strat <- bridged_pop_nrevss[bridged_pop_nrevss$Characteristic %in% "Not Stratified" & bridged_pop_nrevss$State %!in% "US", ] %>%
  `rownames<-`(NULL)

# Fill in the HHS Region that the state is in.
for(i in 1:nrow(HHS_regions)){
  pop_not_strat[pop_not_strat$State %in% HHS_regions[i, 1], "HHS Region"] <- HHS_regions[i, 2]
}

# Aggregate by HHS Region.
pop_not_strat <- pop_not_strat %>%
  group_by(`HHS Region`, Characteristic, Level) %>% 
  summarise_at(vars(`Base Population_2010`:`Postcensal Population_2020`), sum, na.rm = FALSE) %>%
  ungroup() %>% as.data.frame()


# Add the relevant population estimates. These will be the rate denominator.
nrevss_lab$Population <- NA

for(i in 1:nrow(nrevss_lab)){
  year_observed    <- year(nrevss_lab$`Week Observed`[i])
  years_bridge_pop <- str_extract(colnames(pop_not_strat)[-c(1:4)], "[0-9]{4}")
  
  # Match the appropriate population by metadata and year of observation. Notice
  # that both sets only have "Characteristic = Not Stratified" and "Level = N/A".
  nrevss_lab[i, "Population"] <- 
    pop_not_strat[pop_not_strat$`HHS Region` %in% nrevss_lab$Region[i], 
                str_detect(colnames(pop_not_strat), "Postcensal Population")] %>%
    (\(x) { x[, str_detect(colnames(x), as.character(year_observed) )] }) ()
   
}


# Collapse "the `"Diagnostic Test Type" by simply summing the different tests.
nrevss_lab <- nrevss_lab %>%
  group_by(Region, `Region Type`, Season, `Week Observed`, Characteristic, Level) %>% 
  summarise_at(vars(`Tests Administered`, `Positives Detected`, Population), sum, na.rm = FALSE) %>%
  ungroup() %>% 
  mutate(`Crude Rate` = NA, `Age-Adjusted Rate` = NA,
         `Cumulative Crude Rate` = NA, `Cumulative Age-Adjusted Rate` = NA,
         `Diagnostic Test Type` = "All") %>%
  select(colnames(nrevss_lab)) %>%
  as.data.frame()


# Calculate the rate per 100,000 persons.
nrevss_lab$`Crude Rate` <- round( (nrevss_lab$`Positives Detected` / nrevss_lab$Population) * 100000, digits = 1)

# Now we're going to calculate the "Cumulative Crude Rate" values.

# Generate the search space for subsetting.
search_space <- nrevss_lab[nrevss_lab$`Region Type` %in% "HHS", ] %>% 
  (\(x) { table(x$Region, x$Characteristic, x$Level) }) () %>% 
  as.data.frame() %>%
  (\(x) { x[x$Freq %!in% 0, ] }) () %>% 
  `rownames<-`(NULL)


for(i in 1:nrow(search_space) ){
  # Subset by the metadata.
  subset <- nrevss_lab[nrevss_lab$Region %in% search_space[i, 1] &
                         nrevss_lab$Characteristic %in% search_space[i, 2] &
                         nrevss_lab$Level %in% search_space[i, 3], ]
  
  for(j in 1:length(unique(subset$Season)) ){
    # Save the crude rate.
    rate <- subset[subset$Season %in% unique(subset$Season)[j], "Crude Rate"]
    
    # Calculate and save the cumulative of that rate.
    subset[subset$Season %in% unique(subset$Season)[j], "Cumulative Crude Rate"] <- cumsum(rate)
  }
  
  # Commit the changes.
  nrevss_lab[nrevss_lab$Region %in% search_space[i, 1] &
               nrevss_lab$Characteristic %in% search_space[i, 2] &
               nrevss_lab$Level %in% search_space[i, 3], ] <- subset
}

# Remove the "Population" column.
nrevss_lab <- nrevss_lab %>% select(colnames(rsv_net))

# Organize the rows.
nrevss_lab <- nrevss_lab[with(nrevss_lab, order(`Region Type`, Region, `Diagnostic Test Type`, Season, Characteristic, Level, `Week Observed`)), ] %>% 
  `rownames<-`(NULL)




# -----------------------------
# NREVSS HHS Data

# First we want to inspect if there are unique entries ("posted") made for a given 
# week ("mmwrweek_end").

# Convert the date formats so that they are easier to read.
nrevss_hhs$`mmwrweek_end` <- str_split(nrevss_hhs$`mmwrweek_end`, " ") %>% 
  do.call(rbind, .) %>% .[, 1] %>% as.Date(., format = "%m/%d/%Y")

nrevss_hhs$posted <- str_split(nrevss_hhs$posted, " ") %>% 
  do.call(rbind, .) %>% .[, 1] %>% as.Date(., format = "%m/%d/%Y")

# Remove the percentage columns and alternative test averaging, as those are
# expected to differ in value but provide a new metric from the other methods.
# For this analysis, keeping the 5-week moving average for the current week
# is sufficient.
nrevss_hhs <- nrevss_hhs[, -c(2:5, 7:8)]

# If newly posted entries are unique, then there should not be a large volume,
# if any, duplicated "pcr_detections" or "pcr_tests".

# Number of entries for one post for a given week of tests and region.
nrevss_hhs[nrevss_hhs$posted %in% "2023-09-21" & 
             nrevss_hhs$mmwrweek_end %in% "2020-04-11" &
             nrevss_hhs$level %in% "National", ]

# Number of entries associated with that week and region.
nrevss_hhs[nrevss_hhs$mmwrweek_end %in% "2020-04-11" & 
             nrevss_hhs$level %in% "National", ]

# We clearly see there are a large number of duplicated entries with the
# exact same combination of values for "pcr_detections" and "pcr_tests".
nrevss_hhs[nrevss_hhs$mmwrweek_end %in% "2020-04-11" &
             nrevss_hhs$level %in% "National", -c(4)] %>% distinct()

# While it is possible that the same number of tests and positive results could
# happen at different participating sites and be reported for the same week,
# we will assume that all duplicated entries are not unique. Different reports 
# for "mmwrweek_end" will be averaged.

# Remove the "posted" variable, as it does not provide valuable additional information.
nrevss_hhs <- nrevss_hhs[, -c(4)]

# Take the average of all candidate, unique, entries for a given week and region.
nrevss_hhs <- nrevss_hhs %>%
  group_by(level, mmwrweek_end) %>% 
  summarise_at(vars(pcr_detections, pcr_tests), mean, na.rm = FALSE) %>%
  ungroup() %>% as.data.frame()

# Round to the nearest significant figure.
nrevss_hhs[, c(3:4)] <- sapply(nrevss_hhs[, c(3:4)], function(x) round(x, digits = 1))


# Adhere to the RSV-NET dataset formatting and columns.
nrevss_hhs <- nrevss_hhs %>%
  rename(Region = level, `Positives Detected` = pcr_detections, 
         `Tests Administered` = pcr_tests, `Week Observed` = mmwrweek_end) %>%
  mutate(`Region Type` = "HHS", Season = NA, `Diagnostic Test Type` = "PCR",
         Characteristic = "Not Stratified", Level = "N/A",
         `Crude Rate` = NA, `Age-Adjusted Rate` = NA, `Cumulative Crude Rate` = NA,
         `Cumulative Age-Adjusted Rate` = NA) %>% 
  select(colnames(rsv_net))


# To label the season where the tests results were recorded, we need to define
# the span of years available and the boundaries of that season.
available_years <- year(nrevss_hhs$`Week Observed`) %>% unique() %>% sort()

# Associate a seasons week's boundaries over the span of years available with
# the new season name that conforms to the RSV-NET nomenclature.
season_ranges <- data.frame("Season" = str_c((min(available_years)-1):(max(available_years)), 20:26, sep = "-"),
                            "Start" = str_c(c(min(available_years) - 1, available_years), "-07-01"),
                            "End" = str_c(c(available_years, max(available_years) + 1), "-06-30"))

# Add the appropriate season label as a new variable.
nrevss_hhs$Season <- sapply(nrevss_hhs$`Week Observed`, function(x) {
  ifelse(x >= season_ranges[, "Start"] & x <= season_ranges[, "End"], 1, 0) %>% 
    (\(y) { season_ranges[as.logical(y), "Season"] }) 
})


# Adjust the national-level to specify all participating sites.
nrevss_hhs[nrevss_hhs$Region %in% "National", "Region"] <- "All Sites"

# "Region = All Sites" has "Region Type = NA".
nrevss_hhs[nrevss_hhs$Region %in% "All Sites", "Region Type"] <- NA


# Sum over all of the HHS regions to get the "All Sites" population level.
if(any(pop_not_strat$`HHS Region` %in% "All Sites") == FALSE){
  pop_not_strat <- bind_rows(cbind("HHS Region" = "All Sites", pop_not_strat %>%
    group_by(Characteristic, Level) %>% 
    summarise_at(vars(`Base Population_2010`:`Postcensal Population_2020`), sum, na.rm = FALSE) %>%
    ungroup()), pop_not_strat
  )
}

# Add the relevant population estimates. These will be the rate denominator.
nrevss_hhs$Population <- NA

for(i in 1:nrow(nrevss_hhs)){
  # Match the appropriate population by metadata and year of observation. Notice
  # that both sets only have "Characteristic = Not Stratified" and "Level = N/A".
  # Also notice that the range of available dates exceeds the range of the
  # bridge-race census data. In its place we'll use the most recent available
  # population estimates.
  nrevss_hhs[i, "Population"] <- 
    pop_not_strat[pop_not_strat$`HHS Region` %in% nrevss_hhs$Region[i], 
                  str_detect(colnames(pop_not_strat), "Postcensal Population_2020")]
  
}

# Calculate the rate per 100,000 persons.
nrevss_hhs$`Crude Rate` <- round( (nrevss_hhs$`Positives Detected` / nrevss_hhs$Population) * 100000, digits = 1)

# Organize the rows.
nrevss_hhs <- nrevss_hhs[with(nrevss_hhs, order(`Region Type`, Region, `Diagnostic Test Type`, Season, Characteristic, Level, `Week Observed`)), ] %>% 
  `rownames<-`(NULL)


# Now we're going to calculate the "Cumulative Crude Rate" values.

# Generate the search space for subsetting.
search_space <- nrevss_hhs %>% 
  (\(x) { table(x$Region, x$Characteristic, x$Level) }) () %>% 
  as.data.frame() %>%
  (\(x) { x[x$Freq %!in% 0, ] }) () %>% 
  `rownames<-`(NULL)


for(i in 1:nrow(search_space) ){
  # Subset by the metadata.
  subset <- nrevss_hhs[nrevss_hhs$Region %in% search_space[i, 1] &
                         nrevss_hhs$Characteristic %in% search_space[i, 2] &
                         nrevss_hhs$Level %in% search_space[i, 3], ]
  
  for(j in 1:length(unique(subset$Season)) ){
    # Save the crude rate.
    rate <- subset[subset$Season %in% unique(subset$Season)[j], "Crude Rate"]
    
    # Calculate and save the cumulative of that rate.
    subset[subset$Season %in% unique(subset$Season)[j], "Cumulative Crude Rate"] <- cumsum(rate)
  }
  
  # Commit the changes.
  nrevss_hhs[nrevss_hhs$Region %in% search_space[i, 1] &
               nrevss_hhs$Characteristic %in% search_space[i, 2] &
               nrevss_hhs$Level %in% search_space[i, 3], ] <- subset
}

# Remove the "Population" column.
nrevss_hhs <- nrevss_hhs %>% select(colnames(rsv_net))


# We intend to not differentiate results generated by different quantitative
# methods. We'll change the diagnostic type to "All".
nrevss_hhs$`Diagnostic Test Type` <- "All"

# As a final step we'll round the count columns that should be integers.
nrevss_hhs[, c("Tests Administered", "Positives Detected")] <- sapply(nrevss_hhs[, c("Tests Administered", "Positives Detected")], function(x) round(x, digits = 0))




## ----------------------------------------------------------------
## HARMONIZE - EPIC COSMOS

head(epic)

# About link: https://www.epicresearch.org/data-tracker/communicable-diseases
# This page comes with additional tables for Office Visits and ER visits
# nationally, but this cannot be readily downloaded. It is therefore only
# left to the state-wide rates that have a data download button.

# At the time of download, the season reflected is: Between 12/22/2024 and 1/4/2025.
# Rates are not noted to be age-adjusted, and so they are assumed to be crude rates.
# Additionally, we'll assume that a hospital visit that is counted here implies
# that the visit was with someone who tested positive for RSV. Because the
# tests used to confirm a positive infection is not noted, we'll assume any
# test type is represented.

epic <- read_csv("RSV Infections Data/Raw Download/EpicCosmos_Communicable Diseases by State_Between 12.22.2024 and 01.04.2025_ Downloaded 01.23.2025.csv") %>%
  as.data.frame()


# Adhere to the RSV-NET dataset formatting and columns.
epic <- epic %>%
  rename(Region = State, `Positives Detected` = `Total visits`, 
         `Crude Rate` = `Infections per 100,000 visits`) %>%
  mutate(`Region Type` = "State", Season = "2024-25", `Diagnostic Test Type` = "All",
         Characteristic = "Not Stratified", Level = "N/A", `Week Observed` = NA,
         `Age-Adjusted Rate` = NA, `Cumulative Crude Rate` = NA,
         `Cumulative Age-Adjusted Rate` = NA, `Tests Administered` = NA) %>% 
  select(colnames(rsv_net))


# Regenerate the population values used to estimate the infection rate.
epic$Population <- (epic$`Positives Detected` / epic$`Crude Rate`) * 100000

# Add the national-level representation of all participating sites.
epic <- bind_rows(epic, epic %>%
  summarise_at(vars(`Positives Detected`, Population), sum, na.rm = FALSE) %>%
  ungroup() %>%
  mutate(Region = "All Sites", Season = "2024-25", `Diagnostic Test Type` = "All",
         Characteristic = "Not Stratified", Level = "N/A")
)

# Fill in the HHS Region that the state is in.
for(i in 1:nrow(HHS_regions)){
  epic[epic$Region %in% HHS_regions[i, 1], "HHS Region"] <- HHS_regions[i, 2]
}

# Aggregate by HHS Region.
epic_hhs <- epic[epic$`HHS Region` %!in% NA, ] %>%
  group_by(`HHS Region`) %>% 
  summarise_at(vars(`Positives Detected`, Population), sum, na.rm = FALSE) %>%
  ungroup() %>%
  mutate(`Region Type` = "HHS", Season = "2024-25", `Diagnostic Test Type` = "All",
         Characteristic = "Not Stratified", Level = "N/A") %>%
  rename(Region = `HHS Region`)

# Add the HHS-level reports back to the main dataset.
epic <- bind_rows(epic, epic_hhs)

# Calculate the crude rate for the new regions.
epic[epic$`Crude Rate` %in% NA, "Crude Rate"] <- epic[epic$`Crude Rate` %in% NA, ] %>%
  (\(x) { (x$`Positives Detected` / x$Population) * 100000 }) ()

# Round rate to one significant figure.
epic$`Crude Rate` <- round(epic$`Crude Rate`, digits = 1)

# Remove the "Population" column.
epic <- epic %>% select(colnames(rsv_net))

# Organize the rows.
epic <- epic[with(epic, order(`Region Type`, Region)), ] %>% 
  `rownames<-`(NULL)




## ----------------------------------------------------------------
## COMBINE ALL THREE

# One major difference between the Epic Cosmos dataset and the others is that
# Epic Cosmos only shows the total values over one season. We will therefore
# add this to the other sets so they have some crossover.

# Clear environment of specific variables to ensure no crossover with global
# variables.

rm(dataset)
rm(search_space)
rm(subset)
rm(available_years)
rm(pop)

# Function to add the seasonal rates.
calc_seasonal_rates <- function(dataset){
  
  search_space <- dataset %>% 
    (\(x) { table(x$Region, x$Season, x$`Diagnostic Test Type`, x$Characteristic, x$Level) }) () %>% 
    as.data.frame() %>%
    (\(x) { x[x$Freq %!in% 0, ] }) () %>% 
    `rownames<-`(NULL)
  
  
  result <- list()
  for(i in 1:nrow(search_space) ){
    # Subset by the metadata.
    subset <- dataset[dataset$Region %in% search_space[i, 1] &
                        dataset$Season %in% search_space[i, 2] &
                        dataset$`Diagnostic Test Type` %in% search_space[i, 3] &
                        dataset$Characteristic %in% search_space[i, 4] &
                        dataset$Level %in% search_space[i, 5], ]
    
    # Aggregate by Season.
    seasonal_level <- subset %>%
      summarise_at(vars(`Tests Administered`, `Positives Detected`), sum, na.rm = FALSE) %>%
      mutate(Region = unique(subset$Region), `Region Type` = unique(subset$`Region Type`), 
             Season = unique(subset$Season), `Week Observed` = NA,
             `Diagnostic Test Type` = unique(subset$`Diagnostic Test Type`),
             Characteristic = unique(subset$Characteristic), Level = unique(subset$Level))
    
    
    
    # The new rate needs to be calculated using the current values over that
    # season. If there is no rate reported, then fill the value accordingly.
    if(all(subset$`Crude Rate` %in% NA)){
      seasonal_level$`Crude Rate` <- NA
      
    } else if(mean(subset$`Crude Rate`) == 0){
      seasonal_level$`Crude Rate` <- 0
      
    } else if(mean(subset$`Crude Rate`) != 0){
      available_years <- year(subset$`Week Observed`) %>% unique()
      
      # Average the population represented over the years that the season spans.
      # Use the max "Positives Detected" in each year to regenerate the population used.
      
      if(length(available_years) == 1){
        pop <- subset[year(subset$`Week Observed`) %in% available_years[1], ] %>% 
          (\(x) { x[which(x$`Positives Detected` == max(x$`Positives Detected`)), ] }) () %>%
          (\(x) { (x$`Positives Detected` / x$`Crude Rate`) * 100000 }) () %>% .[1]
        
      } else if(length(available_years) == 2){
        
        # Condition if any span of dates is all zeros or NA.
        
        # If the first range is all zeros or NA, only use the second span of dates.
        if(all(subset[year(subset$`Week Observed`) %in% available_years[1], "Crude Rate"] == 0) | 
           all(subset[year(subset$`Week Observed`) %in% available_years[1], "Crude Rate"] %in% NA)) {
          pop <- subset[year(subset$`Week Observed`) %in% available_years[2], ] %>% 
            (\(x) { x[which(x$`Positives Detected` == max(x$`Positives Detected`)), ] }) () %>%
            (\(x) { (x$`Positives Detected` / x$`Crude Rate`) * 100000 }) () %>% .[1]
          
          # If the second range is all zeros or NA, only use the first span of dates.
        } else if(all(subset[year(subset$`Week Observed`) %in% available_years[2], "Crude Rate"] == 0) | 
                  all(subset[year(subset$`Week Observed`) %in% available_years[2], "Crude Rate"] %in% NA)) {
          pop <- subset[year(subset$`Week Observed`) %in% available_years[1], ] %>% 
            (\(x) { x[which(x$`Positives Detected` == max(x$`Positives Detected`)), ] }) () %>%
            (\(x) { (x$`Positives Detected` / x$`Crude Rate`) * 100000 }) () %>% .[1]
          
          # If both are not zeros or NA then average over both.
        } else{
          pop <- mean(
            subset[year(subset$`Week Observed`) %in% available_years[1], ] %>% 
              (\(x) { x[which(x$`Positives Detected` == max(x$`Positives Detected`)), ] }) () %>%
              (\(x) { (x$`Positives Detected` / x$`Crude Rate`) * 100000 }) () %>% .[1],
            subset[year(subset$`Week Observed`) %in% available_years[2], ] %>% 
              (\(x) { x[which(x$`Positives Detected` == max(x$`Positives Detected`)), ] }) () %>%
              (\(x) { (x$`Positives Detected` / x$`Crude Rate`) * 100000 }) () %>% .[1]
          )
          
        }
        
      }
      
      # Recalculate the Crude Rate over the whole season.
      seasonal_level$`Crude Rate` <- round((seasonal_level$`Positives Detected` / pop) * 100000, 1)
      
      
    }
    
    
    # Commit the changes.
    result[[i]] <- bind_rows(subset, seasonal_level)
  }
  
  do.call(rbind, result)
}

rsv_net_new <- calc_seasonal_rates(rsv_net)
nrevss_lab_new <- calc_seasonal_rates(nrevss_lab)
nrevss_hhs_new <- calc_seasonal_rates(nrevss_hhs)



# -----------------------------
# COMPILE ALL DATASETS

# Positives Detected and Tests Administered

combined <- bind_rows(cbind("Dataset" = rep("RSV-NET", nrow(rsv_net_new)), rsv_net_new),
                      cbind("Dataset" = rep("NREVSS", nrow(nrevss_lab_new)), nrevss_lab_new), 
                      cbind("Dataset" = rep("NREVSS", nrow(nrevss_hhs_new)), nrevss_hhs_new),
                      cbind("Dataset" = rep("Epic Cosmos", nrow(epic)), epic))

# Remove the Diagnostic Test Type variable.
combined <- combined %>% select(-`Diagnostic Test Type`)


write.csv(combined, "RSV Infections Data/Harmonized RSV Infections Datasets_02.11.2025.csv", row.names = FALSE)





