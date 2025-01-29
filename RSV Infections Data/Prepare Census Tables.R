## ----------------------------------------------------------------
## Preparing Census Data
##
## Date: January 28th, 2025

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

bridged_pop_raw <- fread("RSV Infections Data/CDC_Vintage 2020 Bridged-Race Postcensal Population Estimates_v2020_y1020_txt.zip", sep = "", header = FALSE) %>%
  # The file has replaced preceding zeros with spaces. To regain the correct
  # formatting for splitting, these zeros need to be reintroduced.
  `colnames<-`("Joined") %>% sapply(., function(x) str_replace_all(x, "\\s", "0")) %>%
  as.data.table()


# https://transition.fcc.gov/oet/info/maps/census/fips/fips.txt
fips_codes <- read_excel("RSV Infections Data/US FIPS Codes.xls", skip = 1) %>%
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


## ----------------------------------------------------------------
## PREPARE THE BRIDGED CENSUS FILE

# https://www.cdc.gov/nchs/data/nvss/bridged_race/Documentation-Bridged-PostcenV2020.pdf
# PDF pg. 17
bridged_pop <- bridged_pop_raw %>% tidyr::separate(Joined,
  sep = c(4, 6, 9, 11, 12, 13, 21, 29, 37, 45, 53, 61, 69, 77, 85, 93, 101, 109),
  into = c("Vintage", "State_FIPS", "County_FIPS", "Age", "Bridged-Race-Sex", "Hispanic Origin", 
           "Base Population_2010", "Postcensal Population_2010", "Postcensal Population_2011", 
           "Postcensal Population_2012", "Postcensal Population_2013", "Postcensal Population_2014",
           "Postcensal Population_2015", "Postcensal Population_2016", "Postcensal Population_2017", 
           "Postcensal Population_2018", "Postcensal Population_2019", "Postcensal Population_2020"))

# Correct the population numbers format to as.numeric().
bridged_pop[, -c(1:6)] <- sapply(bridged_pop[, -c(1:6)], function(x) as.numeric(x))


state_codes <- unique(bridged_pop$State_FIPS)


# -----------------------------
# Generate the state-level population counts.

state_pop <- c()
options(dplyr.summarise.inform = FALSE)
for(i in 1:length(state_codes)){
  one_state <- bridged_pop[bridged_pop$State_FIPS %in% state_codes[i], ]
  
  # Confirm that each county has the same dimensions.
  check_grouping <- one_state %>%
    group_by(County_FIPS) %>% 
    tally() %>% 
    .[, "n"] %>% unique() %>% length() == 1
  
  if(check_grouping != TRUE){
    warning(str_glue("Inconsitent population reporting over stratifications. State: {state_codes[i]}."))
  }
  
  
  # Collapse the county-level population numbers to the state-level.
  one_state_pop <- one_state[, -c(2:3)] %>%
    group_by(Vintage, Age, `Bridged-Race-Sex`, `Hispanic Origin`) %>% 
    summarise_at(vars(`Base Population_2010`:`Postcensal Population_2020`), sum, na.rm = FALSE) %>%
    ungroup()
  
  # Add the state FIPS Code.
  one_state_pop <- cbind(data.frame("State" = rep(state_codes[i], nrow(one_state_pop))), one_state_pop)
  
  state_pop <- bind_rows(state_pop, one_state_pop)
}


# Add the national-level as well.
bridged_pop_state <- bind_rows(state_pop, state_pop[, -1] %>%
  group_by(Vintage, Age, `Bridged-Race-Sex`, `Hispanic Origin`) %>% 
  summarise_at(vars(`Base Population_2010`:`Postcensal Population_2020`), sum, na.rm = FALSE) %>%
  ungroup()
)

# Introduce FIPS code for the national-level values: "State = 00".
bridged_pop_state[bridged_pop_state$State %in% NA, "State"] <- "00"


# -----------------------------
# Collapse data to only reflect singular characteristic stratifications.

# Combine the "Race-Sex" column to separate population by race or sex. All racial 
# categories reflect those without Hispanic origin with Hispanic included as its 
# own separate category.

# Add separate columns for race, sex, and age grouping.
bridged_pop_state <- bridged_pop_state %>% 
  mutate("Race" = NA, "Sex" = NA, "Age Group" = NA, Age = as.numeric(Age)) %>%
  select("State", "Vintage", "Age Group", "Race", "Sex", "Age", colnames(state_pop)[4:17])



# Re-label each entry based on the new stratification.
bridged_pop_state[bridged_pop_state$`Bridged-Race-Sex` %in% c(1, 3, 5, 7), "Sex"] <- "Male"
bridged_pop_state[bridged_pop_state$`Bridged-Race-Sex` %in% c(2, 4, 6, 8), "Sex"] <- "Female"

# Sum over groups for all counts by sex.
bridged_pop_sex <- bridged_pop_state[, -c(3:4, 6:8)] %>%
  group_by(State, Vintage, Sex) %>% 
  summarise_at(vars(`Base Population_2010`:`Postcensal Population_2020`), sum, na.rm = FALSE) %>%
  ungroup()



# Re-label each entry based on the new stratification.
bridged_pop_state[bridged_pop_state$`Bridged-Race-Sex` %in% c(1:2) & bridged_pop_state$`Hispanic Origin` %in% 1, "Race"] <- "White"
bridged_pop_state[bridged_pop_state$`Bridged-Race-Sex` %in% c(3:4) & bridged_pop_state$`Hispanic Origin` %in% 1, "Race"] <- "Black or African American"
bridged_pop_state[bridged_pop_state$`Bridged-Race-Sex` %in% c(5:6) & bridged_pop_state$`Hispanic Origin` %in% 1, "Race"] <- "American Indian or Alaska Native"
bridged_pop_state[bridged_pop_state$`Bridged-Race-Sex` %in% c(7:8) & bridged_pop_state$`Hispanic Origin` %in% 1, "Race"] <- "Asian or Pacific Islander"
bridged_pop_state[bridged_pop_state$`Hispanic Origin` %in% 2, "Race"] <- "Hispanic or Latino"

# Sum over groups for all counts by racial group.
bridged_pop_race <- bridged_pop_state[, -c(3, 5:8)] %>%
  group_by(State, Vintage, Race) %>% 
  summarise_at(vars(`Base Population_2010`:`Postcensal Population_2020`), sum, na.rm = FALSE) %>%
  ungroup()


# Sum over groups for all counts by non-Hispanic racial groups.
bridged_pop_race <- bind_rows(bridged_pop_race,
  bridged_pop_race[bridged_pop_race$Race %!in% "Hispanic or Latino", ] %>%
  group_by(State, Vintage) %>% 
  summarise_at(vars(`Base Population_2010`:`Postcensal Population_2020`), sum, na.rm = FALSE) %>%
  ungroup()
)

bridged_pop_race[bridged_pop_race$Race %in% NA, "Race"] <- "Multi-Race, non-Hispanic"



## Combine the age-groups to: "<1 Years", "1-4 Years", "5-17 Years", "18-49 Years", 
## "50-64 Years", "65-74 Years", and "75+ Years".

# Re-label each entry based on the new stratification.
bridged_pop_state[bridged_pop_state$Age %in% 0, "Age Group"]     <- "<1 Years"
bridged_pop_state[bridged_pop_state$Age %in% 1:4, "Age Group"]   <- "1-4 Years"
bridged_pop_state[bridged_pop_state$Age %in% 5:17, "Age Group"]  <- "5-17 Years"
bridged_pop_state[bridged_pop_state$Age %in% 18:49, "Age Group"] <- "18-49 Years"
bridged_pop_state[bridged_pop_state$Age %in% 50:64, "Age Group"] <- "50-64 Years"
bridged_pop_state[bridged_pop_state$Age %in% 65:74, "Age Group"] <- "65-74 Years"
bridged_pop_state[bridged_pop_state$Age %in% 75:85, "Age Group"] <- "75+ Years"

# Sum over groups for all counts by age group.
bridged_pop_age <- bridged_pop_state[, -c(4:8)] %>%
  group_by(State, Vintage, `Age Group`) %>% 
  summarise_at(vars(`Base Population_2010`:`Postcensal Population_2020`), sum, na.rm = FALSE) %>%
  ungroup()


# Sum over groups for all counts, not stratified.
bridged_pop_not_strat <- bridged_pop_state[, -c(3:8)] %>%
  group_by(State, Vintage) %>% 
  summarise_at(vars(`Base Population_2010`:`Postcensal Population_2020`), sum, na.rm = FALSE) %>%
  ungroup()


# Format and combine the sets.
bridged_pop_sex <- bridged_pop_sex %>% 
  mutate(Characteristic = "Sex") %>% 
  rename(Level = Sex) %>%
  select("State", "Vintage", "Characteristic", "Level", colnames(bridged_pop_state)[9:20])

bridged_pop_race <- bridged_pop_race %>% 
  mutate(Characteristic = "Race/Ethnicity") %>% 
  rename(Level = Race) %>%
  select("State", "Vintage", "Characteristic", "Level", colnames(bridged_pop_state)[9:20])

bridged_pop_age <- bridged_pop_age %>% 
  mutate(Characteristic = "Age") %>% 
  rename(Level = `Age Group`) %>%
  select("State", "Vintage", "Characteristic", "Level", colnames(bridged_pop_state)[9:20])

bridged_pop_not_strat <- bridged_pop_not_strat %>% 
  mutate(Characteristic = "Not Stratified", Level = "N/A") %>% 
  select("State", "Vintage", "Characteristic", "Level", colnames(bridged_pop_state)[9:20])


# Generate the final table.
bridged_pop_all <- bind_rows(bridged_pop_not_strat, bridged_pop_age, bridged_pop_race, bridged_pop_sex) %>%
  as.data.frame() %>% `rownames<-`(NULL)




# -----------------------------
# Translate the state FIPS codes.

fips_state_only <- distinct(fips_codes[, c("State", "FIPS State")]) %>%
  rbind(data.frame("State" = "US", "FIPS State" = "00", check.names = FALSE), .)


bridged_pop_all <- rename(bridged_pop_all, `FIPS State` = State) %>%
  mutate("State" = NA) %>%
  select(`FIPS State`, State, colnames(bridged_pop_all)[-1])


# Translate the FIPS codes
for(i in 1:nrow(fips_state_only)){
  bridged_pop_all[bridged_pop_all$`FIPS State` %in% fips_state_only[i, 2], "State"] <- fips_state_only[i, 1]
}

# Keep only the relevant columns.
bridged_pop_all <- bridged_pop_all %>%
  select(-c(`FIPS State`, Vintage)) %>% `rownames<-`(NULL)




# -----------------------------
# Save results.
write.csv(bridged_pop_all, "RSV Infections Data/CDC_Vintage 2020 Bridged-Race Postcensal Population Estimates_v2020_y1020_Prepared.csv", row.names=FALSE)

# Return warning options to baseline.
options(dplyr.summarise.inform = TRUE)





## ----------------------------------------------------------------
## PREPARE THE CENSUS FILE







