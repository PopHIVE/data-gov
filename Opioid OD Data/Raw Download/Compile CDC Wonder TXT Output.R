## ----------------------------------------------------------------
## Compile the CDC Wonder Output
##
## Date: January 10th, 2024
## Goal: Efficiently compile the sparse CDC Wonder datasets and
##       save them as paraquet files.

## ----------------------------------------------------------------
## SET UP THE ENVIRONMENT

library("readxl")
library("tidyr")
library("dplyr")
library("stringr")
library("lubridate")
library("glue")

"%!in%" <- function(x,y)!("%in%"(x,y))

#https://wonder.cdc.gov/controller/saved/D157/D420F574
# Need to split the dates for the export.


## ----------------------------------------------------------------
## LOAD THE DATA


# Individual example
file_dir = file.path(getwd(), "Opioid OD Data/Raw Download/CDC Wonder TXT Output", 
                     "State/Ten-Year Age Groups", "*.txt")

# "National/Ten-Year Age Groups"

file_names <- sapply(Sys.glob(file_dir), function(x) str_split(x, "/")[[1]] %>% tail(., n = 1)) %>% `names<-`(NULL)

saved_sets <- lapply(Sys.glob(file_dir), function(x) read.delim(x)) %>%
  `names<-`(file_names)




## ----------------------------------------------------------------
## CONFIRM PARAMETERS

confirm_notes <- function(input_list, state_level = FALSE, summarize_results = FALSE){
  ## A function to confirm data query settings are correct so that the labeling
  ## added in this script is confirmed to be accurate.
  
  notes_ok <- c()
  for(i in 1:length(input_list)){
    # Separate out each component of information for comparisons with the output
    # notes section.
    file_name_components <- str_split(names(input_list)[i], "_")[[1]]
    
    # Extract out the CDC WONDER data query tool notes section.
    notes <- input_list[[i]] %>% .[.$Notes %!in% "", "Notes"]
    
    # -----------------------------
    # Evaluate salient output settings to confirm the data set is compiled
    # and labeled correctly.
    
    result <- c()
    
    # -----------------------------
    # Confirm correct data set was queried.
    if(file_name_components[2] == "Multiple Cause of Death 2018-2022"){
      # At the time of exporting the data, the 2023 data was just being released
      # for select stratification.
      result[1] <- str_detect(notes[str_detect(notes, "Dataset:")], "Multiple Cause of Death, 2018-202(2|3), Single Race")
    } else{
      result[1] <- "Dataset error"
      
    }
    
    # -----------------------------
    # Confirm the place of death is as expected.
    if(file_name_components[3] == "All Locations"){
      # Check that the results are not being grouped by "Place of Death", which
      # should also not be present as a query parameter in the notes section.
      result[2] <- str_detect(notes[str_detect(notes, "Group By:")], "Place of Death") == FALSE &
        length(notes[str_detect(notes, "Place of Death:")]) == 0
      
    } else if(file_name_components[3] == "IP:ED"){
      # Most extractions will pull both Place of Death separated by inpatient
      # and emergency department deaths.
      if(length(notes[str_detect(notes, "Place of Death:")]) != 0){
        result[2] <- str_detect(notes[str_detect(notes, "Place of Death:")], "Medical Facility - Inpatient") &
          str_detect(notes[str_detect(notes, "Place of Death:")], "Medical Facility - Outpatient or ER")
        
      } else if(length(notes[str_detect(notes, "Place of Death:")]) == 0){
        result[2] <- FALSE
      }
      
    } else if(str_detect(file_name_components[3], "IP")){
      # If only inpatient was selected for this subset, then check that emergency
      # department was not also selected.
      if(length(notes[str_detect(notes, "Place of Death:")]) != 0){
        result[2] <- str_detect(notes[str_detect(notes, "Place of Death:")], "Medical Facility - Inpatient") &
          str_detect(notes[str_detect(notes, "Place of Death:")], "Medical Facility - Outpatient or ER") == FALSE
        
      } else if(length(notes[str_detect(notes, "Place of Death:")]) == 0){
        result[2] <- FALSE
      }
      
    } else if(str_detect(file_name_components[3], "ED")){
      # If only emergency department was selected for this subset, then check 
      # that inpatient was not also selected.
      if(length(notes[str_detect(notes, "Place of Death:")]) != 0){
        result[2] <- str_detect(notes[str_detect(notes, "Place of Death:")], "Medical Facility - Inpatient") == FALSE &
          str_detect(notes[str_detect(notes, "Place of Death:")], "Medical Facility - Outpatient or ER")
        
      } else if(length(notes[str_detect(notes, "Place of Death:")]) == 0){
        result[2] <- FALSE
      }
      
    } else{
      result[2] <- "Place of death error"
      
    }
    
    # -----------------------------
    # Confirm the underlying cause of death either includes all types or
    # unintentional (ICD-10 codes X40-X44 and Y10-Y14) only.
    # NOTE: when stratifying by the manner of death, only "UCD - Drug/Alcohol 
    #       Induced Causes" was toggled under section "6. Select underlying
    #       cause of death". If all underlying causes are included then this
    #       field will not show up.
    if(file_name_components[4] == "All Deaths"){
      result[3] <- length(notes[str_detect(notes, "UCD - Drug/Alcohol Induced Causes:")]) == 0
      
    } else if(file_name_components[4] == "Unintentional"){
      # UCD has a fixed length for the correct query.
      ucd_codes = notes[str_which(notes, "UCD - Drug/Alcohol Induced Causes:")[1]:(str_which(notes, "UCD - Drug/Alcohol Induced Causes:")[1]+1)] %>%
        str_flatten(collapse = " ")
      
      # Must include both ICD-10 codes: X40-X44 and Y10-Y14.
      result[3] <- str_detect(ucd_codes, "Drug poisonings \\(overdose\\) Unintentional \\(X40-X44\\)") &
        # "Undetermined" means ICD-10 codes Y10-Y14
        str_detect(ucd_codes, "Drug poisonings \\(overdose\\) Undetermined \\(Y10-Y14\\)")
      
    } else{
      result[3] <- "Manner of death error"
      
    }
    
    # -----------------------------
    # Confirm the ICD-10 code for the drug type or combination is correct.
    if(str_detect(file_name_components[5], "\\+") == FALSE){
      # Sometimes more than one line in the notes is dedicated to MCD query
      # setting. The number of rows and following setting also vary. The following
      # are generalized steps to locate all of the MCD query conditions.
      subsection_locations = which(str_detect(notes, ":"))
      mcd_location         = which(str_detect(notes, "MCD - ICD-10 Codes:") & str_detect(notes, ":"))
      pull_mcd_range       = which(subsection_locations %in% mcd_location)
      mcd_range            = subsection_locations[pull_mcd_range:(pull_mcd_range+1)]
      
      mcd_all = notes[mcd_range[1]:(mcd_range[2]-1)] %>%
        str_flatten(collapse = " ")
      
      if(str_detect(file_name_components[5], "All Drugs")){
        # Confirm that the correct opioid ICD-10 and polysubstance codes are
        # exclusively being referenced.
        any_opioids <- str_detect(mcd_all, "F11.0 \\(Mental and behavioural disorders due to use of opioids, acute intoxication\\)") &
          str_detect(mcd_all, "T40.0 \\(Opium\\)") &
          str_detect(mcd_all, "T40.1 \\(Heroin\\)") &
          str_detect(mcd_all, "T40.2 \\(Other opioids\\)") &
          str_detect(mcd_all, "T40.3 \\(Methadone\\)") &
          str_detect(mcd_all, "T40.4 \\(Other synthetic narcotics\\)") &
          str_detect(mcd_all, "T40.6 \\(Other and unspecified narcotics\\)") &
          str_detect(mcd_all, "T42.4 \\(Benzodiazepines\\)") &
          str_detect(mcd_all, "T40.5 \\(Cocaine\\)") &
          # And does not contain other ICD-10 codes than what is expected.
          str_extract_all(mcd_all, "[A-Z]+[0-9]{2}.[0-9]{1}")[[1]] %in% c("F11.0", "T40.0", "T40.1", "T40.2", "T40.3", "T40.4", "T40.5", "T40.6", "T42.4") %>% all()
        
        # When all drugs individual results are queried, the output needs to be 
        # stratified by MCD as well.
        grouped_by_mcd <- str_detect(notes[str_detect(notes, "Group By:")], "Multiple Cause of death")
        
        result[4] <- c(any_opioids, grouped_by_mcd) %>% all()
        
      } else if(str_detect(file_name_components[5], "All Opioids")){
        # Confirm that the correct opioid ICD-10 are exclusively being referenced.
        result[4] <- str_detect(mcd_all, "F11.0 \\(Mental and behavioural disorders due to use of opioids, acute intoxication\\)") &
          str_detect(mcd_all, "T40.0 \\(Opium\\)") &
          str_detect(mcd_all, "T40.1 \\(Heroin\\)") &
          str_detect(mcd_all, "T40.2 \\(Other opioids\\)") &
          str_detect(mcd_all, "T40.3 \\(Methadone\\)") &
          str_detect(mcd_all, "T40.4 \\(Other synthetic narcotics\\)") &
          str_detect(mcd_all, "T40.6 \\(Other and unspecified narcotics\\)") &
          # And does not contain other ICD-10 codes than what is expected.
          str_extract_all(mcd_all, "[A-Z]+[0-9]{2}.[0-9]{1}")[[1]] %in% c("F11.0", "T40.0", "T40.1", "T40.2", "T40.3", "T40.4", "T40.6") %>% all()
        
      }
      
    } else if(str_detect(file_name_components[5], "\\+")){
      drug_conditioning = notes[str_detect(notes, "MCD - ICD-10 Codes:")]
      
      # Sometimes more than one line in the notes is dedicated to the first 
      # query conditioning.
      first_condition   = notes[str_which(notes, "MCD - ICD-10 Codes:")[1]:(str_which(notes, "MCD - ICD-10 Codes:")[2]-1)] %>%
        str_flatten(collapse = " ")
      
      # Confirm that the correct opioid ICD-10 codes are exclusively being
      # referenced in the first record selection. This box pulls any match
      # to codes listed here.
      any_opioids <- str_detect(first_condition, "F11.0 \\(Mental and behavioural disorders due to use of opioids, acute intoxication\\)") &
        str_detect(first_condition, "T40.0 \\(Opium\\)") &
        str_detect(first_condition, "T40.1 \\(Heroin\\)") &
        str_detect(first_condition, "T40.2 \\(Other opioids\\)") &
        str_detect(first_condition, "T40.3 \\(Methadone\\)") &
        str_detect(first_condition, "T40.4 \\(Other synthetic narcotics\\)") &
        str_detect(first_condition, "T40.6 \\(Other and unspecified narcotics\\)") &
        # And does not contain the polysubstance conditions.
        str_detect(first_condition, "T42.4 \\(Benzodiazepines\\)") == FALSE &
        str_detect(first_condition, "T40.5 \\(Cocaine\\)") == FALSE &
        # And does not contain other ICD-10 codes than what is expected.
        str_extract_all(first_condition, "[A-Z]+[0-9]{2}.[0-9]{1}")[[1]] %in% c("F11.0", "T40.0", "T40.1", "T40.2", "T40.3", "T40.4", "T40.6") %>% all()
      
      # Must contain three entries for "MCD - ICD-10 Codes": the first item
      # selection box, the Boolean for the two boxes, and the second selection
      # box.
      query_length  <- drug_conditioning %>% length() == 3
      
      # Boolean needs to be "AND" to be a polysubstance match.
      boolean_boxes <- str_detect(drug_conditioning[2], "AND")
      
      # The second conditioning box is where the polysubstance is included.
      # It can either be the depressant Benzodiazepines or stimulant Cocaine.
      # Other entries are identified as FALSE.
      if(str_detect(file_name_components[5], "Benzodiazepines")){
        polysubstance = str_detect(drug_conditioning[3], "T42.4 \\(Benzodiazepines\\)")
        
      } else if(str_detect(file_name_components[5], "Cocaine")){
        polysubstance = str_detect(drug_conditioning[3], "T40.5 \\(Cocaine\\)")
        
      } else{polysubstance = FALSE}
      
      # Confirm that all of these four conditions are TRUE.
      result[4] <- c(any_opioids, query_length, boolean_boxes, polysubstance) %>% all()
      
    } else{
      result[4] <- "Drug selection error"
      
    }
    
    # -----------------------------
    # Confirm the stratification is correct, and exclusively is stratified by
    # that one setting. Include a test to confirm that all stratifications
    # are age-adjusted except for the stratification by age section.
    if(file_name_components[6] == "Not Stratified"){
      # Only needs to not be grouped by any of the stratification conditions.
      result[5] <- str_detect(notes[str_detect(notes, "Group By:")], c("Single Race 6", "Hispanic Origin", "Ten-Year Age Groups", "Gender")) %>% any() == FALSE &
        str_detect(notes, "age-adjusted") %>% any()
      
    } else if(file_name_components[6] ==  "Single Race 6 Not Hispanic"){
      result[5] <- str_detect(notes[str_detect(notes, "Group By:")], "Single Race 6") &
        str_detect(notes[str_detect(notes, "Hispanic Origin:")], "Not Hispanic or Latino") &
        str_detect(notes[str_detect(notes, "Group By:")], c("Hispanic Origin", "Ten-Year Age Groups", "Gender")) %>% any() == FALSE &
        str_detect(notes, "age-adjusted") %>% any()
      
    } else if(file_name_components[6] ==  "Hispanic"){
      result[5] <- only_stratification <- str_detect(notes[str_detect(notes, "Group By:")], "Hispanic Origin") &
        str_detect(notes[str_detect(notes, "Group By:")], c("Single Race 6", "Ten-Year Age Groups", "Gender")) %>% any() == FALSE &
        str_detect(notes, "age-adjusted") %>% any()
      
    } else if(file_name_components[6] ==  "Age"){
      result[5] <- str_detect(notes[str_detect(notes, "Group By:")], "Ten-Year Age Groups") &
        str_detect(notes[str_detect(notes, "Group By:")], c("Single Race 6", "Hispanic Origin", "Gender")) %>% any() == FALSE &
        colnames(input_list[[i]]) %in% "Age.Adjusted.Rate" %>% any() == FALSE
      
    } else if(file_name_components[6] ==  "Gender"){
      result[5] <- str_detect(notes[str_detect(notes, "Group By:")], "Gender") &
        str_detect(notes[str_detect(notes, "Group By:")], c("Single Race 6", "Hispanic Origin", "Ten-Year Age Groups")) %>% any() == FALSE &
        str_detect(notes, "age-adjusted") %>% any()
      
    } else{
      result[5] <- "Stratification error"
      
    }
    
    # -----------------------------
    # Confirm the dates are what is expected by the file name. For output that
    # was too large, this was divided into parts 1 or 2 by dividing the dates
    # in half. This check is only looking to confirm that the groupings were
    # by Year or Month.
    result[6] <- ifelse(str_detect(notes[str_detect(notes, "Group By:")], str_replace(file_name_components[7], " pt (1|2)", "")),
                        TRUE, "Dates error")
    
    
    # -----------------------------
    # Confirm the all of the entries under "State" are a state or the District 
    # of Columbia.
    if(state_level == TRUE){
      states_represented <- input_list[[i]][, colnames(input_list[[i]]) %in% "State"] %>% unique()
      # NOTE: The test is designed to tolerate empty entries as those will be caught
      #       down the road.
      if(length(states_represented) == 0){
        result[7] <- FALSE
        
      } else if(length(states_represented) != 0){
        result[7] <- all(states_represented %in% c(state.name, "District of Columbia", ""))
        
      }
      
    } else if(state_level == FALSE){
      result[7] <- any(colnames(input_list[[i]]) %in% "State") == FALSE

    }
    
    # The QC test is intended to all be vectors unless an unknown error arises.
    if(class(result) == "logical"){
      # Allow for summarizing the results.
      if(summarize_results == TRUE){
        message     = str_c(c("Dataset: ", "Setting: ", "Underlying cause: ", "Drug conditioning: ", 
                              "Stratification: ", "Dates: ", "Regional: "), result)
        
        # Show only which sections failed the Boolean test.
        if(any(str_detect(message, "FALSE")) == TRUE){
          notes_ok[i] <- message[str_detect(message, "FALSE")] %>% str_flatten(collapse = "; ")
          
        # State that all test levels were passed.
        } else if(any(str_detect(message, "FALSE")) == FALSE){
          notes_ok[i] <- "All passed."
          
        }
        
      # If not summarizing the results, only print the result as one Boolean.
      } else if(summarize_results == FALSE){
        notes_ok[i] <- all(result)
        
      }
      
    # Capture the error statement.
    } else if(class(result) == "character"){
      notes_ok[i] <- result[result %!in% c("FALSE", "TRUE")] %>% str_flatten(collapse = "; ")
      
    }
  }
  
  notes_ok
}


# Individual example
confirm_notes(saved_sets, state_level = TRUE, summarize_results = FALSE)
confirm_notes(saved_sets[1:2], state_level = TRUE, summarize_results = TRUE)




## ----------------------------------------------------------------
## FORMAT AND AGGREGATE FILES

standardize_format <- function(input_list){
  ## Conform the files formatting so that they can be correctly compiles.
  
  # -----------------------------
  # Basic dataset formatting generalized to all raw sets.
  
  # Remove the notes section and only keep the data table.
  df <- lapply(input_list, function(x) x[1:(str_which(x$Notes, "---")[1]-1), ])
  
  # The "Month.Code" or "Year.Code" column has the desired dates nomenclature.
  # To prompt the algorithm to keep this column, those column names are changed
  # to "Date" in all list entries.
  df[str_detect(names(df), "Month")] <- lapply(df[str_detect(names(df), "Month")], function(x) rename(x, Date = Month.Code))
  df[str_detect(names(df), "Year")]  <- lapply(df[str_detect(names(df), "Year")], function(x) rename(x, Date = Year.Code))
  
  # Adjust the names of columns to adhere to cross source nomenclature.
  df[str_detect(names(df), "All Drugs")] <- lapply(df[str_detect(names(df), "All Drugs")], function(x) rename(x, Drug = Multiple.Cause.of.death))
  df[str_detect(names(df), "IP:ED")]     <- lapply(df[str_detect(names(df), "IP:ED")], function(x) rename(x, Setting = Place.of.Death))
  
  df <- lapply(df, function(x) rename(x, Count = Deaths))
  
  
  # Remove the unnecessary columns including those with the word "Code" in them,
  # which are duplicates of the non-coded variable column, and "Month"/"Year"
  # since the nomenclature change above to "Date".
  #
  # NOTE: "Year"' needs to be handled separately to avoid removing the
  #       "Ten.Year.Age.Groups" stratification column.
  df <- lapply(df, function(x) x[, !str_detect(colnames(x), "Code|Month")])
  df <- lapply(df, function(x) x[, colnames(x) %!in% "Year"])
  
  # Format the column names so that special characters are removed.
  for (i in 1:length(df)) {
    colnames(df[[i]]) <- str_replace_all(colnames(df[[i]]), "[^a-zA-Z\\d]", " ")
  }
  
  
  # -----------------------------
  # Standardize the column names so that each has the intended information. The
  # largest missing information at this point include the "Setting" and "Drug".
  # Some relevant information is piped from the file name.
  
  # Code to find the correct string matching conditions for the expected
  # data set sizes with similar structures.
  #col_dim = lapply(df, function(x) ncol(x)) %>% `names<-`(NULL) %>% unlist()
  #names(df)[col_dim == 11] # IP/ED, All Drugs
  #names(df)[col_dim == 9]  # All Locations, All Drugs OR IP/ED, All Opioids, All Opioids +
  #names(df)[col_dim == 7]  # All Locations, All Opioids, All Opioids +
  
  # Isolate column names based on what columns need to be added.
  add_setting      <- names(df)[str_which(names(df), "All (Locations.*Drugs|Drugs.*Locations)")]
  add_drug         <- names(df)[str_which(names(df), "IP:ED.*All Opioids")]
  add_setting_drug <- names(df)[str_which(names(df), "All (Locations.*Opioids|Opioids.*Locations)")]
  
  # Add "Setting" based on the file name.
  for(i in 1:length(c(add_setting, add_setting_drug))){
    subset <- df[names(df) %in% c(add_setting, add_setting_drug)][[i]]
    subset$Setting <- "All"
    
    df[names(df) %in% c(add_setting, add_setting_drug)][[i]] <- subset
  }
  
  # Add "Drug" based on the file name.
  for(i in 1:length(c(add_drug, add_setting_drug))){
    subset <- df[names(df) %in% c(add_drug, add_setting_drug)][[i]]
    
    # Separate out each component of information and extract the drug class
    # the data represents.
    file_name_components <- str_split(names(df)[names(df) %in% c(add_drug, add_setting_drug)][i], "_")[[1]][5]
    
    subset$Drug <- file_name_components
    df[names(df) %in% c(add_drug, add_setting_drug)][[i]] <- subset
    
  }
  
  # Code to confirm that all of the column names are the same.
  #lapply(df, function(x) all(colnames(x) %in% c("Notes", "Date", "Setting", "Manner_of_Death",
  #                                              "Count", "Population", "Crude Rate", 
  #                                              "Age Adjusted Rate", "Drug")) ) %>% unlist()
  
  
  # -----------------------------
  # Add the "Manner of Death", "Characteristic", and "Level" columns and 
  # conform column classes.
  for(i in 1:length(df)){
    subset <- df[[i]]
    file_name_components <- str_split(names(df)[i], "_")[[1]]
    
    # Add the "Characteristic" and "Level" columns based on the file names
    # so that the nomenclature matches the coalesced dataset.
    if(file_name_components[6] == "Not Stratified"){
      subset$Characteristic <- "Not Stratified"
      subset$Level          <- "N/A"
      
    } else if(file_name_components[6] ==  "Single Race 6 Not Hispanic"){
      subset$Characteristic <- "Race/Ethnicity"
      subset <- rename(subset, Level = `Single Race 6`)
      
    } else if(file_name_components[6] ==  "Hispanic"){
      subset$Characteristic <- "Race/Ethnicity"
      subset <- rename(subset, Level = `Hispanic Origin`)
      
    } else if(file_name_components[6] ==  "Age"){
      subset$Characteristic <- "Age"
      subset <- rename(subset, Level = `Ten Year Age Groups`)
      
      # Data stratified by age groups cannot be age adjusted. To match the
      # column dimensions a vector of NA's is added.
      subset$`Age Adjusted Rate` <- NA
      
    } else if(file_name_components[6] ==  "Gender"){
      subset$Characteristic <- "Sex"
      subset <- rename(subset, Level = `Gender`)
      
    }
    
    
    # Add the "Manner of Death".
    if(file_name_components[4] == "All Deaths"){
      subset$Manner_of_Death <- "All"
      
    } else if(file_name_components[4] ==  "Unintentional"){
      subset$Manner_of_Death <- "Unintentional"
      
    } else {
      warning(str_glue("Manner of death file name is inconsistent. File name: {names(df)[i]}."))
      
    }
    
    
    # Add the national or state level column if it is missing.
    if(any(colnames(subset) %in% "State") == FALSE){
      subset$State <- "US"
    }
    
    # -----------------------------
    # Convert the columns to the same type.
    subset[, c("Notes", "Setting", "Manner_of_Death", "Drug", "State", "Characteristic", "Level")] <- 
      sapply(subset[, c("Notes", "Setting", "Manner_of_Death", "Drug", "State", "Characteristic", "Level")], as.character)
    
    # The following four numeric variables are sometimes reported as character
    # vectors where unreliable or suppressed values are present. In order to
    # preserve this information the following coding is applied prioir to
    # coercing them into a numeric or integer class.
    #     - Not Applicable = NA
    #     - Unreliable = 0.888
    #     - Suppressed = 0.999
    
    num_var <- c("Crude Rate", "Age Adjusted Rate", "Count", "Population")
    
    for(j in 1:length(num_var)){
      correct <- subset[, num_var]
      
      correct[, j][correct[, j] == "Unreliable"] <- "0.888"
      correct[, j][correct[, j] == "Suppressed"] <- "0.999"
      correct[, j][correct[, j] == "Not Applicable"] <- NA
      
      subset[, num_var] <- correct
    }
    
    subset[, c("Crude Rate", "Age Adjusted Rate", "Count", "Population")] <- 
      sapply(subset[, c("Crude Rate", "Age Adjusted Rate", "Count", "Population")], as.numeric)
    
    subset[, c("Date")] <- as.character(subset[, c("Date")])
    
    
    # -----------------------------
    # Reorder the columns to be the same.
    subset <- subset %>% select(Notes, State, Date, Setting, `Manner_of_Death`, 
                                Drug, Characteristic, Level, Count, Population, 
                                `Crude Rate`, `Age Adjusted Rate`)
    
    # -----------------------------
    # Save the results
    df[[i]] <- subset
  }
  
  df
}


# Individual example
formatted <- standardize_format(saved_sets)

result = do.call(bind_rows, formatted)




## ----------------------------------------------------------------
## COMPILE ALL FOLDERS

subdir <- "Opioid OD Data/Raw Download/CDC Wonder TXT Output"

compile_sets <- function(starting_directory, export_parts_month = TRUE){
  # 
  # 
  # "export_parts_month =TRUE" prompts the algorithm to test if exports separated 
  # over different date ranges span the time frame to the month. Otherwise 
  # the accuracy is tested to the year of data entry.
  
  # The first set of directories associated with the data pull are regional;
  # either National or State.
  regional_dir <- sapply(list.dirs(starting_directory, recursive = FALSE), function(x) str_split(x, "/")[[1]] %>% 
                           tail(., n = 1)) %>% `names<-`(NULL)
  
  result <- list()
  qc     <- list()
  # Loop over the regional-level directories of stratified data.
  for(i in 1:length(regional_dir)){
    # The second set of directories associated with the data pull are the
    # requested stratification levels: Age, Sex, Race, and Not Stratified.
    # NOTE: race was stratified twice, once for non-Hispanic Race 6 classifications
    #       only and a second time for Hispanic origin only.
    stratification_dir <- sapply(list.dirs(file.path(subdir, regional_dir[i]), recursive = FALSE), function(x) str_split(x, "/")[[1]] %>% 
                                   tail(., n = 1)) %>% `names<-`(NULL)

    # Loop over the different kinds of stratified for that regional-level of data.
    result_strat <- list()
    qc_strat     <- data.frame("Query_Settings" = c("NA"), "Split_by_Date" = c("NA"),
                               "Num_Rows" = c(999))
    for(j in 1:length(stratification_dir)){
      # -----------------------------
      # Extract the files contained within this regional and stratification
      # sub directory.
      
      # List the files contained within one specific set of directories.
      file_dir   = file.path(getwd(), subdir, regional_dir[i], stratification_dir[j], "*.txt")
      
      # Extract the file name and strip the full file path off.
      file_names = sapply(Sys.glob(file_dir), function(x) str_split(x, "/")[[1]] %>% 
                            tail(., n = 1)) %>% `names<-`(NULL)
      
      # Read in those files as a list.
      saved_sets <- lapply(Sys.glob(file_dir), function(x) read.delim(x)) %>%
        `names<-`(file_names)
      
      
      # -----------------------------
      # QUALITY CONTROL: confirm that the file names match the export notes
      #                  generated by the CDC WONDER tool.
      qc_test <- c()
      
      # Parse the test based on the regional-level of stratification.
      if(regional_dir[i] == "National"){
        qc_test <- confirm_notes(saved_sets, state_level = FALSE, summarize_results = FALSE)
        qc_test_summary <- confirm_notes(saved_sets, state_level = FALSE, summarize_results = TRUE)
        
      } else if(regional_dir[i] == "State"){
        qc_test <- confirm_notes(saved_sets, state_level = TRUE, summarize_results = FALSE)
        qc_test_summary <- confirm_notes(saved_sets, state_level = TRUE, summarize_results = TRUE)
        
      }
      
      if(is.logical(qc_test) == TRUE){
        
        # Issue a warning if the error detected by the QC test is recognized
        # to be a match failure (all Boolean results).
        if(all(qc_test) != TRUE){
          warning(str_glue("QC test detected mismatch with file name and output notes. File directory: {file.path(regional_dir[i], stratification_dir[j])}."))
          
        # If no error was found then format and compile the files in this
        # sub directory.
        } else if(all(qc_test) == TRUE){
          
          # -----------------------------
          # Confirm exports over different date ranges are matched and not overlapping.
          # 
          # During the data acquisition process, some file exports were too large
          # for the web tool to export. In these scenarios, the stratification
          # settings were maintained to avoid confusion, but two files
          # were downloaded spanning half of the available dates; labeled "pt 1"
          # or "pt 2". Only at most two parts were needed to successfully
          # query the dataset.
          #
          # It is important to confirm that a pair is present for that stratification
          # and that the dates are mutually exclusive while covering the full
          # available span of time.
          
          subset_parts <- saved_sets[str_detect(names(saved_sets), "pt")]
          
          if(length(subset_parts) != 0){
            qc_parts <- c()
            
            # STEP 1: "pt 1" needs a "pt 2" pair.
            
            # Extract the file name components that denote the stratification.
            file_name_components <- lapply(names(subset_parts), function(x){ 
              # Remove the first two levels of information: dataset source and
              # the dataset type.
              x %>% str_replace("^.*?_", "") %>% str_replace("^.*?_", "") %>% 
                # Remove the last level of information indicating the date the
                # file was downloaded and remove the " pt (1|2)" notation.
                str_replace("_[^_]+$", "") %>% str_replace(" pt (1|2)", "")
            }) %>% unlist()
            
            # Remove the special character "+" that interferes with string matching.
            file_name_components <- file_name_components %>% 
              str_replace_all( " \\+ ", " ")
            
            
            # Pairs come in two; issue a warning if pairings are not even.
            if(length(file_name_components) %% 2 != 0){
              warning(str_glue("Dates divided into parts 1 and 2 are missing a pair. File directory: {file.path(regional_dir[i], stratification_dir[j])}."))
              
            } else if(length(file_name_components) %% 2 == 0){
              
              # While loop to check for unique pairs. Stop when the expected number
              # of pairs is reached or the entire search space has been searched. 
              file_pairs <- c()
              match      <- 1     # Number of unique pairs found.
              index      <- 1     # Search index.
              while(match < length(file_name_components)/2 + 1 && index < length(file_name_components) + 1){
                # Candidate pairing found.
                pair_result <- str_which(file_name_components, str_replace_all(file_name_components[index], " \\+ ", " "))
                
                # Pairs need to be bijective; matches need to be with only one other
                # index. Issue a warning if this is not the case.
                if(length(pair_result) != 2){
                  warning(str_glue("Dates divided into parts 1 and 2 have an even number of files but is missing or has too many matches. File directory: {file.path(regional_dir[i], stratification_dir[j])}. File: {file_name_components[index]}."))
                  
                  # Cause the process to terminate.
                  index <- length(file_name_components) + 1
                  
                } else if(length(pair_result) == 2){
                  # If this is a new pair match, then "file" the indices for later.
                  if(all(str_flatten(pair_result, collapse = "-") %!in% file_pairs)){
                    
                    # Both possible orientations of the indices are equivalent.
                    # Save both for now to ensure these are not counted as unique.
                    file_pairs <- c(file_pairs,
                                    str_flatten(c(pair_result[1], pair_result[2]), collapse = "-"),
                                    str_flatten(c(pair_result[2], pair_result[1]), collapse = "-"))
                    
                    match <- match + 1
                    index <- index + 1
                    
                    # If this is not a new pair match, then continue the search.
                  } else if(any(str_flatten(pair_result, collapse = "-") %in% file_pairs)){
                    index <- index + 1
                    
                  }
                }
              }
              
              # If the previous while loop ended because the index reached its
              # max and not because the expected number of matches were found,
              # then issue a warning.
              if(index == length(file_name_components) + 1){
                warning(str_glue("Dates divided into parts 1 and 2 have an even number of files but missing matches. File directory: {file.path(regional_dir[i], stratification_dir[j])}."))
                
              }
              
              # Final result pulls only the unique matches, represented in either
              # the odd OR even indices. Pull only the odd indices.
              found_pairs <- file_pairs[lapply(1:length(file_pairs), "%%", 2) != 0]
              
              # Confirm that each index is uniquely identified and all indices
              # have one pair match.
              qc_parts[1] <- found_pairs %>% 
                str_flatten(collapse = "-") %>% str_split_1("-") %>% 
                unique() %>% length() == length(file_name_components)
              
              # Confirm that the expected number of pairs have been found.
              qc_parts[2] <- length(found_pairs) == length(file_name_components)/2
              
              
              for(k in 1:length(found_pairs)){
                # STEP 2: "pt 1" and "pt 2" dates are mutually exclusive.
                
                # Subset the data frames that are presumably paired.
                subset_pairs_only <- as.numeric(str_split_1(found_pairs[k], "-")) %>% subset_parts[.]
                
                # Extract out the full dates. For "Month" stratified this will be
                # "YYYY/mm" format and for "Year" stratified this will be "YYYY" format.
                full_dates <- lapply(subset_pairs_only, function(x){
                  x[, str_detect(colnames(x), "(Month|Year).Code")] %>% .[. %!in% ""] %>% 
                    unique()
                })
                
                # Confirm that these sets have mutually exclusive dates.
                qc_parts[3] <- any(full_dates[[1]] %in% full_dates[[2]]) == FALSE & 
                  any(full_dates[[2]] %in% full_dates[[1]]) == FALSE
                
                
                # STEP 3: "pt 1" and "pt 2" together span the available range of dates.
                # NOTE: Step 3 is limited insofar as it only confirms the range of
                #       dates at the year-level and not the month-level. Not all months
                #       are expected to be available for each export
                
                # Extract out the exported date range expected. Found in the CDC 
                # WONDER data query tool's notes section.
                export_dates <- lapply(subset_pairs_only, function(x){
                  notes <- x[x$Notes %!in% "", "Notes"]
                  str_extract(notes[str_detect(notes, "Dataset: ")], "[0-9]{4}-[0-9]{4}") %>% 
                    str_split(pattern = "-") %>% .[[1]]
                })
                
                # for loop to confirm matches over the whole range.
                for(r in 1:2){
                  # Prompts to confirm the span of dates at the year level of accuracy.
                  if(export_parts_month == FALSE){
                    # If the data was stratified by "Month", then we need to
                    # extract out only the unique years represented.
                    if(all(str_detect(names(full_dates), "_Year_")) != TRUE){
                      full_dates <- lapply(full_dates, function(x){
                        str_split(x, "/") %>% do.call(rbind, .) %>% .[, 1] %>% unique()
                      })
                    }
                    
                    exported_range <- unlist(full_dates) %>% `names<-`(NULL) %>% as.integer()
                    expected_range <- export_dates[[r]][1]:export_dates[[r]][2]
                    
                    # Confirm that the full expected range of dates is represented,
                    # and that the max and min dates correspond. This does not
                    # detect replicated dates.
                    qc_parts[4] <- all(exported_range %in% expected_range) &
                      all(expected_range %in% exported_range)
                    
                    # Prompts to confirm the span of dates at the month level of accuracy.
                  } else if(export_parts_month == TRUE){
                    
                    exported_range <- unlist(full_dates) %>% `names<-`(NULL)
                    # Generate the full range of months expected from the years.
                    expected_range <- lapply(export_dates[[r]][1]:export_dates[[r]][2], function(x){ 
                      str_c(x, "/0", 1:9) %>% c(str_c(x, "/", 10:12))
                    }) %>% unlist()
                    
                    # Confirm that the full expected range of dates is represented,
                    # and that the max and min dates correspond. This does not
                    # detect replicated dates.
                    qc_parts[4] <- all(exported_range %in% expected_range) &
                      all(expected_range %in% exported_range)
                    
                  }
                }
                
              }
              
            }
            
            # Issue a warning if the error detected by the QC test is recognized
            # to be a match failure (all Boolean results).
            if(all(qc_parts) != TRUE){
              warning(str_glue("QC test detected problems with the stratified exports that had parts divided by dates. File directory: {file.path(regional_dir[i], stratification_dir[j])}."))
              
            }
          } else if(length(subset_parts) == 0){
            qc_parts <- NA
            
          }
          
          
          # -----------------------------
          # Format and zip all the files together.
          
          formatted <- standardize_format(saved_sets)
          
          # Combine all of the disparate datasets.
          result_strat[[j]] <- do.call(bind_rows, formatted)
          
          # Save the salient details of the QC tests.
          qc_test_result <- str_c("`All passed`: ", sum(qc_test_summary == "All passed."),
                                  " of ", length(qc_test_summary))
          
          rows_strat = lapply(saved_sets, function(x) x[1:(str_which(x$Notes, "---")[1]-1), ]) %>% 
            lapply(., nrow) %>% unlist() %>% sum()
          
          qc_strat <- rbind(qc_strat, c(qc_test_result, all(qc_parts), rows_strat))
          
        }
      
      # Issue a warning if the error detected by the QC test is recognized
      # to be an unknown error with one of the sub tests resulting in a statement.
      } else if(is.character(qc_test) == TRUE){
      
      # Issue a warning if the error detected by the QC test is not recognized.
      } else if(length(qc_test) == 0){
        warning(str_glue("QC test filed for unknown error. File directory: {file.path(regional_dir[i], stratification_dir[j])}."))
      }
      
    }
    
    result[[i]] <- do.call(bind_rows, result_strat)
    qc[[i]]     <- qc_strat[-1, ] %>% cbind("Region" = rep(regional_dir[i], nrow(.)), 
                                            "Stratification" = stratification_dir, .) %>% `rownames<-`(NULL)
    
  }
  
  final_df <- list(do.call(bind_rows, result), do.call(bind_rows, qc))
  final_df
}




## ----------------------------------------------------------------
## CREATE THE COMPILED DATASET

df_result <- compile_sets(subdir, export_parts_month = TRUE)

# Confirm that no datasets or rows went missing or were double counted.
df_result[[1]] %>% nrow() == df_result[[2]]$Num_Rows %>% as.integer() %>% sum()

# Do all datasets have a different number of observations?
df_result[[2]]$Num_Rows %>% as.integer() %>% unique() %>% length() == nrow(df_result[[2]])


# Save whole result. NOTE: This generates a large file that cannot be shared
# over GitHub. The file will need to be processed to attempt reducing the file
# size down.
#write.csv(df_result[[1]], "CDC WONDER Raw Compiled_01.16.2024.csv", row.names=FALSE)




## ----------------------------------------------------------------
## CLEAN AND TRANSFORM

df <- df_result[[1]]
original_rows <- nrow(df)


# -----------------------------
# Correct missing or irrelevant entries.

# The easiest variables to inspect for this are "Date", "Notes", and "State".
# Starting with the "Date", we see that there are unexpected rows with NA or
# no value.
df$Date %>% unique()

# Remove rows where there is no date entry.
df <- df[-which(str_length(df$Date) == 0), ] %>% `rownames<-`(NULL)

# Remove rows where the date is NA.
df <- df[df$Date %!in% NA, ] %>% `rownames<-`(NULL)


# Now we'll move onto "State".
df$State %>% unique()

# We see that all of the entries with no "State" entry are a "Notes = Total"
# row. These are not entries that hold any bearing on our analysis and so
# we'll remove them.
df[str_length(df$State) == 0, "Notes"] %>% unique()

# Remove rows where there is no entry in the "State" column.
df <- df[str_length(df$State) != 0, ] %>% `rownames<-`(NULL)


# It is expected that entries where "Notes" is blank are correct. As for the
# "Notes = Total" rows, these were typically generated by the CDC WONDER export
# tool that calculated total with that stratification. To ensure the "Not Stratified"
# rows were correct, as they required separation by region, dates, drugs, and
# death setting, those were done individually. Therefore, the "Notes = Total"
# entries are either a duplicate or irrelevant total count over the stratification
# by race, sex, or age. They will be all removed.
df$Notes %>% unique()

# Remove rows where "Notes = Total".
df <- df[df$Notes %!in% "Total", ] %>% `rownames<-`(NULL)

# Remove the "Notes" column.
df <- df[, colnames(df) %!in% "Notes"]


# We expect "Characteristic" to label the gross category for "Level", but not
# be unique from it. To confirm these we check that only only "Characteristic"
# is associated with the correct "Level".
table(df$Characteristic, df$Level)

# "Characteristic == Not Stated" fails this test. This was a variable that appears
# to have resulted from stratification by Age and Race/Ethnicity. Since they
# do not hold bearing on the analysis at hand, these entries will be removed.

df <- df[df$Level %!in% "Not Stated", ]


# Number of rows removed.
original_rows - nrow(df)




# -----------------------------
# Aggregate months to quarterly dates.

# Pull the months only for aggregation.
df_months <- df[str_detect(df$Date, "[0-9]{4}/[0-9]{2}"), ]

# Format the dates.
df_months$Date <- ym(df_months$Date) #%>% format.Date(., format = "%Y-%m")


# No unique "Population" or rates (crude or age adjusted) need to be considered. 
# A simple aggregation over the "Counts" is sufficient.
df_months$Population %>% unique()
df_months$`Crude Rate` %>% unique()
df_months$`Age Adjusted Rate` %>% unique()

# Recall that some entries in the above three columns of variables and "Count"
# were labeled with coded numerical values.
#     - Not Applicable or Available = NA
#     - Unreliable = 0.888
#     - Suppressed = 0.999

# We want to ensure that aggregating by unique row entries to generate counts
# by quarter do not sum these placeholder values. Additionally, we want to
# ensure that only quarters with a complete representation of the entries is
# reflected.

# Check if any placeholder values are present in the months-only data subset.
df_months[df_months$Count %in% 0.999, ] %>% nrow()
df_months[df_months$Count %in% 0.888, ] %>% nrow()
df_months[df_months$Count %in% NA, ] %>% nrow()

# There are some placeholder values for "Suppressed" counts due to low
# incidence rates. Further, we see that all legitimate counts are integers, 
# as expected.
df_months[df_months$Count < 1 & df_months$Count != 0, ] %>% nrow()

# Now we'll check the row dimensions present in the dataset with the number
# we'd expect if all dates are represented. Notice that "Characteristic"
# does not differentiate rows, it only categorizes the "Level" entry by the
# gross classification for that stratification. Therefore, only "Level" is used.
entries_for_aggregation <- sapply(df_months[, c("State", "Setting", "Manner_of_Death", "Drug", "Level")], function(x) unique(x) %>% length())
expected_unique_entries <- prod(entries_for_aggregation) * length(unique(df_months$Date))

# We see that about 1/4 of the expected unique entries are present in this dataset.
# This validates the suspicion that there has been incomplete reporting, necessitating
# granular quarterly aggregation.
nrow(df_months) / expected_unique_entries

# Generate a table for all of the possible combinations. Notice that "Characteristic"
# does not add new possible combinations with "Level" present, and so it is not
# included.
entries_to_quarter <- table(df_months$State, df_months$Setting, df_months$Manner_of_Death, df_months$Drug, df_months$Level) %>% 
  as.data.frame() %>% `colnames<-`(c("State", "Setting", "Manner_of_Death", "Drug", "Level", "Freq"))

# Most possible combinations are not even represented once. We will remove these 
# possible combinations to reduce the search space to only those that are expected.
entries_to_quarter %>% nrow()
entries_to_quarter %>% nrow() - entries_to_quarter[entries_to_quarter$Freq == 0, ] %>% nrow()

entries_to_quarter <- entries_to_quarter[entries_to_quarter$Freq != 0, ] %>% `rownames<-`(NULL)

# Confirm that the number of times a combination shows up does not exceed
# the maximum possible range of dates.
entries_to_quarter$Freq %>% max() == length(unique(df_months$Date))



# Label which months are associated with which quarter.
target_quarters <- list("Q1" = 1:3, "Q2" = 4:6, "Q3" = 7:9, "Q4" = 10:12)

# The following function takes a few minutes. This progress bar has been
# added to show where the function is in the for loop.
pb = txtProgressBar(min = 0, max = nrow(entries_to_quarter), initial = 0)

final <- list()
#for(i in 1:nrow(entries_to_quarter)) {
  # Extract the current sub-stratification.
  combination = entries_to_quarter[i, ] %>% droplevels()
  
  # Subset the dataset based on this individual stratification.
  subset_df <- df_months[df_months$State %in% combination$State &
                 df_months$Setting %in% combination$Setting &
                 df_months$Manner_of_Death %in% combination$Manner_of_Death &
                 df_months$Drug %in% combination$Drug &
                 df_months$Level %in% combination$Level, ]
  
  
  # Conditionally prepare the dataset. If no values were available at that
  # stratification then issue a warning.
  if(length(subset_df$Date) == 0) {
    warning(str_glue("No data present. Stratification: {combination}."))
    
  } else if(length(subset_df$Date) != 0){
    make_quarters <- data.frame("Date"  = subset_df$Date,
                                "Year"  = subset_df$Date %>% year(),
                                "Month" = subset_df$Date %>% month(),
                                "Count" = subset_df$Count)
    
  }
  
  # -----------------------------
  # Prepare the dataset with conditions for labeling incomplete information,
  # suppressed counts, and unreliable counts.
  
  # Remove entries where "Count = NA". Keep the other numeric codings for
  # interpretation later.
  make_quarters <- make_quarters[make_quarters$Count %!in% NA, ] %>% `rownames<-`(NULL)
  
  # Available years of entry.
  avail_years <- unique(make_quarters$Year)
  
  quartered <- list()
  for(j in 1:length(avail_years)) {
    # Extract one year for the given sub-stratification.
    quarter_each_year <- make_quarters[make_quarters$Year == avail_years[j], ] %>% `rownames<-`(NULL)
    
    # All placeholder values will not be integers. To ensure these are caught,
    # the remaining integers need to be subtracted from the quarterly sum.
    quarter_each_year$Comparison <- quarter_each_year$Count %>% as.integer()
    
    # Aggregate subset by one year partition to quarters.
    df_quarter <- aggregate(Count ~ Year + Quarter, transform(quarter_each_year, Quarter = quarters(Date)), sum)
    df_quarter_comparison <- aggregate(Comparison ~ Year + Quarter, transform(quarter_each_year, Quarter = quarters(Date)), sum)
    
    # Generate the "Placeholder" column for comparison.
    df_quarter <- df_quarter %>% mutate("Placeholder" = round(df_quarter$Count - df_quarter_comparison$Comparison, digits = 3))
    
    # -----------------------------
    # Check if all expected months are present.
    dates_present  <- lapply(target_quarters, function(x) all(x %in% quarter_each_year$Month)) %>% unlist()
    # Check if any of the expected months are absent.
    dates_absent   <- lapply(target_quarters, function(x) all(x %in% quarter_each_year$Month == FALSE)) %>% unlist()
    
    # Check if all of the values within a quarter were numerically coded as
    # a suppressed or unreliable count.
    dates_suppressed  <- (df_quarter$Placeholder != 0 & df_quarter$Placeholder %% 0.999 %% 0.888 == 0 & df_quarter$Placeholder %% 0.999 == 0) %>% `names<-`(c(df_quarter$Quarter))
    dates_unreliable  <- (df_quarter$Placeholder != 0 & df_quarter$Placeholder %% 0.888 %% 0.999 == 0 & df_quarter$Placeholder %% 0.888 == 0) %>% `names<-`(c(df_quarter$Quarter))
    # Some dates might have some suppressed, unreliable, or a mix of the two 
    # coded counts. These will be generally labeled as incomplete, coded by the 
    # numeric placeholder 0.777.
    dates_incomplete  <- (df_quarter$Placeholder != 0 & 
      (df_quarter$Placeholder %% 0.888 %% 0.999 == 0 & df_quarter$Placeholder %% 0.999 != 0) | 
      (df_quarter$Placeholder %% 0.999 %% 0.888 == 0 & df_quarter$Placeholder %% 0.888 != 0) ) %>% `names<-`(c(df_quarter$Quarter))
    
    # Combine these test results into one reference table.
    results <- bind_rows(dates_present, dates_absent, dates_suppressed, dates_unreliable, dates_incomplete) %>% 
      as.data.frame() %>% `rownames<-`(c("Present", "Absent", "Suppressed", "Unreliable", "Incomplete"))
    
    
    # -----------------------------
    # Quarter values calculated with incomplete information (missing months or
    # counts labeled as suppressed or unreliable) will need to be relabeled with 
    # a numeric placeholder: 0.777.
    if(all(dates_present) == FALSE | any(dates_incomplete)) {
      # Values neither present nor absent had incomplete representation of that
      # quarter's months. Also include values that have some months labeled as
      # suppressed or unreliable.
      incomplete <- ((dates_present | dates_absent) == FALSE) | results["Incomplete", ] %>% `rownames<-`(NULL)
      
      # Change the count to a placeholder numeric.
      df_quarter[df_quarter$Quarter %in% names(target_quarters)[incomplete], "Count"] <- 0.777
      
    }
    
    # If all of the months within a quarter are supressed values, then report
    # them as such.
    all_suppressed <- df_quarter$Placeholder / 3 == 0.999
    
    if(any(all_suppressed) == TRUE) {
      # Change the count to a placeholder numeric.
      df_quarter[all_suppressed, "Count"] <- 0.999
      
    }
    
    
    # If all of the months within a quarter are absent values, then report
    # them as such.
    all_absent <- df_quarter$Placeholder / 3 == 0.888
    
    if(any(all_absent) == TRUE) {
      # Change the count to a placeholder numeric.
      df_quarter[all_absent, "Count"] <- 0.888
      
    }
    
    # Add back in missing quarters with "Counts = NA".
    if(any(dates_absent)) {
      # Expected order and representation of quarters.
      fill_in_missing <- data.frame("Quarter" = c("Q1", "Q2", "Q3", "Q4"))
      
      # Fill these in by merging and allowing missing values to be filled with NA.
      df_quarter <- merge(fill_in_missing, df_quarter, by = "Quarter", all = TRUE)
      # Fix the missing year.
      df_quarter[df_quarter$Year %in% NA, "Year"] <- avail_years[j]
      
    }
    
    # Column-merge the quartered results with the metadata. Add back in the
    # "Characteristic" column.
    quartered[[j]] <- merge(combination, df_quarter) %>% 
      mutate(Characteristic = unique(subset_df$Characteristic)) %>%
      select(State, Year, Quarter, Setting, Manner_of_Death, Drug, Characteristic, Level, Count)
    
  }
  
  # Print the for loop's progress.
  setTxtProgressBar(pb, i)
  
  # Combine all results over the available years for a given stratification.
  final[[i]] <- do.call(rbind, quartered)
}

# Compile all quartered stratifications to the final data frame.
df_quarters <- do.call(rbind, final) %>% `rownames<-`(NULL)




# -----------------------------
# Combine the by-month dataset summarized by quarters and annual values dataset.

# Add the two missing columns, which were earlier shown to be all NA's.
df_quarters <- df_quarters %>% 
  mutate(Population = NA, `Crude Rate` = NA, `Age Adjusted Rate` = NA)

# Correct the class for each variable.
df_quarters[, c("State", "Quarter", "Setting", "Manner_of_Death", "Drug", "Characteristic", "Level")] <- sapply(df_quarters[, c("State", "Quarter", "Setting", "Manner_of_Death", "Drug", "Characteristic", "Level")], as.character)
df_quarters[, c("Count", "Crude Rate", "Age Adjusted Rate")] <- sapply(df_quarters[, c("Count", "Crude Rate", "Age Adjusted Rate")], as.numeric)
df_quarters[, c("Year", "Population")] <- sapply(df_quarters[, c("Year", "Population")], as.integer)


# Extract out the annual values, add the missing column, and format.
df_years <- df[!str_detect(df$Date, "[0-9]{4}/[0-9]{2}"), ] %>% 
  mutate(Quarter = NA) %>% rename(Year = Date) %>%
  select(colnames(df_quarters)) %>% `rownames<-`(NULL)

# Correct the class for each variable.
df_years[, c("State", "Quarter", "Setting", "Manner_of_Death", "Drug", "Characteristic", "Level")] <- sapply(df_years[, c("State", "Quarter", "Setting", "Manner_of_Death", "Drug", "Characteristic", "Level")], as.character)
df_years[, c("Count", "Crude Rate", "Age Adjusted Rate")] <- sapply(df_years[, c("Count", "Crude Rate", "Age Adjusted Rate")], as.numeric)
df_years[, c("Year", "Population")] <- sapply(df_years[, c("Year", "Population")], as.integer)


# Recombine to attain the final resulting table.
df_final <- bind_rows(df_years, df_quarters)




# -----------------------------
# Aggregate ages to groups that match the HCUP formatting.

# The HCUP dataset groups ages into the following groups: "<1 Year", "1-24 Years",
# "25-44 Years", "45-64 Years", and "65+ Years".

# Confirm that when "Population = NA" all rates also equal NA.
df_final[df_final$Population %in% NA, c("Crude Rate", "Age Adjusted Rate")] %>% sapply(., function(x) all(x %in% NA))

# Confirm that "Population != NA" all rates are not NA.
df_final[df_final$Population %!in% NA, c("Crude Rate", "Age Adjusted Rate")] %>% sapply(., function(x) any(x %in% NA))

# We see that this is the case for the "Crude Rate" but not "Age Adjusted Rate".
# This is likely because counts stratified by age are not age-adjusted. Then
# we need to check "Age Adjusted Rate = NA" only when "Population != NA" and
# "Characteristic = Age".
df_final[df_final$Population %!in% NA & df_final$Characteristic %!in% "Age", c("Crude Rate", "Age Adjusted Rate")] %>% sapply(., function(x) any(x %in% NA))


# These tests are as expected. Therefore, we only need to worry about combining
# the "Counts" correctly and can leave the "Age-Adjusted Rates" as NA. For the
# "Crude Rate", we only need to reproduce the rate when "Population != NA".

df_final[df_final$Quarter %!in% NA, "Population"] %>% unique()

# We see that the only unique "Count" outcomes that are not integers are
# 0, 0.999, 0.777, and NA. Therefore, the algorithm will only account for the
# placeholder numerics 0.999 and 0.777.
df_final[df_final$Count < 1, "Count"] %>% unique()


# For each HCUP group, sum the following age groupings:
#     "<1 Year":  "< 1 year"
#  "1-24 Years":  "1-4 years", "5-14 years", "15-24 years"
# "25-44 Years":  "25-34 years", "35-44 years"
# "45-64 Years":  "45-54 years", "55-64 years"
#   "65+ Years":  "65-74 years", "75-84 years", "85+ years"

# Subset the dataset to only reflect the entries for "Age"
df_age <- df_final[df_final$Characteristic %in% "Age", ] %>% `rownames<-`(NULL)

# Based on the HCUP dataset, we'll want to group prospective age ranges so
# the algorithm knows what gets batched together.
df_age$Group <- NA

# SUDORS includes incidences in infants with the < 15 years of age group.
# Therefore, the < 1 year age group will also be added there as well to improve
# cross source comparison.
df_age[df_age$Level %in% c("< 1 year"), "Group"] <- 2
df_age[df_age$Level %in% c("1-4 years", "5-14 years", "15-24 years"), "Group"] <- 2
df_age[df_age$Level %in% c("25-34 years", "35-44 years"), "Group"] <- 3
df_age[df_age$Level %in% c("45-54 years", "55-64 years"), "Group"] <- 4
df_age[df_age$Level %in% c("65-74 years", "75-84 years", "85+ years"), "Group"] <- 5


# Generate a table for all of the possible combinations. Instead of using
# "Characteristic" or "Level" the temporary vector "Group" is used. This allows
# subsetting by the desired age groups for aggregation.
entries_to_group <- table(df_age$State, df_age$Year, df_age$Quarter, df_age$Setting, df_age$Manner_of_Death, df_age$Drug, df_age$Group, useNA = "ifany") %>% 
  as.data.frame() %>% `colnames<-`(c("State", "Year", "Quarter", "Setting", "Manner_of_Death", "Drug", "Group", "Freq"))

# Most possible combinations are not even represented once. We will remove these 
# possible combinations to reduce the search space to only those that are expected.
entries_to_group <- entries_to_group[entries_to_group$Freq != 0, ] %>% `rownames<-`(NULL)

# Confirm that the number of times a combination shows up does not exceed
# the maximum number of age groups that are being aggregated together (3).
entries_to_group$Freq %>% max() == 4


# Now we need some reference tables for the algorithm:
# Label which months are associated with which group.
target_groups <- list("2" = c("< 1 year", "1-4 years", "5-14 years", "15-24 years"), 
                      "3" = c("25-34 years", "35-44 years"), 
                      "4" = c("45-54 years", "55-64 years"), 
                      "5" = c("65-74 years", "75-84 years", "85+ years"))

# Denote the new label to be applied to that group.
new_age_groups <- list("2" = c("<24 Years"), "3" = c("25-44 Years"), 
                       "4" = c("45-64 Years"), "5" = c("65+ Years"))


# The following function takes a few hours. The progress bar has been
# added to show where the function is in the for loop.
pb = txtProgressBar(min = 0, max = nrow(entries_to_group), initial = 0)

final <- list()
#for(i in 1:nrow(entries_to_group)) {
  # Extract the current sub-stratification.
  combination = entries_to_group[i, ] %>% droplevels()
  
  # Subset the dataset based on this individual stratification.
  subset_df <- df_age[df_age$State %in% combination$State &
                        df_age$Year %in% combination$Year &
                        df_age$Quarter %in% combination$Quarter &
                        df_age$Setting %in% combination$Setting &
                        df_age$Manner_of_Death %in% combination$Manner_of_Death &
                        df_age$Drug %in% combination$Drug &
                        df_age$Group %in% combination$Group, ]

  
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
    
    # Check if all of the values within a quarter were numerically coded as
    # a suppressed or incomplete count.
    dates_suppressed  <- (subset_df$Count != 0 & subset_df$Count %% 0.999 %% 0.777 == 0 & subset_df$Count %% 0.999 == 0) %>% `names<-`(c(subset_df$Level))
    dates_incomplete  <- (subset_df$Count != 0 & subset_df$Count %% 0.777 %% 0.999 == 0 & subset_df$Count %% 0.777 == 0) %>% `names<-`(c(subset_df$Level))
    # Some dates might have some suppressed, incomplete, or a mix of the two 
    # coded counts. These will be generally labeled as incomplete, coded by the 
    # numeric placeholder 0.777.
    dates_mixed  <- (subset_df$Count != 0 & 
                       (subset_df$Count %% 0.777 %% 0.999 == 0 & subset_df$Count %% 0.999 != 0) | 
                       (subset_df$Count %% 0.999 %% 0.777 == 0 & subset_df$Count %% 0.777 != 0) ) %>% `names<-`(c(subset_df$Level))
    
    # Combine these test results into one reference table.
    results <- bind_rows(dates_present, dates_suppressed, dates_incomplete, dates_mixed) %>% 
      as.data.frame() %>% `rownames<-`(c("Present", "Suppressed", "Incomplete", "Mixed"))
    
    
    # Counts calculated with incomplete information (missing levels or counts 
    # labeled as suppressed or unreliable) will need to be relabeled with a 
    # numeric placeholder: 0.777.
    if(all(dates_present) == FALSE | any(dates_mixed) | all(dates_incomplete) == TRUE) {
      # Change the count to a placeholder numeric.
      grouped_counts <- 0.777
      
    }
    
    if(all(dates_suppressed) == TRUE) {
      # Change the count to a placeholder numeric.
      grouped_counts <- 0.999
      
    }
    
  }
    
  # Print the for loop's progress.
  setTxtProgressBar(pb, i)
  
  # Column-merge the quartered results with the metadata. Add back in the
  # "Characteristic", "Level", and "Population" columns.
  final[[i]] <- merge(combination, grouped_counts) %>%
    rename(Count = y) %>%
    mutate(Characteristic = "Age", Level = target, Population = sum(subset_df$Population)) %>%
    select(State, Year, Quarter, Setting, Manner_of_Death, Drug, Characteristic, Level, Count, Population)
    
}

# Compile all quartered stratifications to the final data frame.
df_grouped <- do.call(rbind, final) %>% `rownames<-`(NULL)

# Because the for loop takes so long, save the dataset in an extra, unused variable.
#extra_var <- df_grouped


# Confirm that the only non-integer values are the expected coded numerics: 0.777 and 0.999.
df_grouped[df_grouped$Count < 1, "Count"] %>% unique()


# Calculate the "Crude Rate".
for_rate <- df_grouped[, c("Count", "Population")]

new_rate <- c()
for(i in 1:nrow(for_rate)){
  if( is.na(for_rate[i, 1]) & is.na(for_rate[i, 2]) ) {
    new_rate[i]  <- NA
    
  }  else if( for_rate[i, 1] %in% 0.999 ) {
    # NOTE: To avoid the chances (while very small) that a rate will equal 9999
    #       the 0.999 placeholder is used instead to quality check that "Crude
    #       Rate = 9999" only when information is incomplete.
    new_rate[i] <- 0.999
    
  } else if( for_rate[i, 1] %in% 0.777 | (is.na(for_rate[i, 1]) & is.numeric(for_rate[i, 2])) | (is.numeric(for_rate[i, 1]) & is.na(for_rate[i, 2])) ) {
    new_rate[i] <- 0.777
    
  } else if( is.numeric(for_rate[i, 1]) & is.numeric(for_rate[i, 2]) ) {
    new_rate[i] <- round((for_rate[i, 1] * 100000) / for_rate[i, 2], digits = 1)
    
  }
}

# Add these rate columns to the age-grouped dataset.
df_grouped <- df_grouped %>% mutate("Crude Rate" = new_rate, "Age Adjusted Rate" = NA)

# Correct the class for each variable.
df_grouped[, c("State", "Year", "Quarter", "Setting", "Manner_of_Death", "Drug", "Characteristic", "Level")] <- sapply(df_grouped[, c("State", "Year", "Quarter", "Setting", "Manner_of_Death", "Drug", "Characteristic", "Level")], as.character)
df_grouped[, c("Count", "Crude Rate", "Age Adjusted Rate")] <- sapply(df_grouped[, c("Count", "Crude Rate", "Age Adjusted Rate")], as.numeric)
df_grouped[, c("Year", "Population")] <- sapply(df_grouped[, c("Year", "Population")], as.integer)

# Confirm what numeric placeholders are present.
df_grouped[df_grouped$Count < 1 & df_grouped$Count %!in% 0, "Count"] %>% unique()
df_grouped[df_grouped$Population < 1 & df_grouped$Population %!in% 0, "Population"] %>% unique()
df_grouped[df_grouped$`Crude Rate` < 1 & df_grouped$`Crude Rate` %!in% 0, "Crude Rate"] %>% unique()



# Subset the dataset the excludes "Characteristic = Age".
df_no_age <- df_final[df_final$Characteristic %!in% "Age", ] %>% `rownames<-`(NULL)

# Confirm what numeric placeholders are present.
df_no_age[df_no_age$Count < 1 & df_no_age$Count %!in% 0, "Count"] %>% unique()
df_no_age[df_no_age$Population < 1 & df_no_age$Population %!in% 0, "Population"] %>% unique()
df_no_age[df_no_age$`Crude Rate` < 1 & df_no_age$`Crude Rate` %!in% 0, "Crude Rate"] %>% unique()

# Correct the class for each variable.
df_no_age[, c("State", "Year", "Quarter", "Setting", "Manner_of_Death", "Drug", "Characteristic", "Level")] <- sapply(df_no_age[, c("State", "Year", "Quarter", "Setting", "Manner_of_Death", "Drug", "Characteristic", "Level")], as.character)
df_no_age[, c("Count", "Crude Rate", "Age Adjusted Rate")] <- sapply(df_no_age[, c("Count", "Crude Rate", "Age Adjusted Rate")], as.numeric)
df_no_age[, c("Year", "Population")] <- sapply(df_no_age[, c("Year", "Population")], as.integer)



# Recombine to attain the final resulting table.
df_total <- bind_rows(df_grouped, df_no_age)

# We'd like to recode the numerical placeholders to match the nomenclature used
# in the other datasets. First, confirm that there will be no existing counts
# that will erroneously be replaced.
df_total[df_total$Count %in% 7777, ] %>% nrow() == 0
df_total[df_total$Count %in% 9999, ] %>% nrow() == 0

df_total[df_total$`Crude Rate` %in% 7777, ] %>% nrow() == 0
df_total[df_total$`Crude Rate` %in% 8888, ] %>% nrow() == 0
df_total[df_total$`Crude Rate` %in% 9999, ] %>% nrow() == 0

# We're good to go replacing those values.
df_total[df_total$Count %in% 0.777, "Count"] <- 7777
df_total[df_total$Count %in% 0.999, "Count"] <- 9999

df_total[df_total$`Crude Rate` %in% 0.777, "Crude Rate"] <- 7777
df_total[df_total$`Crude Rate` %in% 0.888, "Crude Rate"] <- 8888
df_total[df_total$`Crude Rate` %in% 0.999, "Crude Rate"] <- 9999


# How much of the total raw dataset has been reduced to.
nrow(df_total)/nrow(df)






## ----------------------------------------------------------------
## SAVE THE CLEANED DATASET

# Save whole result. NOTE: This generates a large file that cannot be shared
# over GitHub. The file will need to be processed to attempt reducing the file
# size down.
write.csv(df_total, "CDC WONDER_Cleaned_01.21.2024.csv", row.names=FALSE)





