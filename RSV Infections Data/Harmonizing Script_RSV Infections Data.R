## ----------------------------------------------------------------
## Harmonizing RSV Infections Data
##
## Date: December 23rd, 2024
## Source:
##    - RSV-NET from the CDC (connected with NREVSS data)

## ----------------------------------------------------------------
## SET UP THE ENVIRONMENT

library("readxl")
library("readr")
library("tidyr")
library("dplyr")
library("stringr")

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



# bronchiolitis (infants only), rsv

# EPIC, NERVESS, and Google Trends not expected to have stratification except by state.








