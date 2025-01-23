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


# bronchiolitis (infants only), rsv

# EPIC, NERVESS, and Google Trends not expected to have stratification except by state.








