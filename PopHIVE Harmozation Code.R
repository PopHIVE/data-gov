## ----------------------------------------------------------------
## Save PopHIVE first draft harmonization code
##
## Date: February 28th, 2025
## Description: There were sparse bits of code which harmonized or
##              otherwise cleaned data. This is a place to save that
##              code for adaptation into the streamlined process.

## ----------------------------------------------------------------
## SET UP THE ENVIRONMENT

suppressPackageStartupMessages({
  #https://r-graph-gallery.com/183-choropleth-map-with-leaflet.html
  library(tidyverse)
  library(plotly)
  library(shiny)
  library(tidyverse)
  library(scales)
  library(janitor)
  library(MMWRweek)
  library(arrow)
  #library(rnaturalearth)
  library(parquetize)
  library(viridisLite)
  library(tigris)
  library(usmap)
  library(cowplot)
  library(leaflet)
  library(viridis)
  library(sf)
  library(ggrepel)
  library(readxl)
  library(ggalluvial)
  library(waffle)
  #library(cdcfluview)
  #install.packages("remotes")
  #remotes::install_github("hrbrmstr/cdcfluview")
  #hhs_regions$state <- state.abb[match(hhs_regions$state_or_territory,state.name)]
  #state.hhs.codes <- aggregate(hhs_regions$state, #list(hhs_regions$region), paste, collapse=",")
  #state.hhs.codes$x <- gsub('NA,','',state.hhs.codes$x)
  #state.hhs.codes$x <- gsub(',NA','',state.hhs.codes$x)
  #saveRDS(state.hhs.codes, './hhs_regions.rds')
})

"%!in%" <- function(x,y)!("%in%"(x,y))




## ----------------------------------------------------------------
## epic_age_import.R FUNCTION

epic_age_import <- function(ds_name, skipN=15) {
  
  ds_out <- readr::read_csv(paste0("./Data/CONFIDENTIAL/",ds_name), skip=skipN, col_names=F) %>%
    rename(geography=X1, age=X2) %>%
    tidyr::fill( geography, .direction = 'down') %>%
    reshape2::melt(., id.vars=c('geography','age')) %>%
    arrange(geography, age, variable) %>%
    group_by(geography, age) %>%
    #week END date
    mutate(date= seq.Date(from=as.Date('2019-01-12'), length.out=n() , by='week')) %>%
    ungroup() %>%
    rename(N_cases=value) %>%
    mutate( N_cases = if_else(N_cases=='10 or fewer',NA_character_, N_cases),
            N_cases = as.numeric(N_cases),
            geography= if_else(geography=='Total', 'United States', geography)) %>%
    dplyr::select(-variable)%>%
    mutate( Level = if_else(age %in% c('Less than 1 years', 'Less than 1 year'), '<1 Years',
                            if_else( age %in%  c('? 1 and < 5 years','1 year or more and less than 5 years'),'1-4 Years',
                                     if_else(age %in% c('? 5 and < 18 years','5 years or more and less than 18 years (1)') , "5-17 Years",
                                             if_else( age %in% c("? 18 and < 65 years",'18 years or more and less than 65 years') ,"18-64 Years" ,         
                                                      if_else( age %in% c("65 years or more","? 65 and < 110 years") , "65+ Years" , NA_character_        
                                                               
                                                      ) ))))) %>%
    dplyr::select(-age) %>%
    ungroup() %>%
    arrange( geography,Level, date) %>%
    group_by( geography,Level) %>%
    rename(N_cases_epic=N_cases) %>%
    filter(date>='2023-07-01') %>%
    mutate( N_cases_epic_3m=zoo::rollmean(N_cases_epic, k = 3, fill = NA, align='right'),
            max_grp= max(N_cases_epic_3m, na.rm=T),
            scale_age_epic = N_cases_epic_3m/max_grp*100
    ) %>%
    ungroup() %>%
    filter(!is.na(geography))
  
  return(ds_out)
}




## ----------------------------------------------------------------
## play2.qmd epic_data BLOCK

e1 <- readr::read_csv('./Data/CONFIDENTIAL/RSVICD10week_age_state.csv', skip=15, col_names=F) %>%
  rename(geography=X1, age=X2) %>%
  tidyr::fill( geography, .direction = 'down') %>%
  reshape2::melt(., id.vars=c('geography','age')) %>%
  arrange(geography, age, variable) %>%
  group_by(geography, age) %>%
  #week END date
  mutate(date= seq.Date(from=as.Date('2019-01-12'), length.out=n() , by='week')) %>%
  ungroup() %>%
  rename(N_cases=value) %>%
  mutate( N_cases = if_else(N_cases=='10 or fewer',NA_character_, N_cases),
          N_cases = as.numeric(N_cases),
          geography= if_else(geography=='Total', 'United States', geography)) %>%
  dplyr::select(-variable)

e1_all_ages <- e1 %>% filter(age=='Total') %>%
  rename(state=geography, N_epic=N_cases) 

e1_age <-  e1 %>% filter(age!='Total' & geography !='United States' & age != 'No value') %>%
  mutate( Level = if_else(age == 'Less than 1 years', '<1 Years',
                          if_else( age=='? 1 and < 5 years','1-4 Years',
                                   if_else(age=='? 5 and < 18 years' , "5-17 Years",
                                           if_else( age=="? 18 and < 50 years" ,"18-49 Years" ,         
                                                    if_else( age=="? 50 and < 65 years" , "50-64 Years" ,        
                                                             if_else( age=="? 65 and < 75 years" ,"65-74 Years",          
                                                                      if_else( age=="75 years or more" , "75+ Years", NA_character_         
                                                                      ) )))))
  )) %>%
  dplyr::select(-age) %>%
  ungroup() %>%
  arrange( geography,Level, date) %>%
  group_by( geography,Level) %>%
  rename(N_cases_epic=N_cases) %>%
  filter(date>='2023-07-01') %>%
  mutate( N_cases_epic_3m=zoo::rollmean(N_cases_epic, k = 3, fill = NA, align='right'),
          max_grp= max(N_cases_epic_3m, na.rm=T),
          scale_age_epic = N_cases_epic_3m/max_grp*100
  ) %>%
  ungroup()



#EPIC ED all cause
epic_ed_all <- epic_age_import(ds_name="all_ED_week_age_state_sunday.csv" ) %>%
  rename(N_ED_epic= N_cases_epic
  ) %>%
  dplyr::select(geography,Level, date,N_ED_epic)

epic_ed_rsv <- epic_age_import(ds_name="RSV_ED_week_age_state_sunday.csv" ) %>%
  rename( N_RSV_ED_epic= N_cases_epic) %>%
  dplyr::select(geography,Level, date,N_RSV_ED_epic)

epic_ed_flu <- epic_age_import(ds_name="FLU_ED_week_age_state_sunday.csv" ) %>%
  rename( N_flu_ED_epic= N_cases_epic) %>%
  dplyr::select(geography,Level, date,N_flu_ED_epic)

epic_ed_covid <- epic_age_import(ds_name="COVID_ED_week_age_state_sunday.csv" ) %>%
  rename( N_covid_ED_epic= N_cases_epic) %>%
  dplyr::select(geography,Level, date,N_covid_ED_epic)

epic_ed_combo <- epic_ed_all %>%
  left_join(epic_ed_rsv, by=c('geography','Level', 'date')) %>%
  left_join(epic_ed_flu, by=c('geography','Level', 'date')) %>%
  left_join(epic_ed_covid, by=c('geography','Level', 'date')) %>%
  arrange(Level, geography, date) %>%
  group_by(Level, geography) %>%
  mutate(pct_RSV_ED_epic =N_RSV_ED_epic/N_ED_epic*100,
         pct_flu_ED_epic =N_flu_ED_epic/N_ED_epic*100,
         pct_covid_ED_epic =N_covid_ED_epic/N_ED_epic*100,
         
         pct_RSV_ED_epic <- zoo::rollmean(pct_RSV_ED_epic, k = 3, fill = NA, align='right'),
         pct_flu_ED_epic <- zoo::rollmean(pct_flu_ED_epic, k = 3, fill = NA, align='right'),
         pct_covid_ED_epic <- zoo::rollmean(pct_covid_ED_epic, k = 3, fill = NA, align='right'),
         
         ED_epic_scale_RSV= 100*pct_RSV_ED_epic/max(pct_RSV_ED_epic,na.rm=T),
         ED_epic_scale_flu= 100*pct_flu_ED_epic/max(pct_flu_ED_epic,na.rm=T),
         ED_epic_scale_covid= 100*pct_covid_ED_epic/max(pct_covid_ED_epic,na.rm=T),
  )

# e1_all_ages <- readr::read_csv('./Data/CONFIDENTIAL/RSVICD10week_state.csv', skip=15, col_names=F) %>%
#   rename(geography=X1) %>%
#   tidyr::fill( geography, .direction = 'down') %>%
#   reshape2::melt(., id.vars=c('geography')) %>%
#   arrange(geography, variable) %>%
#   group_by(geography) %>%
#   #Date based on week END
#   mutate(date= seq.Date(from=as.Date('2019-01-12'), length.out=n() , by='week')) %>%
#   ungroup() %>%
#   rename(N_cases=value) %>%
#  mutate( N_cases = if_else(N_cases=='10 or fewer',NA_character_, N_cases),
#          N_cases = as.numeric(N_cases),
#          geography= if_else(geography=='Total', 'United States', geography)) %>%
#   dplyr::select(-variable) %>%
#   rename(state=geography, N_epic=N_cases)




## ----------------------------------------------------------------
## play2.qmd rsv_data BLOCK

#csv_to_parquet('https://raw.githubusercontent.com/ysph-dsde/data-gov/refs/heads/main/RSV%20Infections%20Data/Raw%20Download/NREVSS%20by%20HHS%20Region_Respiratory%20Syncytial%20Virus%20(RSV)%20Surveillance%20Archive_Downloaded%2001.23.2025.csv',path_to_parquet ='./Data/rsv_testing.parquet')

rsv1_tests <- open_dataset('./Data/rsv_testing.parquet') %>%
  collect() %>%
  as.data.frame()


key <- readRDS('./Data/hhs_regions.rds')

rsv_ts <- rsv1_tests %>%
  mutate(date= as.Date(substr(mmwrweek_end,1,10 ), '%m/%d/%Y'),
         postdate= as.Date(substr(posted,1,10 ), '%m/%d/%Y')) %>%
  filter(postdate==max(postdate)) %>%
  ungroup() %>%
  filter(level != 'National') %>%
  group_by(level ) %>%
  left_join(key, by=c('level'='Group.1')) %>%
  mutate(scaled_cases = pcr_detections/max(pcr_detections)*100,
         hhs_abbr = x  ) %>%
  ungroup()

dates2_rsv_ts <- MMWRweek(as.Date(rsv_ts$date))

max.wk.yr <- max(dates2_rsv_ts$MMWRweek[dates2_rsv_ts$MMWRyear==max(dates2_rsv_ts$MMWRyear)])

rsv_ts <- cbind.data.frame(rsv_ts,dates2_rsv_ts[,c('MMWRyear', 'MMWRweek')]) %>%
  mutate( epiyr = MMWRyear, 
          epiyr = if_else(MMWRweek<=26,MMWRyear - 1 ,MMWRyear),
          epiwk  = if_else( MMWRweek<=26, MMWRweek+52, MMWRweek  ),
          epiwk=epiwk-26
  )

#Detailed data, did a static download; updated regularly via API rdmq-nq56
##convert from CSV to parquet, which is much more compact!
# csv_to_parquet(path_to_file = './Data/CopyOfNSSP_Emergency_Department_Visit_Trajectories_by_State_and_Sub_State_Regions-_COVID-19__Flu__RSV__Combined___20250130.csv', path_to_parquet ='./Data/NSSP_detailed.parquet') 
dates <- seq.Date(from=as.Date('2022-10-01'), to=as.Date('2025-01-30'),by='week')

#TODO fix county mapping in CT https://stackoverflow.com/questions/78599492/plotting-old-connecticut-county-regions-in-usmap
#for(i in 1:length(dates)){
#}

# 
# d1_all <- open_dataset('./Data/NSSP_detailed.parquet') %>%
#   filter(county!='All') %>%collect()


# Waste water Latest: https://www.cdc.gov/wcms/vizdata/NCEZID_DIDRI/RSVStateLevelDownloadCSV.csv

#hospitalizations
#
# csv_to_parquet('https://raw.githubusercontent.com/ysph-dsde/data-gov/refs/heads/main/RSV%20Infections%20Data/Harmonized%20RSV-NET_01.29.2025.csv',path_to_parquet ='./Data/rsvnet_hosp.parquet')


# csv_to_parquet('./Data/NSSP_demographics.csv',path_to_parquet ='./Data/NSSP_demographics.parquet')

h1 <- open_dataset('./Data/rsvnet_hosp.parquet') %>%
  filter(`Region Type`=='State' & `Diagnostic Test Type`=='All' & Characteristic=='Not Stratified' & Region != 'All Sites') %>%
  rename(state=Region, hosp_rate=`Crude Rate`, date=`Week Observed`) %>%
  dplyr::select(state, date, hosp_rate) %>%
  collect() %>%
  as.data.frame()

h1.age <- open_dataset('./Data/rsvnet_hosp.parquet') %>%
  filter(`Region Type`=='State' & `Diagnostic Test Type`=='All' & Characteristic=='Age'  & Region != 'All Sites') %>%
  rename(state=Region, hosp_rate=`Crude Rate`, date=`Week Observed`) %>%
  dplyr::select(state, date, hosp_rate, Level) %>%
  collect() %>%
  ungroup() %>%
  filter( date >=as.Date('2023-07-01')) %>%
  group_by(state, Level) %>%
  mutate(hosp_rate_3m=zoo::rollmean(hosp_rate, k = 3, fill = NA, align='right'),
         scale_age=hosp_rate_3m/max(hosp_rate_3m, na.rm=T )*100,
  ) %>%
  as.data.frame()

#Wastewaster by state and date
w1 <- read.csv('./Data/RSVStateLevelDownloadCSV_ww.csv') %>%
  mutate(date=as.Date(Week_Ending_Date, '%m/%d/%Y')) %>%
  filter(Data_Collection_Period=='All Results') %>%
  rename(state=State.Territory, rsv_ww=State.Territory_WVAL) %>%
  arrange(state, date) %>%
  dplyr::select(state, date, rsv_ww)


#ED visits by state overall
d1 <- open_dataset('./Data/NSSP_detailed.parquet') %>%
  filter(county=='All'  ) %>%
  rename(state=geography, date='week_end') %>%
  dplyr::select(state, date, percent_visits_rsv) %>%
  collect() %>%
  as.data.frame()

#combine hospital, wastewater, ED visits
dwh <- d1 %>%
  full_join(w1, by=c('state','date')) %>%
  full_join(h1, by=c('state', 'date')) %>%
  left_join(e1_all_ages, by=c('state','date')) %>%
  arrange(state, date) %>%
  group_by(state) %>%
  filter(date>='2023-07-01') %>%
  mutate(percent_visits_rsv_3m = zoo::rollmean(percent_visits_rsv, k = 3, fill = NA, align='right'),
         rsv_ww_3m = zoo::rollmean(rsv_ww, k = 3, fill = NA, align='right'),
         hosp_rate_3m = zoo::rollmean(hosp_rate, k = 3, fill = NA, align='right'),
         N_epic_3m = zoo::rollmean(N_epic, k = 3, fill = NA, align='right'),
         
         percent_visits_rsv_3m = percent_visits_rsv_3m / max(percent_visits_rsv_3m, na.rm=T)*100,
         rsv_ww_3m= rsv_ww_3m/max(rsv_ww_3m, na.rm=T)*100,
         hosp_rate_3m=hosp_rate_3m/max(hosp_rate_3m, na.rm=T)*100,
         N_epic_3m = N_epic_3m/max(N_epic_3m, na.rm=T)*100,
         
         
         
         
         
  )

dates2 <- MMWRweek(as.Date(dwh$date))

max.wk.yr <- max(dates2$MMWRweek[dates2$MMWRyear==max(dates2$MMWRyear)])

dwh <- cbind.data.frame(dwh,dates2[,c('MMWRyear', 'MMWRweek')]) %>%
  mutate( epiyr = MMWRyear, 
          epiyr = if_else(MMWRweek<=26,MMWRyear - 1 ,MMWRyear),
          epiwk  = if_else( MMWRweek<=26, MMWRweek+52, MMWRweek  ),
          epiwk=epiwk-26
  )


#ED visits by demographics
# d1 <- open_dataset('./Data/NSSP_demographics.parquet') %>%
#   collect()
#   filter(county=='All'  ) %>%
#   rename(state=geography, date='week_end') %>%
#   dplyr::select(state, date, percent_visits_rsv) %>%
#   collect() %>%
#   as.data.frame()




## ----------------------------------------------------------------
## play2.qmd vax_data BLOCK

#pediatric vaccine yptake
#https://data.cdc.gov/Child-Vaccinations/Vaccination-Coverage-among-Young-Children-0-35-Mon/fhky-rtsk/about_data

# csv_to_parquet('./Data/vax/Vaccination_Coverage_among_Young_Children__0-35_Months__20250204.csv',path_to_parquet ='./Data/vax/peds_vax.parquet')
#this breaks out the age data; also has info on income, race, urbanicity, etc
vax_age <- open_dataset('./Data/vax/peds_vax.parquet') %>%
  rename(birth_year = `Birth Year/Birth Cohort`, dim1=`Dimension Type`, age=Dimension,vax_uptake=`Estimate (%)`, samp_size_vax=`Sample Size`) %>%
  collect() %>%
  filter(birth_year==2021 & dim1=='Age') %>%
  dplyr::select(Vaccine,Geography, Dose, dim1, vax_uptake,samp_size_vax, age) %>%
  filter(age=='35 Months' & Geography %in% state.name &
           (Vaccine %in% c('≥1 Dose MMR','≥1 Dose Varicella' ) | 
              (Vaccine=='DTaP' & Dose =='≥4 Doses') | 
              (Vaccine=='Hep A' & Dose =='≥2 Doses') | 
              (Vaccine=='Hep B' & Dose =='≥3 Doses') | 
              (Vaccine=='Hib' & Dose =='Full Series') | 
              (Vaccine=='PCV' & Dose =='≥4 Doses') 
           ) )

vax_urban <- open_dataset('./Data/vax/peds_vax.parquet') %>%
  rename(birth_year = `Birth Year/Birth Cohort`, dim1=`Dimension Type`, urban=Dimension,vax_uptake=`Estimate (%)`, samp_size_vax=`Sample Size`) %>%
  collect() %>%
  filter(birth_year=='2016-2019' & dim1=='Urbanicity') %>%
  dplyr::select(Vaccine,Geography, Dose, dim1, vax_uptake,samp_size_vax, urban) %>%
  filter( Geography %in% state.name &
            (Vaccine %in% c('≥1 Dose MMR','≥1 Dose Varicella' ) | 
               (Vaccine=='DTaP' & Dose =='≥4 Doses') | 
               (Vaccine=='Hep A' & Dose =='≥2 Doses') | 
               (Vaccine=='Hep B' & Dose =='≥3 Doses') | 
               (Vaccine=='Hib' & Dose =='Full Series') | 
               (Vaccine=='PCV' & Dose =='≥4 Doses') 
            ) ) %>%
  mutate(urban= factor(urban, levels= c("Living In a Non-MSA", "Living In a MSA Non-Principal City","Living In a MSA Principal City"),labels=c('Rural','Smaller City', 'Larger City') ))

vax_insurance <- open_dataset('./Data/vax/peds_vax.parquet') %>%
  rename(birth_year = `Birth Year/Birth Cohort`, dim1=`Dimension Type`, insurance=Dimension,vax_uptake=`Estimate (%)`, samp_size_vax=`Sample Size`) %>%
  collect() %>%
  filter(birth_year=='2016-2019' & dim1=='Insurance Coverage') %>%
  dplyr::select(Vaccine,Geography, Dose, dim1, vax_uptake,samp_size_vax, insurance) %>%
  filter( Geography %in% state.name &
            (Vaccine %in% c('≥1 Dose MMR','≥1 Dose Varicella' ) | 
               (Vaccine=='DTaP' & Dose =='≥4 Doses') | 
               (Vaccine=='Hep A' & Dose =='≥2 Doses') | 
               (Vaccine=='Hep B' & Dose =='≥3 Doses') | 
               (Vaccine=='Hib' & Dose =='Full Series') | 
               (Vaccine=='PCV' & Dose =='≥4 Doses') 
            ) ) %>%
  mutate(insurance= factor(insurance, levels= c("Uninsured", "Any Medicaid","Private Insurance Only","Other"),labels=c('Uninsured','Medicaid', 'Private','Other') ))




## ----------------------------------------------------------------
## play2.qmd pneumococcus_data BLOCK

###Pneumococcal disease
#csv_to_parquet('https://data.cdc.gov/resource/qvzb-qs6p.csv',path_to_parquet ='./Data/ipd_cdc1998.parquet')

ipd1 <- readRDS('./Data/pneumococcus/ABCs_st_1998_2021.rds') %>%
  rename(agec = "Age.Group..years.",
         year=Year,
         st=IPD.Serotype,
         N_IPD = Frequency.Count) %>%
  mutate( st= if_else(st=='16','16F', st),
          agec1 = if_else(agec %in% c("Age <2","Age 2-4") ,1,2 ),
          agec=gsub('Age ', '', agec),
          agec2 = if_else( agec %in% c('<2','2-4'), '<5',
                           if_else( agec %in% c('5-17','18-49'), '5-49',
                                    if_else( agec %in% c('50-64','65+'), '50+',NA))),
          agec2 = factor(agec2, levels=c('<5','5-49','50+'), labels=c('<5 years', '5-49 years', '50+ years') )
  ) %>%
  group_by(st,agec2, year) %>%
  summarize(N_IPD=sum(N_IPD)) %>%
  ungroup()

all.sts <- unique(ipd1$st)


# pneumococcal serotype by state
b2019 <- read.csv('./Data/pneumococcus/jiac058_suppl_supplementary_table_s2.csv') %>%
  group_by(State, sero) %>%
  summarize(N_cases=n()) %>%
  mutate(sero=as.factor(sero)) %>%
  ungroup() %>%
  group_by(State, sero) %>%
  mutate(mean_cases=max(N_cases,na.rm=T)
  ) %>%
  group_by(State) %>%
  mutate(         pct = N_cases/sum(N_cases, na.rm=T)*100) %>%
  ungroup() %>%
  tidyr::complete(sero,State , fill=list(pct=0))  #fills 0



## ----------------------------------------------------------------
## play2.qmd pneumococcus_data_2 BLOCK

pcv7 <- c('4','6B','9V','14','18C','19F','23F')

pcv10gsk <- c(pcv7,'1','5','7F')

pcv13 <- c(pcv7, '1','3','5','6A','7F','19A')

s1 <- readRDS('./Data/pneumococcus/ABCs_st_1998_2021.rds') %>%
  rename(agec = "Age.Group..years.",
         year=Year,
         st=IPD.Serotype,
         N_IPD = Frequency.Count) %>%
  mutate( st= if_else(st=='16','16F', st)) %>%
  group_by(st, year) %>%
  summarize(N_IPD=sum(N_IPD)) %>%
  ungroup() %>%
  group_by(st) %>%
  mutate(cum_N= sum(N_IPD)) %>%
  filter(cum_N>100) %>%
  ungroup() %>%
  mutate(pcv13st = if_else(st %in% pcv13,'VT','NVT'))
s2_pre<- s1 %>%
  filter(year %in% c(1998, 1999)) %>%
  group_by(st, year) %>%
  summarize(N_IPD=sum(N_IPD)) %>%
  ungroup() %>%
  group_by(st) %>%
  summarize(N_IPD_pre=mean(N_IPD)) %>%
  ungroup() %>%
  tidyr::complete(st,  fill=list(N_IPD_pre=0))  #fills 0

s2_pre13<- s1 %>%
  filter(year %in% c(2008, 2009)) %>%
  group_by(st, year) %>%
  summarize(N_IPD=sum(N_IPD)) %>%
  ungroup() %>%
  group_by(st) %>%
  summarize(N_IPD_pre=mean(N_IPD)) %>%
  ungroup() %>%
  tidyr::complete(st,  fill=list(N_IPD_pre=0))  #fills 0

s2<- s1 %>%
  group_by(st, year) %>%
  summarize(N_IPD=sum(N_IPD)) %>% #sum across age group
  ungroup() %>%
  tidyr::complete(st, year, fill=list(N_IPD=0))%>%  #fills 0
  left_join(s2_pre, by='st') %>%
  mutate(N_IPD_pre = if_else(is.na(N_IPD_pre),0, N_IPD_pre) ,
         logRR = log((N_IPD+1)/(N_IPD_pre+1) )) 

max_RR <- s2 %>%
  group_by(st) %>%
  summarize(max_RR = max(logRR)) %>%
  arrange((max_RR))

s2$st <- factor(s2$st, levels = max_RR$st)

df_wide <- s2 %>%
  dplyr::select(year, st, logRR) %>%
  tidyr::pivot_wider(names_from = year, values_from = logRR)


df_vt <- df_wide %>%
  mutate(pcv13st = if_else(st %in% pcv13,'VT','NVT')) %>%
  filter(pcv13st =='VT') %>%
  dplyr::select(-pcv13st) 

df_nvt <- df_wide %>%
  mutate(pcv13st = if_else(st %in% pcv13,'VT','NVT')) %>%
  filter(pcv13st =='NVT') %>%
  dplyr::select(-pcv13st) 

rr_nvt <- s2 %>%
  mutate(pcv13st = if_else(st %in% pcv13,'VT','NVT')) %>%
  filter(pcv13st =='NVT') %>%
  ggplot( aes(x = factor(year), y = st, fill = logRR)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(x = "Year", y =
         "serotype", fill = "log RR") +
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45, hjust=1))

rr_vt <- s2 %>%
  mutate(pcv13st = if_else(st %in% pcv13,'VT','NVT')) %>%
  filter(pcv13st =='VT') %>%
  ggplot( aes(x = factor(year), y = st, fill = logRR)) +
  geom_tile() +
  #     guides(fill="none")+
  
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(x = "Year", y =
         "serotype", fill = "log RR") +
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45, hjust=1))




## ----------------------------------------------------------------
## play2.qmd pneumococcus_data_3 BLOCK

ipd1 <- readRDS('./Data/pneumococcus/ABCs_st_1998_2021.rds') %>%
  rename(agec = "Age.Group..years.",
         year=Year,
         st=IPD.Serotype,
         N_IPD = Frequency.Count) %>%
  mutate( st= if_else(st=='16','16F', st),
          st = if_else(st %in% c('15B','15C'), '15BC',st),
          if_else(st %in% c('6A','6C'), '6AC',st)
  ) %>%
  filter(year %in% c(2019,2020) & agec %in% c('Age 50-64','Age 65+')) %>%
  group_by(st) %>%
  summarize( N_IPD= sum(N_IPD))

uad <- readxl::read_excel('./Data/pneumococcus/SSUAD/ramirez_ofid_2025_ofae727.xlsx') %>%
  mutate(N_SSUAD= over65 + a50_64_with_indication + a50_64_no_indication ) %>%
  full_join(ipd1, by='st') %>%
  filter(!is.na(N_SSUAD) & !is.na(N_IPD))

ggplot(uad, aes(x=N_IPD, y=N_SSUAD, label=st)) +
  geom_point()+
  geom_text( vjust = 1)+
  theme_classic() +
  ggtitle('IPD vs pneumonia 50+ years in US')






