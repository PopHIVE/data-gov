#install.packages('ipfp')
library(tidyverse)
library(arrow)
library(tidycensus)
library(reshape2)
library(mipfp)
library(rstan)


pop <- read.csv('https://raw.githubusercontent.com/ysph-dsde/data-gov/refs/heads/main/RSV%20Infections%20Data/CDC_Vintage%202020%20Bridged-Race%20Postcensal%20Population%20Estimates_v2020_y1020_All%20Counties.csv') %>%
  dplyr::select(State, Characteristic, Level,Postcensal.Population_2020)

d1 <- open_dataset('./website_play3/Data/NSSP_detailed.parquet') %>%
  filter(county!='All' & week_end=='2023-12-09' ) %>%
  mutate(
    percent_visits_rsv=if_else(percent_visits_rsv>1,1,percent_visits_rsv) ,
    percent_visits_combined=if_else(percent_visits_combined>10,10,percent_visits_combined) ) %>%
  collect()


d1_demo <- read_csv('./website_play3/Data/NSSP_demographics.csv') %>%
  mutate(date=as.Date(week_end,'%m/%d/%Y'))

d1_sex <- d1_demo %>% 
  filter(demographics_type=='Sex' & pathogen=='RSV' & date=='2023-12-09')

d1_race <- d1_demo %>% 
  filter(demographics_type=='Race/Ethnicity' & pathogen=='RSV' & date=='2023-12-09')

d1_age <- d1_demo %>% 
  filter(demographics_type=='Age Group' & pathogen=='RSV' & date=='2023-12-09')

d1_state <- read_csv('./website_play3/Data/NSSP_Emergency_Department_Visit_Trajectories_by_State_and_Sub_State_Regions-_COVID-19__Flu__RSV__Combined___20250130.csv') %>%
  filter(county=='All'  & geography!='United States' &  week_end =='2023-12-09') %>%
  dplyr::select(week_end, geography, percent_visits_rsv) %>%
  rename(date=week_end, state=geography)

pop_age_us <- pop %>%
  filter(Characteristic=='Age' & State=='US') %>%
  mutate(agec2 = if_else(Level %in% c('<1 Years','1-4 Years'),'0-4 years',
                         if_else(Level %in% c('5-17 Years'),'5-17 years',
                         if_else(Level %in% c('18-49 Years','50-64 Years'),'18-64 years',
                         if_else(Level %in% c('65-74 Years','75+ Years'),'65+ years','unmatched'
                         ))))) %>%
  group_by(agec2) %>%
  summarize(pop_age_us = sum(Postcensal.Population_2020))

pop_age_state <- pop %>%
  filter(Characteristic=='Age' & State!='US') %>%
  mutate(agec2 = if_else(Level %in% c('<1 Years','1-4 Years'),'0-4 years',
                         if_else(Level %in% c('5-17 Years'),'5-17 years',
                                 if_else(Level %in% c('18-49 Years','50-64 Years'),'18-64 years',
                                         if_else(Level %in% c('65-74 Years','75+ Years'),'65+ years','unmatched'
                                         ))))) %>%
  group_by(agec2,State) %>%
  summarize(pop_age_us = sum(Postcensal.Population_2020)) %>%
  rename(state=State) %>%
  ungroup()

#Race/Ethnicity
pop_race_state <- pop %>%
  filter(Characteristic=='Race/Ethnicity' & State!='US') %>%
  mutate(race = if_else(Level %in% c('Asian or Pacific Islander'),'Asian/NHOPI, NH',
                         if_else(Level %in% c('American Indian or Alaska Native'),'AI/AN, NH',
                                 if_else(Level %in% c('Black or African American'),'Black, NH',
                                         if_else(Level %in% c('Hispanic or Latino'),'Hispanic',
                                                 if_else(Level %in% c('White'),'White, NH',
                                                 if_else(Level %in% c('Multi-Race, non-Hispanic'),'Multiple/Other, NH',             
                                                 'NA'
                                         ))))))) %>%
  group_by(race,State) %>%
  summarize(pop_race_us = sum(Postcensal.Population_2020)) %>%
  rename(state=State) %>%
  ungroup()


#age
d1_age_pop_prop <- pop_age_state %>%
  group_by(agec2) %>%
  summarize(pop_age_us=sum(pop_age_us)) %>%
  left_join(d1_age, by=c('agec2'='demographics_values')) %>%
  mutate(N_age_rsv = pop_age_us *percent_visits/100 ) #https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/nssp.html

#state
d1_state_pop_prop <- pop_age_state %>%
  group_by(state) %>%
  summarize(pop_age_us=sum(pop_age_us)) %>%
  left_join(d1_state, by='state') %>%
  mutate(N_state_rsv = pop_age_us *percent_visits_rsv/100 ) #https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/nssp.html

#race
d1_race_pop_prop <- pop_race_state %>%
  group_by(race) %>%
  summarize(pop_race_us=sum(pop_race_us)) %>%
  full_join(d1_race, by=c('race'='demographics_values')) %>%
  mutate(N_race_rsv = pop_race_us *percent_visits/100 ) #https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/nssp.html


# Define the marginal distributions
state_marginal <- d1_state_pop_prop$N_state_rsv
age_marginal <- d1_age_pop_prop$N_age_rsv

# Create a matrix for the initial joint distribution with uniform values
initial_joint_dist <- matrix(1, nrow = length(age_marginal), ncol = length(state_marginal))

# storing the margins in a list
target_list <- list(
   age_marginal,
   state_marginal
)

# list of dimensions of each marginal constrain
tgt.list.2d <- list(1,2)

# Perform IPF
fit <- mipfp::Ipfp(initial_joint_dist,tgt.list.2d, target_list,na.target = TRUE)

# Extract the estimated joint probabilities
joint_dist <- fit$p.hat


joint_dist_count <- fit$x.hat

#estimated weight by state
#state_probs <- apply(joint_dist,2,sum)
#how does estimates weight relate to state population?
#plot(state_probs,d1_state_pop_prop$pop_age_us)

# Extract the estimated counts
joint_counts_age_state <- fit$x.hat


#use MLE function in mipfp

ObtainModelEstimates(seed=123,tgt.list.2d, target_list,na.target = TRUE )

###########################################################
###########################################################
## How about doing this with optim?
# Initialize numerical parameter vectors

#set.seed(123)
#age_marginal <- rpois(5, lambda = 10)  # Simulated marginal data
#state_marginal <- rpois(4, lambda = 15)

init_age_parms <- rep( 1 , length(age_marginal))  
init_state_parms <- rep(1, length(state_marginal))  
init_state_parms[is.na(init_state_parms)] <- mean(init_state_parms, na.rm=T)

# Define the optimization function
pop_opt <- function(age.parms, state.parms, age_marginal, state_marginal) {
  
  #Compute joint age-state effect (outer product)
  joint_age_state_effect <- exp(outer(age.parms, state.parms))  
  
  joint_age_state_effect <- ifelse(is.na(joint_age_state_effect), 1e-10, joint_age_state_effect)
  
  joint_age_state_effect <- pmax(joint_age_state_effect, 1e-10)
  
  # Compute predicted margins
  pred_age <- rowSums(joint_age_state_effect, na.rm = TRUE)
  pred_state <- colSums(joint_age_state_effect, na.rm = TRUE)
  
  # Compute diff
  age_diff <- abs(age_marginal - pred_age)
  state_diff <- abs(state_marginal - pred_state )
  
  # Total log-likelihood (sum over all observations)
  diff_total <- sum(age_diff, na.rm=T) + sum(state_diff, na.rm=T)
  
  return(diff_total)  #  minimization
}

# Optimization
optim_result <- optim(par = c(init_age_parms, init_state_parms), 
                      fn = function(par) {
                        n_age <- length(age_marginal)
                        n_state <- length(state_marginal)
                        age_parms <- par[1:n_age]
                        state_parms <- par[(n_age + 1):(n_age + n_state)]
                        pop_opt(age_parms, state_parms, age_marginal, state_marginal)
                      },
                      method = "BFGS")  # Use "L-BFGS-B" if bounds are needed

# Check results
optim_result

age_mu <- optim_result$par[1:length(age_marginal)]
state_mu <- optim_result$par[(length(age_marginal) + 1):(length(age_marginal) + length(state_marginal))]
joint_age_state_beta_optim <- exp(outer(age_mu, state_mu))  

check.rows <- apply(joint_age_state_beta_optim,1,sum)
plot(check.rows,age_marginal)
abline(a=0, b=1)


check.cols <-  apply(joint_age_state_beta_optim,2,sum)
plot(check.cols,state_marginal)
abline(a=0, b=1)

#compare optim results with mipfp results
plot(joint_counts_age_state,joint_age_state_beta_optim)+
  abline(a=0, b=1)

########################################
########################################

#STAN
N_ages <- length(age_marginal)   # Number of age groups
state_marginal_nomiss <- state_marginal[!is.na(state_marginal)]
N_states <- length(state_marginal_nomiss)   # Number of state groups
age_state_prior <- matrix(runif(N_ages * N_states, 0.1, 1), N_ages, N_states) # Prior joint distribution

# Prepare data for Stan
stan_data <- list(
  N_ages = N_ages,
  N_states = N_states,
  age_state_prior = age_state_prior,
  age_marginal = round(age_marginal/1000),
  state_marginal = round(state_marginal_nomiss/1000)
)

# Compile and fit the model
fit <- stan(file = "latent_joint_model.stan", data = stan_data, iter = 2000, chains = 4)

# Print summary
print(fit, pars = c("beta_age", "beta_state", "sigma"))
print(fit, pars = c("lambda"))

plot(fit, plotfun = "trace", pars = c("beta_age"), inc_warmup = FALSE)
plot(fit, plotfun = "trace", pars = c("beta_state"), inc_warmup = FALSE)



