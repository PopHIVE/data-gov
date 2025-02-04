library(rstan)

# Example Data
N_ages <- 5   # Number of age groups
N_states <- 51   # Number of state groups

set.seed(123)
age_marginal <- rpois(N_ages, 100)   # Simulated age marginals
state_marginal <- rpois(N_states, 80)  # Simulated state marginals
age_state_prior <- matrix(runif(N_ages * N_states, 0.1, 1), N_ages, S) # Prior joint distribution

# Prepare data for Stan
stan_data <- list(
  N_ages = N_ages,
  N_states = N_states,
  age_state_prior = age_state_prior,
  age_marginal = age_marginal,
  state_marginal = state_marginal
)

# Compile and fit the model
fit <- stan(file = "latent_joint_model.stan", data = stan_data, iter = 2000, chains = 4)

# Print summary
print(fit, pars = c("beta_age", "beta_state", "sigma"))
