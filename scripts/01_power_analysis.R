library(here)
source(here("scripts/00_initial_model.R"))

# The first brms model that will be updated iteratively below is imported from
# "00_initial_model.R". The model is called "initial_model".

# Parallel processing setup -----------------------------------------------

# We will fit as many models as possible in parallel. If we allow ourselves to 
# use all the cores minus one, the number of models will be 
# floor(n_available / n_cores), n_cores being the number of cores used for each 
# model ("floor" rounds the number down to avoid using one excess core).

# detecting the number of cores available
n_available <- parallel::detectCores() - 1
# number of cores/chains for each model
n_cores <- 2
# number of models to fit in parallel
n_models <- floor(n_available / n_cores)

# recruiting the workers (cores) for parallel processing
plan(multisession, workers = n_models)

# defining the number of iterations per chain based on cores per model (+ 1000 
# warm-up iterations)
n_iter <- ceiling(40000 / n_cores) + 1000

# Combinations to explore -------------------------------------------------

# number of participants
# n_ppts <- seq(40, 70, 10)
n_ppts <- c(6) # testing

# number of trials per participant
# n_trials <- seq(110, 160, 10)
n_trials <- seq(80, 160, 10) # testing

# effect of the interaction
# beta_context_task <- seq(0.06, 0.13, 0.01)
beta_context_task <- seq(0.04, 0.16, 0.01) # testing

# number of simulations for each combination
# n_sims <- 100
n_sims <- 200 # testing

# creating a grid of parameters and associated datasets
combinations <- crossing(
  n_ppts = n_ppts, 
  n_trials = n_trials, 
  beta_context_task = beta_context_task,
  simulation = 1:n_sims
)

# Power analysis ----------------------------------------------------------

# recording the time at which the simulation started
start <- proc.time()[3]

# running the simulations in parallel
with_progress({
  p <- progressor(steps = nrow(combinations))
  simulation_results <- 
    future_pmap(
      .l = combinations, 
      .f = sim_and_fit_bf, 
      model = initial_model,
      p = p, # to pass to .f for progressr
      .options = furrr_options(seed = TRUE)
    )
})

simulation_results <-  
  bind_cols(combinations, tibble(bf = simulation_results)) |> 
  unnest_wider(col = bf, names_sep = "_") |> 
  na.omit()

# calculating total computation time
total_time <- 
  (proc.time()[3] - start) |> 
  round(digits = 2) |> 
  seconds_to_period()

cat(paste0(
  "The entire power analysis took ", total_time, ".\n",
  "-------------------------------------------------------------------------",
  "\n"
))

# 4min 26s to fit 19 models on 19 workers with 2 chains/cores per model

# 7min 10s to fit 38 models on 19 workers with 2 chains/cores per model
# --> theoretically 18min 51s to fit 100 models

# 15min 55s to fit 100 models with this 19 x 2 setup. Even better than expected.
# If we multiply by 64, that would be 17 hours
# But...

# It took 10h 21min 21s to fit 6400 models with the 19 x 2 setup!

# Now for the second power analysis, we launched 19200 simulations. Let's see!
# Result: 1d 8H 18M 31S. Cool!

# saveRDS(
#   simulation_results, 
#   file = "data/r-data-structures/power-analyses-results-240702.rds"
# )
