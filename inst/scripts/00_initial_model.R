source(here::here("R/simulation.R"))
set.seed(14051998)

priors <- c(
  brms::prior(normal(0, 1), class = Intercept),
  brms::prior(normal(0, 0.1), class = b)
)

# Two custom functions are imported from the R/ folder:
# - sim_data creates a simulated df with a given number of participants, trials,
#   and model parameters
# - define_contrasts is a wrapper around the base R function contrasts that
#   allows defining contrasts inside a dplyr pipe

# Simulating a dataframe for the initial model
df <- 
  sim_data(
    n_ppts = 60, 
    n_trials = 60, 
    beta_context_task = 0.2
  ) |> 
  dplyr::group_by(participant, context, task) |>
  dplyr::mutate(n_trials = dplyr::n()) |> 
  dplyr::reframe(
    nb_da = sum(response == 1),
    n_trials = unique(n_trials)
  ) |> 
  define_contrasts("context", c(-0.5, +0.5)) |>
  define_contrasts("task", c(+0.5, -0.5))


# Building the initial model that will be updated iteratively in the power 
# analysis

# Detecting the number of cores to use
n_cores <- parallel::detectCores() - 1
# Defining the number of iterations per chain (+ 1000 warm-up)
n_iter <- ceiling(4000 / n_cores) + 1000
# Defining the formula
formula <- nb_da | trials(n_trials) ~ 1 + context * task +
  (1 + context * task | participant)
# Fitting the model
initial_model <- brms::brm(
  formula = formula,
  data = df,
  family = binomial(link = "logit"),
  prior = priors,
  sample_prior = TRUE,
  chains = n_cores,
  cores  = n_cores,
  warmup = 1000, 
  iter = n_iter,
  silent = 1,
  refresh = 200,
  save_pars = brms::save_pars(all = TRUE),
  file = here::here("inst/extdata/models/initial_model.rds"),
  file_compress = "xz",
  file_refit = "on_change"
)

rm(df)