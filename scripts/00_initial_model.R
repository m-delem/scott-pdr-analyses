library(here)
source(here("scripts/_setup.R"))
source(here("scripts/_functions.R"))

priors <- c(
  prior(normal(0, 1), class = Intercept),
  prior(normal(0, 0.1), class = b)
)

# Two custom functions are imported from _functions.R:
# - sim_data creates a simulated df with a given number of participants, trials,
#   and model parameters
# - define_contrasts is a wrapper around the base R function contrasts that
#   allows defining contrasts inside a dplyr pipe


# Simulating a first dataframe --------------------------------------------

df <- 
  sim_data(
    n_ppts = 60, 
    n_trials = 60, 
    beta_context_task = 0.2
  ) |> 
  group_by(participant, context, task) |>
  mutate(n_trials = n()) |> 
  reframe(
    nb_da = sum(response == 1),
    n_trials = unique(n_trials)
  ) |> 
  define_contrasts("context", c(-0.5, +0.5)) |>
  define_contrasts("task", c(+0.5, -0.5))


# Fitting the first model -------------------------------------------------

# detecting the number of cores to use
n_cores <- parallel::detectCores() - 1
# defining the number of iterations per chain (+ 1000 warm-up)
n_iter <- ceiling(40000 / n_cores) + 1000
# cmdstanr options
options(cmdstanr_warn_inits = FALSE)

# defining the formula
formula <- nb_da | trials(n_trials) ~ 1 + context * task +
  (1 + context * task | participant)

# fitting
initial_model <- brm(
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
  refresh = 500,
  backend = "cmdstanr",
  stan_model_args = list(stanc_options = list("O1")),
  save_pars = save_pars(all = TRUE),
  file = here("data/r-data-structures/initial_model"),
  file_compress = "xz"
)

rm(df)