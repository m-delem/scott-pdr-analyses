# functions shared across scripts are placed here

# Utility functions -------------------------------------------------------

#logit and inverse-logit
logit <- function (x) {log(x / (1 - x) )}
inv_logit <- function (x) {1 / (1 + exp(-x) )}

# converting betas to proportions
betas_to_proportions <- function (b0, b_cont, b_task, b_int) {
  
  al_match <- inv_logit(b0 + (-0.5) * b_cont + (-0.5) * b_task + 0.25 * b_int)
  al_contr <- inv_logit(b0 + (-0.5) * b_cont + 0.5 * b_task + (-0.25) * b_int)
  ar_match <- inv_logit(b0 + 0.5 * b_cont + (-0.5) * b_task + (-0.25) * b_int)
  ar_contr <- inv_logit(b0 + 0.5 * b_cont + 0.5 * b_task + 0.25 * b_int)
  
  return (data.frame(
    al_match = al_match, 
    ar_match = ar_match,
    al_contr = al_contr, 
    ar_contr = ar_contr
  ))
}

# converting betas to interaction contrast
betas_to_interaction <- function(b0, b_cont, b_task, b_int){
  
  betas_to_proportions(b0, b_cont, b_task, b_int) |>
    reframe(interaction_diff = (ar_contr - al_contr) - (ar_match - al_match)) |> 
    pull(interaction_diff)
}

# Simulating binomial data (i.e., % of "da" response per task) -------

# from https://debruine.github.io/lmem_sim/articles/appendix3a_binomial.html
sim_data <- function (
    # number of participants
    n_ppts = 50,
    # number of trials per participant and task
    n_trials = 50,
    # overall intercept
    beta_0 = 0,
    # effect of contextual sound (e.g., "al" vs. "ar")
    beta_context = 0.3,
    # effect of task (matching vs. contrasting)
    beta_task = 0.25,
    # interaction between context and task
    beta_context_task = 0.2,
    # by-participant varying intercept sd
    tau_0 = 0.05,
    # by-participant varying slope sd
    tau_context = 0.05,
    # by-participant varying slope sd
    tau_task = 0.05,
    # by-participant varying slope sd
    tau_context_task = 0.05,
    # by-participant varying-effect correlation structure
    # subj_0 * subj_context, subj_task, subj_context_task
    # subj_context * subj_task, subj_context_task
    # subj_task * subj_context_task
    participant_rho = c(0, 0, 0, 0, 0, 0)
) {
  
  # simulate a sample of trials
  trials <- crossing(
    trials = 1:n_trials,
    context = factor(c("al", "ar"), ordered = FALSE),
    task = factor(c("matching", "contrasting"), ordered = FALSE)
  )
  
  # simulate a sample of participants
  participants <- faux::rnorm_multi(
    n = n_ppts, 
    mu = 0,
    sd = c(tau_0, tau_context, tau_task, tau_context_task),
    r = participant_rho,
    varnames = c("S_0", "S_context", "S_task", "S_context_task")
    ) |> 
    mutate(participant = faux::make_id(n(), "S")) |> 
    select(participant, everything())
  
  # crossing participants and trials
  dat <- 
    crossing(participants, trials) |> 
    mutate(
      # recoding predictors
      X_context = case_match(context, "al" ~ -0.5, "ar" ~ 0.5), # recode was superseded by `case_match`
      X_task = case_match(task, "matching" ~ -0.5, "contrasting" ~ 0.5),
      # add together fixed and random effects for each effect
      B_0 = beta_0 + S_0,
      B_context = beta_context + S_context,
      B_task = beta_task + S_task,
      B_context_task = beta_context_task + S_context_task,
      # linear model
      Y = B_0 + (B_context * X_context) + (B_task * X_task) + (B_context_task * X_context * X_task),
      # converting to probability of getting 1
      pr = inv_logit(Y),
      # sampling from Bernoulli distribution
      response = rbinom(n(), 1, pr)
    ) |> 
    select(participant, context, task, Y, pr, response)
  
  # returning these data
  return (dat)
}


# Define contrasts inside a dplyr pipe ------------------------------------

define_contrasts <- function(df, col, contrast) {
  contrasts(df[[col]]) <- contrast
  return(df)
}


# Simulate data, fit the model and extract Bayes factors ------------------

sim_and_fit_bf <- function (
    n_ppts, 
    n_trials, 
    beta_context_task, 
    simulation,
    model,
    p
) {
  # Simulating data
  df <- 
    sim_data(
      n_ppts = n_ppts, 
      n_trials = n_trials, 
      beta_context_task = beta_context_task
    ) |> 
    # summarising data
    group_by(participant, context, task) |>
    mutate(n_trials = n()) |> 
    reframe(
      nb_da = sum(response == 1),
      n_trials = unique(n_trials)
    ) |> 
    # defining the sum contrasts
    define_contrasts("context", c(-0.5, +0.5)) |>
    define_contrasts("task", c(+0.5, -0.5))
  
  # Fitting the model
  updated_model <- update(
    object = model,
    newdata = df,
    chains = n_cores,
    cores  = n_cores,
    iter = n_iter,
    silent = 2,
    refresh = 0
  )
  
  # Extracting Bayes factors
  hyp_two_sided <- hypothesis(updated_model, "context1:task1 = 0")
  hyp_one_sided <- hypothesis(updated_model, "context1:task1 > 0")
  
  # retrieving the two-sided BF10 (1/BF01) and the one-sided BF10
  bf10 <- 1 / as.numeric(hyp_two_sided$hypothesis$Evid.Ratio)
  bfdir <- as.numeric(hyp_one_sided$hypothesis$Evid.Ratio)
  
  # updating the `progressr` progress bar
  p()
  
  # returning the BFs
  return (c(bf10, bfdir))
}
