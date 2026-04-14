source(here::here("inst/scripts/00_initial_model.R"))
pacman::p_load(
  parallel,  # parallel processing
  future,    # parallel with brms
  furrr,     # parallel with purrr
  progressr  # progress bar with furrr
)
# The first brms model that will be updated iteratively below is imported from
# "00_initial_model.R". The model is called "initial_model".

# Parallel processing setup -----------------------------------------------
# We will fit as many models as possible in parallel. If we allow ourselves to 
# use all the cores minus one, the number of models will be 
# floor(n_available / n_cores), n_cores being the number of cores used for each 
# model ("floor" rounds the number down to avoid using one excess core).

# Detecting the number of cores available
n_available <- parallel::detectCores() - 1
# Number of cores/chains for each model
n_cores <- 2
# Number of models to fit in parallel
n_models <- floor(n_available / n_cores)
# Recruiting the workers (cores) for parallel processing
future::plan(multisession, workers = n_models)

# Parameter combinations to explore --------------------------------------------
# Parameters tested in the first a priori power analysis (2024-07-02)
# Number of participants
# n_ppts <- seq(40, 70, 10)
# Number of trials per participant
# n_trials <- seq(110, 160, 10)
# Interaction effect sizes
# beta_context_task <- seq(0.06, 0.13, 0.01)
# Number of simulations for each combination of parameters
# n_sims <- 100
# file <- here::here("data/power-analyses-results-240702.rds")
# These parameters resulted in 19200 simulations. They have been computed by
# spreading 19 models across 19 workers with 2 chains/cores per model.
# Total duration: 1d 8H 18M 31S

# Parameters tested in the later a posteriori analysis (2026-04-14) using the 
# final n_ppts and n_trials chosen in the preregistration 
n_ppts <- 60
n_trials <- 150
beta_context_task <- seq(0.04, 0.16, 0.005)
n_sims <- 100
file <- here::here("data/power-analyses-results-260414.rds")

# Creating a dataset with one line per simulation based on a grid of parameters
combinations <- tidyr::crossing(
  n_ppts = n_ppts, 
  n_trials = n_trials, 
  beta_context_task = beta_context_task,
  simulation = 1:n_sims
)

# Power analysis ----------------------------------------------------------
# Recording the time at which the simulation started
start <- proc.time()[3]

# Running the simulations in parallel
progressr::with_progress({
  p <- progressr::progressor(steps = nrow(combinations))
  simulation_results <- 
    furrr::future_pmap(
      .l = combinations, 
      .f = sim_and_fit_bf, 
      model = initial_model,
      p = p, # to pass to .f for progressr
      n_cores = n_cores,
      .options = furrr::furrr_options(seed = TRUE)
    )
})

simulation_results <-  
  dplyr::bind_cols(combinations, tibble::tibble(bf = simulation_results)) |> 
  tidyr::unnest_wider(col = bf, names_sep = "_") |> 
  na.omit()

# Calculating total computation time
total_time <- 
  (proc.time()[3] - start) |> 
  round(digits = 2) |> 
  lubridate::seconds_to_period()

cat(paste0(
  "The entire power analysis took ", total_time, ".\n",
  "-------------------------------------------------------------------------",
  "\n"
))

saveRDS(
  simulation_results,
  file = file
)

bf_threshold <- 10

df_power <-
  simulation_results |> 
  dplyr::rowwise() |> 
  dplyr::mutate(
    n_ppts = paste(n_ppts, "participants"),
    n_trials = paste(n_trials, "trials"),
    prop = round(betas_to_interaction(0, 0.3, 0.25, beta_context_task), 3),
    perc = prop * 100,
    dplyr::across(c(n_ppts, n_trials, beta_context_task, prop, perc), factor)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::group_by(n_ppts, n_trials, beta_context_task, prop, perc) |> 
  dplyr::reframe(
    # power for BF10
    bf_10_attained = sum(bf_1 >= bf_threshold),
    bf_10_power = bf_10_attained / dplyr::n(),
    bf_10_se = sqrt(bf_10_power * (1 - bf_10_power) / dplyr::n()),
    # power for BF+
    bf_dir_attained = sum(bf_2 >= bf_threshold),
    bf_dir_power = bf_dir_attained / dplyr::n(),
    bf_dir_se = sqrt(bf_dir_power * (1 - bf_dir_power) / dplyr::n())
  ) |> 
  dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~round(., 3))) |> 
  dplyr::select(!tidyselect::contains("attained"))
  
knitr::kable(df_power)

df_power |> 
  ggplot2::ggplot(ggplot2::aes(
    x = perc,
    y = bf_dir_power,
    color = n_trials,
    fill = n_trials,
    group = n_trials)
  ) +
  ggplot2::geom_hline(
    yintercept = 0.9, 
    linetype = 3, 
    alpha = 0.8, 
    linewidth = 0.7,
    color = "red"
  ) +
  ggplot2::geom_line(linewidth = 0.5) +
  ggplot2::geom_point(
    pch = 21, 
    color = "white",
    show.legend = FALSE,
    size = 3
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = bf_dir_power - bf_dir_se,
      ymax = bf_dir_power + bf_dir_se),
    width = 0,
    linewidth = .5,
    show.legend = FALSE
  ) +
  # geom_smooth(
  #   method = "lm", 
  #   se = FALSE,
  #   na.rm = TRUE,
  #   linewidth = 0.5, 
  #   alpha = 0.1, 
  #   fullrange = TRUE, 
  #   show.legend = FALSE
  # ) +
  # annotate(
  #   geom = "text",
  #   label = "90% power",
  #   size = 4,
  #   color = "black",
  #   x = -Inf,
  #   y = 0.9,
  #   hjust = -0.1,
  #   vjust = -1
  # ) +
  ggplot2::labs(
    title = "A priori power analysis (100 simulations per dot, 2500 total)",
    x = "Interaction effect size (in percentage points)",
    y = "Statistical power (proportion of BF+ >= 10)",
    color = "Number of\ntrials"
  ) +
  ggplot2::facet_wrap(ggplot2::vars(n_ppts), nrow = 2) +
  ggplot2::scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2), 
    expand = ggplot2::expansion(c(0, 0.05))) +
  ggplot2::scale_color_viridis_d() + 
  ggplot2::scale_fill_viridis_d() + 
  ggplot2::theme_minimal() +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_line(colour = "grey90", linewidth = 0.5),
    panel.grid.minor.y = ggplot2::element_line(colour = "grey95", linewidth = 0.4),
    # panel.background = element_rect(colour = "black"),
    panel.border = ggplot2::element_rect(fill = NA, colour = "black"),
    axis.line = ggplot2::element_blank()
  )
