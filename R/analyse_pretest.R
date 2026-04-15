#####################################################
# Import and analyse pretest data                   #
# ------------------------------------------------- #
# OSF project: https://osf.io/4vzkh/                #
# Written by Ladislas Nalborczyk                    #
# E-mail: ladislas.nalborczyk@cnrs.fr               #
# Edited by Maël Delem                              #
# Last updated on April 15, 2026                    #
#####################################################

# Get a participant's ID from a JSON pretest file
get_participant_id <- function(file_path) {
  data_list <- jsonlite::fromJSON(file_path)
  df <- 
    tibble::tibble(
      names = names(data_list),
      data = data_list
    ) |> 
    tidyr::pivot_wider(names_from = names, values_from = data) |> 
    tidyr::unnest(cols = tidyselect::everything())
  
  id <- df$id[1]
  return(id)
}

# Read and analyse pretest data from a JSON file
analyse_pretest_data <- function(file_path) {
  # Read and parse JSON as a list
  data_list <- jsonlite::fromJSON(file_path)
  
  # Reshape into a dataframe, recode responses to 0/1
  df_full <- 
    tibble::tibble(
      names = names(data_list),
      data = data_list
    ) |> 
    tidyr::pivot_wider(names_from = names, values_from = data) |> 
    tidyr::unnest(cols = tidyselect::everything()) |> 
    dplyr::mutate(
      response_bin = dplyr::recode_values(
        response, 
        "d" ~ 1, "g" ~ 0, 
        default = NA)
    )
  
  id <- df_full$id[1]
  
  # Remove headphone check data
  df_pretest <-
    df_full |> 
    dplyr::filter(exp_part == "main_pretest") |> 
    dplyr::select(!c("exp_part", "check_correct_response", "check_accuracy"))
  
  # Aggregate by stimulus/frequency
  # Useful for seeing proportion of "d" responses at each level
  df_agg <- 
    df_pretest |>
    dplyr::group_by(stimulus, freq) |>
    dplyr::summarise(
      n_trials = dplyr::n(),
      n_d = sum(response_bin),
      prop_d = mean(response_bin),
      mean_rt = mean(rt),
      .groups = "drop"
    ) |>
    dplyr::arrange(freq)
  
  df_stims <-
    data.frame(
      stimulus = paste0("daga", seq(1, 2001, 1)),
      freq = seq(1650, 2650, 0.5)
    )
  
  # Fit probit regression
  mod_probit <- 
    glm(
      response_bin ~ freq,
      family = binomial(link = "probit"),
      data = df_pretest
    )
  # summary(mod_probit)
  
  # Predicted probabilities for presented stimuli
  df_pred <- 
    df_stims |>
    dplyr::mutate(
      pred_prob = predict(mod_probit, newdata = df_stims, type = "response")
    )
  
  # Find presented stimuli closest to 40%, 50%, 60%
  targets <- c(0.40, 0.50, 0.60)
  
  closest_stimuli <- 
    lapply(
      targets, 
      function (t) {
        df_pred |>
          dplyr::mutate(distance_to_target = abs(pred_prob - t)) |>
          dplyr::slice_min(
            order_by = distance_to_target, 
            n = 1, 
            with_ties = FALSE
          ) |>
          dplyr::mutate(target_threshold = t)
      }) |> 
    dplyr::bind_rows() |>
    dplyr::select(
      target_threshold, stimulus, freq, pred_prob, distance_to_target)
  
  # Also compute the exact frequency corresponding
  # to 40%, 50%, and 60% on the fitted probit curve
  # For probit GLM:
  #   qnorm(p) = b0 + b1 * freq
  #   freq = (qnorm(p) - b0) / b1
  coefs <- coef(mod_probit)
  b0 <- coefs[1]
  b1 <- coefs[2]
  
  threshold_table <- 
    data.frame(
      target_probability = targets,
      z_value = qnorm(targets),
      estimated_freq = (qnorm(targets) - b0) / b1
    )
  
  # Plot the psychometric curve
  freq_grid <- 
    data.frame(
      freq = seq(min(df_pretest$freq), max(df_pretest$freq), length.out = 200)
    )
  freq_grid$pred_prob <- 
    predict(
      mod_probit, 
      newdata = freq_grid, 
      type = "response"
    )
  pred_link <- 
    predict(
      mod_probit, 
      newdata = freq_grid, 
      type = "link", 
      se.fit = TRUE
    )
  
  # Keep only the threshold positions that fall within the plotted range
  freq_grid <- 
    freq_grid |>
    dplyr::mutate(
      fit_link = pred_link$fit,
      se_link  = pred_link$se.fit,
      pred_prob  = pnorm(fit_link),
      lower_prob = pnorm(fit_link - 1.96 * se_link),
      upper_prob = pnorm(fit_link + 1.96 * se_link)
    )
  
  threshold_plot <- 
    threshold_table |>
    dplyr::filter(
      estimated_freq >= min(freq_grid$freq),
      estimated_freq <= max(freq_grid$freq)
    ) |>
    dplyr::mutate(label = paste0(target_probability * 100, "%") )
  
  # Join closest stimulus info to the aggregated data
  highlight_df <- 
    closest_stimuli |>
    dplyr::rename(target_probability = target_threshold) |>
    dplyr::left_join(df_pred, by = c("stimulus", "freq", "pred_prob")) |>
    dplyr::mutate(label = paste0(stimulus, "\n(", round(pred_prob, 2), ")"))
  
  # Plot
  p <- 
    ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = freq_grid,
      ggplot2::aes(x = freq, ymin = lower_prob, ymax = upper_prob),
      alpha = 0.2
    ) +
    ggplot2::geom_line(
      data = freq_grid,
      ggplot2::aes(x = freq, y = pred_prob),
      linewidth = 1
    ) +
    ggplot2::geom_point(
      data = df_agg,
      ggplot2::aes(x = freq, y = prop_d),
      size = 3
    ) +
    ggplot2::geom_hline(
      yintercept = c(0.4, 0.5, 0.6),
      linetype = "dashed",
      linewidth = 0.4
    ) +
    ggplot2::geom_vline(
      data = threshold_plot,
      ggplot2::aes(xintercept = estimated_freq),
      linetype = "dotted",
      linewidth = 0.5
    ) +
    ggplot2::geom_point(
      data = highlight_df,
      ggplot2::aes(x = freq, y = pred_prob),
      size = 4,
      shape = 21,
      stroke = 1.2,
      fill = "gold"
    ) +
    ggplot2::geom_label(
      data = highlight_df,
      ggplot2::aes(x = freq, y = pred_prob, label = stimulus),
      vjust = -0.3,
      hjust = -0.1,
      size = 4
    ) +
    ggplot2::geom_text(
      data = threshold_plot,
      x = 1950,
      ggplot2::aes(y = target_probability, label = label),
      hjust = -0.1,
      vjust = -0.4,
      size = 4
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.1),
      expand = ggplot2::expansion(0.02)
    ) +
    ggplot2::scale_x_continuous(
      limits = c(1650, 2650),
      breaks = seq(1650, 2650, by = 100),
      expand = ggplot2::expansion(0.02)
    ) +
    ggplot2::labs(
      x = "Frequency (Hz)",
      y = "Pr(response = 'd')",
      title = paste0("Probit psychometric function, subject '", id, "'"),
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme(
      plot.margin = ggplot2::margin(5.5, 40, 5.5, 5.5),
      panel.grid.minor.y = ggplot2::element_blank()
    )
  
  # Save the plot
  ggplot2::ggsave(
    filename = here::here(
      paste0("inst/figures/pilot_pretest_probit_curve_", id, ".png")),
    plot = p,
    width = 12, 
    height = 8, 
    dpi = 300,
    device = "png"
  )
  
  print(p)
  return(closest_stimuli)
}