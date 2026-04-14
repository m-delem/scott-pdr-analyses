#logit and inverse-logit
logit <- function (x) {log(x / (1 - x) )}
inv_logit <- function (x) {1 / (1 + exp(-x) )}

# converting betas to proportions
betas_to_proportions <- function (b0, b_cont, b_task, b_int) {
  al_match <- inv_logit(b0 + (-0.5) * b_cont + (-0.5) * b_task + 0.25 * b_int)
  al_contr <- inv_logit(b0 + (-0.5) * b_cont + 0.5 * b_task + (-0.25) * b_int)
  ar_match <- inv_logit(b0 + 0.5 * b_cont + (-0.5) * b_task + (-0.25) * b_int)
  ar_contr <- inv_logit(b0 + 0.5 * b_cont + 0.5 * b_task + 0.25 * b_int)
  df_props <- data.frame(
    al_match = al_match, 
    ar_match = ar_match,
    al_contr = al_contr, 
    ar_contr = ar_contr
  )
  return(df_props)
}

# converting betas to interaction contrast
betas_to_interaction <- function(b0, b_cont, b_task, b_int){
  df_int <- 
    betas_to_proportions(b0, b_cont, b_task, b_int) |>
    dplyr::reframe(
      interaction_diff = (ar_contr - al_contr) - (ar_match - al_match)
    ) |> 
    dplyr::pull(interaction_diff)
  return(df_int)
}

# Define contrasts inside a dplyr pipe
define_contrasts <- function(df, col, contrast) {
  contrasts(df[[col]]) <- contrast
  return(df)
}