#####################################################
# Import and analyse pretest data                   #
# ------------------------------------------------- #
# OSF project: https://osf.io/4vzkh/                #
# Written by Ladislas Nalborczyk                    #
# E-mail: ladislas.nalborczyk@cnrs.fr               #
# Edited by Maël Delem                              #
# Last updated on April 15, 2026                    #
#####################################################

# Get the functions to read and analyse pretest data from a JSON file
source(here::here("R/analyse_pretest.R"))

# List the .txt files containing the pretest data for each participant 
file_paths <- 
  here::here("inst/extdata/pretest") |> 
  fs::dir_ls(glob = "*.txt", recurse = TRUE)

# Analyse each file and store the results in a table
results <- 
  tibble::tibble(
    id = file_paths |> sapply(get_participant_id) |> as.character(),
    result = file_paths |> lapply(analyse_pretest_data)
  ) |> 
  tidyr::unnest("result")

# Save results as CSV
write.csv(
  results, 
  file = here::here("data/pretest_results.csv"), 
  row.names = FALSE
)
