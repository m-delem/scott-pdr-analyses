
# Renv --------------------------------------------------------------------

# Don't forget to use `renv` for a reproducible environment!
# see https://rstudio.github.io/renv/articles/renv.html for more details

# install.packages("renv")  # if you don't have it yet
# library("renv")           # same as above

# renv::init() has already been used to create the renv.lock file (the file that
# contains the exact details of the packages you used) in this template, so now 
# the project only needs to be restored each time you start working. You will
# be asked to install the packages that I added down below upon running this 
# script, but you change this to suit your needs.
renv::restore()


# CmdStanR for Bayesian modelling ------------------------------------------

# The cmdstan backend, if not already installed, has to be installed on your 
# computer first, **outside** of the project:
# install.packages("cmdstanr")
# library("cmdstanr")
# check_cmdstan_toolchain() # check if RTools is setup
# nb_cores <- parallel::detectCores() - 1
# install_cmdstan(cores = nb_cores)

# Now inside the project, you need to run a special install for cmdstanr:
# renv::install("stan-dev/cmdstanr")

# Packages ----------------------------------------------------------------

# pacman allows to check/install/load packages with a single call
# if (!require("pacman")) install.packages("pacman") # already in renv.lock
library("pacman")

# packages to load (and install if needed)
pacman::p_load(
  here,       # easy file paths
  see,        # theme_modern and okabeito palette
  report,     # reporting various info 
  ggbeeswarm, # jittered plots
  scales,     # scales for ggplot2
  Hmisc,      # plot stats
  # ---- Modelling
  faux,      # simulating data
  bayesplot, # plotting for Bayesian models
  brms,      # Bayesian regression models
  rstan,     # Stan interface
  parallel,  # parallel processing
  future,    # parallel with brms
  furrr,     # parallel with purrr
  progressr, # progress bar with furrr
  cmdstanr,  # Stan interface
  emmeans,   # post-hoc tests
  # Should remain last to avoid conflicts with other packages
  quarto,     # quarto reports
  tidyverse   # modern R ecosystem
)

# Global cosmetic theme ---------------------------------------------------

theme_set(theme_modern(base_size = 14)) # from see in easystats
color_scheme_set("green") # for bayesplot

# setting my favourite palettes as ggplot2 defaults
options( 
  ggplot2.discrete.colour   = scale_colour_okabeito,
  ggplot2.discrete.fill     = scale_fill_okabeito,
  ggplot2.continuous.colour = scale_colour_viridis_c,
  ggplot2.continuous.fill   = scale_fill_viridis_c
)

# Fixing a seed for reproducibility ---------------------------------------
set.seed(14051998)


# Adding all packages' citations to a .bib --------------------------------
knitr::write_bib(c(.packages()), file = here("bibliography/packages.bib"))
