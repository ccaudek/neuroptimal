# Load libraries for the NeurOptimal project.
#
# Corrado Caudek

# Prelims

options(show.signif.stars = FALSE)
options(max.print = 999999)

# Load the necessary libraries 
if (!require("pacman"))
  install.packages("pacman")
library("pacman")

pacman::p_load(
  "tidyverse", "knitr", "rmarkdown", "forcats", "gdata",
  "rstanarm", "rstan", "brms", "ggeffects", "ggthemes",
  "bayesplot", "R.matlab", "hBayesDM", "lme4", "ggmcmc",
  "ggthemes", "bayesplot", "grid", "effects",
  "tidybayes", "loo", "tidyr", "kableExtra", "RColorBrewer",
  "patchwork"
)


# Options -----------------------------------------------------------------


options(mc.cores = parallel::detectCores())

# rstan_options(auto_write = TRUE)
# reuse_models = TRUE

theme_set(bayesplot::theme_default(base_family = "sans", base_size=14))

options(max.print=999999)


