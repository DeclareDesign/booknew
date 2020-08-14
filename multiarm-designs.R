# ---
# Multiarm Designs
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R
library(blockTools)

design <-
  declare_population(N = 100, u = rnorm(N)) +
  declare_potential_outcomes(Y ~ Z + u) +
  declare_assignment(prob = 0.5) +
  declare_reveal(Y, Z) +
  declare_estimator(Y ~ Z, model = difference_in_means)





kable(head(simulations_pilot))
