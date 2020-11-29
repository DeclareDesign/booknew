# ---
# Part I Exercises
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

design <- 
  declare_population(N = 100, X = rnorm(N), U = rnorm(N)) +
  declare_potential_outcomes(Y ~ 0.25 * Z + X + U) + 
  declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) + 
  declare_assignment(prob = 0.5) + 
  reveal_outcomes(outcome_variables = Y, assignment_variables = Z) 

declare_estimator(Y ~ Z + X, model = lm_robust, estimand = "ATE")
