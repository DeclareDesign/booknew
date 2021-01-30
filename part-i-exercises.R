# ---
# Part I Exercises
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

design <- 
  declare_model(N = 100, X = rnorm(N), U = rnorm(N),
                potential_outcomes(Y ~ 0.25 * Z + X + U)) + 
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) + 
  declare_assignment(Z = complete_ra(N, prob = 0.5), 
                     legacy = FALSE) + 
  declare_measurement(Y = reveal_outcome(Y ~ Z)) 

declare_estimator(Y ~ Z + X, model = lm_robust, inquiry = "ATE")
