# ---
# Reconciliation
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = T)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

design1 <- declare_population(N = 100, u = rnorm(N)) +
  declare_potential_outcomes(Y ~ Z + u) +
  declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_sampling(n = 75) +
  declare_assignment(m = 50) +
  declare_reveal(Y, Z) +
  declare_estimator(Y ~ Z, estimand = "ATE")

design2 <- declare_population(N = 200, u = rnorm(N)) +
  declare_potential_outcomes(Y ~ 0.5*Z + u) +
  declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_sampling(n = 100) +
  declare_assignment(m = 25) +
  declare_reveal(Y, Z) +
  declare_estimator(Y ~ Z, model = lm_robust, estimand = "ATE")
 
 compare_designs(design1, design2)
 compare_design_code(design1, design2)
 compare_design_summaries(design1, design2)
 compare_design_data(design1, design2)
 compare_design_estimates(design1, design2)
 compare_design_estimands(design1, design2)
