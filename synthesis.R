# ---
# Synthesis
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R
library(metafor)
library(broom)

study_design_fixed <- 
  declare_model(N = 100, U = rnorm(N), potential_outcomes(Y ~ 0.1 * Z + U)) + 
  declare_assignment(Z = complete_ra(N, m = 50), legacy = FALSE) + 
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) + 
  declare_estimator(Y ~ Z, model = difference_in_means)

study_design_random <- 
  declare_model(N = 100, U = rnorm(N), potential_outcomes(Y ~ rnorm(1, mean = 0.1, sd = 0.1) * Z + U)) + 
  declare_assignment(Z = complete_ra(N, m = 50), legacy = FALSE) + 
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) + 
  declare_estimator(Y ~ Z, model = difference_in_means)

model <-
  declare_model(data = simulate_design(study_design_fixed, study_design_random, sims = 10))

answer_strategy <- 
  declare_estimator(handler = label_estimator(function(data){
    with(data, tidy(rma.uni(yi = estimate, sei = std.error, subset = design_label == "study_design_fixed", method = "REML")))
  }), label = "FE-RE") +
  declare_estimator(handler = label_estimator(function(data){
    with(data, tidy(rma.uni(yi = estimate, sei = std.error, subset = design_label == "study_design_random", method = "REML")))
  }), label = "RE-RE") + 
  declare_estimator(handler = label_estimator(function(data){
    with(data, tidy(rma.uni(yi = estimate, sei = std.error, subset = design_label == "study_design_fixed", method = "FE")))
  }), label = "FE-FE") + 
  declare_estimator(handler = label_estimator(function(data){
    with(data, tidy(rma.uni(yi = estimate, sei = std.error, subset = design_label == "study_design_random", method = "FE")))
  }), label = "RE-FE")

design <- model + answer_strategy
