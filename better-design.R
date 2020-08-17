# ---
# Better Design
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

# we should turn this into a picture labeling MIDA
simple_design <- 
  
  # M: model
  
  # 50 citizens in each of 100 villages
  declare_population(
    # 100 villages
    villages = add_level(N = 100, N_citizens_per_village = sample(20:100, N, replace = TRUE)),
    
    # 50 citizens in each village
    citizens = add_level(N = N_citizens_per_village, u = rnorm(N))
  ) +
  
  # two potential outcomes, Y_Z_0 and Y_Z_1
  # Y_Z_0 is the control potential outcome (what would happen if the unit is untreated)
  #   it is equal to the unobserved shock 'u'
  # Y_Z_1 is the treated potential outcome 
  #   it is equal to the control potential outcome plus a treatment effect of 0.25
  declare_potential_outcomes(
    Y_Z_0 = u, 
    Y_Z_1 = Y_Z_0 + 0.25) +
  
  # I: inquiry
  
  # we are interested in the average treatment effect in the population
  declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) +
  
  # D: data strategy
  
  # sampling: we randomly sample 50 of the 100 villages in the population
  declare_sampling(n = 50, clusters = villages) +
  
  # assignment: we randomly assign half of the 50 sampled units to treatment (half to control)
  declare_assignment(prob = 0.5, clusters = villages) +
  
  # measurement: construct outcomes from the potential outcomes named Y depending on 
  #   the realized value of their assignment variable named Z
  #   we measure a binary outcome Yobs from the unobserved, latent variable Y
  declare_measurement(
    Yobs = case_when(
      Z == 1 & Y_Z_1 > 0 ~ 1,
      Z == 1 & Y_Z_1 <= 0 ~ 0,
      Z == 0 & Y_Z_0 > 0 ~ 1,
      Z == 0 & Y_Z_0 <= 0 ~ 0)
  ) + 
                        
  declare_reveal(outcome_variables = Y, assignment_variables = Z) +
  
  # A: answer strategy
  
  # calculate the difference-in-means of Y depending on Z 
  # we link this estimator to ATE because this is our estimate of our inquiry
  declare_estimator(Y ~ Z, clusters = villages, model = difference_in_means, estimand = "ATE")

# Select diagnosands
simple_design_diagnosands <- 
  declare_diagnosands(select = c(bias, rmse, power))

# Diagnose the design
simple_design_diagnosis <- 
  diagnose_design(simple_design, diagnosands = simple_design_diagnosands, sims = 500)

get_diagnosands(simple_design_diagnosis) %>% select(estimand_label, estimator_label, bias, rmse, power) %>% kable
