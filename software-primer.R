# ---
# Software primer
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = T)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

## install.packages(c("DeclareDesign", "fabricatr", "randomizr", "estimatr", "DesignLibrary"))

## install.packages("tidyverse")

# we should turn this into a picture labeling MIDA
simple_design <- 
  
  # M: model
  
  # a 100-unit population with an unobserved shock 'e'
  declare_population(N = 100, u = rnorm(N)) +
  
  # two potential outcomes, Y_Z_0 and Y_Z_1
  # Y_Z_0 is the control potential outcome (what would happen if the unit is untreated)
  #   it is equal to the unobserved shock 'u'
  # Y_Z_1 is the treated potential outcome 
  #   it is equal to the control potential outcome plus a treatment effect of 0.25
  declare_potential_outcomes(Y_Z_0 = u, Y_Z_1 = Y_Z_0 + 0.25) +
  
  # I: inquiry
  
  # we are interested in the average treatment effect in the population (PATE)
  declare_estimand(PATE = mean(Y_Z_1 - Y_Z_0)) +
  
  # D: data strategy
  
  # sampling: we randomly sample 50 of the 100 units in the population
  declare_sampling(n = 50) +
  
  # assignment: we randomly assign half of the 50 sampled units to treatment (half to control)
  declare_assignment(prob = 0.5) +
  
  # reveal outcomes: construct outcomes from the potential outcomes named Y depending on 
  #   the realized value of their assignment variable named Z
  declare_reveal(outcome_variables = Y, assignment_variables = Z) +
  
  # A: answer strategy
  
  # calculate the difference-in-means of Y depending on Z 
  # we link this estimator to PATE because this is our estimate of our inquiry
  declare_estimator(Y ~ Z, model = difference_in_means, estimand = "PATE")

# Select diagnosands
simple_design_diagnosands <- 
  declare_diagnosands(select = c(bias, rmse, power))

# Diagnose the design
simple_design_diagnosis <- 
  diagnose_design(simple_design, diagnosands = simple_design_diagnosands, sims = 500)

get_diagnosands(simple_design_diagnosis) %>% select(estimand_label, estimator_label, bias, rmse, power) %>% kable

redesigned_simple_design <-
  replace_step(simple_design, 
               step = 4, 
               new_step = declare_sampling(n = 100))

voter_file <- fabricate(
  N = 100,
  age = sample(18:80, N, replace = TRUE),
  sex = sample(c("F", "M"), N, replace = TRUE),
  party = sample(c("DEM", "REP", "GRN"), N, replace = TRUE),
  precinct = sample(2000:10000, N, replace = TRUE)
)

kable(head(voter_file))

voter_file <- voter_file %>% 
  mutate(Z = sample(c(0, 1), size = 100, replace = TRUE, prob = c(0.5, 0.5)))

kable(head(voter_file))

voter_file <- voter_file %>% 
  mutate(Z = simple_ra(N = 100, prob = 0.5))

simple_random_assignment_function <- function(data) {
  data %>% mutate(Z = simple_ra(N = 100, prob = 0.5))
}

## simple_random_assignment_function(voter_file)
simple_random_assignment_function(voter_file) %>% head %>% kable

simple_random_assignment_step <- declare_assignment(prob = 0.5)

## simple_random_assignment_step(voter_file)
simple_random_assignment_step(voter_file) %>% head %>% kable

## declare_population(data = voter_file)
declare_population(data = voter_file)() %>% head %>% kable

## declare_population(N = 100, u = rnorm(N))

tab2 <- declare_population(N = 100, u = rnorm(N))() %>% head
tab1 <- declare_population(N = 100, u = rnorm(N))() %>% head
tab3 <- declare_population(N = 100, u = rnorm(N))() %>% head
kable(list(tab1, tab2, tab3), booktabs = TRUE) #%>% kable_styling() # does not work with PDF

## declare_population(
##   households = add_level(N = 100, individuals_per_hh = sample(1:10, N, replace = TRUE)),
##   individuals = add_level(N = individuals_per_hh, age = sample(1:100, N, replace = TRUE))
## )

## complex_population_function <- function(data, N_units) {
##   data.frame(u = rnorm(N_units))
## }
## 
## declare_population(handler = complex_population_function, N_units = 100)

## declare_potential_outcomes(
##   Y_Z_0 = u,
##   Y_Z_1 = Y_Z_0 + 0.25)

## des <- declare_population(N = 100, u = rnorm(N)) +
##   declare_potential_outcomes(Y_Z_0 = u, Y_Z_1 = Y_Z_0 + 0.25)
## 
## draw_data(des)
des <- declare_population(N = 100, u = rnorm(N)) +
  declare_potential_outcomes(Y_Z_0 = u, Y_Z_1 = Y_Z_0 + 0.25)

draw_data(des) %>% head %>% kable

## declare_potential_outcomes(Y ~ u + 0.25 * Z, assignment_variables = Z)

## declare_estimand(PATE = mean(Y_Z_1 - Y_Z_0))

## declare_sampling(n = 50)

simple_design <- 
  declare_population(N = 100, u = rnorm(N)) +
  declare_potential_outcomes(Y_Z_0 = u, Y_Z_1 = Y_Z_0 + 0.25) +
  declare_estimand(PATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_sampling(n = 50) 

draw_data(simple_design) %>% head %>% kable

## declare_assignment(prob = 0.5)

simple_design <- 
  declare_population(N = 100, u = rnorm(N)) +
  declare_potential_outcomes(Y_Z_0 = u, Y_Z_1 = Y_Z_0 + 0.25) +
  declare_estimand(PATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_sampling(n = 50) +
  declare_assignment(prob = 0.5)

draw_data(simple_design) %>% head %>% kable

## declare_step(handler = fabricate, add_variable = rnorm(N))

## collapse_data <- function(data, collapse_by) {
##   data %>% group_by({{ collapse_by }}) %>% summarize_all(mean, na.rm = TRUE)
## }
## 
## declare_step(handler = collapse_data, collapse_by = district)

simple_design <- 
  declare_population(N = 100, u = rnorm(N)) +
  declare_potential_outcomes(Y_Z_0 = u, Y_Z_1 = Y_Z_0 + 0.25) +
  declare_estimand(PATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_sampling(n = 50) +
  declare_assignment(prob = 0.5) +
  declare_reveal(outcome_variables = Y, assignment_variables = Z) +
  declare_estimator(Y ~ Z, model = difference_in_means, estimand = "PATE")
simple_design_data <- draw_data(simple_design)
simple_design_data %>% head %>% kable

## simple_design_data %>% summarize(DiM = mean(Y[Z == 1]) - mean(Y[Z == 0]))
simple_design_data %>% summarize(DiM = mean(Y[Z == 1]) - mean(Y[Z == 0])) %>% kable

## difference_in_means(Y ~ Z, data = simple_design_data)
difference_in_means(Y ~ Z, data = simple_design_data) %>% tidy %>% kable

## declare_estimator(Y ~ Z, model = difference_in_means, estimand = "PATE")

population <- declare_population(N = 100, u = rnorm(N)) 
potential_outcomes <- declare_potential_outcomes(Y_Z_0 = u, Y_Z_1 = Y_Z_0 + 0.25) 
estimand <- declare_estimand(PATE = mean(Y_Z_1 - Y_Z_0)) 
sampling <- declare_sampling(n = 50) 
assignment <- declare_assignment(prob = 0.5) 
reveal <- declare_reveal(outcome_variables = Y, assignment_variables = Z) 
estimator <- declare_estimator(Y ~ Z, model = difference_in_means, estimand = "PATE")

simple_design <- 
  population + potential_outcomes + estimand + sampling + assignment + reveal + estimator

simple_design <- 
  declare_population(N = 100, u = rnorm(N)) +
  declare_potential_outcomes(Y_Z_0 = u, Y_Z_1 = Y_Z_0 + 0.25) +
  declare_estimand(PATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_sampling(n = 50) +
  declare_assignment(prob = 0.5) +
  declare_reveal(outcome_variables = Y, assignment_variables = Z) +
  declare_estimator(Y ~ Z, model = difference_in_means, estimand = "PATE")

## population + potential_outcomes + estimand + sampling + assignment + reveal + estimator

## population() %>% potential_outcomes %>% estimand
population() %>% potential_outcomes %>% estimand %>% kable

## population() %>% potential_outcomes %>% sampling %>%  assignment %>% reveal %>% estimator
population() %>% potential_outcomes %>% sampling %>%  assignment %>% reveal %>% estimator %>% kable

## draw_data(simple_design)
draw_data(simple_design) %>% head %>% kable

## draw_estimands(simple_design)
draw_estimands(simple_design) %>% kable

## draw_estimates(simple_design)
draw_estimates(simple_design) %>% kable

## simulate_design(simple_design, sims = 500)
simulations_df <- simulate_design(simple_design, sims = 5) 

simulations_df %>% kable

## simulations_df %>%
##   group_by(estimand_label, estimator_label) %>%
##   summarize(bias = mean(estimate - estimand),
##             rmse = sqrt(mean((estimate - estimand)^2)),
##             power = mean(p.value < .05))
simulations_df %>% 
  group_by(estimand_label, estimator_label) %>% 
  summarize(bias = mean(estimate - estimand),
            rmse = sqrt(mean((estimate - estimand)^2)),
            power = mean(p.value < .05)) %>% 
  kable

study_diagnosands <- declare_diagnosands(
  select = c(bias, rmse, power), 
  mse = mean((estimate - estimand)^2))

## diagnose_design(simulations_df, diagnosands = study_diagnosands)
diagnose_design(simulations_df, diagnosands = study_diagnosands) %>% get_diagnosands %>% kable

## diagnose_design(simple_design, diagnosands = study_diagnosands)

## compare_designs(simple_design, redesigned_simple_design)

## compare_diagnoses(simple_design, redesigned_simple_design)
## # temporarily disabled until we are are on CRAN
## compare_diagnoses(simple_design, redesigned_simple_design, sims = sims)$diagnosands_df %>% kable

simple_designer <- function(sample_size, effect_size) {
  declare_population(N = sample_size, u = rnorm(N)) +
    declare_potential_outcomes(Y_Z_0 = u, Y_Z_1 = Y_Z_0 + effect_size) +
    declare_estimand(PATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_sampling(n = 50) +
    declare_assignment(prob = 0.5) +
    declare_reveal(outcome_variables = Y, assignment_variables = Z) +
    declare_estimator(Y ~ Z, model = difference_in_means, estimand = "PATE")
}

simple_design <- simple_designer(sample_size = 100, effect_size = 0.25)

## simple_designs <- expand_design(simple_designer, sample_size = c(100, 500, 1000), effect_size = 0.25)

## diagnose_design(simple_designs)

## library(DesignLibrary)
## 
## b_c_design <- block_cluster_two_arm_designer(N = 1000, N_blocks = 10)
## 
## diagnose_design(b_c_design)
