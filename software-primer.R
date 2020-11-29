# ---
# Software primer
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

## install.packages(c(
##   "DeclareDesign",
##   "fabricatr",
##   "randomizr",
##   "estimatr",
##   "DesignLibrary"
## ))

## install.packages("tidyverse")

voter_file <- fabricate(
  N = 100,
  age = sample(18:80, N, replace = TRUE),
  sex = sample(c("F", "M"), N, replace = TRUE),
  party = sample(c("DEM", "REP", "GRN"), N, replace = TRUE),
  precinct = sample(2000:10000, N, replace = TRUE)
)

kable(head(voter_file, 5), digits = 3, caption = "Example data", booktabs = TRUE)

simple_random_assignment_step <- declare_assignment(prob = 0.6)

## simple_random_assignment_step(voter_file)
simple_random_assignment_step(voter_file) %>% 
  head(5) %>% 
  kable(caption = "Data output following implementation of an assignment step.", digits = 3, booktabs = TRUE)

## custom_assignment <- function(data) {
##   mutate(data, Z = rbinom(n = nrow(data), 1, prob = 0.5))
## }
## 
## my_assignment_step <- declare_assignment(handler = custom_assignment)
## 
## my_assignment_step(voter_file)

custom_assignment <- function(data)
  mutate(data, Z = rbinom(n = nrow(data), 1, prob = 0.5))
 
my_assignment_step <- declare_assignment(handler = custom_assignment)

my_assignment_step(voter_file) %>% head(5) %>% kable(caption = "Data generated using a custom function", booktabs = TRUE)

## declare_population(data = voter_file)
declare_population(data = voter_file)() %>% head(5) %>% kable(digits = 3, caption = "Draw from a fixed population", booktabs = TRUE)

## declare_population(N = 100, U = rnorm(N))

tab1 <- declare_population(N = 100, U = rnorm(N))() %>% head(5) 
tab2 <- declare_population(N = 100, U = rnorm(N))() %>% head(5) 
tab3 <- declare_population(N = 100, U = rnorm(N))() %>% head(5)
tab4 <- declare_population(N = 100, U = rnorm(N))() %>% head(5)
tab5 <- declare_population(N = 100, U = rnorm(N))() %>% head(5)

gt_df <- 
  bind_cols(tab1, tab2, tab3, tab4, tab5) %>%
  set_names(rep(c("ID", "U"), 5)) 

gt_df %>%
  kable(digits = 3, caption = "Five draws from the population.", booktabs = TRUE) %>%
  add_header_above(c("Draw 1" = 2,
                     "Draw 2" = 2,
                     "Draw 3" = 2,
                     "Draw 4" = 2,
                     "Draw 5" = 2))

## declare_population(
##   households = add_level(
##     N = 100,
##     individuals_per_hh = sample(1:6, N, replace = TRUE)
##   ),
##   individuals = add_level(
##     N = individuals_per_hh,
##     age = sample(1:100, N, replace = TRUE)
##   )
## )

## complex_population_function <- function(data, N_units) {
##   data.frame(U = rnorm(N_units))
## }
## 
## declare_population(
##   handler = complex_population_function, N_units = 100
## )

## declare_potential_outcomes(
##   Y_Z_0 = U,
##   Y_Z_1 = Y_Z_0 + 0.25)

## design <-
##   declare_population(N = 100, U = rnorm(N)) +
##   declare_potential_outcomes(Y ~ 0.25 * Z + U)
## 
## draw_data(design)

design <- 
  declare_population(N = 100, U = rnorm(N)) +
  declare_potential_outcomes(Y ~ 0.25 * Z + U)

draw_data(design) %>% 
  head(5) %>% 
  kable(digits = 3, caption = "Adding potential outcomes to the population.", booktabs = TRUE)

## declare_potential_outcomes(Y ~ 0.25 * Z + U, assignment_variables = Z)

## declare_estimand(PATE = mean(Y_Z_1 - Y_Z_0))

## declare_sampling(n = 50)

simple_design <- 
  declare_population(N = 100, U = rnorm(N)) +
  declare_potential_outcomes(Y ~ 0.25 * Z + U) +
  declare_estimand(PATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_sampling(n = 50) 

draw_data(simple_design) %>% head(5) %>% kable(digits = 3, caption = "Sampled data.", booktabs = TRUE)

## declare_assignment(prob = 0.5)

## reveal_outcomes(Y, Z)

simple_design <- 
  declare_population(N = 100, U = rnorm(N)) +
  declare_potential_outcomes(Y ~ 0.25 * Z + U) +
  declare_estimand(PATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_sampling(n = 50) +
  declare_assignment(prob = 0.5) +
  reveal_outcomes(Y, Z)

draw_data(simple_design) %>% 
  head(5) %>% 
  kable(digits = 3, caption = "Sampled data with assignment indicator.", booktabs = TRUE)

## declare_measurement(Y_binary = rbinom(N, 1, prob = pnorm(Y)))

simple_design <- 
  declare_population(N = 100, U = rnorm(N)) +
  declare_potential_outcomes(Y ~ 0.25 * Z + U) +
  declare_estimand(PATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_sampling(n = 50) +
  declare_assignment(prob = 0.5) +
  declare_measurement(Y_binary = rbinom(N, 1, prob = pnorm(Y)))

draw_data(simple_design) %>% 
  select(-fab_ID_1) %>%
  head(5) %>% 
  kable(digits = 3, caption = "Sampled data with an explicitly measured outcome.", booktabs = TRUE)

simple_design <- 
  declare_population(N = 100, U = rnorm(N)) +
  declare_potential_outcomes(Y ~ 0.25 * Z + U) +
  declare_estimand(PATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_sampling(n = 50) +
  declare_assignment(prob = 0.5) +
  reveal_outcomes(outcome_variables = Y, assignment_variables = Z) +
  declare_estimator(Y ~ Z, model = difference_in_means, estimand = "PATE")

simple_design_data <- draw_data(simple_design)
simple_design_data %>% head(5) %>% kable(digits = 3, caption = "Data with revealed outcomes.", booktabs = TRUE)

## difference_in_means(Y ~ Z, data = simple_design_data)
difference_in_means(Y ~ Z, data = simple_design_data) %>% 
tidy %>% 
kable(digits = 3, caption = "Difference-in-means estimate from simulated data.", booktabs = TRUE)

## declare_estimator(
##   Y ~ Z, model = difference_in_means, estimand = "PATE"
## )

## declare_estimator(
##   Y ~ Z, model = lm_robust, model_summary = tidy
## )

## declare_estimator(
##   Y ~ Z, model = lm_robust, model_summary = glance
## )

## tidy_margins <- function(x) {
##   tidy(margins(x, data = x$data), conf.int = TRUE)
## }
## 
## declare_estimator(
##   Y ~ Z + X,
##   model = glm,
##   family = binomial("logit"),
##   model_summary = tidy_margins,
##   term = "Z"
## )

my_estimator <- function(data){
  data.frame(estimate = mean(data$Y))
}
declare_estimator(handler = label_estimator(my_estimator), label = "mean", estimand = "Y_bar")

## declare_step(handler = fabricate, added_variable = rnorm(N))

## collapse_data <- function(data, collapse_by) {
##   data %>%
##     group_by({{ collapse_by }}) %>%
##     summarize_all(mean, na.rm = TRUE)
## }
## 
## declare_step(handler = collapse_data, collapse_by = district)
## 
## # Note: The `{{ }}` syntax is handy for writing functions in `dplyr`
## # where you want to be able to reuse the function with different variable
## # names. Here, the `collapse_data` function will `group_by` the
## # variable you send to the argument `collapse_by`, which in our
## # declaration we set to `district`. The pipeline within the function
## # then calculates the mean in each district.

population <- 
  declare_population(N = 100, U = rnorm(N)) 

potential_outcomes <- 
  declare_potential_outcomes(Y ~ 0.25 * Z + U) 

estimand <- 
  declare_estimand(PATE = mean(Y_Z_1 - Y_Z_0)) 

sampling <- 
  declare_sampling(n = 50) 

assignment <- 
  declare_assignment(prob = 0.5) 

reveal <- 
  reveal_outcomes(outcome_variables = Y, assignment_variables = Z) 

estimator <- 
  declare_estimator(
    Y ~ Z, model = difference_in_means, estimand = "PATE"
  )

design <- 
  population + potential_outcomes + estimand + 
  sampling + assignment + reveal + estimator

design <- 
  declare_population(N = 100, U = rnorm(N)) +
  declare_potential_outcomes(Y ~ 0.25 * Z + U) +
  declare_estimand(PATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_sampling(n = 50) +
  declare_assignment(prob = 0.5) +
  reveal_outcomes(outcome_variables = Y, assignment_variables = Z) +
  declare_estimator(
    Y ~ Z, model = difference_in_means, estimand = "PATE"
  )

## population + potential_outcomes + estimand +
##   sampling + assignment + reveal + estimator

## population + potential_outcomes + sampling +
##   estimand + assignment + reveal + estimator

## draw_data(design)
draw_data(design) %>% head(5) %>% kable(digits = 3, caption = "Simulated data draw.", booktabs = TRUE)

## draw_estimands(design)
draw_estimands(design) %>% 
  kable(digits = 3, caption = "Estimands calculated from simulated data.", booktabs = TRUE)

## draw_estimates(design)

draw_estimates(design) %>% 
  select(-estimator_label) %>%
  kable(digits = 3, caption = "Estimates calculated from simulated data.", booktabs = TRUE)

## simulate_design(design)





simulation_df %>% 
  head(5) %>% 
  select(-design_label, -estimator_label, -estimand_label, -term, -outcome) %>% 
  kable(digits = 3, caption = "Simulations data frame.", booktabs = TRUE)

study_diagnosands <- declare_diagnosands(
  bias = mean(estimate - estimand),
  rmse = sqrt(mean((estimate - estimand)^2)),
  power = mean(p.value <= 0.05)
)

## diagnose_design(simulation_df, diagnosands = study_diagnosands)

diagnose_design(simulation_df, diagnosands = study_diagnosands) %>% 
reshape_diagnosis() %>% 
  select(Bias, RMSE, Power) %>%
  kable(digits = 3, caption = "Design diagnosis.", booktabs = TRUE)

## diagnose_design(design, diagnosands = study_diagnosands)

## redesign(design, N = c(100, 200, 300, 400, 500))

simple_designer <- function(sample_size, effect_size) {
  declare_population(N = sample_size, U = rnorm(N)) +
    declare_potential_outcomes(Y ~ effect_size * Z + U) +
    declare_estimand(PATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_sampling(n = 50) +
    declare_assignment(prob = 0.5) +
    reveal_outcomes(outcome_variables = Y, assignment_variables = Z) +
    declare_estimator(
      Y ~ Z, model = difference_in_means, estimand = "PATE"
    )
}

design <- simple_designer(sample_size = 100, effect_size = 0.25)

## designs <- expand_design(
##   simple_designer,
##   sample_size = c(100, 500, 1000),
##   effect_size = 0.25
## )

## diagnose_design(designs)

## compare_designs(planned_design, implemented_design)

## compare_diagnoses(planned_design, implemented_design)
## compare_diagnoses(simple_design, redesigned_simple_design, sims = sims)$compared_diagnoses_df %>%
## kable(digits = 3, caption = "Comparison of two designs.", booktabs = TRUE)

## library(DesignLibrary)
## 
## b_c_design <- block_cluster_two_arm_designer(N = 1000, N_blocks = 10)
