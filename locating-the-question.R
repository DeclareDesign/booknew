# ---
# Locating the question
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

library(metafor)
library(car)


# An estimator that estimates on testing data only if 
# estimates on training data were significant

new_estimator <- function(data){
    with(data, data.frame(
      estimate = ifelse(train_p[1] <= .05, 
                        coef(lm(Y ~ Z*X, subset = !train))[4], 
                        NA),
      p.value = ifelse(train_p[1] <= .05, 
                       coef(summary(lm(Y ~ Z * X, subset = !train)))[4,4],
                       NA),
      term = "Z:X",
      stringsAsFactors = FALSE))}

# An estimator that estimates on all data only if estimates are significant

comparison_estimator <- function(data){
  with(data, data.frame(
    estimate = ifelse(all_p[1] < .05, coef(lm(Y ~ Z*X))[4], NA),
    p.value = ifelse(all_p[1] < .05, all_p[1], NA),
    term = "Z:X",
    stringsAsFactors = FALSE))}

# The design

discovery <- 
  declare_model(
    N = 200, 
    X = sample(1:N %% 2)==1,
    het_effect = sample(c(0,.5),1,TRUE),
    train = sample(1:N %% 2)==1,
    u = rnorm(N),
    potential_outcomes(Y ~ Z + het_effect * Z * X + u)) + 
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N = N), legacy = FALSE) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) + 
    
  # Main analysis
  declare_estimator(Y ~ Z, inquiry = "ATE", label = "Main") +
  
  # Exploration
  declare_step(
      train_p = (lm_robust(Y ~ Z * X, subset = train) %>% tidy())[4,4],
      all_p   = (lm_robust(Y ~ Z * X) %>% tidy())[4,4],
      handler = fabricate) +
    
  declare_inquiry(
    het = mean(Y_Z_1[X] - Y_Z_0[X]) - mean(Y_Z_1[!X] - Y_Z_0[!X])) +
  
  # Handler estimates only if p low in training group  
  declare_estimator(
    handler = label_estimator(new_estimator), 
    inquiry = "het",
    label = "Discovery") +
    
  declare_estimator(
    handler = label_estimator(comparison_estimator), 
    inquiry = "het",
    label = "Comparison")

discovery_diagnosands <-
  declare_diagnosands(
    bias = mean((estimate - estimand), na.rm = TRUE),
    RMSE = sqrt(mean((estimate - estimand)^2, na.rm = TRUE)),
    frequency = mean(!is.na(estimate)),
    false_pos = mean(p.value[estimand == 0] < 0.05, na.rm = TRUE),
    false_neg = 1 - mean(p.value[estimand != 0] < 0.05, na.rm = TRUE))





diagnosis %>%
  reshape_diagnosis() %>% select(-Term, - 'N Sims') %>%
  kable(digits = 3, booktabs = TRUE, caption = "Complete random sampling design diagnosis")

kable(reshape_diagnosis(diagnosis)[c("Estimator Label",
                                     "Term",
                                     "Bias",
                                     "RMSE",
                                     "Frequency",
                                     "False Pos",
                                     "False Neg")])
