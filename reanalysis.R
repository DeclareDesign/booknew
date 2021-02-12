# ---
# Reanalysis
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

A <- declare_estimator(Y ~ Z, model = lm_robust, label = "A")
A_prime <- declare_estimator(Y ~ Z + X, model = lm_robust, label = "A_prime")

# X is a confounder and is measured pretreatment
model_1 <- 
  declare_model(
    N = 100,
    U = rnorm(N),
    X = rnorm(N), # X is pretreatment
    Z = rbinom(N, 1, prob = plogis(0.5 + X)),
    potential_outcomes(Y ~ 0.1 * Z + 0.25 * X + U),
    Y = reveal_outcomes(Y ~ Z)
  ) 

## draw_estimates(model_1 + A + A_prime)

set.seed(3)
est <- draw_estimates(model_1 + A + A_prime)
est %>% select(estimator_label, estimate, std.error, p.value) %>% kable(digits = 3)

# X is not a confounder and is measured pretreatment
model_2 <- 
  declare_model(
    N = 100,
    U = rnorm(N),
    X = rnorm(N),
    Z = rbinom(N, 1, prob = plogis(0.5)),
    potential_outcomes(Y ~ 0.1 * Z + 0.25 * X + U),
    Y = reveal_outcomes(Y ~ Z)
  ) 

# X is not a confounder and is measured posttreatment
model_3 <- 
  declare_model(
    N = 100,
    U = rnorm(N),
    Z = rbinom(N, 1, prob = plogis(0.5)),
    potential_outcomes(Y ~ 0.1 * Z + U),
    Y = reveal_outcomes(Y ~ Z),
    X = 0.1 * Z + 5 * Y + rnorm(N)
  ) 

design_1 <- model_1 + I + A + A_prime
design_2 <- model_2 + I + A + A_prime
design_3 <- model_3 + I + A + A_prime





get_diagnosands(diagnosis) %>% 
  select(design_label, estimator_label, bias)
