# ---
# Structural estimation
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

library(metafor)
library(car)


structural_estimator <- function(data, pi, y, chi = 3/4){
    
    # Define negative log likelihood as a function of k, d and q
    LL  <- function(k, d, q) {
      m <- with(data, y(Z, d))
      R <- q * dbeta(data[pi][[1]], k * chi, k * (1- chi)) +
        (1 - q) * dbeta(data[pi][[1]], k * m, k * (1 - m))
      - sum(log(R))
    }

  # Estimation
  M <- mle2(
    LL,
    method = "L-BFGS-B",
    start = list(k = 2, d = 0.50,  q = 0.50),
    lower = c(k = 1,    d = 0.01,  q = 0.01),
    upper = c(k = 1000, d = 0.99,  q = 0.99)
  )
  
  # Format output from estimation
  out <- data.frame(coef(summary(M)), outcome = pi)
  
  names(out) <- c("estimate", "std.error", "statistic", "p.value", "outcome")
  
  # Use estimates of q and delta to predict average treatment effects (ATEs)
  # Predicted ATE for n=2
  out[4, 1] <- (1 - out["q", "estimate"]) * (2 * out["d", "estimate"] - 1)
  
  # Predicted ATE for n=infinity
  out[5, 1] <- (1 - out["q", "estimate"]) * (2 * out["d", "estimate"] /
                                               (1 + out["d", "estimate"]) - 1)
  
  out
}

# Declare the design ----------------------------------------------------------

# Define parameter values:
d = 0.8       # True delta (unknown)
k = 6         # Parameter to governance variance (unknown)
q = 0.5       # Share of behavioral types in the population (unknown)
chi = 0.75    # Price paid by norm following ("behavioral" customers) (known)

# Design declaration:

design <- 
  
  # Define the population: indicator for behavioral type (norm = 1)
  declare_population(N = 500, norm = rbinom(N, 1, q)) +
  
  # Define mean potential outcomes for n = 2 
  declare_potential_outcomes(
    pi_two ~ norm*chi + (1-norm)*(Z*d + (1-Z)*(1-d))) +
  
  # Define mean potential outcomes for n = infinity
  declare_potential_outcomes(
    pi_inf ~ norm*chi + (1-norm)*(Z*d/(1+d) + (1-Z)*(1-d/(1+d)))) +
  
  # Define estimands (quantities we want to estimate)
  declare_estimand(ATE_two = mean(pi_two_Z_1 - pi_two_Z_0), # ATE n = 2
                   ATE_inf = mean(pi_inf_Z_1 - pi_inf_Z_0), # ATE n = infinity
                   k = k,                                   # kappa
                   d = d,                                   # delta
                   q = q) +                                 # q

  # Declare assignment process 
  declare_assignment() +
  
  # Declare revealed potential outcomes
  reveal_outcomes(pi_two, Z) + reveal_outcomes(pi_inf, Z) +
  
  # Get draws from beta distribution given means for n = 2 and n = infinity
  declare_measurement(
    pi_two_obs = rbeta(N, pi_two*k, (1-pi_two)*k),      
    pi_inf_obs = rbeta(N, pi_inf*k, (1-pi_inf)*k)) +
  
  # Declare estimators
  # Difference-in-means for n = 2
  declare_estimator(pi_two_obs ~ Z, estimand = "ATE_two", label = "DIM_two") +
  
  # Difference-in-means for n = infinity
  declare_estimator(pi_inf_obs ~ Z, estimand = "ATE_inf", label = "DIM_inf") +
  
  # MLE for n = 2
  declare_estimator(handler = tidy_estimator(structural_estimator), 
                    pi = "pi_two_obs", 
                    y = function(Z, d) Z * d + (1 - Z) * (1 - d), 
                    estimand = c("k","d", "q", "ATE_two", "ATE_inf"), 
                    label = "Struc_two") +
  
  # MLE for n = infinity
  declare_estimator(handler = tidy_estimator(structural_estimator),
                    pi = "pi_inf_obs", 
                    y = function(Z, d) Z*d/(1+d) +  (1-Z)*(1-d/(1+d)),
                    estimand = c("k","d","q","ATE_two", "ATE_inf"), 
                    label = "Struc_inf") 





diagnosis %>%
  reshape_diagnosis() %>%
  select(-Coverage, -'Mean Se', - 'N Sims', - 'Type S Rate', - Term) %>%
  kable(digits = 3, booktabs = TRUE, caption = "Complete random sampling design diagnosis")
