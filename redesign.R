# ---
# Redesign
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R


# Decision parameters

N <- 1000  # Number of cases
r <- 1     # Measurement error
c <- .2   # Cost of policy

# Design
design <- 
  declare_model(
    b = add_level(N = 1, b = rnorm(1,0,1)),
    i = add_level(N = N, X = rnorm(N), Y = 1+b*X + rnorm(N))) +
  declare_measurement(Y_seen = Y + rnorm(N, 0, r)) +
  declare_inquiry(b = b[1]) +
  declare_estimator(Y_seen ~ X, model = lm_robust) 

# Utility as a diagnosand

diagnosands <- declare_diagnosands(
  utility = mean((p.value <= 0.05)*(estimate > c)*(estimand - c) - N/100000+ r/250))
                                  
design <- set_diagnosands(design, diagnosands)

designs <- redesign(design, N = 100*2^(0:6), r = (0:8)/2)
