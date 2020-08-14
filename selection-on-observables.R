# ---
# Selection on Observables
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = T)

 
design <-
  declare_population(N = 100, 
                     X_1 = rnorm(N),
                     X_2 = rnorm(N),
                     Z = if_else(X_1 + X_2 > 0, 1, 0),
                     U = rnorm(N)) +
  declare_potential_outcomes(Y ~ Z + X_1 + X_2 + U) +
  declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_reveal() +
  declare_estimator(Y ~ X_1 + X_2, model = lm_robust, estimand = "LATE") 


dag <- dagify(Y ~ X_1 + X_2 + Z + U,
              Z ~ X_1 + X_2)
nodes <-
  tibble(
    name = c("X_1", "X_2", "U", "Z", "Y"),
    label = c("X<sup>1</sup>", "X<sup>2</sup>", "U", "Z", "Y"),
    annotation = c(
      "**Control variable 1**",
      "**Control variable 2**",
      "**Unknown heterogeneity**",
      "**Treatment assignment**",
      "**Outcome variable**"
    ),
    x = c(1, 1, 5, 3, 5),
    y = c(1, 4, 4, 2.5, 2.5),
    nudge_direction = c("S", "N", "N", "N", "S"),
    answer_strategy = c("controlled", "controlled",  "uncontrolled", "uncontrolled", "uncontrolled")
  )


ggdd_df <- make_dag_df(dag, nodes, design)

base_dag_plot %+% ggdd_df



# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

knitr::include_graphics("figures/regression_dag_1.png")

design_1 <-
  declare_population(N = 100, 
                     U_z = rnorm(N),
                     U_x = rnorm(N),
                     U_y = rnorm(N),
                     X = U_x) +
  declare_potential_outcomes(Y ~ 0.5*Z + X + U_y) +
  declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(prob_unit = pnorm(U_z + U_x), simple = TRUE) +
  declare_estimator(Y ~ Z, estimand = "ATE", label = "DIM") +
  declare_estimator(Y ~ Z + X, model = lm, estimand = "ATE", label = "OLS")

dx_1 <- diagnose_design(design_1, sims = sims, bootstrap_sims = b_sims)
dx_1

design_2 <-
  declare_population(N = 100, 
                     U_z = rnorm(N),
                     U_x = rnorm(N),
                     U_y = rnorm(N),
                     X = U_x) +
  declare_potential_outcomes(Y ~ 0.5*Z + X + X^2 + U_y) +
  declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(prob_unit = pnorm(U_z + U_x + U_x^2), simple = TRUE) +
  declare_estimator(Y ~ Z, estimand = "ATE", label = "DIM") +
  declare_estimator(Y ~ Z + X, model = lm, estimand = "ATE", label = "OLS")

dx_2 <- diagnose_design(design_2, sims = sims, bootstrap_sims = b_sims)
dx_2

knitr::include_graphics("figures/regression_dag_2.png", )

design_3 <-
  declare_population(N = 100, 
                     U_z = rnorm(N),
                     U_x = rnorm(N),
                     U_y = correlate(rnorm, given = U_z, rho = 0.9),
                     X = U_x) +
  declare_potential_outcomes(Y ~ 0.5*Z + X + U_y) +
  declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(prob_unit = pnorm(U_z + U_x), simple = TRUE) +
  declare_estimator(Y ~ Z, estimand = "ATE", label = "DIM") +
  declare_estimator(Y ~ Z + X, model = lm, estimand = "ATE", label = "OLS")

dx_3 <- diagnose_design(design_3, sims = sims, bootstrap_sims = b_sims)
dx_3

knitr::include_graphics("figures/regression_dag_3.png", )

design_4 <-
  declare_population(N = 100, 
                     U_z = rnorm(N),
                     U_m = rnorm(N),
                     U_y = rnorm(N)) +
  declare_potential_outcomes(M ~ 0.5*Z + U_m) +
  declare_potential_outcomes(Y ~ 0.5*Z + (0.5*Z + U_m) + U_y) +
  declare_assignment(prob_unit = pnorm(U_z), simple = TRUE) +
  declare_reveal(c(M, Y), Z) +
  declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_estimator(Y ~ Z, estimand = "ATE", label = "DIM") +
  declare_estimator(Y ~ Z + M, model = lm, estimand = "ATE", label = "OLS")

dx_4 <- diagnose_design(design_4, sims = sims, bootstrap_sims = b_sims)
dx_4
