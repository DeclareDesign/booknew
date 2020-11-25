# ---
# Instrumental variables
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

design <-
  declare_population(N = 100, U = rnorm(N)) +
  declare_potential_outcomes(D ~ if_else(Z + U > 0, 1, 0), 
                             assignment_variables = Z) + 
  declare_potential_outcomes(Y ~ 0.1 * D + 0.25 + U, 
                             assignment_variables = D) +
  declare_estimand(LATE = mean(Y_D_1[D_Z_1 == 1 & D_Z_0 == 0] - 
                                 Y_D_0[D_Z_1 == 1 & D_Z_0 == 0])) +
  declare_assignment(prob = 0.5) +
  reveal_outcomes(D, Z) + 
  reveal_outcomes(Y, D) + 
  declare_estimator(Y ~ D | Z, model = iv_robust, estimand = "LATE") 

dag <- dagify(Y ~ D + U,
              D ~ Z + U)

nodes <-
  tibble(
    name = c("Z", "D", "U", "Y"),
    label = c("Z", "D", "U", "Y"),
    annotation = c(
      "**Exogenous variable**<br>Instrument",
      "**Endogenous variable**",
      "**Unknown heterogeneity**",
      "**Outcome**"
    ),
    x = c(1, 3, 5, 5),
    y = c(1.5, 1.5, 3.5, 1.5), 
    nudge_direction = c("S", "S", "N", "S"),
    answer_strategy = "uncontrolled"
  )

ggdd_df <- make_dag_df(dag, nodes, design)

base_dag_plot %+% ggdd_df
