# ---
# Two arm trials
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = T)


design <-
  declare_population(N = 100,
                     X = rbinom(N, 1, 0.3),
                     U = rnorm(N)) +
  declare_potential_outcomes(Y ~ Z + X + U) +
  declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(blocks = X, block_prob = c(0.1, 0.5)) +
  declare_estimator(Y ~ Z, estimand = "ATE", label = "Naive DIM") +
  declare_estimator(Y ~ Z,
                    blocks = X,
                    estimand = "ATE",
                    label = "Blocked DIM")

dag <- dagify(
  Y ~ Z + X + U,
  Z ~ X
)

nodes <-
  tibble(
    name = c("Y", "Z", "U", "X"),
    label = c("Y", "Z", "U", "X"),
    annotation = c(
      "**Outcome**<br>",
      "**Random assignment**<br>",
      "**Unknown heterogeneity**",
      "**Covariate**<br>Used for blocking"),
    x = c(5, 1, 5, 1),
    y = c(2.5, 2.5, 4, 4), 
    nudge_direction = c("S", "S", "N", "N"),
    answer_strategy = "uncontrolled"
  )
ggdd_df <- make_dag_df(dag, nodes, design)

base_dag_plot %+% ggdd_df
