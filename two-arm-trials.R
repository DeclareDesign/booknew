# ---
# Two arm trials
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

design <-
  declare_population(N = 100, U = rnorm(N)) +
  declare_potential_outcomes(Y ~ Z + U) +
  declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(prob = 0.5) +
  declare_estimator(Y ~ Z, estimand = "ATE")

dag <-
dagify(
  Y ~ Z + U
)

# dag <- get_dag(design)

nodes <-
  tibble(
    name = c("Y", "Z", "U"),
    label = c("Y", "Z", "U"),
    annotation = c(
      "**Outcome**<br>",
      "**Random assignment**<br>",
      "**Unknown heterogeneity**"),
    x = c(5, 1, 5),
    y = c(2.5, 2.5, 4),
    nudge_direction = c("S", "S", "N"),
    answer_strategy = "uncontrolled"
  )

ggdd_df <- make_dag_df(dag, nodes, design)

base_dag_plot %+% ggdd_df + coord_fixed(ylim = c(2, 4.5), xlim = c(0.5, 5.5))
