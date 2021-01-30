# ---
# Cluster-randomized designs
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

design <-
  declare_model(
    V = add_level(
      N = 100,
      X = rbinom(N, 1, 0.3),
      Q = rnorm(N)
    ),
    I = add_level(N = 5,
                  U = rnorm(N))) +
  declare_potential_outcomes(Y ~ Z * X + U + Q) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(clusters = V,
                     blocks = X,
                     block_prob = c(0.1, 0.5)) +
  declare_estimator(Y ~ Z,
                    model = difference_in_means,
                    inquiry = "ATE",
                    label = "Naive DIM") +
  declare_estimator(
    Y ~ Z,
    clusters = V,
    blocks = X,
    model = difference_in_means,
    inquiry = "ATE",
    label = "Blocked and Clustered DIM"
  ) +
  declare_estimator(
    Y ~ Z,
    clusters = V,
    fixed_effects = X,
    model = lm_robust,
    inquiry = "ATE",
    label = "Naive FE"
  )

dag <- dagify(
  Y ~ Z + X + U + V,
  Z ~ X + V
)

nodes <-
  tibble(
    name = c("Y", "Z", "U", "V", "X"),
    label = c("Y", "Z", "U", "V", "X"),
    annotation = c(
      "**Outcome**<br>",
      "**Random assignment**<br>",
      "**Unknown heterogeneity**",
      "**Villages**<br>Used for clustering",
      "**Covariate**<br>Used for blocking"),
    x = c(5, 1, 5, 2.5,  1),
    y = c(2.5, 2.5, 4, 4, 4), 
    nudge_direction = c("S", "S", "N", "N", "N"),
    data_strategy = c("unmanipulated", "assignment", "unmanipulated", "unmanipulated", "unmanipulated"),
    answer_strategy = "uncontrolled"
  )
ggdd_df <- make_dag_df(dag, nodes)

base_dag_plot %+% ggdd_df + coord_fixed(ylim = c(2.05, 4.6), xlim = c(0.5 - epsilon, 5.25 + epsilon))
