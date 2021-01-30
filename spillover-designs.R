# ---
# Spillover Designs
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

design <-
  declare_model(
    group = add_level(N = 50, X = rnorm(N)),
    unit = add_level(N = 50, U = rnorm(N))
  ) +
  declare_assignment(S = cluster_ra(N, clusters = group, conditions = c("low", "high")), legacy = FALSE) +
  declare_step(S_prob = case_when(S == "low" ~ 0.25, S == "high" ~ 0.75), mutate) +
  declare_assignment(blocks = group, prob_unit = S_prob) +
  declare_step(spillover = ave(Z, group, FUN = mean),
               handler = fabricate)  +
  declare_potential_outcomes(
    Y ~ Z + spillover * (S == "low") + Z * spillover * (S == "high") + X + U,
    conditions = list(Z = c(0, 1), S = c("low", "high"))) +
  declare_inquiry(ATE_saturation = mean(Y_Z_0_S_high - Y_Z_0_S_low),
                   ate_no_spill = mean(Y_Z_1_S_low - Y_Z_0_S_low)) +
  declare_reveal(Y, c(Z, S)) +
  declare_estimator(Y ~ Z + S,
                    weights = 1 / (S_cond_prob * Z_cond_prob),
                    model = lm_robust,
                    term = c("Z", "Shigh"),
                    inquiry = c("ATE_saturation", "ate_no_spill"),
                    label = "main effect")


dag <- dagify(Y ~ Z + S + U,
              Z ~ S)

nodes <-
  tibble(
    name = c("U", "S", "Z", "Y"),
    label = c("U", "S", "Z", "Y"),
    annotation = c(
      "**Unknown heterogeneity**",
      "**Treatment assignment 1**<br>Saturation level",
      "**Treatment assignment 2**<br>Individual assignment",
      "**Outcome variable**"
    ),
    x = c(5, 1, 1, 5),
    y = c(4, 4, 1, 2.5),
    nudge_direction = c( "N", "N", "S", "S"),
    data_strategy = c("unmanipulated", "assignment", "assignment", "unmanipulated"),
    answer_strategy = "uncontrolled"
  )

ggdd_df <- make_dag_df(dag, nodes)

base_dag_plot %+% ggdd_df
