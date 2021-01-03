# ---
# Better Design
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

model <-
  declare_population(
    villages = add_level(N = 192),
    citizens = add_level(N = 48, U = runif(N))
  ) +
  declare_potential_outcomes(Y_Z_neutral = U,
                             Y_Z_personal = Y_Z_neutral + 0.02,
                             Y_Z_social = Y_Z_neutral + 0.03)

inquiry <- declare_estimand(
  ATE_personal = mean(Y_Z_personal - Y_Z_neutral),
  ATE_social = mean(Y_Z_social - Y_Z_neutral)
)

data_strategy <-
  declare_assignment(
    clusters = villages, 
    conditions = c("neutral", "personal", "social")
  ) + 
  reveal_outcomes(outcome_variables = Y, assignment_variables = Z) +
  declare_measurement(Yobs = if_else(Y > 0.97, 1, 0))

answer_strategy <- 
  declare_estimator(Y ~ Z, term = c("Zpersonal", "Zsocial"), 
                    clusters = villages, 
                    model = lm_robust,
                    estimand = c("ATE_personal", "ATE_social"))

design <- model + inquiry + data_strategy + answer_strategy

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
      "**villages**<br>Used for cluster assignment"),
    x = c(5, 1, 5, 1),
    y = c(2.5, 2.5, 4, 4), 
    nudge_direction = c("S", "S", "N", "N"),
    data_strategy = c("unmanipulated", "assignment", "unmanipulated", "unmanipulated"),
    answer_strategy = "uncontrolled"
  )
ggdd_df <- make_dag_df(dag, nodes)

base_dag_plot %+% ggdd_df + coord_fixed(ylim = c(2.05, 4.6), xlim = c(0.25 - epsilon, 5.75 + epsilon))

diagnosands <- declare_diagnosands(
  bias = mean(estimate - estimand),
  rmse = sqrt(mean((estimate - estimand)^2)),
  power = mean(p.value <= 0.05)
)





get_diagnosands(simple_design_diagnosis) %>% 
  select(estimand_label, estimator_label, bias, rmse, power) %>%
  kable(digits = 3, caption = "Diagnosis of the simplified Gulzar-Khan design.", booktabs = TRUE)
