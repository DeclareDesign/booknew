# ---
# Better Design
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

model <-
  declare_population(
    villages = add_level(
      N = 100,
      N_citizens_per_village = sample(20:100, N, replace = TRUE)
    ),
    citizens = add_level(N = N_citizens_per_village, u = runif(N))
  ) +
  declare_potential_outcomes(Y_Z_0 = u,
                             Y_Z_1 = pmin(1, Y_Z_0 + 0.025))

inquiry <-
  declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))

data_strategy <-
  declare_sampling(n = 50, clusters = villages) +
  declare_assignment(prob = 0.5, clusters = villages) + 
  declare_reveal(outcome_variables = Y, assignment_variables = Z) +
  declare_measurement(Yobs = if_else(Y > 0.95, 1, 0))

answer_strategy <- declare_estimator(Y ~ Z, clusters = villages, 
                                     model = difference_in_means, 
                                     estimand = "ATE")

design <- model + inquiry + data_strategy + answer_strategy


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
      "**villages**<br>Used for cluster assignment"),
    x = c(5, 1, 5, 1),
    y = c(2.5, 2.5, 4, 4), 
    nudge_direction = c("S", "S", "N", "N"),
    answer_strategy = "uncontrolled"
  )
ggdd_df <- make_dag_df(dag, nodes, design)

base_dag_plot %+% ggdd_df + coord_fixed(ylim = c(2.05, 4.6), xlim = c(0.25 - epsilon, 5.75 + epsilon))

# Select diagnosands
simple_design_diagnosands <- 
  declare_diagnosands(select = c(bias, rmse, power))





get_diagnosands(simple_design_diagnosis) %>% select(estimand_label, estimator_label, bias, rmse, power) %>% kable(digits = 3, caption = "Diagnosis of simple design.")
