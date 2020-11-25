# ---
# Specifying the model
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R
library(dagitty)
library(dddag)
library(ggraph)

design <-
  declare_population(N = 100, U = rnorm(N)) +
  declare_potential_outcomes(Y ~ Z + U) +
  declare_assignment(prob = 0.5) 


dag <- dagify(Y ~ Z + U)

nodes <-
  tibble(
    name = c("Z", "U", "Y"),
    label = c("Z", "U", "Y"),
    annotation = c(
      "**Treatment assignment**",
      "**Unknown heterogeneity**",
      "**Outcome**"
    ),
    x = c(1, 5, 5),
    y = c(1.5, 3.5,  1.5), 
    nudge_direction = c("S", "N", "S"),
    answer_strategy = "uncontrolled"
  )

ggdd_df <- make_dag_df(dag, nodes, design)

base_dag_plot %+% ggdd_df

M <-
  declare_population(N = 100, 
                     U = rnorm(N), 
                     tau = rnorm(N, mean = 1, sd = 0.1), 
                     Z = rbinom(N, 1, prob = 0.5)) +
  declare_potential_outcomes(Y ~ tau * Z + U)

draw_data(M) %>% 
  head(5) %>% kable(caption = "Data from a simple model", digits = 3, booktabs = TRUE)


M <-
  declare_population(
    N = 100,
    U = rbinom(N, size = 1, prob = 0.25),
    X1 = rbinom(N, size = 1, prob = 0.25),
    X2 = rbinom(N, size = 1, prob = 0.25)
  ) +
  declare_potential_outcomes(D ~ Z * X1) +
  declare_potential_outcomes(M ~ D, assignment_variables = D) +
  declare_potential_outcomes(K ~ D * U, assignment_variables = D) +
  declare_potential_outcomes(Y ~ X2 + X1 + M + U, 
                             assignment_variables = c(M)) +
  declare_assignment(prob = 0.5) +
  reveal_outcomes(D, Z) +
  reveal_outcomes(M, c(D)) +
  reveal_outcomes(K, c(D)) +
  reveal_outcomes(Y, c(M))
# draw_data(design)


dag <- dagify(
  Y ~ X2 + X1 + M + U,
  M ~ D,
  K ~ D + U,
  D ~ Z + X1
)

# x = c(M = 3, U = 4, X1 = 3, X2 = 2, X3 = 4, Z = 1, Y = 4, K = 3)
# y = c(M = 1, U = 0, X1 = 2, X2 = 1, X3 = 2, Z = 1, Y = 1, K = 0)

nodes <-
  tibble(
    name = c("Z", "X1", "U", "Y", "X2", "K", "M", "D"),
    label = c("Z", "X1", "U", "Y", "X2", "K", "M", "D"),
    annotation = c(
      "**Instrument**",
      "**Confounder**",
      "**Unknown heterogeneity**",
      "**Outcome**",
      "**Moderator**",
      "**Collider**",
      "**Mediator**",
      "**Explanatory variable**"
    ),
    x = c(1, 3, 5, 5, 5, 3, 3, 2),
    y = c(2.5, 4, 1, 2.5, 4, 1, 2.5, 2.5), 
    nudge_direction = c("N", "N", "S", "E", "N", "S", "N", "S"),
    answer_strategy = "uncontrolled"
  )

ggdd_df <- make_dag_df(dag, nodes, M)

base_dag_plot %+% ggdd_df

tau_X0 <- 0.5
tau_X1 <- 1

M <-
  declare_population(
    N = 100, 
    U = rnorm(N), 
    X = rbinom(N, 1, .5)
  ) +
  declare_potential_outcomes(
    Y ~ (X == 0) * Z * tau_X0 + (X == 1) * Z * tau_X1 + U
  )
