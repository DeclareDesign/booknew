# ---
# Specifying the model
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R
library(dagitty)
library(dddag)
library(ggraph)

# a_blue <- "#0072B2"
# a_gray <- "grey80"
# 
# dag <-
#   dagify(Y ~ X + Z + U,
#          X ~ U,
#          latent = "U")
# 
# gg_df <-
#   tidy_dagitty(dag,
#                layout = "manual",
#                x = c(1, 0, -1, 1),
#                y = c(1, 1, 0, 0))
# 
# gg_df <-
#   gg_df %>%
#   mutate(
#     color = case_when(
#       name == "U" ~ a_gray,
#       name == "X" ~ a_blue,
#       name == "Y" ~ a_blue,
#       name == "Z" ~ a_blue
#     )
#   )
# 
# 
# g <-
# ggplot(gg_df, aes(x = x, y = y, xend = xend, yend = yend)) +
#   geom_dag_node(aes(color = color)) +
#   scale_color_identity() +
#   geom_dag_text(color = "black", family = "Helvetica") +
#   geom_dag_edges() +
#   theme_dag()
# 
# g

design <-
  declare_population(N = 100, U = rnorm(N), X = rnorm(N)) +
  declare_potential_outcomes(Y ~ Z + X + U) +
  declare_assignment(prob = 0.5) 


dag <- dagify(Y ~ Z + U + X, X ~ U)

nodes <-
  tibble(
    name = c("Z", "X", "U", "Y"),
    label = c("Z", "X", "U", "Y"),
    annotation = c(
      "**Treatment assignment**",
      "**Observed covariate**",
      "**Unknown heterogeneity**",
      "**Outcome**"
    ),
    x = c(1, 3, 5, 5),
    y = c(1.5, 3.5, 3.5, 1.5), 
    nudge_direction = c("S", "N", "N", "S"),
    answer_strategy = "uncontrolled"
  )

ggdd_df <- make_dag_df(dag, nodes, design)

base_dag_plot %+% ggdd_df


design <-
  declare_population(
    N = 100,
    U = rbinom(N, size = 1, prob = 0.25),
    X1 = rbinom(N, size = 1, prob = 0.25),
    X2 = rbinom(N, size = 1, prob = 0.25)
  ) +
  declare_potential_outcomes(D ~ Z * X1) +
  declare_potential_outcomes(M ~ D, assignment_variables = c(D)) +
  declare_potential_outcomes(K ~ D * U, assignment_variables = D) +
  declare_potential_outcomes(Y ~ X2 + X1 + M + U, assignment_variables = c(M)) +
  declare_assignment(prob = 0.5) +
  declare_reveal(D, Z) +
  declare_reveal(M, c(D)) +
  declare_reveal(K, c(D)) +
  declare_reveal(Y, c(M))
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

ggdd_df <- make_dag_df(dag, nodes, design)

base_dag_plot %+% ggdd_df

design <-
  declare_population(N = 100, U = rnorm(N), tau = 1+rnorm(N), X = rbinom(N, 1, .5)) +
  declare_potential_outcomes(Y ~ 0.5 * X + U)

draw_data(design) %>% 
  head %>% kable(caption = "Data from a simple model")

tau_X0 <- 0.5
tau_X1 <- 1

design <-
  declare_population(N = 100, U = rnorm(N), X = rbinom(N, 1, .5)) +
  declare_potential_outcomes(Y ~ (X==0)*Z*tau_X0 + (X==1)*Z*tau_X1+U)
