# ---
# Poststratification
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

fixed_population <- declare_population(N = 500, 
                                       X = sample(c("A", "B", "C"), N, replace = TRUE),
                                       Y = sample(1:7, N, replace = TRUE))()

design <- 
  declare_population(data = fixed_population) + 
  declare_estimand(Ybar = mean(Y)) + 
  declare_sampling(strata_prob = c(0.2, 0.1, 0.3), strata = X) + 
  declare_step(B_demeaned = (X == "B") - mean(X == "B"),
               C_demeaned = (X == "C") - mean(X == "C"), mutate) + 
  declare_estimator(Y ~ B_demeaned + C_demeaned, term = "(Intercept)", model = lm_robust, estimand = "Ybar")

fixed_population <-
  fabricate(
    group = add_level(
      N = 2,
      X = c(0, 1),
      population_n = c(100, 50)
    ),
    individual = add_level(N = population_n,
                           Y = X + sample(0:1, N, replace = TRUE))
  )

design <-
  declare_population(data = fixed_population) +
  declare_estimand(Ybar = mean(Y)) +
  declare_sampling(strata_n = c(10, 10), strata = X) +
  declare_step(handler = group_by, groups = X) +
  declare_step(handler = mutate,
               sample_n = n(),
               weight = population_n / sample_n) +
  declare_estimator(
    Y ~ 1,
    term = "(Intercept)",
    model = lm_robust,
    weights = weight,
    estimand = "Ybar"
  )

dag <- dagify(Y ~ S + X,
              S ~ X)
nodes <-
  tibble(
    name = c("X", "Y", "S"),
    label = c("X", "Y", "S"),
    annotation = c(
      "**Strata**<br>changes sampling probabilites",
      "**Outcome**<br>measured only for sampled units",
      "**Sampling indicator**<br>randomly set by designer"),
    x = c(1, 5, 1),
    y = c(4, 1, 1),
    nudge_direction = c("N", "S", "S"),
    answer_strategy = "uncontrolled",
  ) 

ggdd_df <- make_dag_df(dag, nodes, design)

base_dag_plot %+% ggdd_df



# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R
library(lme4)
library(prediction)
