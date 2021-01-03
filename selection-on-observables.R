# ---
# Selection on Observables
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

design <-
  declare_population(N = 100, 
                     X_1 = rnorm(N),
                     X_2 = rnorm(N),
                     D = if_else(X_1 + X_2 > 0, 1, 0),
                     U = rnorm(N)) +
  declare_potential_outcomes(Y ~ D + X_1 + X_2 + U,
                             assignment_variable = D) +
  declare_estimand(ATE = mean(Y_D_1 - Y_D_0)) +
  reveal_outcomes(Y, D) +
  declare_estimator(Y ~ D + X_1 + X_2, model = lm_robust, 
                    estimand = "ATE") 

dag <- dagify(Y ~ X_1 + X_2 + D + U,
              D ~ X_1 + X_2)
nodes <-
  tibble(
    name = c("X_1", "X_2", "U", "D", "Y"),
    label = c("X<sup>1</sup>", "X<sup>2</sup>", "U", "D", "Y"),
    annotation = c(
      "**Control variable 1**",
      "**Control variable 2**",
      "**Unknown heterogeneity**",
      "**Treatment**",
      "**Outcome variable**"
    ),
    x = c(1, 1, 5, 3, 5),
    y = c(1, 4, 4, 2.5, 2.5),
    nudge_direction = c("S", "N", "N", "W", "S"),
    data_strategy = "unmanipulated",
    answer_strategy = c("controlled", "controlled",  "uncontrolled", "uncontrolled", "uncontrolled")
  )


ggdd_df <- make_dag_df(dag, nodes)

base_dag_plot %+% ggdd_df

design <-
  declare_population(N = 100, 
                     X_1 = rnorm(N),
                     X_2 = rnorm(N),
                     U = rnorm(N),
                     D = if_else(X_1 + X_2 + U > 0, 1, 0)) +
  declare_potential_outcomes(Y ~ D + X_1 + X_2 + U,
                             assignment_variable = D) +
  declare_estimand(ATE = mean(Y_D_1 - Y_D_0)) +
  reveal_outcomes(Y, D) +
  declare_estimator(Y ~ D + X_1 + X_2, model = lm_robust, 
                    estimand = "ATE") 

dag <- dagify(Y ~ X_1 + X_2 + D + U,
              D ~ X_1 + X_2 + U)
nodes <-
  tibble(
    name = c("X_1", "X_2", "U", "D", "Y"),
    label = c("X<sup>1</sup>", "X<sup>2</sup>", "U", "D", "Y"),
    annotation = c(
      "**Control variable 1**",
      "**Control variable 2**",
      "**Unknown heterogeneity**",
      "**Treatment**",
      "**Outcome variable**"
    ),
    x = c(1, 1, 5, 3, 5),
    y = c(1, 4, 4, 2.5, 2.5),
    nudge_direction = c("S", "N", "N", "W", "S"),
    data_strategy = "unmanipulated",
    answer_strategy = c("controlled", "controlled",  "uncontrolled", "uncontrolled", "uncontrolled")
  )


ggdd_df <- make_dag_df(dag, nodes)

base_dag_plot %+% ggdd_df

design <-
  declare_population(N = 100, 
                     X_2 = rnorm(N),
                     D = if_else( X_2 > 0, 1, 0),
                     U = rnorm(N)) +
  declare_potential_outcomes(Y ~ D + X_2 + U,
                             assignment_variable = D) +
  declare_estimand(ATE = mean(Y_D_1 - Y_D_0)) +
  reveal_outcomes(Y, D) +
  declare_measurement(X_1 =  D + rnorm(N)) +
  declare_estimator(Y ~ D + X_1 + X_2, model = lm_robust, 
                    estimand = "ATE") 

dag <- dagify(Y ~ X_2 +  X_1 + D + U,
              X_1 ~ D,
              D ~ X_2)
nodes <-
  tibble(
    name = c("X_1", "X_2", "U", "D", "Y"),
    label = c("X<sup>1</sup>", "X<sup>2</sup>", "U", "D", "Y"),
    annotation = c(
      "**Control variable 1**",
      "**Control variable 2**",
      "**Unknown heterogeneity**",
      "**Treatment**",
      "**Outcome variable**"
    ),
    x = c(4, 1, 5, 3, 5),
    y = c(1, 4, 4, 2.5, 2.5),
    nudge_direction = c("S", "N", "N", "W", "S"),
    data_strategy = "unmanipulated",
    answer_strategy = c("controlled", "controlled",  "uncontrolled", "uncontrolled", "uncontrolled")
  )


ggdd_df <- make_dag_df(dag, nodes)

base_dag_plot %+% ggdd_df
