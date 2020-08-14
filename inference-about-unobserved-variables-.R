# ---
# Inference about unobserved variables 
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = T)

design <- 
  declare_population(N = 100, Y_star = rnorm(N)) +
  declare_estimand(Y_bar = mean(Y_star)) + 
  declare_measurement(Y_1 = 0.1 * Y_star + rnorm(N, sd = 0.25),
                      Y_2 = Y_star + rnorm(N, sd = 0.25),
                      Y_3 = 1 + 0.5 * Y_star + rnorm(N, sd = 0.25),
                      Y_idx = (Y_1 + Y_2 + Y_3) / 3) + 
  declare_estimator(Y_idx ~ 1, model = lm_robust, estimand = "Y_bar")



dag <-
  dagify(Y_1 ~ Y_star, Y_2 ~ Y_star, 
         Y_3 ~ Y_star, 
         Y_idx ~ Y_1 + Y_2 + Y_3)

nodes <-
  tibble(
    name = c("Y_star", "Y_1", "Y_2", "Y_3", "Y_idx"),
    label = c("Y^*", "Y<sup>1</sup>", "Y<sup>2</sup>", "Y<sup>3</sup>", "I"),
    answer_strategy = "uncontrolled",
    annotation = c(
      "**Latent outcome**<br>unmeasurable",
      "**Measured outcome**",
      "",
      "",
      "**Constructed Index**"),
    x = c(1, 3, 3, 3, 5),
    y = c(2.5, 3.5, 2.5, 1.5, 2.5),
    nudge_direction = c("N", "N", "S", "N", "N")
  )

ggdd_df <- make_dag_df(dag, nodes, design)
base_dag_plot %+% ggdd_df



# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R
