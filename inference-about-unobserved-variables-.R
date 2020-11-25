# ---
# Inference about unobserved variables 
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)


design <-
  declare_population(N = 250, gender = rep(0:1, N/2), Y_star = 1 + gender + 2* rnorm(N)) +
  declare_estimand(Y_bar = mean(scale(Y_star)[gender == 1])) + 
  declare_measurement(Y_1 = 3 + 0.1 * Y_star + rnorm(N, sd = 0.5),
                      Y_2 = 2 + 1.0 * Y_star + rnorm(N, sd = 1),
                      Y_3 = 1 + 0.5 * Y_star + rnorm(N, sd = 0.5),
                      Y_av = scale((scale(Y_1) + scale(Y_2) + scale(Y_3))),
                      Y_fa  = princomp(~ Y_1 + Y_2 + Y_2, cor = TRUE)$scores[,1]) + 
  declare_estimator(Y_av ~ 1, model = lm_robust, estimand = "Y_bar", subset = gender ==1, label = "Average") +
  declare_estimator(Y_fa ~ 1, model = lm_robust, estimand = "Y_bar", subset = gender ==1, label = "principal components")



dag <-
  dagify(Y_1 ~ Y_star, Y_2 ~ Y_star, 
         Y_3 ~ Y_star, 
         Y_fa ~ Y_1 + Y_2 + Y_3)

nodes <-
  tibble(
    name = c("Y_star", "Y_1", "Y_2", "Y_3", "Y_fa"),
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



## diagnosis <- diagnose_design(design)





diagnosis %>%
  reshape_diagnosis() %>%
  select(`Estimator Label`, `Mean Estimand`, Bias, RMSE) %>%
  kable(digits = 3, booktabs = TRUE, caption = "Estimation of the conditional mean of a normalized latent variable")
