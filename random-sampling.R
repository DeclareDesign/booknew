# ---
# Random sampling
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

portola <-
  fabricate(
    households = add_level(N = 500, 
                           n_adults = sample(1:3, N, replace = TRUE),
                           household_shock = rnorm(N, mean = 1)),
    adults = add_level(N = n_adults, 
                       individual_shock = rnorm(N, sd = 0.1),
                       Ystar = household_shock + individual_shock)
    )

design <- 
  declare_model(data = portola) + 
  declare_measurement(Y = as.numeric(cut(Ystar, 7))) + 
  declare_inquiry(Y_bar = mean(Y)) + 
  declare_sampling(n = 100) + 
  declare_estimator(Y ~ 1, model = lm_robust, inquiry = "Y_bar")

dag <- dagify(Y ~ Q + Ystar + S)

nodes <-
  tibble(
    name = c("Y", "S", "Q", "Ystar"),
    label = c("Y", "S", "Q", "Y^*"),
    annotation = c("**Measured outcome**<br>observed only for sampled units",
                   "**Sampling indicator**<br>randomly set by designer",
                   "**Survey Question**",
                   "**Latent outcome**"),
    x = c(5, 1, 5, 1),
    y = c(1.5, 3.5, 3.5, 1.5), 
    nudge_direction = c("S", "N", "N", "S"),
    data_strategy = c("unmanipulated", "sampling", "measurement", "unmanipulated"),
    answer_strategy = "uncontrolled"
  )

ggdd_df <- make_dag_df(dag, nodes)

base_dag_plot %+% ggdd_df

## diagnosands <- declare_diagnosands(
##   bias = mean(estimate - estimand),
##   rmse = sqrt(mean((estimate - estimand) ^ 2))
## )
## diagnosis <- diagnose_design(design, diagnosands = diagnosands)





diagnosis %>%
  reshape_diagnosis() %>%
  select(Bias, RMSE) %>%
  kable(digits = 3, booktabs = TRUE, caption = "Complete random sampling design diagnosis")

design <-
  declare_model(data = portola) +
  declare_measurement(Y = as.numeric(cut(Ystar, 7))) +
  declare_inquiry(Y_bar = mean(Y)) +
  declare_sampling(clusters = households, n = 50) +
  declare_estimator(Y ~ 1,
                    model = lm_robust,
                    inquiry = "Y_bar",
                    label = "Standard errors not clustered") +
  declare_estimator(Y ~ 1,
                    clusters = households,
                    model = lm_robust,
                    inquiry = "Y_bar",
                    label = "Standard errors clustered")

diagnosands <- declare_diagnosands(
  bias = mean(estimate - estimand),
  rmse = sqrt(mean((estimate - estimand) ^ 2)),
  coverage = mean(estimand <= conf.high & estimand >= conf.low)
)
diagnosis <- diagnose_design(design, diagnosands = diagnosands) 



diagnosis %>%
  reshape_diagnosis() %>%
  select(`Estimator Label`, Bias, RMSE, Coverage) %>%
  kable(digits = 3, booktabs = TRUE, caption = "Cluster random sampling design diagnosis")
