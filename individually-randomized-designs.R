# ---
# Individually-randomized designs
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

dag <- dagify(Y ~ Z + Q + U)

nodes <-
  tibble(
    name = c("Y", "Z", "U", "Q"),
    label = c("Y", "Z", "U", "Q"),
    annotation = c(
      "**Outcome**",
      "**Random assignment**",
      "**Unknown heterogeneity**",
      "**Measurement procedure**"),
    x = c(5, 1, 5, 1),
    y = c(2.5, 2.5, 4, 4),
    nudge_direction = c("S", "S", "N", "N"),
    data_strategy = c("unmanipulated", "assignment","unmanipulated", "measurement"),
    answer_strategy = "uncontrolled"
  )

ggdd_df <- make_dag_df(dag, nodes)

base_dag_plot %+% ggdd_df + coord_fixed(ylim = c(2, 4.5), xlim = c(0.5, 5.5))

eq_3.4_designer <-
  function(N, m, var_Y0, var_Y1, cov_Y0_Y1, mean_Y0, mean_Y1) {
    
    fixed_sample <-
      MASS::mvrnorm(
        n = N,
        mu = c(mean_Y0, mean_Y1),
        Sigma = matrix(c(var_Y0, cov_Y0_Y1, cov_Y0_Y1, var_Y1), nrow = 2),
        empirical = TRUE # this line makes the means and variances "exact" in the sample data
      ) %>%
      magrittr::set_colnames(c("Y_Z_0", "Y_Z_1"))
    
    declare_population(data = fixed_sample) +
      declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) +
      declare_assignment(m = m) +
      reveal_outcomes() +
      declare_estimator(Y ~ Z, estimand = "ATE")
    
  }

## designs <-
##   expand_design(designer = eq_3.4_designer,
##                 N = 100,
##                 m = seq(10, 90, 10),
##                 var_Y0 = 1,
##                 var_Y1 = 2,
##                 cov_Y0_Y1 = 0.5,
##                 mean_Y0 = 1.0,
##                 mean_Y1 = 1.75)
## 
## dx <- diagnose_designs(designs, sims = 100, bootstrap_sims = FALSE)
## 





gg_df <-
  dx %>%
  get_diagnosands() %>%
  pivot_longer(c(power, sd_estimate, coverage, bias))


dx %>%
  get_diagnosands() %>%
  filter(sd_estimate == min(sd_estimate))

eq_3.4 <- function(N, m, var_Y0, var_Y1, cov_Y0_Y1) {
  n <- N - m
  sqrt(1 / (N - 1) * (m / n  * var_Y0 + n / m * var_Y1 + 2 * cov_Y0_Y1))
}

theory_df <- 
  tibble(
    m = seq(10, 90, 1),
    sd_estimate = eq_3.4(N = 100, m = m, var_Y0 = 1, var_Y1 = 2, cov_Y0_Y1 = 0.5),
    bias = 0,
    power = 0.80,
    coverage = 0.95
  ) %>%
  pivot_longer(c(power, sd_estimate, coverage, bias))

g <- 
  ggplot(gg_df, aes(m, value)) +
  geom_point() +
  geom_line() +
  geom_line(data = theory_df, color = "purple") +
  theme_minimal() +
  facet_wrap(~name) +
  labs(y = "Diagnosand value", x = "Number of treated units (m)", title = "Simulating a two arm trial", subtitle = c("N = 100, var_Y0 = 1, var_Y1 = 2, cov_Y0_Y1 = 0.5, mean_Y0 = 1.0, mean_Y1 = 1.75"), caption = "Theoretical values and design targets in purple.")
g

## library(DeclareDesign)
## library(tidyverse)
## 
## fixed_pop <-
##   declare_population(
##     N = 50,
##     X1 = rbinom(N, 1, 0.5),
##     X2 = rbinom(N, 1, 0.5),
##     U = rnorm(N, sd = 0.1)
##   )()
## 
## 
## design <-
##   declare_population(data = fixed_pop) +
##   declare_potential_outcomes(Y ~ Z + X1 + X2 + U) +
##   declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
## 
## 
## 
##   d1 <-
##   design + declare_assignment(prob = 0.5) + declare_estimator(Y ~ Z, model = lm_robust, estimand = "ATE")
## d2 <-
##   design + declare_assignment(blocks = X1) + declare_estimator(Y ~ Z, model = lm_robust, estimand = "ATE")
## d3 <-
##   design + declare_assignment(blocks = paste0(X1, X2)) + declare_estimator(Y ~ Z, model = lm_robust, estimand = "ATE")
## d4 <-
##   design + declare_assignment(prob = 0.5) + declare_estimator(Y ~ Z + X1, model = lm_robust, estimand = "ATE")
## d5 <-
##   design + declare_assignment(prob = 0.5) + declare_estimator(Y ~ Z + X1:X2 , model = lm_robust, estimand = "ATE")
## 
## balance_test <-
##   declare_test(Z ~ X1:X2, model = lm_robust, model_summary = glance, label = "balance")
## 
## d1 <- d1 + balance_test
## d2 <- d2 + balance_test
## d3 <- d3 + balance_test
## d4 <- d4 + balance_test
## d5 <- d5 + balance_test
## 
## simulations <- simulate_designs(list(d1, d2, d3, d4, d5))
## 
## gg_df <-
##   simulations %>%
##   group_by(design_label, estimator_label) %>%
##   summarise(sd_estimate = sd(estimate),
##             power = mean(p.value <= 0.05)) %>%
##   pivot_wider(id_cols = design_label, names_from = estimator_label, values_from = c(sd_estimate, power)) %>%
##   transmute(design_label, sd_estimate = sd_estimate_estimator, power = power_estimator, prop_imbalance = 1 - power_balance)
## 
## 
## simulations <-
##   simulations %>%
##   mutate(design = factor(
##     design_label,
##     levels = paste0("design_", c(3, 2, 1, 4, 5)),
##     labels = c(
##       "Block on two covariates",
##       "Block on one covariate",
##       "No blocks or controls",
##       "Control for one covariate",
##       "Control for two covariates"
##     )
##   ))
## 
## 
## ggplot(simulations, aes(estimate)) +
##   geom_histogram(bins = 30) +
##   facet_wrap( ~ design, ncol = 5)
## 
