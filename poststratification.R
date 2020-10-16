# ---
# Poststratification
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
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

fixed_population <- declare_population(
  group = add_level(N = 2, X = c(0,1), population_n = c(100,50)),
  individual = add_level(N = population_n, Y = X + sample(0:1, N, replace = TRUE)))()

design <- 
  declare_population(data = fixed_population) + 
  declare_estimand(Ybar = mean(Y)) + 
  declare_sampling(strata_n = c(10,10), strata = X) + 
  declare_step(handler = group_by, groups = X) +
  declare_step(handler = mutate, 
               sample_n = n(), 
               weight = population_n / sample_n) +
  declare_estimator(Y ~ 1, term = "(Intercept)", model = lm_robust, weights = weight, estimand = "Ybar")

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

delaware_senate_districts_df <- read_rds("data/delaware.RDS")
kable(head(delaware_senate_districts_df))

## 
## # US population
## delaware_population_df <- fabricate(
##   data = delaware_senate_districts_df,
##   individuals = add_level(
##     N = population_size,
##     race_white = rbinom(N, 1, prob = prop_white),
##     race_black = rbinom(N, 1, prob = prop_black),
##     race_asian = rbinom(N, 1, prob = prop_black),
##     race_hispanic_other = rbinom(N, 1, prob = prop_hispanic_other),
##     pid_republican = rbinom(N, 1, prob = prop_republican),
##     pid_democrat = rbinom(N, 1, prob = prop_democrat)
##   )
## ) %>%
##   select(-starts_with("prop_"), -population_size)
## 
## # population weights for MRP
## mrp_weights <- delaware_population_df %>%
##   group_by(district, race_white, race_black, race_asian, race_hispanic_other, pid_republican, pid_democrat) %>%
##   summarize(n_cell = n()) %>%
##   group_by(district) %>%
##   mutate(proportion_cell = n_cell/sum(n_cell)) %>%
##   select(-n_cell) %>%
##   ungroup
## 
## delaware_population_df <- mrp_weights %>%
##   select(district, proportion_cell) %>%
##   right_join(delaware_population_df)
## 
## # Lax and Philips APSR 2009
## # Policies are coded dichotomously, 1 for the progay policy and 0 otherwise: Adoption (9 states allow second-parent adoption in all jurisdictions)
## 
## design <-
##   declare_population(
##     data = delaware_population_df,
## 
##     districts = modify_level(district_effect = rnorm(N)),
## 
##     individuals = modify_level(
##       noise = rnorm(N, mean = district_effect),
##       policy_support = rbinom(N, 1, prob = pnorm(
##         0.25 + 0.2 * race_white - 0.1 * race_black - 0.2 * race_hispanic_other -
##           0.1 * pid_democrat + 0.15 * pid_republican + noise))
##     )
##   ) +
## 
##   declare_estimand(handler = function(data) {
##     data %>%
##       group_by(district) %>%
##       summarize(estimand = mean(policy_support)) %>%
##       ungroup %>%
##       mutate(estimand_label = "mean_policy_support")
##   }) +
## 
##   declare_sampling(n = 500) +
## 
##   declare_estimator(handler = tidy_estimator(function(data) {
##     data %>%
##       group_by(district) %>%
##       summarize(estimate = mean(policy_support))
##   }), label = "strata_means", estimand = "mean_policy_support") +
## 
##   # this estimator owes code to https://timmastny.rbind.io/blog/multilevel-mrp-tidybayes-brms-stan/
##   declare_estimator(handler = tidy_estimator(function(data) {
## 
##     model_fit <- glmer(
##       formula = policy_support ~ race_white + race_black + race_asian + race_hispanic_other +
##         pid_democrat + pid_republican + (1 | district),
##       data = data, family = binomial(link = "logit"))
## 
##     data %>%
##       mutate(
##         support_predicted =
##           prediction(model_fit, data = ., allow.new.levels = TRUE, type = "response"),
##         support_predicted_weighted = support_predicted * proportion_cell
##       ) %>%
##       group_by(district) %>%
##       summarize(estimate = sum(support_predicted_weighted))
## 
##   }), label = "mrp_mle", estimand = "mean_policy_support")
## 
## dat <- draw_data(design)
## 
## draw_estimates(design)
## 
## sims <- simulate_design(design, sims = 3)
## 
## diag <- diagnose_design(design, sims = 100, diagnosands = declare_diagnosands(select = bias), add_grouping_variables = "state")





kable(head(simulations_pilot))
