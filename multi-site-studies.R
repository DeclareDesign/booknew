# ---
# Multi-site studies
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

library(metafor)
library(car)

random_effects_meta_analysis <- function(data){
  site_estimates_df <- data %>%
    group_by(site) %>%
    do(tidy(lm_robust(Y ~ Z, data = .))) %>%
    filter(term == "Z") %>%
    ungroup

  meta_fit <- rma(estimate, std.error, data = site_estimates_df, method = "REML")

  with(meta_fit, tibble(
    estimate = as.vector(beta), std.error = se, p.value = pval, conf.low = ci.lb, conf.high = ci.ub))
}

post_stratification <- function(data, pr_types_population) {
  if(length(unique(data$site)) > 1) {
    fit <- lm_robust(Y ~ Z*as.factor(subject_type) + as.factor(site), data = data)
    tidy(fit)
  } else {
    fit <- lm_robust(Y ~ Z*as.factor(subject_type), data = data)
  }
  
  alpha <- .05
  
  lh_fit <- try({ linearHypothesis(
    fit, 
    hypothesis.matrix = paste(paste(paste(pr_types_population[91:100][-1], "*", matchCoefs(fit, "Z"), sep = ""), collapse = " + "), " = 0"), 
    level = 1 - alpha) })
  
  if(!inherits(lh_fit, "try-error")) {
    tibble(estimate = drop(attr(lh_fit, "value")), 
           std.error = sqrt(diag(attr(lh_fit, "vcov"))),
           df = fit$df.residual, 
           statistic = estimate / std.error, 
           p.value = 2 * pt(abs(statistic), df, lower.tail = FALSE),
           conf.low = estimate + std.error * qt(alpha / 2, df),
           conf.high = estimate + std.error * qt(1 - alpha / 2, df))
  } else {
    tibble(error = TRUE)
  }
}

single_site_design <- 
  declare_population(
    site = add_level(
      N = 10, 
      feasible_site = sample(c(rep(1, 8), rep(0, 2)), N, replace = FALSE),
      study_effect = seq(from = -0.1, to = 0.1, length.out = N) 
    ),
    subjects = add_level(N = n_subjects_per_site, noise = rnorm(N))
  ) + 
  declare_potential_outcomes(Y ~ Z * (0.1 + study_effect + 0.025 * feasible_site) + noise) +
  declare_estimand(PATE = mean(Y_Z_1 - Y_Z_0)) + 
  declare_estimand(PATE_feasible = mean(Y_Z_1 - Y_Z_0), subset = feasible_site == TRUE) + 
  declare_sampling(clusters = site, strata = feasible_site, strata_n = c(0, 1)) + 
  declare_sampling(strata = site, n = n_subjects_per_site) + 
  declare_assignment(blocks = site, prob = 0.5) + 
  declare_estimator(Y ~ Z, model = lm_robust)

multi_site_design <- 
  declare_population(
    site = add_level(
      N = 10, 
      feasible_site = sample(c(rep(1, 8), rep(0, 2)), N, replace = FALSE),
      study_effect = seq(from = -0.1, to = 0.1, length.out = N) 
    ),
    subjects = add_level(N = n_subjects_per_site, noise = rnorm(N))
  ) + 
  declare_potential_outcomes(Y ~ Z * (0.1 + study_effect + 0.025 * feasible_site) + noise) +
  declare_estimand(PATE = mean(Y_Z_1 - Y_Z_0)) + 
  declare_estimand(PATE_feasible = mean(Y_Z_1 - Y_Z_0), subset = feasible_site == TRUE) + 
  declare_sampling(clusters = site, strata = feasible_site, strata_n = c(0, n_study_sites)) + 
  declare_sampling(strata = site, n = n_subjects_per_site) + 
  declare_assignment(blocks = site, prob = 0.5) + 
  declare_estimator(handler = label_estimator(random_effects_meta_analysis), label = "random-effects")

single_site_large_design <- redesign(single_site_design, n_subjects_per_site = 2500)

small_study_five_sites <- redesign(multi_site_design, n_study_sites = 5, n_subjects_per_site = 500)





kable(diagnosis_small_large %>% reshape_diagnosis %>% select(`Design Label`, `Estimand Label`, Bias, RMSE), booktabs = TRUE)
