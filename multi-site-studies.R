# ---
# Multi-site studies
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = TRUE)

library(metafor)
library(car)

meta_re_estimator <- function(data){
  site_estimates_df <- data %>% 
    group_by(site) %>% 
    do(tidy(lm_robust(Y ~ Z, data = .))) %>% 
    filter(term == "Z") %>% 
    ungroup 
  
  meta_fit <- rma(estimate, std.error, data = site_estimates_df, method = "REML")
  
  with(meta_fit, tibble(
    estimate = as.vector(beta), std.error = se, p.value = pval, conf.low = ci.lb, conf.high = ci.ub))
}

post_strat_estimator <- function(data, pr_types_population) {
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

# need to have biased sampling to get bias here
# two kinds of populations, one in which the study type determines the subject types and you select on study type
#   a second kind where study type determines study shock 
#   in second type if you adjust for subject type then you will be able to unbiased recover global

multi_site_designer <- function(
  N_sites = 10,
  n_study_sites = 5,
  n_subjects_per_site = 1000,
  feasible_effect = 0,
  subject_type_effects = seq(from = -0.1, to = 0.1, length.out = 10),
  pr_types = c( # rows are sites, columns are types
    0.005, 0.005, 0.09, 0.15, 0.25, 0.1, 0, 0.1, 0.15, 0.15,
    0.1, 0.15, 0.15, 0.15, 0.25, 0.005, 0, 0.1, 0.09, 0.005,
    0.15, 0.15, 0.15, 0.005, 0.005, 0, 0.25, 0.09, 0.1, 0.1,
    0, 0.15, 0.005, 0.09, 0.005, 0.15, 0.25, 0.1, 0.1, 0.15,
    0.005, 0.1, 0.09, 0.25, 0.15, 0.15, 0.005, 0, 0.1, 0.15,
    0.005, 0.15, 0.25, 0.1, 0, 0.1, 0.005, 0.15, 0.09, 0.15,
    0.15, 0.15, 0.005, 0.25, 0.1, 0.15, 0.09, 0.005, 0.1, 0,
    0.25, 0.1, 0.15, 0, 0.005, 0.15, 0.15, 0.1, 0.005, 0.09,
    0.005, 0.1, 0.1, 0.15, 0, 0.25, 0.15, 0.09, 0.005, 0.15,
    0.005, 0.09, 0.15, 0.1, 0, 0.1, 0.15, 0.005, 0.25, 0.15)
) {
  declare_population(
    site = add_level(N = N_sites, feasible_site = sample(c(rep(1, 8), rep(0, 2)), N, replace = FALSE)),
    subject_types = add_level(
      N = 10,
      subject_type = 1:10,
      subject_type_effect = subject_type_effects,
      type_proportion = pr_types,
      N_subjects = ceiling(2500 * type_proportion)
    ),
    subjects = add_level(N = N_subjects, noise = rnorm(N))
  ) + 
    declare_potential_outcomes(Y ~ Z * (0.1 + subject_type_effect + feasible_effect * feasible_site) + noise) +
    declare_estimand(ATE_feasible = mean(Y_Z_1 - Y_Z_0), subset = feasible_site == FALSE) + # true effect for feasible sites
    declare_sampling(clusters = site, strata = feasible_site, strata_n = c(0, n_study_sites)) + 
    declare_sampling(strata = site, n = n_subjects_per_site) + 
    declare_assignment(blocks = site, prob = 0.5) + 
    declare_estimand(study_site_ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_estimator(handler = tidy_estimator(post_strat_estimator), pr_types_population = pr_types, label = "post-strat")
}

single_site_large_design <- multi_site_designer(n_study_sites = 1, n_subjects_per_site = 2500)

small_study_five_sites <- multi_site_designer(n_study_sites = 5, n_subjects_per_site = 500)





kable(get_diagnosands(diagnosis_small_large))

## stan_model <- "
## data {
##   int<lower=0> J;         // number of sites
##   real y[J];              // estimated effects
##   real<lower=0> sigma[J]; // s.e. of effect estimates
## }
## parameters {
##   real mu;
##   real<lower=0> tau;
##   real eta[J];
## }
## transformed parameters {
##   real theta[J];
##   real tau_sq = tau^2;
##   for (j in 1:J)
##     theta[j] = mu + tau * eta[j];
## }
## model {
##   target += normal_lpdf(eta | 0, 1);
##   target += normal_lpdf(y | theta, sigma);
## }
## "
## 
## stan_re_estimator <- function(data) {
##   site_estimates_df <- data %>%
##     group_by(site) %>%
##     do(tidy(lm_robust(Y ~ Z, data = .))) %>%
##     filter(term == "Z") %>%
##     ungroup
## 
##   J      <- nrow(site_estimates_df)
##   df     <- list(J = J, y = site_estimates_df$estimate, sigma = site_estimates_df$std.error)
##   fit    <- stan(model_code = stan_model, data = site_estimates_df)
##   fit_sm <- summary(fit)$summary
##   data.frame(estimate = fit_sm[,1][c("mu", "tau", "theta[1]", "prob_pos")])
## }
## 
## bayes_estimator <- declare_estimator(handler = stan_re_estimator)

small_study_five_sites_feasible_effects <- multi_site_designer(n_study_sites = 5, n_subjects_per_site = 500, feasible_effect  = -0.25)





kable(get_diagnosands(diagnosis_feasible_effects))
