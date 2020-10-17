# ---
# Choosing an answer strategy
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = TRUE)

ATE <- 0.0

design <- 
  declare_population(N = 1000,
                     binary_covariate = rbinom(N, 1, 0.5),
                     normal_error = rnorm(N)) +
  # crucial step in POs: effects are not heterogeneous
  declare_potential_outcomes(Y ~ ATE * Z + normal_error) +
  declare_assignment(prob = 0.5) +
  declare_estimator(Y ~ Z, subset = (binary_covariate == 0), label = "CATE(0)") + 
  declare_estimator(Y ~ Z, subset = (binary_covariate == 1), label = "CATE(1)") +
  declare_estimator(Y ~ Z * binary_covariate, 
                    model = lm_robust, term = "Z:binary_covariate", label = "Interaction")




g1 <- ggplot(data = estimates %>% filter(term == "Z"), aes(estimator_label, estimate)) + 
  geom_point() + 
  geom_errorbar(aes(x = estimator_label, ymin = conf.low, ymax = conf.high), width = 0.2) + 
  ylab("Estimate (95% confidence interval)") +
  geom_hline(yintercept = 0, lty = "dashed") +
  ggtitle("Visualization A") +
  dd_theme() + 
  theme(axis.title.x = element_blank())

g2 <- ggplot(data = estimates, aes(estimator_label, estimate)) + 
  geom_point() + 
  geom_errorbar(aes(x = estimator_label, ymin = conf.low, ymax = conf.high), width = 0.2) + 
  ylab("Estimate (95% confidence interval)") +
  geom_hline(yintercept = 0, lty = "dashed") +
  ggtitle("Visualization B") +
  dd_theme() + 
  theme(axis.title.x = element_blank())

g1 + g2

# sweep across all ATEs from 0 to 0.5
designs <- redesign(design, ATE = seq(0, 0.5, 0.05))





# Summarize simulations ---------------------------------------------------

reshaped_simulations <-
  simulations_one_significant_not_other %>%
  transmute(ATE,
            sim_ID,
            estimator_label,
            estimate,
            conf.high,
            conf.low,
            significant = p.value < 0.05) %>%
  pivot_wider(id_cols = c("ATE", "sim_ID"), names_from = "estimator_label", values_from = c("estimate", "conf.high", "conf.low", "significant"))


# Plot 1 ------------------------------------------------------------------

gg_df <- 
  reshaped_simulations %>%
  group_by(ATE) %>%
  summarize(`Significant for one group but not the other` = mean(xor(significant_CATE_0, significant_CATE_1)),
            `Difference in subgroup effects is significant` = mean(significant_interaction)) %>%
  gather(condition, power, -ATE)

ggplot(gg_df, aes(ATE, power, color = condition)) +
  geom_point() +
  geom_line() +
  geom_label(data = (. %>% filter(ATE == 0.2)),
             aes(label = condition),
             nudge_y = 0.02,
             family = "Palatino") +
  dd_theme() +
  scale_color_manual(values = c("red", "blue")) +
  theme(legend.position = "none") +
  labs(
    x = "True constant effect size",
    y = "Probability of result (akin to statistical power)"
  )

report_lower_p_value <- function(data){
  fit_nocov <- lm_robust(Y ~ Z, data)
  fit_cov <- lm_robust(Y ~ Z + X, data)
  
  # select fit with lower p.value on Z
  if(fit_cov$p.value[2] < fit_nocov$p.value[2]){
    fit_selected <- fit_cov
  } else {
    fit_selected <- fit_nocov
  }
  fit_selected %>% tidy %>% filter(term == "Z")
}

design <-
  declare_population(    
    N = 100, X = rbinom(N, 1, 0.5), u = rnorm(N)
  ) + 
  declare_potential_outcomes(Y ~ 0.25 * Z + 10 * X + u) + 
  declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) + 
  declare_assignment(prob = 0.5) + 
  declare_reveal(Y, Z) + 
  declare_estimator(Y ~ Z, model = lm_robust, label = "nocov", estimand = "ATE") + 
  declare_estimator(Y ~ Z, model = lm_robust, label = "cov", estimand = "ATE") + 
  declare_estimator(
    handler = label_estimator(report_lower_p_value),
    label = "select-lower-p-value",
    estimand = "ATE") 

diags <- diagnose_design(design, sims = sims)

kable(get_diagnosands(diags))

bivariate_correlation_decision <- function(data) {
  fit <- lm_robust(y2 ~ y1, data) %>% tidy %>% filter(term == "y1")
  tibble(decision = fit$p.value <= 0.05)
}

interacted_correlation_decision <- function(data) {
  fit <- lm_robust(y2 ~ y1 + x, data) %>% tidy %>% filter(term == "y1")
  tibble(decision = fit$p.value <= 0.05)
}

robustness_check_decision <- function(data) {
  main_analysis <- bivariate_correlation_decision(data)
  robustness_check <- interacted_correlation_decision(data)
  tibble(decision = main_analysis$decision == TRUE & robustness_check$decision == TRUE)
}

robustness_checks_design <- 
  declare_population(
    N = 100,
    x = rnorm(N),
    y1 = rnorm(N),
    y2 = 0.15 * y1 + 0.01 * x + rnorm(N)
  ) +
  declare_estimand(y1_y2_are_related = TRUE) + 
  declare_estimator(handler = label_estimator(bivariate_correlation_decision), label = "bivariate") + 
  declare_estimator(handler = label_estimator(robustness_check_decision), label = "robustness-check")

decision_diagnosis <- declare_diagnosands(correct = mean(decision == estimand), keep_defaults = FALSE)

diag <- diagnose_design(robustness_checks_design, sims = sims, diagnosands = decision_diagnosis)

robustness_checks_design <-
  robustness_checks_design +
  declare_estimator(handler = label_estimator(interacted_correlation_decision), label = "interacted")

robustness_checks_design_dgp2 <- replace_step(
  robustness_checks_design,
  step = 1,
  new_step = 
    declare_population(
      N = 100,
      x = rnorm(N),
      y1 = rnorm(N),
      y2 = 0.15 * y1 + 0.01 * x + 0.05 * y1 * x + rnorm(N)
    )
)

robustness_checks_design_dgp3 <- replace_step(
  robustness_checks_design,
  step = 1,
  new_step = 
    declare_population(
      N = 100,
      x = rnorm(N),
      y1 = 0.15 * x + rnorm(N),
      y2 = 0.15 * x + rnorm(N)
    )
)

robustness_checks_design_dgp3 <- replace_step(
  robustness_checks_design_dgp3, 
  step = 2,
  new_step = declare_estimand(y1_y2_are_related = FALSE)
)

decision_diagnosis <- declare_diagnosands(correct = mean(decision == estimand), keep_defaults = FALSE)

diag <- diagnose_design(
  robustness_checks_design, robustness_checks_design_dgp2, robustness_checks_design_dgp3, 
  sims = sims, diagnosands = decision_diagnosis)
