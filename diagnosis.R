# ---
# Diagnosis
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R
library(ggforce)
library(ggridges)

design <-
  declare_population(N = 100, 
                     Tau = rnorm(N, mean = 0.1, sd = 0.1),
                     U = rnorm(N, 0, 0.2)) +
  declare_potential_outcomes(Y ~  Tau * Z + U) + 
  declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(prob = 0.5) +
  declare_estimator(Y ~ Z, estimand = "ATE")

## run_design(design)

design <-
  declare_population(N = 100, U = rnorm(N, 0, 0.2)) +
  declare_potential_outcomes(Y ~ rnorm(N, mean = 0.1, sd = 0.1) * Z + U) + 
  declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment() +
  declare_estimator(Y ~ Z, estimand = "ATE", model = lm_robust)

# diagnose_design(design, sims = 250)

set.seed(343)
simulations <- 
  simulate_design(design, sims = 10)   %>%
  mutate(significant = p.value <= 0.05)


gg_df <-
  simulations %>%
  group_by(sim_ID) %>%
  nest %>%
  mutate(densities = lapply(data, function(df) {
    with(df, tibble(
      x = seq(-2, 2, 0.0001),
      density = dnorm(x, mean = estimate, sd = std.error)
    ))
  })) %>%
  unnest(cols = c(data, densities)) %>%
  group_by(sim_ID)


gg_df <- 
  gg_df %>%
  mutate(
    
    low_cut = case_when(
      estimate < 0 ~ x < -2*abs(estimate),
      estimate > 0 ~ x <= 0),
    
    high_cut = case_when(estimate < 0 ~ x >= 0,
                         estimate > 0 ~ x >= 2*abs(estimate) ),
    
    area_under = case_when(low_cut ~ "low",
                           high_cut ~ "high",
                           TRUE ~ "middle")
  ) 

simulation_1 <- simulations %>% filter(sim_ID == 1)

simulation_1 %>% 
  select(estimand, estimate, std.error, conf.low, conf.high, p.value) %>%
  knitr::kable(digits = 3)

gg_df_1 <- gg_df %>% filter(sim_ID == 1)

ggplot(simulation_1, aes(x = estimate, y = as.factor(sim_ID)), color = dd_light_blue) +
  # geom_rect(xmin = -10, xmax = 0, ymin = -100, ymax = 100, fill = gray(0.96), size = 0) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.075,
                 size = 0.25,
                 color = gray(0.5)) +
  geom_point(size = 1.5, color = gray(0.5)) +
  geom_point(
    aes(x = estimand),
    size = 1.5,
    fill = NA,
    color = gray(0.5),
    pch = 24,
    position = position_nudge(y = -0.1)
  ) +
  # first plot the density for p-values
  geom_ridgeline(
    data = gg_df_1,
    size = 0,
    aes(x = x, height = density, fill = area_under),
    scale = 0.05,
    min_height = 0.01
  ) +
  # then the density lines (to make it plot correctly and not be cut off)
  geom_ridgeline(
    data = gg_df_1,
    size = 0.1,
    aes(x = x, height = density, fill = NA),
    scale = 0.05,
    min_height = 0.01,
    color = dd_light_blue,
  ) +
  theme_void() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  ) +
  coord_cartesian(xlim = c(-0.1, 0.25)) +
  geom_vline(
    xintercept = 0,
    color = gray(0.5),
    linetype = "dashed",
    alpha = 0.5
  ) +
  # geom_vline(
  #   xintercept = 0.1,
  #   color = dd_light_blue,
  #   linetype = "dotted",
  #   alpha = 0.5
  # ) +
  scale_fill_manual(name = "Probability",
                    values = c(dd_light_blue_alpha, dd_light_blue_alpha, NA)) +
  labs(x = "Estimated sampling distribution (from point estimate and standard error)")

ggplot(simulations, aes(x = estimate, y = as.factor(sim_ID)), color = dd_light_blue) +
  # geom_rect(xmin = -10, xmax = 0, ymin = -100, ymax = 100, fill = gray(0.96), size = 0) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.075,
                 size = 0.25,
                 color = gray(0.5)) +
  geom_point(size = 1.5, color = gray(0.5)) +
  geom_point(
    aes(x = estimand),
    size = 1.5,
    fill = NA,
    color = gray(0.5),
    pch = 24,
    position = position_nudge(y = -0.1)
  ) +
  # first plot the density for p-values
  geom_ridgeline(
    data = gg_df,
    size = 0,
    aes(x = x, height = density, fill = area_under),
    scale = 0.05,
    min_height = 0.01
  ) +
  # then the density lines (to make it plot correctly and not be cut off)
  geom_ridgeline(
    data = gg_df,
    size = 0.1,
    aes(x = x, height = density, fill = NA),
    scale = 0.05,
    min_height = 0.01,
    color = dd_light_blue,
  ) +
  theme_void() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  ) +
  coord_cartesian(xlim = c(-0.1, 0.25)) +
  geom_vline(
    xintercept = 0,
    color = gray(0.5),
    linetype = "dashed",
    alpha = 0.5
  ) +
  geom_vline(
    xintercept = 0.1,
    color = dd_light_blue,
    linetype = "dotted",
    alpha = 0.5
  ) +
  scale_fill_manual(name = "Probability",
                    values = c(dd_light_blue_alpha, dd_light_blue_alpha, NA)) +
  labs(x = "Estimated sampling distribution (from point estimate and standard error)")

## diagnosis <- diagnose_design(design, sims = 1000,
##                              diagnosands = declare_diagnosands(
##                                select = c("bias", "sd_estimate", "rmse", "power", "coverage")))





diagnosis

knitr::include_app('https://gblair.shinyapps.io/diagnosis/', height = '400px')

n <- 50

points_df <- tibble(rho = sqrt(runif(n)),
                    theta = runif(n, 0, 2*pi),
                    x = rho * cos(theta),
                    y = rho * sin(theta)
)

summary_df <- points_df %>% 
  summarize(
    bias = round(mean(sqrt(x^2 + y^2)), 2),
    mse = round(mean(x^2 + y^2), 2),
    var = round(mean((x - mean(x))^2 + (y - mean(y))^2), 3)
  )

unbiased_lowprecision <- ggplot() + 
  geom_circle(
    data = tibble(
      x0 = rep(0, 5),
      y0 = rep(0, 5),
      r = rev(seq(0.1, 1, length.out = 5))
    ), 
    aes(x0 = x0, y0 = y0, r = r, fill = fct_rev(as.factor(r))),
    col = gray(0.25), lwd = 0.25, alpha = 0.5) +
  geom_point(data = points_df, aes(x, y), size = 0.5) + 
  scale_fill_manual(values = rev(c("yellow", "red", "cyan", "black", "white"))) + 
  coord_fixed() + 
  dd_theme() + 
  xlab("") + ylab("") + 
  ggtitle("Unbiased, imprecise", subtitle = glue::glue("Variance = {summary_df$var}\nBias = {summary_df$bias}\nRMSE = {summary_df$mse}")) + 
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

n <- 50

points_df <- tibble(
  rho = sqrt(runif(n, min = 0, max = 0.015)),
  theta = runif(n, 0, 2*pi),
  x = rho * cos(theta),
  y = rho * sin(theta)
)

summary_df <- points_df %>% 
  summarize(
    bias = round(mean(sqrt(x^2 + y^2)), 2),
    mse = round(mean(x^2 + y^2), 2),
    var = round(mean((x - mean(x))^2 + (y - mean(y))^2), 3)
  )

unbiased_highprecision <- ggplot() + 
  geom_circle(
    data = tibble(
      x0 = rep(0, 5),
      y0 = rep(0, 5),
      r = rev(seq(0.1, 1, length.out = 5))
    ), 
    aes(x0 = x0, y0 = y0, r = r, fill = fct_rev(as.factor(r))),
    col = gray(0.25), lwd = 0.25, alpha = 0.5) +
  geom_point(data = points_df, aes(x, y), size = 0.5) + 
  scale_fill_manual(values = rev(c("yellow", "red", "cyan", "black", "white"))) + 
  coord_fixed() + 
  dd_theme() + 
  xlab("") + ylab("") + 
  ggtitle("Unbiased, imprecise", subtitle = glue::glue("Variance = {summary_df$var}\nBias = {summary_df$bias}\nRMSE = {summary_df$mse}")) + 
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

n <- 50

points_df <- tibble(
  rho = sqrt(runif(n, min = 0, max = 0.015)),
  theta = runif(n, 0, 2*pi),
  x = rho * cos(theta) + 0.5,
  y = rho * sin(theta) - 0.2
)

summary_df <- points_df %>% 
  summarize(
    bias = round(mean(sqrt(x^2 + y^2)), 2),
    mse = round(mean(x^2 + y^2), 2),
    var = round(mean((x - mean(x))^2 + (y - mean(y))^2), 3)
  )

biased_highprecision <- ggplot() + 
  geom_circle(
    data = tibble(
      x0 = rep(0, 5),
      y0 = rep(0, 5),
      r = rev(seq(0.1, 1, length.out = 5))
    ), 
    aes(x0 = x0, y0 = y0, r = r, fill = fct_rev(as.factor(r))),
    col = gray(0.25), lwd = 0.25, alpha = 0.5) +
  geom_point(data = points_df, aes(x, y), size = 0.5) + 
  scale_fill_manual(values = rev(c("yellow", "red", "cyan", "black", "white"))) + 
  coord_fixed() + 
  dd_theme() + 
  xlab("") + ylab("") + 
  ggtitle("Unbiased, imprecise", subtitle = glue::glue("Variance = {summary_df$var}\nBias = {summary_df$bias}\nRMSE = {summary_df$mse}")) + 
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

library(patchwork)

unbiased_lowprecision + unbiased_highprecision + biased_highprecision

n <- 50

points_df <- tibble(
  rho = sqrt(runif(n, min = 0, max = 0.015)),
  theta = runif(n, 0, 2*pi),
  x = rho * cos(theta),
  y = rho * sin(theta)
)

summary_df <- points_df %>% 
  summarize(
    bias_1 = round(mean(sqrt(x^2 + y^2)), 2),
    mse_1 = round(mean(x^2 + y^2), 2),
    var_1 = round(mean((x - mean(x))^2 + (y - mean(y))^2), 3),
    
    bias_2 = round(mean(sqrt((x-0.25)^2 + y^2)), 2),
    mse_2 = round(mean((x-0.25)^2 + y^2), 2),
    var_2 = round(mean(((x-0.25) - mean(x-0.25))^2 + (y - mean(y))^2), 3)
  )

circle_df <- 
  bind_rows(
    "left" = tibble(
      x0 = rep(0, 5),
      y0 = rep(0, 5),
      r = rev(seq(0.1, 1, length.out = 5))
    ),
    "right" = tibble(
      x0 = rep(0.35, 5),
      y0 = rep(0, 5),
      r = rev(seq(0.1, 1, length.out = 5))
    ),
    .id = "side"
  ) %>% 
  arrange(desc(r), side)

dual_estimands <- ggplot() + 
  geom_circle(
    data = circle_df, 
    aes(x0 = x0, y0 = y0, r = r, fill = fct_rev(as.factor(r))),
    col = gray(0.25), lwd = 0.25) +
  geom_point(data = points_df, aes(x, y), size = 0.5) + 
  scale_fill_manual(values = rev(c("yellow", "red", "cyan", "black", "white"))) + 
  coord_fixed() + 
  dd_theme() + 
  xlab("") + ylab("") + 
  ggtitle("Diagnosands depend on the estimand", subtitle = glue::glue("Variance = {summary_df$var_1} (left), {summary_df$var_2} (right)\nBias = {summary_df$bias_1} (left), {summary_df$bias_2} (right)\nRMSE = {summary_df$mse_1} (left), {summary_df$mse_2} (right)")) + 
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )
dual_estimands

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





## # Summarize simulations ---------------------------------------------------
## 
## reshaped_simulations <-
##   simulations_one_significant_not_other %>%
##   transmute(ATE,
##             sim_ID,
##             estimator_label,
##             estimate,
##             conf.high,
##             conf.low,
##             significant = p.value < 0.05) %>%
##   pivot_wider(id_cols = c("ATE", "sim_ID"), names_from = "estimator_label", values_from = c("estimate", "conf.high", "conf.low", "significant"))
## 
## 
## # Plot 1 ------------------------------------------------------------------
## 
## gg_df <-
##   reshaped_simulations %>%
##   group_by(ATE) %>%
##   summarize(`Significant for one group but not the other` = mean(xor(significant_CATE_0, significant_CATE_1)),
##             `Difference in subgroup effects is significant` = mean(significant_interaction)) %>%
##   gather(condition, power, -ATE)
## 
## ggplot(gg_df, aes(ATE, power, color = condition)) +
##   geom_point() +
##   geom_line() +
##   geom_label(data = (. %>% filter(ATE == 0.2)),
##              aes(label = condition),
##              nudge_y = 0.02,
##              family = "Palatino") +
##   dd_theme() +
##   scale_color_manual(values = c("red", "blue")) +
##   theme(legend.position = "none") +
##   labs(
##     x = "True constant effect size",
##     y = "Probability of result (akin to statistical power)"
##   )

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
