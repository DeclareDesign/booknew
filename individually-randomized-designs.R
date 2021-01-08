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






gg_df <- 
simulations %>%
  select(sim_ID, estimator_label, estimate) %>%
  pivot_wider(names_from = estimator_label, values_from = estimate) %>%
  mutate(balanced = balance == 0 )

ggplot(gg_df, aes(DIM, fill = balanced)) +geom_histogram(position = "identity", bins = 30, alpha = 0.8) +
  scale_fill_manual(values = c(dd_light_gray, dd_dark_blue)) +
  dd_theme() +
  geom_vline(xintercept = 0.2, color = dd_dark_blue, linetype = "dashed") +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()) +
  annotate("text", x = 0.30, y = 50, label = "Assignments that\nexactly balance X", color = dd_dark_blue,hjust = 0) +
  annotate("text", x = -0.40, y = 20, label = "Assignments that do not\nexactly balance X", color = dd_light_gray,hjust = 1) +
  xlim(-1.5, 1.5) +
  xlab("Difference-in-means estimate")

gg_df2 <-
  gg_df %>%
  pivot_longer(cols = c(DIM, OLS),
               names_to = "estimator",
               values_to = "estimate") %>%
  mutate(estimator = factor(estimator, levels = c("OLS", "DIM")))



ggplot(gg_df2, aes(estimate, estimator, color = balanced, group = sim_ID)) +
  geom_line(data = (. %>% filter(balanced)), alpha = 0.9) +
  geom_line(data = (. %>% filter(!balanced)), alpha = 0.3) +
  scale_color_manual(values = c(dd_light_gray, dd_dark_blue)) +
  geom_vline(xintercept = 0.2, color = dd_dark_blue, linetype = "dashed") +
  annotate(
    "text",
    x = 0.20,
    y = 2.25,
    label = "Assignments that\nexactly balance X",
    color = dd_dark_blue
  ) +
  annotate(
    "text",
    x = -0.40,
    y = 1.25,
    label = "Assignments that do not\nexactly balance X",
    color = dd_light_gray,
    hjust = 1
  ) +
  xlim(-1.5, 1.5) +
  xlab("Average treatment effect estimate") +
  theme(legend.position = "none")


fixed_pop <-
  fabricate(
    N = 100,
    X = rbinom(N, 1, 0.5),
    U = rnorm(N)
  )

design <-
  declare_population(data = fixed_pop) +
  declare_potential_outcomes(Y ~ 0.2*Z + X + U) +
  declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))

# Data strategies
complete_assignment <- 
  declare_assignment(Z = complete_ra(N = N), handler = fabricate) + 
  declare_reveal()
blocked_assignment <- 
  declare_assignment(Z = block_ra(blocks = X), handler = fabricate) + 
  declare_reveal()

# Answer strategies
unadjusted_estimator <- declare_estimator(Y ~ Z, estimand = "ATE")
adjusted_estimator <- declare_estimator(Y ~ Z + X, model = lm_robust, estimand = "ATE")

design_1 <- design + complete_assignment + unadjusted_estimator
design_2 <- design + blocked_assignment + unadjusted_estimator
design_3 <- design + complete_assignment + adjusted_estimator
design_4 <- design + blocked_assignment + adjusted_estimator

## diagnose_designs(list(design_1, design_2, design_3, design_4))





gg_df <-
  simulations %>%
  group_by(design_label) %>%
  summarise(sd_estimate = sd(estimate), avg_std_error = mean(std.error), .groups = "drop") %>%
  transmute(`Data Strategy` = if_else(design_label %in% c("design_1", "design_3"), 
                                   "Complete Random Assignment",
                                   "Block Random Assignment"),
         `Answer Strategy` = if_else(design_label %in% c("design_1", "design_2"), 
                                   "Difference-in-means",
                                   "Covariate Adjustment"),
         `True Standard Error` = sd_estimate,
         `Average Estimated Standard Error` = avg_std_error)
kable(gg_df, digits = 3)
