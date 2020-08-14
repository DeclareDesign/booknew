# ---
# Difference-in-differences
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = T)


design <- 
  declare_population(
    unit = add_level(N = 2, X = rnorm(N, sd = 0.5)),
    period = add_level(N = 2, nest = FALSE),
    unit_period = cross_levels(by = join(unit, period), U = rnorm(N, sd = 0.01))
  ) + 
  declare_potential_outcomes(Y ~ X + 0.5 * as.numeric(period) + Z + U) + 
  declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0), subset = period == 2) + 
  declare_step(Z = X == max(X), handler = mutate) + 
  declare_reveal(Y = if_else(Z == 0 | period == 1, Y_Z_0, Y_Z_1), handler = mutate) +
  declare_estimator(Y ~ Z * period, model = lm_robust, se_type = "none")

dag <- dagify(Y ~ X + period + Z + U,
              Z ~ X)


nodes <-
  tibble(
    name = c("U", "X", "period", "Z", "Y"),
    label = c("U", "X", "T", "Z", "Y"),
    annotation = c(
      "**Unknown heterogeneity**",
      "**Unit effect**",
      "**Time period**",
      "**Treatment assignment**",
      "**Outcome variable**"
    ),
    x = c(5, 1, 1, 3, 5),
    y = c(1.5, 3.5, 1, 2.5, 2.5),
    nudge_direction = c("S", "N", "S", "S", "N"),
    answer_strategy = "uncontrolled"
  )

ggdd_df <- make_dag_df(dag, nodes, design)

base_dag_plot %+% ggdd_df



# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

N_units <- 2
N_time_periods <- 2

two_period_two_group_design <- 
  
  declare_population(
    units = add_level(N = N_units, unit_shock = rnorm(N, sd = 0.5)),
    periods = add_level(N = N_time_periods, nest = FALSE,
                        time = (1:N_time_periods) - N_time_periods + 1),
    unit_period = cross_levels(by = join(units, periods), unit_time_shock = rnorm(N, sd = 0.01))
  ) + 
  
  # internal note: the unbiasedness obtains whether or not there is a unit-time shock
  declare_potential_outcomes(
    Y_Z_0 = unit_shock + 0.5 * time + unit_time_shock, # common pretreatment trend
    Y_Z_1 = Y_Z_0 + 1) +
  
  declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0), subset = time == 1) + 
  
  declare_assignment(Z = unit_shock == max(unit_shock), handler = mutate) + 
  
  declare_reveal(
    Y = case_when(Z == 0 | time < 1 ~ Y_Z_0, TRUE ~ Y_Z_1), handler = mutate) +
  
  declare_estimator(estimate = (mean(Y[Z == 1 & time == 1]) - mean(Y[Z == 0 & time == 1])) -
                      (mean(Y[Z == 1 & time == 0]) - mean(Y[Z == 0 & time == 0])),
                    estimator_label = "DiD", handler = summarize, label = "DiD") +
  
  declare_estimator(estimate = mean(Y[Z == 1 & time == 1]) - mean(Y[Z == 1 & time == 0]),
                    estimator_label = "Diff", handler = summarize, label = "Over-Time") +
  
  declare_estimator(estimate = mean(Y[Z == 1 & time == 1]) - mean(Y[Z == 0 & time == 1]),
                    estimator_label = "DiM", handler = summarize, label = "DiM")





kable(get_diagnosands(diagnosis_two_period_two_group))

# add an additional pretreatment time period in order to visually test for parallel pre-trends
three_period_two_group_design <- redesign(two_period_two_group_design, N_time_periods = 3)

draw_data(three_period_two_group_design) %>% 
  group_by(Z, time) %>% 
  summarize(Y = mean(Y)) %>% 
  mutate(Z_color = factor(Z, levels = c(FALSE, TRUE), labels = c("Untreated", "Treated"))) %>% 
  ggplot(aes(time, Y, color = Z_color)) + 
  geom_line() + 
  scale_color_discrete("") +
  scale_x_discrete("Time", limits = c(-1, 0, 1))

N_units <- 20
N_time_periods <- 20

multi_period_design <- 
  
  declare_population(
    units = add_level(N = N_units, 
                      unit_shock = rnorm(N), 
                      unit_treated = 1*(unit_shock > median(unit_shock)), 
                      unit_treatment_start = 
                        sample(2:(N_time_periods - 1) - N_time_periods + 1, N, replace = TRUE)),
    periods = add_level(N = N_time_periods, nest = FALSE, 
                        time = (1:N_time_periods) - N_time_periods + 1),
    unit_period = cross_levels(by = join(units, periods),
                               noise = rnorm(N), 
                               pretreatment = 1*(time < unit_treatment_start))
  ) + 
  
  declare_potential_outcomes(
    Y_Z_0 = unit_shock + 0.5 * time + noise, # common pretreatment trend
    Y_Z_1 = Y_Z_0 + 0.2) +
  
  declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0), subset = time == 1) + 
  
  declare_assignment(Z = 1*(unit_treated & pretreatment == FALSE), handler = fabricate) + 
  declare_reveal(Y, Z) + 
  
  declare_estimator(Y ~ Z + time, fixed_effects = ~ units + periods, 
                    model = lm_robust, label = "twoway-fe", estimand = "ATE") 
  





kable(get_diagnosands(diagnosis_multi_period_multi_group))
