# ---
# Causal inquiries about a single unit
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

library(Synth)
# library(CausalQueries)

dag <- dagify(Y ~ X + D + U,
              D ~ X)
nodes <-
  tibble(
    name = c("X", "U", "D", "Y"),
    label = c("X", "U", "D", "Y"),
    annotation = c(
      "**Control variable**",
      "**Unknown heterogeneity**",
      "**Treatment**",
      "**Outcome variable**"
    ),
    x = c(1, 5, 3, 5),
    y = c(4, 4, 2.5, 2.5),
    nudge_direction = c("N", "N", "W", "S"),
    data_strategy = "unmanipulated",
    answer_strategy = c("controlled",  "uncontrolled", "uncontrolled", "uncontrolled")
  )


ggdd_df <- make_dag_df(dag, nodes)

base_dag_plot %+% ggdd_df



## query_model(model,
##             query = "Y[X=1] > Y[X=0]",
##             given = list("Y==1 & X==1",
##                          "Y==1 & X==1 & M==0",
##                          "Y==1 & X==1 & M==1",
##                          "Y==1 & X==1 & W==0",
##                          "Y==1 & X==1 & W==1"),
##             using = "parameters",
##             expand_grid = TRUE)



design <- 
  declare_population(
    N = 2, 
    time = 0:1,
    U_time = rnorm(N),
    potential_outcomes(
      Y ~ time_trend * time + time_specific_effect * U_time + 0.5 * Z
    ),
    Z = 1
  ) + 
  declare_estimand(TET = Y_Z_1 - Y_Z_0, subset = time == 1) + 
  declare_measurement(Y = ifelse(time == 0, Y_Z_0, Y_Z_1)) + 
  declare_estimator(
    estimate = Y[time == 1] - Y[time == 0], 
    estimand_label = "TET", handler = summarize)

designs <- redesign(
  design, time_trend = c(0, 0.5), time_specific_effect = c(0, 1))





kable(get_diagnosands(diagnosis) %>% select(time_trend, time_specific_effect, bias, `se(bias)`), booktabs = TRUE, digits = 3)

design <- 
  declare_population(
    N = 2, 
    time = 1,
    U_unit = rnorm(N),
    potential_outcomes(Y ~ 0.5 * Z + unit_specific_effect * U_unit),
    Z = if_else(U_unit == max(U_unit), 1, 0)
  ) + 
  declare_estimand(TET = Y_Z_1 - Y_Z_0, subset = time == 1) + 
  declare_measurement(Y = ifelse(Z == 0, Y_Z_0, Y_Z_1)) + 
  declare_estimator(
    estimate = Y[Z == 1] - Y[Z == 0], 
    estimand_label = "TET", handler = summarize)

designs <- redesign(design, unit_specific_effect = c(0, 1))





kable(get_diagnosands(diagnosis) %>% select(unit_specific_effect, bias, `se(bias)`), booktabs = TRUE, digits = 3)

design <- 
  declare_population(
    unit = add_level(N = 2, U_unit = rnorm(N, sd = 0.5), Z = if_else(U_unit == max(U_unit), 1, 0)),
    period = add_level(N = 2, time = 0:1, U_time = rnorm(N), nest = FALSE),
    unit_period = cross_levels(
      by = join(unit, period), 
      U = rnorm(N, sd = 0.01),
      potential_outcomes(
        Y ~ time_trend * 0.5 * time + 
          time_specific_effect * U_time + 
          unit_specific_effect * U_unit + 
          1.25 * Z + U)
    )
  ) + 
  declare_estimand(TET = Y_Z_1 - Y_Z_0, subset = time == 1) + 
  declare_measurement(Y = if_else(Z == 0 | period == 1, Y_Z_0, Y_Z_1)) + 
  declare_estimator(
    estimate = 
      (mean(Y[Z == 1 & time == 2]) - mean(Y[Z == 1 & time == 1])) - 
      (mean(Y[Z == 0 & time == 2]) - mean(Y[Z == 0 & time == 1])), 
    estimand_label = "ATT", handler = summarize)

designs <- redesign(design, 
                    time_trend = c(0, 0.5), 
                    time_specific_effect = c(0, 1), 
                    unit_specific_effect = c(0, 1))





kable(get_diagnosands(diagnosis) %>% select(time_trend, time_specific_effect, unit_specific_effect, bias, `se(bias)`), booktabs = TRUE, digits = 3)

synth_weights_tidy <- function(data, predictors, time.predictors.prior, dependent, unit.variable, time.variable, treatment.identifier, controls.identifier) {
  dataprep.out <- dataprep(
    foo = data,
    predictors = predictors,
    predictors.op = "mean",
    time.predictors.prior = time.predictors.prior,
    dependent = dependent,
    unit.variable = unit.variable,
    time.variable = time.variable,
    treatment.identifier = treatment.identifier,
    controls.identifier = controls.identifier, 
    time.optimize.ssr = time.predictors.prior,
    time.plot = time.predictors.prior)
  capture.output(fit <- synth(data.prep.obj = dataprep.out))
  tab <- synth.tab(dataprep.res = dataprep.out, synth.res = fit) 
  
  weights_df <- tab$tab.w %>% mutate(synth_weights = w.weights) %>% 
    dplyr::select(synth_weights, !!unit.variable := unit.numbers)
  
  data %>%
    left_join(weights_df) %>%
    mutate(synth_weights = replace_na(synth_weights, 1))
}


design <- 
  declare_population(
    units = add_level(N = 10, unit_ID = 1:10, U_unit = rnorm(N), X = rnorm(N), Z = if_else(unit_ID == 1, 1, 0)), # if_else(U_unit == max(U_unit), 1, 0)),
    periods = add_level(N = 3, time = -1:1, U_time = rnorm(N), nest = FALSE),
    unit_periods = cross_levels(
      by = join(units, periods), 
      U = rnorm(N),
      potential_outcomes(Y ~ time_trend * 0.5 * time + time_specific_effect * U_time + 
          unit_specific_effect * U_unit + 
            1.25 * Z + X + U),
      Y = if_else(Z == 0 | time <= 0, Y_Z_0, Y_Z_1)
    )
  ) + 
  declare_estimand(ATT = mean(Y_Z_1 - Y_Z_0), subset = time == 1) + 
  declare_measurement(predictors = "X",
                    time.predictors.prior = -1:0,
                    dependent = "Y",
                    unit.variable = "unit_ID",
                    time.variable = "time",
                    treatment.identifier = 1,
                    controls.identifier = 2:10, 
                    handler = synth_weights_tidy) +
  declare_estimator(Y ~ Z, subset = time == 1, weights = synth_weights, 
                    model = lm_robust, label = "synth")

## designs <- redesign(design,
##                     time_trend = c(0, 0.5),
##                     time_specific_effect = c(0, 1),
##                     unit_specific_effect = c(0, 1))
## 
## diagnosis <- diagnose_design(designs, sims = 5000, diagnosands = declare_diagnosands(bias = mean(estimate - estimand, na.rm = TRUE)))
## 
## get_diagnosands(diagnosis) %>% select(time_trend, time_specific_effect, unit_specific_effect, bias, `se(bias)`) %>% round(2)
